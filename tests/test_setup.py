"""Offline safety checks. Run: python3 -m unittest discover -s tests -v"""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]


class SetupTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="dotfiles setup ")
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name).resolve()
        self.home = self.base / "home"
        self.home.mkdir()
        self.repo = self.base / "checkout with spaces"
        self.repo.mkdir()
        # Copy only versioned config sources, never local credentials/state or
        # ignored plugin checkouts. Include new setup files before they are staged.
        tracked = subprocess.check_output(
            ["git", "ls-files", "-z"], cwd=ROOT
        ).decode().split("\0")
        sources = set(tracked) | {
            "setup-macos.sh", "Brewfile", "Brewfile.apps",
            ".config/alacritty/alacritty.toml", "scripts/bootstrap-nvim.lua",
        }
        for relative in filter(None, sources):
            source = ROOT / relative
            target = self.repo / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            if source.is_symlink():
                target.symlink_to(os.readlink(source))
            else:
                shutil.copy2(source, target)
        # Let link-only tests run on Linux and in root-owned CI containers too.
        self.bin = self.base / "bin"
        self.bin.mkdir()
        for name, body in {
            "uname": 'if [ "$1" = "-m" ]; then echo arm64; else echo Darwin; fi',
            "id": 'if [ "$1" = "-u" ]; then echo 501; else echo testuser; fi',
        }.items():
            path = self.bin / name
            path.write_text("#!/bin/sh\n" + body + "\n")
            path.chmod(0o755)
        self.env = dict(os.environ, HOME=str(self.home),
                        PATH=str(self.bin) + os.pathsep + os.environ["PATH"])

    def run_setup(self, *args, success=True):
        result = subprocess.run(
            ["/bin/bash", str(self.repo / "setup-macos.sh"), *args],
            cwd=self.base, env=self.env, capture_output=True, text=True,
        )
        if success:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0)
        return result

    def test_links_backup_and_repeat_run(self):
        (self.home / ".vimrc").write_text("old editor settings")
        (self.home / ".vim").mkdir()
        (self.home / ".vim" / "local.vim").write_text("keep me")
        (self.home / ".config" / "fish").mkdir(parents=True)
        (self.home / ".config" / "fish" / "fish_variables").write_text("local state")
        (self.home / ".gitconfig").symlink_to(self.home / "missing")
        self.run_setup("--links-only")
        self.assertEqual((self.home / ".vimrc").resolve(), self.repo / ".vimrc")
        self.assertEqual((self.home / ".config" / "nvim").resolve(), self.repo / ".vim")
        self.assertTrue((self.home / ".config" / "nvim" / "init.vim").is_file())
        vscode = self.home / "Library/Application Support/Code/User/settings.json"
        self.assertEqual(vscode.resolve(), self.repo / ".config/Code/User/settings.json")
        self.assertEqual((self.home / ".config/fish/fish_variables").read_text(), "local state")
        self.assertFalse((self.home / ".config/spectrwm").exists())
        backup = list((self.home / ".dotfiles-backups").iterdir())
        self.assertEqual(len(backup), 1)
        self.assertEqual((backup[0] / ".vimrc").read_text(), "old editor settings")
        self.assertEqual((backup[0] / ".vim/local.vim").read_text(), "keep me")
        self.assertTrue((backup[0] / ".gitconfig").is_symlink())
        self.run_setup("--links-only")
        self.assertEqual(list((self.home / ".dotfiles-backups").iterdir()), backup)

    def test_existing_config_symlink_does_not_create_link_loops(self):
        (self.home / ".config").symlink_to(self.repo / ".config")
        self.run_setup("--links-only")
        self.assertEqual((self.home / ".config/nvim").resolve(), self.repo / ".vim")
        self.assertFalse((self.repo / ".config/ghostty/config").exists())
        self.run_setup("--links-only")

    def test_apps_manifest_contains_only_requested_apps(self):
        entries = [line.strip() for line in (self.repo / "Brewfile.apps").read_text().splitlines()
                   if line.strip() and not line.lstrip().startswith("#")]
        self.assertEqual(entries, [
            'cask "ghostty"', 'cask "dropbox"', 'cask "betterdisplay"',
            'cask "chatgpt"', 'cask "zen"', 'cask "brave-browser"', 'cask "firefox"',
        ])

    def test_pi_and_herdr_are_in_default_installation(self):
        entries = (self.repo / "Brewfile").read_text().splitlines()
        self.assertIn('brew "pi-coding-agent"', entries)
        self.assertIn('brew "herdr"', entries)
        result = self.run_setup("--dry-run", "--no-shell")
        self.assertNotIn("Brewfile.apps", result.stdout)
        self.assertIn("/bin/pi --version", result.stdout)
        self.assertIn("/bin/herdr --version", result.stdout)
        self.assertIn("for tool in pi herdr", result.stdout.replace("\\ ", " "))
        self.assertEqual(list(self.home.iterdir()), [])

    def test_full_dry_run_has_no_side_effects(self):
        (self.home / ".vimrc").write_text("untouched")
        result = self.run_setup("--dry-run", "--apps")
        self.assertIn("Brewfile.apps", result.stdout)
        self.assertIn("DOTFILES_NVIM_STAGE=plugins", result.stdout)
        self.assertIn("DOTFILES_NVIM_STAGE=editor", result.stdout)
        self.assertIn("gopls@latest", result.stdout)
        self.assertIn("cargo-nextest", result.stdout)
        self.assertEqual(list(self.home.iterdir()), [self.home / ".vimrc"])
        self.assertEqual((self.home / ".vimrc").read_text(), "untouched")
        self.assertFalse((self.repo / ".vim/pack").exists())

    @unittest.skipUnless(shutil.which("fish"), "fish not installed")
    def test_fish_starts_without_a_rust_env_or_machine_specific_ssh_key(self):
        self.run_setup("--links-only")
        (self.home / ".local/bin").mkdir(parents=True)
        (self.home / ".cargo/bin").mkdir(parents=True)
        env = dict(self.env, XDG_CONFIG_HOME=str(self.home / ".config"))
        result = subprocess.run(
            [shutil.which("fish"), "-c",
             'false; fish_prompt; true; fish_prompt; fish_title; fish_title printf; '
             'test "$EDITOR" = nvim; and test "$GOBIN" = "$HOME/.local/bin"; '
             'and contains -- "$HOME/.local/bin" $PATH; '
             'and contains -- "$HOME/.cargo/bin" $PATH'],
            cwd=self.base, env=env, capture_output=True, text=True, timeout=15,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(result.stderr, "")
        self.assertIn("|> ", result.stdout)
        self.assertFalse((self.home / ".ssh").exists())

    @unittest.skipUnless(shutil.which("nvim"), "Neovim not installed")
    def test_neovim_config_boots_and_initializes_minpac_on_first_call(self):
        self.run_setup("--links-only")
        autoload = self.repo / ".vim/pack/minpac/opt/minpac/autoload"
        autoload.mkdir(parents=True)
        # Autoload functions must not exist until PackInit calls minpac#init.
        # This catches the original exists('*minpac#init') bootstrap bug.
        (autoload / "minpac.vim").write_text("""
function! minpac#init(options) abort
  let g:minpac#opt = a:options
  let g:minpac#pluglist = {}
endfunction
function! minpac#add(name, ...) abort
  let g:minpac#pluglist[a:name] = get(a:000, 0, {})
endfunction
""")
        smoke = self.base / "smoke.lua"
        smoke.write_text("""
local ok, err = pcall(function()
  assert(vim.v.errmsg == '', vim.v.errmsg)
  vim.fn.PackInit()
  assert(vim.g['minpac#opt'].dir == vim.fn.expand('~/.vim'))
  assert(vim.tbl_count(vim.g['minpac#pluglist']) == 9)
  assert(vim.g['minpac#pluglist']['nvim-treesitter/nvim-treesitter'].branch == 'main')
  assert(#vim.g.dotfiles_treesitter_languages == 18)
  assert(#vim.g.coc_global_extensions == 10)
end)
if not ok then print(err); vim.cmd('cquit 1') else vim.cmd('qall!') end
""")
        env = dict(self.env, XDG_CONFIG_HOME=str(self.home / ".config"),
                   XDG_DATA_HOME=str(self.home / ".local/share"),
                   XDG_STATE_HOME=str(self.home / ".local/state"),
                   XDG_CACHE_HOME=str(self.home / ".cache"),
                   DOTFILES_TEST_SCRIPT=str(smoke))
        for key in ("NVIM_APPNAME", "VIMINIT", "EXINIT"):
            env.pop(key, None)
        result = subprocess.run(
            [shutil.which("nvim"), "--headless", "-i", "NONE",
             "--cmd", "let g:coc_start_at_startup = 0",
             "-c", "lua dofile(vim.env.DOTFILES_TEST_SCRIPT)"],
            env=env, capture_output=True, text=True, timeout=15,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_unknown_option_and_help_do_not_change_home(self):
        self.run_setup("--unknown", success=False)
        self.run_setup("--help")
        self.assertEqual(list(self.home.iterdir()), [])

    def test_refuses_to_move_checkout_ancestor(self):
        # Simulate someone placing the checkout inside the old ~/.vim directory.
        old_vim = self.home / ".vim"
        old_vim.mkdir()
        nested_repo = old_vim / "dotfiles"
        shutil.move(str(self.repo), nested_repo)
        self.repo = nested_repo
        result = self.run_setup("--links-only", success=False)
        self.assertIn("Refusing to replace directory containing the checkout", result.stderr)
        self.assertTrue((nested_repo / "setup-macos.sh").exists())


if __name__ == "__main__":
    unittest.main()
