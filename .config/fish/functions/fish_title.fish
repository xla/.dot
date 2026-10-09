# Use the current git repository name rather than the raw working directory.
# Fish passes the running command as the first argument (empty at the prompt).
function fish_title
  set -l title (prompt_pwd)
  set -l git_root (command git rev-parse --show-toplevel 2>/dev/null)
  if test -n "$git_root"
    set title (basename "$git_root")
  end

  if set -q argv[1]; and test -n "$argv[1]"
    echo "$argv[1] - $title"
  else
    echo "$title"
  end
end
