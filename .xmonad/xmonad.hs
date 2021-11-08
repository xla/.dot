import qualified Data.Map as Map
import Data.Map (Map)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.Directory (getHomeDirectory)
import System.IO (Handle, hPutStrLn)
import Text.Printf (printf)

import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Hooks.DynamicLog (PP, dynamicLogWithPP, ppCurrent, ppHidden, ppHiddenNoWindows, ppLayout, ppOutput, ppSep, ppTitle, ppUrgent, ppWsSep, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (ToggleStruts(..), avoidStruts, docks, docksEventHook, docksStartupHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout (Full, Mirror, Tall)
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Util.Run (safeSpawn, spawnPipe)

data EnterPrompt = EnterPrompt String

instance XPrompt EnterPrompt where
    showXPrompt (EnterPrompt n) = " " ++ n ++ " "

main :: IO ()
main = do
    home <- getHomeDirectory
    -- xmproc <- spawnPipe "sh $HOME/.config/polybar/launch.sh"
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ docks $ desktopConfig
        { terminal           = myTerm
        , focusedBorderColor = "powder blue"
        , normalBorderColor  = "#0a0a0a"
        , borderWidth        = 1
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        , layoutHook         = avoidStruts $ smartBorders $ myLayout
        , logHook            = dynamicLogWithPP (myBarConfig xmproc)
        , manageHook         = manageDocks <+> manageHook def
        -- , startupHook        = docksStartupHook <+> startupHook def
        , workspaces         = [ "λ", "α", "β", "γ", "δ" ]
        }

myBarConfig :: Handle -> PP
myBarConfig h = xmobarPP
    { ppCurrent         = xmobarColor white   black  . wrap " " " "
    , ppHiddenNoWindows = xmobarColor grey    black  . wrap " " " "
    , ppHidden          = xmobarColor light   dark   . wrap " " " "
    , ppUrgent          = xmobarColor black   red
    , ppLayout          = const ""
    , ppWsSep           = ""
    , ppSep             = ""
    , ppOutput          = hPutStrLn h
    , ppTitle           = const ""
    }
  where
    red   = "#ff0000"
    white = "#ffffff"
    black = "#000000"
    light = "#888888"
    dark  = "#333333"
    grey  = "#555555"

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F5),              spawn $ printf "scrot -u -e 'mv $f %s/Dropbox/Screenshots'" home)
       , ((modMask, xK_F6),              spawn $ printf "scrot -e 'mv $f %s/Dropbox/Screenshots'" home)
       , ((modMask, xK_F9),              safeSpawn "toggle-displays" [])
       , ((modMask, xK_F10),             safeSpawn "loginctl" ["lock-session"])
       , ((modMask, xK_Tab),             toggleWS)
       , ((modMask, xK_b),               sendMessage ToggleStruts)
       , ((modMask, xK_u),               sendMessage MirrorShrink)
       , ((modMask, xK_o),               sendMessage MirrorExpand)
       , ((modMask, xK_p),               rofi)
       -- , ((modMask .|. shiftMask, xK_q), confirmPrompt myXPConfig "Exit?" $ io (exitWith ExitSuccess))
       ]

myLayout = ResizableTall 1 (3/100) (1/2) [] ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full ||| ThreeColMid 1 (3/100) (1/2)

myTerm :: FilePath
myTerm = "kitty"

myXPConfig :: XPConfig
myXPConfig = def
  { position          = CenteredAt 0.5 0.09
  , alwaysHighlight   = True
  , height            = 80
  , promptBorderWidth = 1
  , font              = "xft:FiraMono-Regular:size=14"
  , borderColor       = "#888"
  , bgColor           = "#000"
}

-- confirmPrompt :: XPConfig -> String -> X () -> X ()
-- confirmPrompt config app func = mkXPrompt (EnterPrompt app) config (mkComplFunFromList []) $ const func

rofi :: X ()
rofi = safeSpawn
    "rofi" [ "-show", "run"
           , "-auto-select"
           , "-hide-scrollbar"
           , "-levenshtein-sort"
           , "-line-margin", "0"
           , "-lines", "5"
           , "-location", "0"
           , "-matching", "fuzzy"
           , "-padding", "6"
           , "-separator-style", "solid"
           , "-width", "40"
           ]

