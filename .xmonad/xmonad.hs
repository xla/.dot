import qualified Data.Map as Map
import Data.Map (Map)
import System.Directory (getHomeDirectory)
import System.IO (Handle, hPutStrLn)
import Text.Printf (printf)

import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (PP, dynamicLogWithPP, ppCurrent, ppHidden, ppHiddenNoWindows, ppLayout, ppOutput, ppSep, ppTitle, ppUrgent, ppWsSep, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run (safeSpawn, spawnPipe)

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ desktopConfig
        { terminal           = myTerm
        , focusedBorderColor = "#555"
        , normalBorderColor  = "#0a0a0a"
        , borderWidth        = 1
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        -- , layoutHook         = avoidStruts $ smartBorders $ spacing 16 $ layoutHook def
        , layoutHook         = avoidStruts $ smartBorders $ spacing 16 $ ThreeColMid 1 (3/100) (1/2)
        , logHook            = dynamicLogWithPP (myBarConfig xmproc)
        , manageHook         = manageDocks <+> manageHook def
        , workspaces = [ "1", "2", "3", "4", "5" ]
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
       [ ((modMask, xK_F5),   spawn $ printf "scrot -u -e 'mv $f %s/Dropbox/Screenshots'" home)
       , ((modMask, xK_F6),   spawn $ printf "scrot -e 'mv $f %s/Dropbox/Screenshots'" home)
       , ((modMask, xK_F10),  slock)
       , ((modMask, xK_Tab),  toggleWS)
       , ((modMask, xK_p),    rofi)
       ]

myTerm :: FilePath
myTerm = "st -e /bin/fish"

rofi :: X ()
rofi = safeSpawn
    "rofi" [ "-show", "run"
           , "-color-active", "#fdf6e3,#268bd2,#eee8d5,#268bd2,#fdf6e3"
           , "-color-normal", "#444,#bbb,#444,#bbb,#000"
           , "-color-urgent", "#fdf6e3,#dc322f,#eee8d5,#dc322f,#fdf6e3"
           , "-color-window", "#444"
           , "-font", "PragmataPro 13"
           , "-hide-scrollbar"
           , "-levenshtein-sort"
           , "-lines", "5"
           , "-matching", "fuzzy"
           , "-padding", "2"
           , "-separator-style", "solid"
           , "-width", "25"
           ]

slock :: X ()
slock = safeSpawn
      "slock" []
