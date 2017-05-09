import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.StackSet
import XMonad.ManageHook
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS

import System.Directory
import System.IO
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

termName :: FilePath
termName = "st -e /bin/fish"

main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh $ desktopConfig
        { terminal           = termName
        , focusedBorderColor = "#555"
        , normalBorderColor  = "#000"
        , borderWidth        = 1
        , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
        , keys               = myKeys home
        , logHook            = dynamicLogWithPP (barConfig xmproc)
        , layoutHook         = avoidStruts $ smartBorders $ spacing 16 $ layoutHook def
        , manageHook         = manageDocks <+> manageHook def
        }

startup :: X ()
startup = do
    -- Open a terminal if none are open.
    withWindowSet $ \wins -> do
        result <- filterM (runQuery (title =? "fish /home/xla")) (allWindows wins)
        when (null result) $
            safeSpawn "st" ["-e", "/bin/fish"]

barConfig :: Handle -> PP
barConfig h = xmobarPP
    { ppCurrent         = xmobarColor white   black  . wrap " " " "
    , ppHiddenNoWindows = xmobarColor grey    black  . wrap " " " "
    , ppHidden          = xmobarColor light   dark   . wrap " " " "
    , ppUrgent          = xmobarColor black   red
    , ppLayout          = const ""
    , ppWsSep           = ""
    , ppSep             = "  "
    , ppOutput          = hPutStrLn h
    , ppTitle           = const ""
    }
  where
    red   = "red"
    white = "white"
    black = "black"
    light = "#888888"
    dark  = "#333333"
    grey  = "#555555"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myKeys :: FilePath -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys home conf@XConfig { XMonad.modMask = modMask } =
    Map.union ks (XMonad.keys def conf)
  where
    ks = Map.fromList
       [ ((modMask, xK_F12),                  safeSpawn "sudo" ["systemctl", "suspend"])
       , ((modMask .|. shiftMask, xK_F10),    safeSpawn "systemctl" ["hibernate"])
       , ((modMask, xK_F10),                  slock)
       , ((modMask, xK_p),                    dmenu)
       , ((modMask .|. shiftMask, xK_p),      rofi)
       , ((modMask, xK_Tab), toggleWS)
       , ((noModMask, xK_Print),              spawn $ printf "scrot -u -e 'mv $f %s/screenshots'" home)
       ]
    dmenu :: X ()
    dmenu = safeSpawn
        "dmenu_run" [ "-fn", "PragmataPro:size=13"
                    , "-i"
                    , "-nb", "black"
                    , "-nf", "#bbb"
                    , "-sb", "#444"
                    , "-sf", "white"
                    ]
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
