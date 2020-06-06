-- My Xmonad config

--- Imports ---

-- Base
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W

-- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import System.IO
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedScratchpad


myTerminal :: [Char]
myTerminal = "gnome-terminal"


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myWorkspaces :: [String]
myWorkspaces = ["dev", "www", "proc", "mus", "game", "chat", "vid", "other", "other2"]

myKeys :: [([Char], X ())]
myKeys = [("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle") -- use pactl b/c amixer didn't work
         , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
         , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        ]


main:: IO()
main =
  do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks def
    {manageHook = manageDocks <+> manageHook def
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                         , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                         , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                         , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                         , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                         , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                         , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                         , ppExtras  = [windowCount]                           -- # of windows current workspace
                         , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
    , startupHook = myStartupHook
    , terminal    = myTerminal
    , modMask     = mod4Mask
    , borderWidth = 3
    , workspaces = myWorkspaces
    , handleEventHook = fullscreenEventHook
    , layoutHook = avoidStruts  $  layoutHook def
    , focusedBorderColor = "#434C5E"
    } `additionalKeysP` myKeys
   
myStartupHook :: X()
myStartupHook = do
  spawnOnce "feh --bg-scale ~/dotfiles/pikachu.jpg"
