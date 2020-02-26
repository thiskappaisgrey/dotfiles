-- My Xmonad config
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import System.IO

myTerminal :: [Char]
myTerminal = "gnome-terminal"

main:: IO()
main =
  do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    {
      manageHook = manageDocks <+> manageHook def
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    , startupHook = myStartupHook
    , terminal    = myTerminal
    , modMask     = mod4Mask
    , borderWidth = 3
    , layoutHook = avoidStruts  $  layoutHook def
    }
myStartupHook = do
  spawnOnce "feh --bg-scale ~/dotfiles/pikachu.jpg"
