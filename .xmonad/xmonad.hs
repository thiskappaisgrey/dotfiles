-- My Xmonad config
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
myTerminal :: [Char]
myTerminal = "alacritty"

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
    , terminal    = myTerminal
    , modMask     = mod4Mask
    , borderWidth = 3
    , layoutHook = avoidStruts  $  layoutHook def
    }
