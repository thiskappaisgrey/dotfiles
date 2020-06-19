-- My Xmonad config
-- Instructions to use stack for xmonad https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack/

--- Imports ---

-- Base
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks       ( docks
                                                , avoidStruts
                                                , manageDocks
                                                , ToggleStruts(..)
                                                )
import qualified XMonad.StackSet               as W

-- Layouts
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Spacing
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Renamed          ( renamed
                                                , Rename(Replace)
                                                )
-- Utils
import           XMonad.Util.Run                ( spawnPipe
                                                , runProcessWithInput
                                                )
import           XMonad.Util.SpawnOnce
import           System.IO
import           XMonad.Util.EZConfig           ( additionalKeysP )
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Util.NamedScratchpad

-- Prompts
import           XMonad.Prompt
import           XMonad.Prompt.Input
-- import           XMonad.Prompt.Man
-- import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell            ( shellPrompt )
-- import           XMonad.Prompt.Ssh
-- import           XMonad.Prompt.XMonad
-- import           Control.Arrow                  ( first )

-- Data
import           Data.Char                      ( isSpace )
import           Data.List
myTerminal :: [Char]
myTerminal = "alacritty"


-- Copy-pasted from Mr. Distrotube! Thank you! https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
-- mySpacing
--   :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing'
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border 0 0 0 0) True (Border i i i i) True

-- My layouts
myLayout = tiled ||| Mirror tiled ||| Full
 where
     -- default tiling algorithm partitions the screen into two panes
  tiled = renamed [Replace "tall"] $ mySpacing' 2 $ ResizableTall nmaster
                                                                  delta
                                                                  ratio
                                                                  []

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

windowCount :: X (Maybe String)
windowCount =
  gets
    $ Just
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

myWorkspaces :: [String]
myWorkspaces =
  ["dev", "www", "proc", "mus", "game", "chat", "vid", "other", "other2"]



-- Prompts -- Also learned about from distrotube

-- calcPrompt requires a cli calculator called qalcualte-gtk.
-- You could use this as a template for other custom prompts that
-- use command line programs that return a single line of output.
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c (trim ans)
  ?+ \input -> liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where trim = f . f where f = reverse . dropWhile isSpace
myXPConfig :: XPConfig
myXPConfig = def { font                = "xft:Mononoki Nerd Font:size=16"
                 , bgColor             = "#2E3440"
                 , fgColor             = "#D8DEE9"
                 , bgHLight            = "#BF616A"
                 , fgHLight            = "#3B4252"
                 , borderColor         = "#535974"
                 , promptBorderWidth   = 0
                 , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
                 , height              = 20
                 , historySize         = 256
                 , historyFilter       = id
                 , defaultText         = []
                 , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
                 , showCompletionOnTab = False
                 , searchPredicate     = isPrefixOf
                 , alwaysHighlight     = True
                 , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
                 }

-- Scratchpads, very useful feature
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
-- run htop in xterm, find it by title, use default floating window placement
    NS "htop"
       (myTerminal ++ " -t htop -e htop")
       (title =? "htop")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "spt"
       (myTerminal ++ " -t spt -e spt")
       (title =? "spt")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
-- run terminal, find it by title, place it in the floating window
-- 1/6 of screen width from the left, 1/6 of screen height
-- from the top, 2/3 of screen width by 2/3 of screen height
  , NS "terminal"
    -- alacritty -t sets the window title
       (myTerminal ++ " -t scratchpad")
       (title =? "scratchpad")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

-- NOTE For later, emacsclient -c -e "(=rss)" to launch emacs based applications.
myKeys :: [([Char], X ())]
myKeys =
  [
        -- use amixer to set the microphone volume: https://askubuntu.com/questions/27021/setting-microphone-input-volume-using-the-command-line
        -- xbacklight controls the brightness: https://wiki.archlinux.org/index.php/backlight#xbacklight and https://askubuntu.com/questions/715306/xbacklight-no-outputs-have-backlight-property-no-sys-class-backlight-folder
        -- xf86-video-intel
    ( "<XF86AudioMute>"
    , spawn "amixer set Master toggle"
    )  -- Bug prevents it from toggling correctly in 12.04.
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ( "<XF86AudioRaiseVolume>"
    , spawn "amixer set Master 5%+ unmute"
    )
        --("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle") -- use pactl if amixer doesn't work
         --, ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
         --, ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  -- PLAY/PAUSE
  -- My thinkpad keyboard doesn't have audio keys, so I would need two keybindings
  -- Neeeds playerctl to work.
  , ("<XF86AudioPlay>", spawn "playerctl play-pause")
  , ("<XF86AudioPrev>", spawn "playerctl prev")
  , ( "<XF86AudioNext>"
    , spawn "playerctl next"
    )
  -- BRIGHTNESS
  -- brigntnessctl needs to be installed to work
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
  , ( "<XF86MonBrightnessDown>"
    , spawn "brightnessctl s 10%-"
    )
  -- APPLICATIONS
  , ("M-f", spawn "emacsclient -create-frame --alternate-editor=\"\" ")
  , ("M-b", spawn "brave")
  , ( "M-S-r"
    , spawn "xmonad --recompile && xmonad --restart"
    )
  -- SCRATCHPADS -- very useful feature
  , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
  , ( "M-C-h"
    , namedScratchpadAction myScratchPads "htop"
    )
  , ( "M-C-t"
    , namedScratchpadAction myScratchPads "spt"
    )
  -- PROMPTS
  -- Use xmonad-contrib's builtin prompt rather than dmenu
  , ("M-p", shellPrompt myXPConfig)
  , ( "M-q"
    , calcPrompt myXPConfig "qalc"
    ) -- example calculator prompt. Also comes with a useful calculator!

  --- MISC
  , ("M-s", sendMessage ToggleStruts)         -- Toggles struts
  ]



-- Pretty fg
myPP :: PP
myPP = namedScratchpadFilterOutWorkspacePP $ def
  { ppUrgent          = xmobarColor "red" "yellow"
  , ppCurrent         = xmobarColor "#4C566A" "#A3BE8C" . wrap "| " " |" -- Current workspace in xmobar
  , ppVisible         = xmobarColor "#A3BE8C" ""                -- Visible but not current workspace
  , ppHidden          = xmobarColor "#81A1C1" "" . wrap " " " "   -- Hidden workspaces in xmobar
  , ppHiddenNoWindows = xmobarColor "#BF616A" ""        -- Hidden workspaces (no windows)
  , ppTitle           = xmobarColor "#D8DEE9" "" . shorten 60     -- Title of active window in xmobar
  , ppSep             = "<fc=#D8DEE9> | </fc>"                     -- Separators in xmobar
  , ppExtras          = [windowCount]                           -- # of windows current workspace
  , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
  }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-scale ~/Wallpapers/dark-city.jpg"
  spawnOnce "picom &"
  spawnOnce "emacs --daemon"

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchPads <+> manageHook def

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobars/xmobar-nord"
  xmonad
    $                 docks def
                        { manageHook         = myManageHook <+> manageDocks
                        , logHook = dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc }
                        , startupHook        = myStartupHook
                        , terminal           = myTerminal
                        , modMask            = mod4Mask
                        , borderWidth        = 3
                        , workspaces         = myWorkspaces
                        , handleEventHook    = fullscreenEventHook
                        , layoutHook         = avoidStruts $ myLayout
                        , focusedBorderColor = "#434C5E"
                        }
    `additionalKeysP` myKeys
