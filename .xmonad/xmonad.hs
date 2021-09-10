{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-missing-signatures #-}
-- My Xmonad config
--- Imports ---

-- Base
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..)
                                                , avoidStruts
                                                , docks
                                                , manageDocks
                                                )
import qualified XMonad.StackSet               as W

-- Layouts
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.Magnifier
-- Might want to copy Altecration's tabbed layout(combining a web-page with a terminal, useful for web-dev)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed          ( Rename(Replace)
                                                , renamed
                                                )
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing

-- import           XMonad.Prompt.Ssh
-- import           Control.Arrow                  ( first )
import           System.IO
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
-- Prompts
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher     as AL
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Input
-- import           XMonad.Prompt.Man
import           XMonad.Prompt.Pass
import           XMonad.Prompt.Shell            ( shellPrompt )
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
-- Utils
import           XMonad.Util.Run                ( runInTerm
                                                , runProcessWithInput
                                                , spawnPipe
                                                )
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare   ( getSortByIndex )
-- import           XMonad.Prompt.XMonad

-- Data
import           Data.Char                      ( isSpace )
import           Data.List

import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaceOrder
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WithAll         ( killAll )
import           XMonad.Layout.PerWorkspace     ( onWorkspace )
-- Actions
import           XMonad.Util.NamedActions

import           System.Exit

-- Default apps
myTerminal = "alacritty"
myBrowser = "brave --profile-directory=\"Default\""
myBrowser2 = "nyxt"
-- This will start the emacs server if not already started
myEditor = "emacsclient -create-frame --alternate-editor=\"\""

-- Copy-pasted from Mr. Distrotube! Thank you! https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
-- mySpacing
--   :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
-- For spaces between windows. Not used for now!
mySpacing
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-----------------------------------------------------------------
--                           Layouts                           --
-----------------------------------------------------------------

myLayout = onWorkspace wsFloat simpleFloat
  $ avoidStruts (mirrorTall ||| magnified ||| full)
 where
     -- default tiling algorithm partitions the screen into two panes
   -- add mySpacing to the composition to add spacing to tiled windows.
  tiled = renamed [Replace "tall"] $ smartBorders $ ResizableTall nmaster
                                                                  delta
                                                                  ratio
                                                                  []
   where
        -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1 / 2

-- Percent of screen to increment by when resizing panes
    delta   = 3 / 100
  full = noBorders Full
  -- Magnified layout with one window taking up 60% of screen
  magnified =
    renamed [Replace "magified"] $ smartBorders $ magnifiercz' 1.4 $ Tall
      nmaster
      delta
      ratio
   where
        -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta   = 3 / 100
    -- Default proportion of screen occupied by master pane
    ratio   = 60 / 100
  mirrorTall = Mirror (Tall 2 (3/100) (4/5))
-- Number of windows in a workspace, not really needed though
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

-----------------------------------------------------------------
--                          Workspaces                         --
-----------------------------------------------------------------
wsMain = "main"
wsTerm = "term"
wsFloat = "float"
wsGame = "game"
wsMusic = "music"
-- wsVidEdit = "vid-edit"
-- wsVirt = "virt"
myWorkspaces = [wsMain, wsTerm, wsFloat, wsGame]

myProjects :: [Project]
myProjects =
  [ Project
    { projectName      = wsMain
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn
                             wsMain
                             (myEditor ++ " --eval \"(org-agenda-list)\"")
    }
  , Project
    { projectName      = wsTerm
    , projectDirectory = "~/code"
    , projectStartHook = Just $ do
                           spawnOn wsTerm myTerminal
                           -- runInTerm "-t ytop" "ytop"
    }
  , Project { projectName      = wsFloat
            , projectDirectory = "~"
            , projectStartHook = Nothing
            }
  , Project
    { projectName      = wsGame
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn wsGame "steam"
    }
  , Project
    { projectName      = wsMusic
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawnOn wsMusic "brave --profile-directory=\"youtube\""
    }
  ]


calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt myXPConfig (trim' ans)
  ?+ \input -> liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where trim' = f . f where f = reverse . dropWhile isSpace

myXPConfig :: XPConfig
myXPConfig = def { font                = "xft:Liberation Mono:size=20"
                 , bgColor             = "#2E3440"
                 , fgColor             = "#D8DEE9"
                 , bgHLight            = "#BF616A"
                 , fgHLight            = "#3B4252"
                 , borderColor         = "#535974"
                 , promptBorderWidth   = 0
                -- , position            = Top
                 , position = CenteredAt { xpCenterY = 0.3, xpWidth = 0.5 }
                 , height              = 40
                 , historySize         = 256
                 , historyFilter       = id
                 , defaultText         = []
                 -- , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
                 , showCompletionOnTab = False
                 , searchPredicate     = fuzzyMatch
                 , sorter              = fuzzySort
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
-- TODO Extract NamedActions and put it into a custom addDescrKeys so that I can use it for my own version of Which-Key
-- Big thanks to https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs !
myKeys conf =
  let
    subKeys str ks = subtitle str : mkNamedKeymap conf ks
    -- screenKeys = ["w", "v", "z"]
    dirKeys   = ["n", "i", "y", "o"]
    arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
    wsKeys    = map show $ [1 .. 9] ++ [0]
    dirs      = [D, U, L, R]
    zipM m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as
    nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
      >>= \t -> windows . W.view $ t
    prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
      >>= \t -> windows . W.view $ t
    getSortByIndexNoSP =
      fmap (. namedScratchpadFilterOutWorkspace) getSortByIndex
  in
    subKeys
      "System"
      [

        -- use amixer to set the microphone volume: https://askubuntu.com/questions/27021/setting-microphone-input-volume-using-the-command-line
        -- xbacklight controls the brightness: https://wiki.archlinux.org/index.php/backlight#xbacklight and https://askubuntu.com/questions/715306/xbacklight-no-outputs-have-backlight-property-no-sys-class-backlight-folder
        -- xf86-video-intel
    -- Audio, use pulseaudio to change volume
        ( "<XF86AudioMute>"
        , addName "Toggle Mute"
          $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
        ) -- use pactl if amixer doesn't work
      , ( "<XF86AudioLowerVolume>"
        , addName "Lower Volume"
          $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"
        )
      , ( "<XF86AudioRaiseVolume>"
        , addName "Raise Volume"
          $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"
        )
  -- PLAY/PAUSE
  -- My thinkpad keyboard doesn't have audio keys, so I would need two keybindings
  -- Neeeds playerctl to work.
  -- NOTE Also don't forget to install: https://github.com/hoyon/mpv-mpris in order to have mpv work with the MPRIS interface
      , ("<XF86AudioPlay>", addName "Play/Pause" $ spawn "playerctl play-pause")
      , ("<XF86AudioPrev>", addName "Prev Song" $ spawn "playerctl prev")
      , ("<XF86AudioNext>", addName "Next Song" $ spawn "playerctl next")
  -- BRIGHTNESS
  -- brigntnessctl needs to be installed to work
      , ( "<XF86MonBrightnessUp>"
        , addName "Raise Brightness" $ spawn "brightnessctl s +10%"
        )
      , ( "<XF86MonBrightnessDown>"
        , addName "Lower Brightness" $ spawn "brightnessctl s 10%-"
        )
      , ( "M-S-r"
        , addName "Recompile & Restart Xmonad"
          $ spawn "xmonad --recompile && xmonad --restart"
        )
      , ( "M-S-q"
        , addName "Exit XMonad" $ confirmPrompt myXPConfig "Quit XMonad" $ io
          exitSuccess
        )
  -- Screenshots
      , ("C-<Print>"  , addName "Screenshot" $ spawn "scrot")
      , ("C-S-<Print>", addName "Screenshot" $ spawn "scrot -s")
      ]
    ^++^ subKeys
           "Launchers"
           [ ("M-f", addName "Launch Emacs" $ spawn myEditor)
           , ( "M-a"
             , addName "Launch Agenda"
               $ spawn (myEditor ++ " --eval \"(org-agenda-list)\"")
             )
           , ("M-b"       , addName "Launch My Browser" $ spawn myBrowser)
           , ("M-S-b"       , addName "Launch My Other Browser" $ spawn myBrowser2)
           , ("M-<Return>", addName "Launch Terminal" $ spawn myTerminal)
           , ("M-<Space>" , addName "Shell/App Prompt" $ shellPrompt myXPConfig)
           , ("M-S-m", addName "Launch MPV" $ AL.launchApp myXPConfig "mpv")
           , ( "M-c"
             , addName "Launch Org-capture" $ spawn "~/.emacs.d/bin/org-capture"
             )
           , ("M-s s"  , addName "Cancel submap" $ return ())
           , ("M-s M-s", addName "Cancel submap" $ return ())
           , ("M-z", addName "Switch to halmak layout" $ spawn "setxkbmap us -variant norman")
           , ("M-<Backspace>", addName "Switch to qwerty layout" $ spawn "setxkbmap us")
           ]
    ^++^ subKeys
           "Scratchpads"
           [
      -- SCRATCHPADS -- very useful feature
             ( "M-C-<Return>"
             , addName "Terminal Scratchpad"
               $ namedScratchpadAction myScratchPads "terminal"
             )
           , ( "M-C-t"
             , addName "Htop Scratchpad"
               $ namedScratchpadAction myScratchPads "htop"
             )
           ]
    ^++^ subKeys
           "Layouts"
           [ ("M-s"    , addName "Toggle Struts" $ sendMessage ToggleStruts)         -- Toggles struts
           , ("M-<Tab>", addName "Cycle all layouts" $ sendMessage NextLayout)
           , ( "M-S-<Tab>"
             , addName "Reset layout" $ setLayout $ XMonad.layoutHook conf
             )
           ]
    ^++^ subKeys
           "Windows"
           ([ ("M-S-c", addName "Kill" kill)
            , ( "M-C-c"
              , addName "Kill all" $ confirmPrompt myXPConfig "kill all" killAll
              )
            , ("M-["  , addName "Shrink Master Area" $ sendMessage Shrink) -- %! Shrink the master area
            , ("M-]"  , addName "Expand Master Area" $ sendMessage Expand) -- %! Expand the master area
              -- Useful for the Full layout
            , ("M-S-"++(dirKeys !! 0), addName "Next Window" $ windows W.focusUp)
            , ("M-S-"++ (dirKeys !! 1), addName "Prev Window" $ windows W.focusDown)
            , ("M-m", addName "Swap Master Window" $ windows W.swapMaster)
            , ( "M-t"
              , addName "Push windows back into tiling"
              $ withFocused
              $ windows
              . W.sink
              )
            ]
           ++ zipM' "M-"   "Navigate window" dirKeys   dirs windowGo   True
    -- ++ zipM' "M-S-"               "Move window"                               dirKeys dirs windowSwap True
           ++ zipM' "M-C-" "Move window"     dirKeys   dirs windowSwap True
           -- ++ zipM "M-C-"
           --         "Merge w/sublayout"
           --         dirKeys
           --         dirs
           --         (sendMessage . pullGroup)
           ++ zipM' "M-"   "Navigate screen" arrowKeys dirs screenGo   True
           ++ zipM' "M-S-"
                    "Move window to screen"
                    arrowKeys
                    dirs
                    windowToScreen
                    True
           ++ zipM' "M-S-"
                    "Swap workspace to screen"
                    arrowKeys
                    dirs
                    screenSwap
                    True
           )
    ^++^ subKeys
           "Monitors"
           ( [
               ("M-,", addName "Next Monitor" nextScreen),
               ("M-.", addName "Previous Monitor" prevScreen),
               ("M-S-,", addName "Previous Monitor" swapNextScreen),
               ("M-S-.", addName "Previous Monitor" swapPrevScreen)
             ] )
    ^++^ subKeys
           "Workspaces"
           (  [ ( "M-;"
                , addName "Switch to Project" $ switchProjectPrompt myXPConfig
                )
              , ( "M-S-;"
                , addName "Shift to Project" $ shiftToProjectPrompt myXPConfig
                )
              , ( "M-C-;"
                , addName "Switch current project Directory"
                  $ changeProjectDirPrompt myXPConfig
                )
              , ("M-'"  , addName "Next non-empty WS" nextNonEmptyWS)
              , ("M-S-'", addName "Prev non-empty WS" prevNonEmptyWS)
              ]
           ++ zipM "M-"
                   "View      ws"
                   wsKeys
                   [0 ..]
                   (withNthWorkspace W.greedyView)
           ++ zipM "M-S-"
                   "Move w to ws"
                   wsKeys
                   [0 ..]
                   (withNthWorkspace W.shift)
           )
    ^++^ subKeys
           "Prompts"
           [
  -- Use xmonad-contrib's builtin prompt rather than dmenu
  -- Open applications
             ("M-q", addName "Qalc Prompt" $ calcPrompt myXPConfig "qalc") -- example calculator prompt. Also comes with a useful calculator!
  -- PASS - the UNIX password manager
           , ("M-p", addName "Get a Password" $ passPrompt myXPConfig)
           , ( "M-S-p"
             , addName "Generate a Password" $ passGeneratePrompt myXPConfig
             )
           -- , ("M-C-p", addName "Edit a Password" $ passEditPrompt myXPConfig)
           -- , ( "M-C-S-p"
             --, addName "Remove a Password" $ passRemovePrompt myXPConfig
             --)
           ]





-- namedScratchpadFilterOutWorkspacePP $ - if I want to filter out named scratchpads
-- Pretty fg
myPP :: PP
myPP = namedScratchpadFilterOutWorkspacePP $ def
  { ppUrgent          = xmobarColor "red" "yellow"
  , ppCurrent         = xmobarColor "#4C566A" "#A3BE8C" . wrap "| " " |" -- Current workspace in xmobar
  , ppVisible         = xmobarColor "#A3BE8C" ""                -- Visible but not current workspace
  , ppHidden          = xmobarColor "#81A1C1" "" . wrap " " " "   -- Hidden workspaces in xmobar
  -- \( _ ) -> "" to show no hidden windows
  , ppHiddenNoWindows = xmobarColor "#BF616A" ""       -- Hidden workspaces (no windows)
  , ppTitle           = const ""     -- Title of active window in xmobar
  , ppSep             = "<fc=#D8DEE9> | </fc>"                     -- Separators in xmobar
  , ppExtras          = [windowCount] -- show number of windows
  , ppOrder           = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
  }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.fehbg &"
  spawnOnce "emacs --daemon"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
  spawnOnce "caffeine"
  spawnOnce "dunst &"

-- TODO Maybe when I spawn spotify I can have it goes to my fourth workspace
--- nix-shell -p xorg.xwininfo - this program gets the window name!!
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchPads <+> manageHook def

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=terminus"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobars/xmobar-nord.conf"
  xmproc1 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobars/xmobar-nord.conf"
  xmonad
    $ dynamicProjects myProjects
    $ addDescrKeys' ((mod4Mask .|. shiftMask, xK_slash), showKeybindings) myKeys
    $ ewmh
    $ docks def
        { manageHook         = myManageHook <+> manageDocks
        , logHook = dynamicLogWithPP myPP { ppOutput = \x -> hPutStrLn xmproc x
                                                          >> hPutStrLn xmproc1 x
                                          }
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , modMask            = mod4Mask
        -- No borders because picom transparency makes it obvious which window is focused
        , borderWidth        = 0
                        -- do `toWorkspaces myWorkspaces` for treeselect
        , workspaces         = myWorkspaces
        , handleEventHook    = handleEventHook def <+> fullscreenEventHook
        , layoutHook         = myLayout
        , focusedBorderColor = "#D8DEE9"
        , normalBorderColor  = "#434C5E"
        }
