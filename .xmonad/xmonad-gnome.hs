import XMonad
import XMonad.Config.Gnome
-- import XMonad.Layout.Minimize
import qualified Data.Map as M
-- import System.Exit -- exitWith
-- import XMonad.Layout.Fullscreen
-- import XMonad.Layout.NoBorders
-- import XMonad.Layout.Gaps
-- Fullscreen imports:
-- import XMonad.Hooks.ManageHelpers
-- import qualified XMonad.StackSet as W
-- import Control.Monad
-- import Data.Monoid (All (All))
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W



myTerminal :: [Char]
myTerminal = "alacritty"
--myLayout = minimize (Tall 1 (3/100) (1/2)) -- ||| Full

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "spotify" "spotify" (className =? "Spotify") manageSpotify
                , NS "htop" (myTerminal ++ " -e htop") (title =? "htop") defaultFloating
                ]
  where
    spawnTerm  = myTerminal ++  " --name scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.5
                 w = 0.5
                 t = 1 - h
                 l = 1 - w
    manageSpotify = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.5
                 w = 0.5
                 t = 0.95 - h
                 l = 0.95 - w
myKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
myKeys conf@(XConfig {XMonad.modMask = modm}) = -- M.fromList $ -- comment M.fromList when using 'newKeys'
             [  ((modm , xK_Escape)          , kill)
                , ((modm , xK_s)               , spawn $ XMonad.terminal conf)
                , ((modm , xK_b)               , spawn "brave-browser")
                , ((modm , xK_d)               , spawn "nautilus --new-window")
                , ((modm , xK_v)               , spawn "emacs")
                , ((modm .|. shiftMask , xK_Page_Up) , spawn "gnome-session-quit --reboot")
                , ((modm .|. shiftMask , xK_Page_Down) , spawn "gnome-session-quit --power-off")
                -- Scratchpads
                , ((modm .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "htop")
                , ((modm .|. controlMask, xK_c), namedScratchpadAction myScratchPads "spotify")
                , ((modm .|. controlMask, xK_g), namedScratchpadAction myScratchPads "terminal")
             ]

newKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
newKeys x = M.union (keys def x) (M.fromList (myKeys x))

myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchPads

main :: IO ()
main = do
  xmonad $ gnomeConfig
    { terminal    = myTerminal
    , modMask     = mod4Mask
    , manageHook = myManageHook <+> manageHook gnomeConfig
--    , doFloat     = ["Evince", "Totem"]
    , focusFollowsMouse = False
    , borderWidth = 2
    , normalBorderColor  = "#FFFFFF"
    , focusedBorderColor = "#000000" -- "#00FF00" -- terminal green -- "#A6E1FF" -- "green"
    , keys       = newKeys
    , handleEventHook    = fullscreenEventHook
    }
