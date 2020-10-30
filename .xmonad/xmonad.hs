import XMonad
import qualified XMonad.StackSet as W
import System.Exit

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Ssh
import XMonad.Prompt.FuzzyMatch

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog

import Data.Monoid
import qualified Data.Map as M

myTerminal :: String
myTerminal = "st"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 0

myModMask :: KeyMask
myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myEditor :: String
myEditor = "emacs"

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myBrowser :: String
myBrowser = "chromium"

myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom -f &"
  spawnOnce "xfce4-power-manager &"

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

avXPConfig :: XPConfig
avXPConfig = def
  { font                = myFont
  , bgColor             = "#282c34"
  , fgColor             = "#bbc2cf"
  , bgHLight            = "#c792ea"
  , fgHLight            = "#000000"
  , borderColor         = "#535974"
  , promptBorderWidth   = 0
  , position            = Top
  , height              = 20
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
  , showCompletionOnTab = False
  , alwaysHighlight     = True
  , searchPredicate     = fuzzyMatch
  , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
  }

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]

myWorkspaces :: [ String ]
myWorkspaces = clickable . (map xmobarEscape)
  $ [ "dev", "www", "sys" ,"vbox" ,"vm" ,"chat" ,"mus" ,"vid" ,"gfx" ]
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>" |
                  (i,ws) <- zip [1..9] l,
                  let n = i ]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore ]

myLogHook :: X ()
myLogHook = return ()

myEventHook :: Event -> X All
myEventHook = mempty

myKeys :: [ ( String, X () ) ]
myKeys =

  -- launch shell prompt
  [ ("M-S-<Return>", shellPrompt avXPConfig)

  -- launch a terminal
  , ("M-<Return>", spawn myTerminal)

  -- launch gmrun
  , ("M-S-p", spawn "gmrun")

  -- close focused window
  , ("M-S-c", kill)

  -- Rotate through the available layout algorithms
  , ("M-<Space>", sendMessage NextLayout)

  -- Resize viewed windows to the correct size
  , ("M-n", refresh)

  -- Move focus to the next window
  , ("M-<Tab>", windows W.focusDown)

  -- Move focus to the next window
  , ("M-j", windows W.focusDown)

  -- Move focus to the previous window
  , ("M-k", windows W.focusUp  )

  -- Move focus to the master window
  , ("M-m", windows W.focusMaster  )

  -- Swap the focused window with the next window
  , ("M-S-j", windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ("M-S-k", windows W.swapUp    )

  -- Shrink the master area
  , ("M-h", sendMessage Shrink)

  -- Expand the master area
  , ("M-l", sendMessage Expand)

  -- Push window back into tiling
  , ("M-t", withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ("M-,", sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ("M-.", sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.
  --
  -- , ("M-b", sendMessage ToggleStruts)

  -- Quit xmonad
  , ("M-S-q", io (exitWith ExitSuccess))

  -- Restart xmonad
  , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")

  -- Emacs
  , ("C-e e", spawn "emacs")
  , ("C-e d", spawn "emacs --eval '(dired nil)'")

  -- Media keys
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
  , ("<XF86AudioMute>", spawn "amixer set Master toggle")
  ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh def {
                    -- simple stuff
                    terminal           = myTerminal,
                    focusFollowsMouse  = myFocusFollowsMouse,
                    clickJustFocuses   = myClickJustFocuses,
                    borderWidth        = myBorderWidth,
                    modMask            = myModMask,
                    workspaces         = myWorkspaces,
                    normalBorderColor  = myNormalBorderColor,
                    focusedBorderColor = myFocusedBorderColor,
                    
                    -- hooks, layouts
                    layoutHook         = myLayout,
                    manageHook         = myManageHook <+> manageDocks,
                    handleEventHook    = myEventHook <+> fullscreenEventHook <+> docksEventHook,
                    logHook            = myLogHook <+> dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                    , ppHiddenNoWindows = xmobarColor "grey" ""
                    , ppTitle   = xmobarColor "green"  "" . shorten 40
                    , ppVisible = wrap "("")"
                    , ppUrgent  = xmobarColor "red" "yellow"
                    , ppHidden = xmobarColor "grey" "" . wrap "" "*"
                    },
                    startupHook        = myStartupHook
                    } `additionalKeysP` myKeys
