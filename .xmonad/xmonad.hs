--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal      = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth   = 0

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: Keymask
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [ String ]
myWorkspaces    = [ "1", "2", "3" ,"4" ,"5" ,"6" ,"7" ,"8" ,"9" ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor  = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: [ ( String, X () ) ]
myKeys =

    -- launch a terminal
    [ ("M-S-<Return>", spawn myTerminal)

    -- launch dmenu
    , ("M-p", spawn "dmenu_run")

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

    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)

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
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ("M-S-/", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom -f &"
    spawnOnce "xfce4-power-manager &"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmonad $ def {
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
                 manageHook         = myManageHook,
                 handleEventHook    = myEventHook,
                 logHook            = myLogHook,
                 startupHook        = myStartupHook
                 } `additionalKeysP` myKeys

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
                "",
                "-- launching and killing programs",
                "mod-Shift-Enter  Launch xterminal",
                "mod-p            Launch dmenu",
                "mod-Shift-p      Launch gmrun",
                "mod-Shift-c      Close/kill the focused window",
                "mod-Space        Rotate through the available layout algorithms",
                "mod-Shift-Space  Reset the layouts on the current workSpace to default",
                "mod-n            Resize/refresh viewed windows to the correct size",
                "",
                "-- move focus up or down the window stack",
                "mod-Tab        Move focus to the next window",
                "mod-Shift-Tab  Move focus to the previous window",
                "mod-j          Move focus to the next window",
                "mod-k          Move focus to the previous window",
                "mod-m          Move focus to the master window",
                "",
                "-- modifying the window order",
                "mod-Return   Swap the focused window and the master window",
                "mod-Shift-j  Swap the focused window with the next window",
                "mod-Shift-k  Swap the focused window with the previous window",
                "",
                "-- resizing the master/slave ratio",
                "mod-h  Shrink the master area",
                "mod-l  Expand the master area",
                "",
                "-- floating layer support",
                "mod-t  Push window back into tiling; unfloat and re-tile it",
                "",
                "-- increase or decrease number of windows in the master area",
                "mod-comma  (mod-,)   Increment the number of windows in the master area",
                "mod-period (mod-.)   Deincrement the number of windows in the master area",
                "",
                "-- quit, or restart",
                "mod-Shift-q  Quit xmonad",
                "mod-q        Restart xmonad",
                "mod-[1..9]   Switch to workSpace N",
                "",
                "-- Workspaces & screens",
                "mod-Shift-[1..9]   Move client to workspace N",
                "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
                "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
                "",
                "-- Mouse bindings: default actions bound to mouse events",
                "mod-button1  Set the window to floating mode and move by dragging",
                "mod-button2  Raise the window to the top of the stack",
                "mod-button3  Set the window to floating mode and resize by dragging"]
