-------------------------------------------------
--   __   __                                _  --
--   \ \ / /                               | | --
--    \ V / _ __ ___   ___  _ __   __ _  __| | --      
--     > < | '_ ` _ \ / _ \| '_ \ / _` |/ _` | --
--    / . \| | | | | | (_) | | | | (_| | (_| | --
--   /_/ \_\_| |_| |_|\___/|_| |_|\__,_|\__,_| --
-------------------------------------------------                                
-- Imports
import XMonad
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig(additionalKeys)

import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CycleWS
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import Data.Monoid
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Exit
--Layouts
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.SubLayouts
import XMonad.Layout.ResizableTile
--Actions
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS
---myBar = "xmobar -x0 /home/mike/.xmonad/xmobarrc"
myTerminal = "alacritty"

myBrowser = "microsoft-edge-dev"
---- Key binding to toggle the gap for the bar.
myModMask       = mod1Mask
toggleStrutsKey XConfig {XMonad.modMask = modMask}= (modMask, xK_b)
--myWorkspaces    = ["1:Web","2:term","3:mail","4:files","5:steam","6","7","8","9"]
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x    = [x]
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map xmobarEscape) $ [" 1 "," 2 "," 3 ", " 4 "," 5 ", " 6 "," 7 "," 8 "," 9 "]
                                                                              
  where                                                                       
         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,                                        
                            let n = i ]
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask,              xK_Return), spawn myTerminal)
 
    -- launch dmenu
    , ((modMask,               xK_d     ), spawn "dmenu_run")
    -- swich wallpaper next
    , ((modMask,               xK_b     ), spawn "variety -n")
    -- swich wallpaper preview
    , ((modMask .|. shiftMask, xK_b     ), spawn "variety -p")
 
    , ((modMask,               xK_c     ), spawn myBrowser)
   -- close focused window    
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_a     ), killAll)
    --- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
    -- Volume Control
    ,((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
    
    -- Brightness Control
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask .|. controlMask, xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask .|. controlMask, xK_l     ), sendMessage Expand)

    -- prevworkspace
    , ((modMask,               xK_h     ), XMonad.Actions.CycleWS.moveTo Prev NonEmptyWS)
    -- nextworkspace
    , ((modMask,               xK_l     ), XMonad.Actions.CycleWS.moveTo Next NonEmptyWS)
    -- shift to prevworkspace
    , ((modMask .|. shiftMask, xK_h     ), XMonad.Actions.CycleWS.shiftTo Prev NonEmptyWS)
 
    -- shift to nextworkspace
    , ((modMask .|. shiftMask, xK_l     ), XMonad.Actions.CycleWS.shiftTo Next NonEmptyWS)
    --
    -- Increase/decrease spacing (gaps)
    , ((modMask,               xK_i     ), incWindowSpacing 4)
    , ((modMask,               xK_n     ), decWindowSpacing 4)
 
    -- Push window back into tiling

    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
     , ((modMask              , xK_f     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    -- , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modMask              , xK_o    ), namedScratchpadAction myScratchPads "terminal")
    , ((modMask              , xK_p    ), namedScratchpadAction myScratchPads "music")
    -- , ((modMask              , xK_f), sendMessage (Toggle "Full"))
----BSP Layout
--    , ((modMask .|. mod4Mask,               xK_l     ), sendMessage $ ExpandTowards R)
--    , ((modMask .|. mod4Mask,               xK_h     ), sendMessage $ ExpandTowards L)
--    , ((modMask .|. mod4Mask,               xK_j     ), sendMessage $ ExpandTowards D)
--    , ((modMask .|. mod4Mask,               xK_k     ), sendMessage $ ExpandTowards U)
----    , ((modMask .|. mod4Mask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
----    , ((modMask .|. mod4Mask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
----    , ((modMask .|. mod4Mask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
----    , ((modMask .|. mod4Mask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
--    , ((modMask,                           xK_r     ), sendMessage Rotate)
--    , ((modMask,                           xK_s     ), sendMessage Swap)
--    , ((modMask,                           xK_n     ), sendMessage FocusParent)
--   -- , ((modMask .|. ctrlMask,              xK_n     ), sendMessage SelectNode)
--    , ((modMask .|. shiftMask,             xK_n     ), sendMessage MoveNode)
    ]
     ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
        , (\i -> W.greedyView i . W.shift i, shiftMask) ]] -- 可以修改最后的shiftMask为controlMask这样就是用的control键
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
---spawn
--
myStartupHook = do
        spawnOnce "variety &"
        spawnOnce "picom &"
        spawnOnce "fcitx &"
        spawnOnce "nm-applet &"
        spawnOnce "albert &"
        spawnOnce "nextcloud &"
        spawnOnce "mailspring &"
        spawnOnce "mate-power-manager &"
        spawnOnce "albert &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
        spawnOnce "cd ~/software/clash/ && nohup ./clash -d . >/dev/null 2>&1 &"

myScratchPads = [ NS "terminal" spawnTerm  findTerm manageTerm
		, NS "music" spawnPav findPav  managePav
		]
	where

    spawnTerm = myTerminal ++  " -name scratchpad -e cmus"
    findTerm = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below

	where

        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 1       -- height, 10% 
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = 1 -w -- centered left/right
    spawnPav = "spotify"
    findPav = className =? "Spotify"
    managePav = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below

	where

        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 1      -- height, 10% 
        w = 1         -- width, 100%
        t = 1 -h      -- bottom edge
        l = 1 -w -- centered left/right
 
myManageHook = composeAll
    [ className =? "stalonetray"    --> doIgnore
      , className =? "Steam"        --> doFullFloat
      , className =? "Firefox"      --> doFullFloat
      , title =? "FEZ"              --> doFullFloat
      , title =? "Don't Starve"     --> doFullFloat
--      , className =? "mpv"          --> doFullFloat
      , manageDocks
      , isFullscreen                --> (doF W.focusDown <+> doFullFloat)
    ] <+> namedScratchpadManageHook myScratchPads

-- Mouse bindings
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayoutHook = avoidStruts (
--       toggleLayouts Full (Grid) ||| toggleLayouts Full (ThreeColMid 1 (1/20) (1/2)) ||| simpleTabbed ||| toggleLayouts Full (tiled) ||| Mirror tiled)
       tall ||| Full )
        where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
delta = 3/100 


--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"



----Main Function
main = do
    xmproc <- spawnPipe ("xmobar $HOME/.xmonad/xmobarrc")
    xmonad $ ewmh $ docks $ defaults {
	logHook = dynamicLogWithPP $ xmobarPP {
	    ppOutput = hPutStrLn xmproc
	   ,ppVisible = xmobarColor "#7F7F7F" "" 
	   ,ppTitle = xmobarColor "#222222" "" 
	   ,ppCurrent = xmobarColor "#2E9AFE" ""
           ,ppHidden  = xmobarColor "#7F7F7F" ""
	   ,ppLayout = xmobarColor"#7F7F7F" ""
           ,ppUrgent = xmobarColor "#900000" "" . wrap "[" "]" 
        }
	, manageHook = manageDocks <+> myManageHook
	, startupHook = myStartupHook
    , modMask = myModMask
	
    }

defaults = def{
    modMask= mod4Mask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , keys = myKeys
    , layoutHook = smartBorders $ myLayoutHook
    , focusedBorderColor = "#2E9AFE"
    , normalBorderColor = "#000000"
    , mouseBindings = myMouseBindings                           
    , manageHook = myManageHook <+> manageHook def
    , borderWidth         = 1
    , startupHook = myStartupHook
    }
