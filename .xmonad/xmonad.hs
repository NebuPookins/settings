import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops -- For rofi compatibility
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys) -- Needs the arch linux package xmonad-contrib
import XMonad.Actions.GridSelect -- probably needs xmonad-contrib?
import XMonad.Actions.WindowBringer -- probably needs xmonad-contrib?
import System.IO

padR :: Int -> String -> String
padR n s
	| length s < n = s ++ replicate (n - length s) ' '
	| otherwise    = s

baseConfig = desktopConfig

myWorkspaces =
	[ "1:<icon=/home/nebu/.xmonad/cpu.xbm/>" --coding
	, "2:<icon=/home/nebu/.xmonad/fs_01.xbm/>" --web
	, "3:<icon=/home/nebu/.xmonad/pacman.xbm/>" --social
	, "4:<icon=/home/nebu/.xmonad/phones.xbm/>" --media
	, "5:<icon=/home/nebu/.xmonad/arch.xbm/>" --utils
	, "6:<icon=/home/nebu/.xmonad/wifi_01.xbm/>" --temp
	, "7"
	, "8"
	, "9"
	]
myManageHook = composeAll
	[ className =? "KeePass2" --> doShift "5:<icon=/home/nebu/.xmonad/arch.xbm/>"
	, className =? "Pidgin"   --> doShift "3:<icon=/home/nebu/.xmonad/pacman.xbm/>"
	, className =? "Skype"    --> doShift "3:<icon=/home/nebu/.xmonad/pacman.xbm/>"
	-- , className =? "Subl3"    --> doShift "1:<icon=/home/nebu/.xmonad/cpu.xbm/>" --problems with save dialog, etc.
	-- className =? "Chromium" --> doShift "2:<icon=/home/nebu/.xmonad/fs_01.xbm/>" --problems with save dialog, etc.
	]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar /home/nebu/.xmobarrc"
	xmonad $ ewmh baseConfig
	-- xmonad $ desktopConfig
		{ manageHook = composeAll [manageDocks, myManageHook, manageHook baseConfig]
		, terminal = "xfce4-terminal"
		, layoutHook = avoidStruts $ layoutHook baseConfig
		, handleEventHook = docksEventHook <+> handleEventHook baseConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "lightblue" "" . padR 50 . shorten 50
			}
		, workspaces = myWorkspaces
		} `additionalKeys`
		[ ((0, xK_Print), spawn "scrot") -- take screenshot; assumes scot is installed
		, ((mod4Mask, xK_Tab), spawn "rofi -show combi")
		, ((mod1Mask .|. shiftMask, xK_g), goToSelected defaultGSConfig) -- show list of open windows and switch to selected one; provided by XMonad.Actions.GridSelect
		, ((mod1Mask, xK_g), gotoMenu) -- shows a dmenu of windows, and switches to the workspace containing the selected one; provided by XMonad.Actions.WindowBringer
		, ((mod1Mask, xK_b), bringMenu) -- shows a dmenu of windows, and brings the selected one to this workspace; provided by XMonad.Actions.WindowBringer
		]
