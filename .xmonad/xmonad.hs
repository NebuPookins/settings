import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

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
	xmonad $ defaultConfig
		{ manageHook = composeAll [manageDocks, myManageHook, manageHook defaultConfig]
		, terminal = "urxvt"
		, layoutHook = avoidStruts  $  layoutHook defaultConfig
		, handleEventHook = docksEventHook <+> handleEventHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "lightblue" "" . shorten 40
			}
		, workspaces = myWorkspaces
		} `additionalKeys`
		[ ((0, xK_Print), spawn "scrot")
		]
