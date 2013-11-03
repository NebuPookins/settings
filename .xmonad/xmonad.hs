import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myWorkspaces = ["1:coding","2:web","3:social","4:media","5:utils","6:temp","7","8","9"]
myManageHook = composeAll
	[ className =? "KeePass2" --> doShift "5:utils"
	, className =? "Pidgin"   --> doShift "3:social"
	, className =? "Skype"    --> doShift "3:social"
	, className =? "Subl3"    --> doShift "1:coding"
	-- className =? "Chromium" --> doShift "2:web" --problems with save dialog, etc.
	]

main = do
	xmproc <- spawnPipe "/usr/bin/xmobar /home/nebu/.xmobarrc"
	xmonad $ defaultConfig
		{ manageHook = composeAll [manageDocks, myManageHook, manageHook defaultConfig]
		, terminal = "xfce4-terminal"
		, layoutHook = avoidStruts  $  layoutHook defaultConfig
		, handleEventHook = docksEventHook <+> handleEventHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "blue" "" . shorten 50
			}
		, workspaces = myWorkspaces
		} `additionalKeys`
		[ ((0, xK_Print), spawn "scrot")
		]
