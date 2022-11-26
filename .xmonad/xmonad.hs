import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops -- For rofi compatibility
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers -- for doRectFloat
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys) -- Needs the arch linux package xmonad-contrib
import XMonad.Actions.GridSelect -- probably needs xmonad-contrib?
import XMonad.Actions.WindowBringer -- probably needs xmonad-contrib?
import System.IO

import qualified XMonad.StackSet as W -- for RationalRect

padR :: Int -> String -> String
padR n s
  | length s < n = s ++ replicate (n - length s) ' '
  | otherwise    = s

baseConfig = desktopConfig

myWorkspaces =
  [  "1:planning" --plan
  , "2:web" --web
  , "3:socials" --social
  , "4:media" --media
  , "5:coding" --coding
  , "6:utilities" --utils
  , "7:games"
  , "8:moonlight"
  , "9:AWS Workspace"
  , "0:coding docs"
  , "F1:obsidian"
  , "F2:tempA"
  , "F3:tempB"
  , "F4:tempC"
  , "F5:tempD"
  , "F6:tempE"
  , "F7:tempF"
  , "F8:tempG"
  , "F9:tempH"
  , "F10:tempI"
  , "F11:tempJ"
  , "F12:daemons"
  ]

myKeys =
  [ ((0, xK_Print), spawn "scrot") -- take screenshot; assumes scot is installed
  , ((mod4Mask .|. shiftMask, xK_s), spawn "flameshot gui") -- take screenshot; assumes flameshot is installed.
  , ((mod1Mask, xK_p), spawn "rofi -show combi")
  , ((mod1Mask .|. shiftMask, xK_space), layoutSplitScreen 2 (TwoPane 0.5 0.5))
  , ((mod1Mask .|. controlMask .|. shiftMask, xK_space), rescreen)
  ]
  ++
  [ ((m .|. mod1Mask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_F1, xK_F2, xK_F3, xK_F4, xK_F5, xK_F6, xK_F7, xK_F8, xK_F9, xK_F10, xK_F11, xK_F12]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

-- You can use the command `xprop | grep -E "WM_NAME|WM_CLASS|WM_WINDOW_ROLE"` to get the information for the hooks.
-- WM_CLASS[0] -> resource, WM_CLASS[1] -> className, WM_NAME -> title, WM_WINDOW_ROLE -> role

myManageHook = composeAll
  [ className =? "Pidgin"                         --> doShift "3:socials"
  , className =? "Skype"                          --> doShift "3:socials"
  , className =? "Caprine"                        --> doShift "3:socials"
  , className =? "discord"                        --> doShift "3:socials"
  , className =? "KeePass2"                       --> doShift "6:utilities"
  , className =? "Steam"                          --> doShift "7:games"
  , className =? "com.moonlight_stream.Moonlight" --> doShift "8:moonlight"
  , role =? "GtkFileChooserDialog"                --> doRectFloat (W.RationalRect 0.25 0.25 0.5 0.5)
  , className =? "MPlayer"                        --> unfloat
  -- , className =? "Subl3"    --> doShift "1:<icon=/home/nebu/.xmonad/cpu.xbm/>" --problems with save dialog, etc.
  -- className =? "Chromium" --> doShift "2:<icon=/home/nebu/.xmonad/fs_01.xbm/>" --problems with save dialog, etc.
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    unfloat = ask >>= doF . W.sink

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/nebu/.xmobarrc"
  xmonad $ ewmh baseConfig
    { manageHook = composeAll [manageDocks, myManageHook, manageHook baseConfig]
    , terminal = "xfce4-terminal"
    , layoutHook = spacingRaw True (Border 0 0 0 0) False (Border 1 1 1 1) True $ avoidStruts $ layoutHook baseConfig
    , borderWidth = 2
    , focusedBorderColor = "#ffff00"
    , normalBorderColor = "#000000"
    , handleEventHook = docksEventHook <+> handleEventHook baseConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "lightblue" ""
      }
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys
