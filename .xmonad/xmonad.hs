import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/nebu/.xmobarrc"
xmonad $ defaultConfig
  { manageHook = manageDocks <+> manageHook defaultConfig
  , layoutHook = avoidStruts  $  layoutHook defaultConfig
  , logHook = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppTitle = xmobarColor "blue" "" . shorten 50
    }
  } `additionalKeys`
  [ ((0, xK_Print), spawn "scrot")
  ]

