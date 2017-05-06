import XMonad hiding (Tall)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Config.Gnome
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile

myFont = "xft:Ubuntu Mono derivative Powerline Sans:size=10"
focusColor = "#666666"
textColor = "#c0c0c0"
lightTextColor = "#ffffff"
backgroundColor = "#000000"
lightBackgroundColor = "#222222"
jurgentColor = "#ff2020"

myTheme :: Theme
myTheme = defaultTheme
    { activeColor = lightBackgroundColor
    , inactiveColor = backgroundColor
    , urgentColor = backgroundColor
    , activeBorderColor = textColor
    , inactiveTextColor = textColor
    , urgentTextColor = textColor
    , inactiveBorderColor = lightBackgroundColor
    , urgentBorderColor = jurgentColor
    , activeTextColor = lightTextColor
    , fontName = myFont
    }

hintedTile = HintedTile 1 (3/100) (1/2) TopLeft
main = do
    xmproc <- spawnPipe "bash -c \"tee >(xmobar -x0 ~/.xmobarrc) | xmobar -x1 ~/.xmobarrc2\""
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = toggleLayouts (noBorders Full) $ avoidStruts (smartBorders $ hintedTile Tall ||| hintedTile Wide ||| tabbed shrinkText myTheme)
        --, layoutHook = toggleLayouts (noBorders Full) $ avoidStruts (smartBorders $ buttonDeco shrinkText defaultThemeWithButtons (hintedTile Tall ||| hintedTile Wide ||| tabbed shrinkText myTheme))
        , terminal = "termite"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((mod4Mask .|. shiftMask, xK_space), sendMessage ToggleLayout)
        , ((0, xK_Print), spawn "scrot")
        ]
