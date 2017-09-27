import XMonad hiding (Tall)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO
import XMonad.Config.Gnome
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile
import qualified XMonad.StackSet as W
import XMonad.Actions.PhysicalScreens

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
    xmonad $ docks def
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = toggleLayouts (noBorders (Full ||| hintedTile Tall)) $ avoidStruts (smartBorders $ hintedTile Tall ||| hintedTile Wide ||| tabbed shrinkText myTheme)
        --, layoutHook = toggleLayouts (noBorders Full) $ avoidStruts (smartBorders $ buttonDeco shrinkText defaultThemeWithButtons (hintedTile Tall ||| hintedTile Wide ||| tabbed shrinkText myTheme))
        , terminal = "termite"
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        (
            [ ((mod4Mask .|. shiftMask, xK_z), spawn "qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause; slock")
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            , ((mod4Mask .|. shiftMask, xK_space), sendMessage ToggleLayout)
            , ((mod4Mask .|. shiftMask, xK_t), spawn "shutdown 0")
            , ((0, 0x1008ff13), spawn "pactl set-sink-volume 1 +1.5%")
            , ((0, 0x1008ff11), spawn "pactl set-sink-volume 1 -1.5%")
            , ((0, 0x1008ff12), spawn "pactl set-sink-mute 1 toggle")
            , ((0, 0x1008ff16), spawn "qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous;")
            , ((0, 0x1008ff14), spawn "qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause;")
            , ((0, 0x1008ff15), spawn "qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop;")
            , ((0, 0x1008ff17), spawn "qdbus org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next;")
            ]
            ++
            [((mod4Mask .|. mask, key), f sc)
               | (key, sc) <- zip [xK_w, xK_e, xK_r] [0, 1, 2]
               , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
            ]
        )
