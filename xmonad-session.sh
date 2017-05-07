#!/bin/bash
setxkbmap -option caps:escape&
xmodmap -e "pointer = 3 2 1 4 5"
xrandr --output DVI-D-0 --mode 1280x1024 --rate 60.02 --pos 1920x176
xrandr --output DVI-I-1 --mode 1920x1200 --rate 59.95 --pos 0x0
feh --bg-scale /usr/share/backgrounds/Haukland_Beach_view_by_Michele_Agostini.jpg&

exec xmonad-session
