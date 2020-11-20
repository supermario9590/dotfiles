#!/bin/sh

picom -f &

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/desktop.el
