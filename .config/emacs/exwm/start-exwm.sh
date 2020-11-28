#!/bin/bash
picom &

xss-lock -- slock &

exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/desktop.el
