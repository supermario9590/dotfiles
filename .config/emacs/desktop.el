(defun av/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun av/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
      "nitrogen" nil  "nitrogen --restore"))

(defun av/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; Launch apps that will run in the background
  (av/run-in-background "nm-applet")
  (av/run-in-background "xfce4-power-manager"))

(defun av/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun av/exwm-update-title ()
  (pcase exwm-class-name
    ("Chromium" (exwm-workspace-rename-buffer (format            exwm-title)))
    ("Vimb"     (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))))

(defun av/configure-window-by-class ()
  (interactive)
  (message "Window '%s' appeared!" exwm-class-name)

  (pcase exwm-class-name
    ("Chromium" (exwm-workspace-move-window 1) )
    ("Vimb"     (exwm-workspace-move-window 1) )
    ("mpv"      (exwm-floating-toggle-floating)
                (exwm-layout-toggle-mode-line) )))

(use-package exwm
  :config
  (add-hook 'exwm-update-class-hook #'av/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'av/exwm-update-title)

  (add-hook 'exwm-manage-finish-hook #'av/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'av/exwm-init-hook)

  (setq exwm-workspace-number 5)

  (av/set-wallpaper)

  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j
      ?\C-\ ))

  (setq exwm-input-global-keys
        `(
          ([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
           ([?\s-w] . exwm-workspace-switch)

           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                         (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                     (number-sequence 0 9))))

  ;; Automatically move EXWM buffer to current workspace when selected
  (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  (setq exwm-workspace-show-all-buffers t)

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

  ;; Remap CapsLock to Ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.config/emacs/exwm/Xmodmap")

  ;; Systemtray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))
