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

  ;; Polybar
  (av/start-panel)

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
    ("Chromium" (exwm-workspace-move-window 2) )
    ("Vimb"     (exwm-workspace-move-window 2) )
    ("discord"  (exwm-workspace-move-window 6) )
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

  (setq exwm-workspace-number 10)

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

  ;; Enable EXWM
  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; Make sure the server is started (better to do this in your main Emacs config!)
(server-start)

(defvar av/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun av/kill-panel ()
  (interactive)
  (when av/polybar-process
    (ignore-errors
      (kill-process av/polybar-process)))
  (setq av/polybar-process nil))

(defun av/start-panel ()
  (interactive)
  (av/kill-panel)
  (setq av/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun av/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "hello")
    (1 "")
    (2 "")
    (3 "")
    (4 "")
    (5 "")
    (6 "")
    (7 "")
    (8 "")
    (9 "")))

(defun av/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun av/send-polybar-exwm-workspace ()
  (av/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'av/send-polybar-exwm-workspace)
