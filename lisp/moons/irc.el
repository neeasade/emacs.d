(ns/guard ns/enable-home-p)

(use-package circe
  :config
  (setq ns/irc-nick "neeasade")

  (setq-ns lui
    logging-directory (~ ".irc")
    time-stamp-position 'right-margin
    time-stamp-format "%H:%M"
    ;; fluid width windows
    fill-type nil
    )

  (setq-ns circe
    reduce-lurker-spam nil ;; hide part, join, quit
    network-options
    `(("Freenode"
        :nick ,ns/irc-nick
        :host "irc.freenode.net"
        :tls t
        :nickserv-password ,(pass "freenode")
        :channels (:after-auth "#github" "#bspwm" "#qutebrowser" "#emacs" "#k-slug" "#qutebrowser-offtopic")
        )

       ("Nixers"
         :nick ,ns/irc-nick
         :host "irc.unix.chat"
         :port (6667 . 6697)
         :tls t
         :channels ("#unix")
         )

       ("Bitlbee"
         :nick ,ns/irc-nick
         :host "localhost"
         )

       ("Rizon"
         :nick ,ns/irc-nick
         :host "irc.rizon.net"
         :port (6667 . 6697)
         :tls t
         :channels (:after-auth "#rice" "#code" "#leliana")
         :nickserv-password ,(pass "rizon/pass")
         :nickserv-mask ,(rx bol "NickServ!service@rizon.net" eol)
         :nickserv-identify-challenge ,(rx bol "This nickname is registered and protected.")
         :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
         :nickserv-identify-confirmation ,(rx bol "Password accepted - you are now recognized." eol)
         )))

  ;; going to make this monospace, main text regular
  (defun ns/monospace (input)
    (propertize input 'face 'circe-originator-face))

  (defun ns/circe-format-all (type args)
    ;; all the circe-format-x things we care about will go through here
    (let* (
            (nick (plist-get args :nick))
            (body (plist-get args :body))
            (reason (plist-get args :reason))
            ;; mode-change
            (setter (plist-get args :setter))
            (change (plist-get args :change))
            (target (plist-get args :target))
            ;; nick change
            (old-nick (plist-get args :old-nick))
            (new-nick (plist-get args :new-nick))
            )

      (when (eq type 'say)
        (when (string= nick ns/irc-nick) (setq nick "-"))

        (if (not (boundp 'circe-last-nick))
          (setq-local circe-last-nick ""))

        ;; if it's a bot, make it faceless.
        ;; todo: extend this to a channel map
        (when (string= nick "cappuccino")
          (setq nick ""))

        (if (string= nick circe-last-nick)
          (setq nick "")
          (setq-local circe-last-nick nick))
        )

      ;; auto fill/trim left to 8 columns
      (defun make-message (left body)
        (concat
          (ns/monospace
            (s-pad-left 8 " "
              (concat
                (s-left
                  (if (> (length left) 8) 7 8)
                  left)
                (when (> (length left) 8) "…"))))
          (propertize (concat " " body)
            'face 'default)))

      (when reason
        (when (or
                (string= "[No reason given]" reason)
                (string= "Remote host closed the connection" reason)
                (string= "Quit: My MacBook has gone to sleep. ZZZzzz…" reason)
                (s-contains? "Ping Timeout" reason)
                (s-contains? "Quit: WeeChat" reason)
                (s-contains? "Quit: ERC" reason)
                )
          (setq reason ""))

        (when (not (string= "" reason))
          (setq reason (format " (%s)" reason))
          )
        )

      (defun make-action-message (message)
        (make-message ">" message))

      (pcase type
        ('say (make-message nick body))
        ('notice (make-message "!" body))
        ('action (make-action-message (format "%s %s." nick body)))
        ('part (make-action-message (format "%s has left the party%s." nick reason)))
        ('quit (make-action-message (format "%s has left the party%s." nick reason)))
        ('join (make-action-message (format "%s has joined the party." nick)))
        ('nick-change (make-action-message (format "%s is now %s." old-nick new-nick)))
        ('mode-change (make-action-message (format "%s by %s (%s)" change setter target)))
        )))

  (setq-ns circe-format
    notice (fn (ns/circe-format-all 'notice <rest>))
    action (fn (ns/circe-format-all 'action <rest>))
    self-action (fn (ns/circe-format-all 'action <rest>))
    say (fn (ns/circe-format-all 'say <rest>))
    self-say (fn (ns/circe-format-all 'say <rest>))
    server-nick-change (fn (ns/circe-format-all 'nick-change <rest>))
    server-join (fn (ns/circe-format-all 'join <rest>))
    server-part (fn (ns/circe-format-all 'part <rest>))
    server-quit (fn (ns/circe-format-all 'quit <rest>))
    server-quit-channel (fn (ns/circe-format-all 'quit <rest>))
    server-mode-change (fn (ns/circe-format-all 'mode-change <rest>))
    )

  ;; Don't show names list upon joining a channel.
  ;; cf: https://github.com/jorgenschaefer/circe/issues/298#issuecomment-262912703
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  ;; (require 'circe-color-nicks)
  ;; (enable-circe-color-nicks)

  ;; Last reading position.
  ;; (enable-lui-track-bar)
  ;; todo: this hook should be buffer not frame maybe
  ;; (add-hook 'focus-out-hook 'lui-track-bar-move)
  )

(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose `circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-server-network)
          (throw 'return t))))))

(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
        (y-or-n-p (format "Already connected to %s, reconnect? " network)))
    (circe network)))

(defun connect-all-irc()
  (interactive)
  (mapcar '(lambda (network) (circe-maybe-connect (car network)))
    circe-network-options)
  (ns/style-circe))

;; channel name in prompt
(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (let ((prompt (format "%8s" (buffer-name))))
    (when (> (length prompt) 8)
      (setq prompt (concat (substring prompt 0 7) "…")))
    (lui-set-prompt
      (concat (propertize prompt 'face 'circe-prompt-face) " "))))

;; prevent too long pastes/prompt on it:
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(load "lui-logging" nil t)
(enable-lui-logging-globally)

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  ;; timestamp
  (setq right-margin-width 5)
  (setq left-margin-width 0))

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
    fringes-outside-margins t
    right-margin-width 5
    word-wrap t
    wrap-prefix (concat (ns/monospace "        ") " "))
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))

(use-package circe-notifications
  :config
  (autoload 'enable-circe-notifications "circe-notifications" nil t)
  (eval-after-load "circe-notifications"
    '(setq circe-notifications-watch-strings
       ;; example: '("people" "you" "like" "to" "hear" "from")))
       '("neeasade" "bspwm")))

  (add-hook 'circe-server-connected-hook 'enable-circe-notifications)
  )

(defcommand jump-irc()
  (let ((irc-channels
          (mapcar 'buffer-name
            (-concat
              (ns/buffers-by-mode 'circe-channel-mode)
              (ns/buffers-by-mode 'circe-query-mode)))))
    (if (eq (length irc-channels) 0)
      (message "connect to irc first!")
      (ivy-read "channel: " irc-channels
        :action (lambda (option)
                  (counsel-switch-to-buffer-or-window option)
                  ;; todo: fix this the right way
                  (ns/style-circe)
                  )))))

;; emacs freezes completely while pulling in the image
;; (require 'circe-display-images)
;; (setq circe-display-images-max-height 200)
;; (ns/bind-leader-mode 'circe-channel "i" 'circe-display-images-toggle-image-at-point)
;; (enable-circe-display-images)

(advice-add #'ns/style :after #'ns/style-circe)

(defun ns/style-circe ()
  "Make chat pretty."
  (let*
    ((comment-fg (face-attribute 'font-lock-keyword-face :foreground))
      (default-fg (face-attribute 'default :foreground))
      (default-bg (face-attribute 'default :background))
      (highlight-fg (ns/color-tone default-fg 20 20))
      (fade-fg (ns/color-tone default-fg 35 40)))

    (mapc
      (lambda(face)
        (set-face-attribute face nil :foreground fade-fg))
      '(circe-server-face
         lui-time-stamp-face
         circe-prompt-face

         circe-my-message-face
         circe-originator-face))

    ;; (set-face-attribute 'lui-track-bar nil :background
    ;;   (ns/color-tone default-bg  10 10))

    (set-face-attribute 'circe-prompt-face nil :background nil)
    (set-face-attribute 'circe-highlight-nick-face nil :foreground highlight-fg)
    (set-face-attribute 'lui-button-face nil :foreground highlight-fg) ; url
    (ns/set-faces-monospace '(circe-originator-face circe-prompt-face))

    ;; apply the hook to everyone to update body font
    (dolist (b (-concat
                 (ns/buffers-by-mode 'circe-channel-mode)
                 (ns/buffers-by-mode 'circe-query-mode)
                 ))
      (with-current-buffer b (ns/circe-hook)))
    ))

;; todo: loop to circe buffers to appy this
(defun ns/circe-hook ()
  (ns/set-buffer-face-variable))

(add-hook 'circe-channel-mode-hook 'ns/circe-hook)

(define-key circe-channel-mode-map (kbd "<up>") 'lui-previous-input)
(define-key circe-channel-mode-map (kbd "<down>") 'lui-next-input)

;; todo: mute irc bots colors
(ns/bind
  "ai" 'connect-all-irc
  "ni" 'ns/jump-irc
  )
