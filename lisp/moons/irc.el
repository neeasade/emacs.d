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
	   reduce-lurker-spam t ;; hide part, join, quit
	   network-options
	   `(("Freenode"
	      :nick ,ns/irc-nick
	      :host "irc.freenode.net"
	      :tls t
	      :nickserv-password ,(pass "freenode")
	      :channels (:after-auth "#github" "#bspwm" "#qutebrowser" "#emacs" "k-slug" "#qutebrowser-offtopic")
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

  (defun my/circe-format-truncated-nick (type args)
    (let* ((nick (plist-get args :nick))
           (body (plist-get args :body))
           (maxlen (if (eq type 'action) 7 8))
           ;; (lui-nick-trim (concat "{nick:" (number-to-string maxlen) "s}"))
           (lui-nick (s-pad-left maxlen " " (s-left maxlen nick)))
           )

      (when (> (length nick) maxlen)
        (setq lui-nick (concat (substring lui-nick 0 (- maxlen 1)) "…")))

      (if (not (boundp 'circe-last-nick))
          (setq-local circe-last-nick ""))

      (setq lui-nick (ns/monospace lui-nick))
      (if (string= circe-last-nick lui-nick)
          (setq lui-nick (ns/monospace "        "))
        (setq-local circe-last-nick lui-nick))

      (lui-format
       (pcase type
         ('say (concat lui-nick " {body}"))
         ('action (concat "*" lui-nick " {body}*"))
         ('notice (concat lui-nick " ! {body} !")))
       :nick nick :body body)))

  (defun my/circe-format-action (&rest args)
    (my/circe-format-truncated-nick 'action args))

  (defun my/circe-format-notice (&rest args)
    (my/circe-format-truncated-nick 'notice args))

  (defun my/circe-format-say (&rest args)
    (my/circe-format-truncated-nick 'say args))

  (defun my/circe-format-self-say (&rest args)
    (if (not (boundp 'circe-last-nick))
        (setq-local circe-last-nick ""))

    (let*
        (
         (body (plist-get args :body))
         (result
          (if (string= circe-last-nick ns/irc-nick)
	      (lui-format (concat (ns/monospace "        ") " {body}") :body body)
            (lui-format (concat (ns/monospace "       ►") " {body}") :body body)
            )))
      (setq-local circe-last-nick ns/irc-nick)
      result
      )
    )

  (setq-ns circe-format
	   action 'my/circe-format-action
	   notice 'my/circe-format-notice
	   say 'my/circe-format-say
	   self-say 'my/circe-format-self-say
	   self-action (concat (ns/monospace "       ► ") "*{body}*")
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
         (remove-if-not
          (lambda (s) (s-match "#.*" s))
          (mapcar 'buffer-name (buffer-list))
          )))
    (if (eq (length irc-channels) 0)
        (message "connect to irc first!")
      (ivy-read "channel: " irc-channels
		:action (lambda (option)
			  (counsel-switch-to-buffer-or-window option)
			  ;; todo: fix this the right way
			  (ns/style-circe)
			  ))
      )))

;; emacs freezes completely while pulling in the image
;; (require 'circe-display-images)
;; (setq circe-display-images-max-height 200)
;; (ns/bind-leader-mode 'circe-channel "i" 'circe-display-images-toggle-image-at-point)
;; (enable-circe-display-images)

(advice-add #'ns/style :after #'ns/style-circe)
(defun ns/style-circe ()
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
    ))

;; todo: add hook for content font

(defun ns/circe-hook ()
  (ns/set-buffer-face-variable)
  (ns/set-faces-monospace '(circe-originator-face circe-prompt-face)))
(add-hook 'circe-channel-mode-hook 'ns/circe-hook)

(define-key circe-channel-mode-map (kbd "<up>") 'lui-previous-input)
(define-key circe-channel-mode-map (kbd "<down>") 'lui-next-input)

;; todo: mute irc bots colors
(ns/bind
 "ai" 'connect-all-irc
 "ni" 'ns/jump-irc
 )
