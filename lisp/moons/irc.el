(ns/guard ns/enable-home-p)

(use-package circe)

(setq ns/irc-nick "neeasade")
(setq ns/circe-highlights
  `(,ns/irc-nick "bspwm"))

(setq-ns lui
  logging-directory (~ ".irc")
  time-stamp-position 'right-margin
  time-stamp-format "%H:%M"
  ;; fluid width windows
  fill-type nil
  )

(setq-ns circe
  reduce-lurker-spam nil ;; hide part, join, quit
  default-quit-message ""
  default-part-message ""
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

;; auto fill/trim left to 8 columns
(defun ns/make-message (left body &optional left-face message-face)
  (concat
    (propertize
      (s-pad-left 8 " "
        (concat
          (s-left
            (if (> (length left) 8) 7 8)
            left)
          (when (> (length left) 8) "…")))
      'face
      (if left-face left-face 'circe-originator-face))
    (propertize (concat " " body)
      'face (if message-face message-face 'default))))

(defun ns/part-message (nick)
  "Generate a random part message for NICK."
  (let
    ((options
       '(
          "%s has left the party."
          "%s fell off a cliff."
          "%s drank too much and passed out."
          "%s fell into the lava."
          "rocx kidnapped %s!"
          "%s was taken by thanos."
          "%s went down a youtube hole."
          "%s ate too much shrimp."
          "%s left and joined a circus."
          "%s got stuck in tvtropes again."
          "%s got spooped."
          "%s has been eaten by their cat."
          "%s was MURDERED by ben shapiro!"
          "%s has rage quit."
          "%s has joined an lunar amish colony."
          "%s never existed at all."
          "%s's mom showed up to take them home."
          "%s is blasting off AGAINNnnnn...."
          "%s left to catch the dogecoin dip."
          "%s gave all their money to their brother day trader."
          )))
    (format
      (nth (random (- (length options) 1)) options)
      nick)))

(defun ns/join-message (nick)
  "Generate a random join message for NICK."
  (let
    ((options
       '(
          "%s just joined. Everyone, look busy!"
          "%s joined. You must construct additional pylons."
          "Ermagherd. %s is here."
          "A wild %s appeared."
          "Swoooosh. %s just landed."
          "Brace yourselves. %s just joined the server."
          "%s just joined. Hide your bananas."
          "%s just arrived. Seems OP - please nerf."
          "%s just slid into the server."
          "A %s has spawned in the server."
          "Big %s showed up!"
          ;; "Where’s %s? In the server!"
          "%s hopped into the server. Kangaroo!!"
          "%s just showed up. Hold my beer."
          "Challenger approaching - %s has appeared!"
          "It's a bird! It's a plane! Nevermind, it's just %s."
          "It's %s! Praise the sun!"
          ;; "Never gonna give %s up. Never gonna let [!!{username}!!](usernameOnClick) down."
          "Ha! %s has joined! You activated my trap card!"
          "Hey! Listen! %s has joined!"
          ;; "We've been expecting you %s"
          ;; "It's dangerous to go alone, take %s!"
          "%s has joined the server! It's super effective!"
          "Cheers, love! %s is here!"
          "%s is here, as the prophecy foretold."
          "%s has arrived. Party's over."
          "Ready player %s"
          ;; todo dynamic this?
          ;; "%s is here to kick butt and chew bubblegum. And %s is all out of gum."
          "Hello. Is it %s you're looking for?"
          "%s has joined. Stay a while and listen!"
          "Roses are red, violets are blue, %s joined this server with you"
          "%s has joined the party."
          "%s is back from meatspace."
          )))
    (format
      (nth (random (- (length options) 1)) options)
      nick)))

(defun ns/circe-clear-reason (reason nick)
  "Clear out default REASONs."
  (if
    (or
      (string= "" reason)
      (string= "Quit: leaving" reason)
      (string= "Quit: Excess flood" reason)
      (string= "[No reason given]" reason)
      (string= "Remote host closed the connection" reason)
      (string= "Quit: My MacBook has gone to sleep. ZZZzzz…" reason)
      (string= "Quit: Leaving" reason)
      (string= "Quit: Coyote finally caught me" reason)
      (string= (format "Quit: %s" nick) reason)
      (s-contains? "Read error:" reason)
      (s-contains? "Ping timeout" reason)
      (s-contains? "Quit: WeeChat" reason)
      (s-contains? "Quit: ERC" reason)
      ;; assume adverts for clients
      (s-contains? "http" reason)
      )
    nil reason))

(defun ns/circe-handle-say (nick body)
  "update state for say, return nick, highmon"
  (when (string= nick ns/irc-nick) (setq nick "me"))

  (if (not (boundp 'circe-last-nick))
    (setq-local circe-last-nick ""))

  (if (not (boundp 'circe-last-message))
    (setq-local circe-last-message ""))

  ;; bots
  (when (or (-contains-p
              '(
                 "cappuccino"
                 "EggServ"
                 "linkreader"
                 ) nick)
          (s-ends-with-p "bot" nick))
    ;; peek at last message hese
    (when (or
            (s-contains-p "http" circe-last-message)
            (s-starts-with-p "." circe-last-message)
            (s-starts-with-p "s/" circe-last-message)
            (s-starts-with-p "!" circe-last-message)
            )
      (setq nick circe-last-nick)
      ))

  ;; don't double print nicks
  (if (string= nick circe-last-nick)
    (setq nick "")
    (setq-local circe-last-nick nick))

  (setq-local circe-last-message body)

  ;; highlight buffer
  (when (not (get-buffer "*circe-highlight*"))
    (generate-new-buffer "*circe-highlight*")
    (with-current-buffer "*circe-highlight*" (circe-highlight-mode)))

  (when (-any-p (fn (s-contains-p <> body)) ns/circe-highlights)
    (let ((channel (buffer-name))
           (poster circe-last-nick))
      (with-current-buffer "*circe-highlight*"
        (save-restriction
          (widen)
          (goto-char (point-min))
          (insert (format "%s:%s:%s\n" channel poster body))))))

  nick
  )

(defun ns/circe-expand-change (change)
  (let ((op (substring change 0 1))
         (grant (substring change 1 2))
         (person (second (s-split " " change)))
         (mode-map #s(hash-table size 6 test equal
                       data
                       (
                         "q" "owner"
                         "a" "admin"
                         "b" "ban"
                         "o" "operator"
                         "h" "half-operator"
                         "v" "voice"
                         ))))
    (s-join " "
      (list
        (if (string= op "+") "gives" "removes")
        (if (gethash grant mode-map) (gethash grant mode-map) grant)
        (if (string= op "+") "to" "from")
        person))))

(defun ns/circe-format-all (type args)
  "Meta circe formatter by TYPE for ARGS."
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

          ;; topic
          (new-topic (plist-get args :new-topic))
          (channel (plist-get args :channel))
          (topic-ago (plist-get args :topic-ago))
          )

    (when (or (eq type 'say) (eq type 'self-say)) (setq nick (ns/circe-handle-say nick body)))
    (when reason
      (setq reason (ns/circe-clear-reason reason nick))
      (setq reason
        (if reason
          (if (s-contains-p ":" reason)
            (format "%s %s" nick reason)
            (format "%s left: %s" nick reason))
          (ns/part-message nick))))

    (pcase type
      ('say (ns/make-message nick body))
      ('self-say (ns/make-message nick body 'circe-originator-fade-face 'circe-my-message-face))

      ('part (ns/make-message "<" reason 'circe-originator-fade-face 'circe-server-face))
      ('quit (ns/make-message "<" reason 'circe-originator-fade-face 'circe-server-face))
      ('join (ns/make-message ">" (ns/join-message nick) 'circe-originator-fade-face 'circe-server-face))

      ('notice (ns/make-message "!" body))
      ('action (ns/make-message "*" (format "%s %s." nick body)))
      ('nick-change (ns/make-message "*" (format "%s is now %s." old-nick new-nick)))
      ('topic (ns/make-message channel new-topic))
      ('topic-time (ns/make-message channel
                     (format "Topic was set %s ago." topic-ago)
                     'circe-originator-fade-face
                     'circe-server-face
                     ))
      ('mode-change (ns/make-message "*"
                      (format "%s %s (%s)." setter (ns/circe-expand-change change) target)
                      'circe-originator-fade-face 'circe-server-face))
      )))

(defun ns/goto-highlight (input)
  (interactive)
  (let (
         (buf (nth 0 (s-split ":" input)))
         (op (nth 1 (s-split ":" input)))
         (message (s-join ":" (cdr (cdr (s-split ":" input)))))
         )
    (counsel-switch-to-buffer-or-window buf)
    (goto-char (point-max))
    (search-backward message)
    ))

(defun ns/goto-last-highlight ()
  (interactive)
  (if (get-buffer "*circe-highlight*")
    (let ((last-highlight
            (with-current-buffer "*circe-highlight*"
              (save-excursion
                (goto-char (point-min))
                (let ((b (point))
                       (e (progn (end-of-line) (point))))
                  (buffer-substring-no-properties b e)))
              )))
      (if (not (string= "" last-highlight))
        (ns/goto-highlight last-highlight)
        (message "no highlights!")))
    (message "no highlights!")))

(define-derived-mode circe-highlight-mode
  text-mode "circe-highlight"
  "Major mode for circe-highlight-buffer."
  ;; (setq-local case-fold-search nil)
  )

(general-nmap :keymaps 'circe-highlight-mode-map
  "RET" (fn! (ns/goto-highlight (ns/get-current-line))))

(general-nmap :keymaps 'circe-highlight-mode-map
  :prefix "SPC"
  "nn" (fn! (ns/goto-highlight (ns/get-current-line))))

(setq-ns circe-format
  notice              (fn (ns/circe-format-all 'notice      <rest>))
  action              (fn (ns/circe-format-all 'action      <rest>))
  server-message      (fn (ns/circe-format-all 'notice      <rest>))
  self-action         (fn (ns/circe-format-all 'action      <rest>))
  say                 (fn (ns/circe-format-all 'say         <rest>))
  self-say            (fn (ns/circe-format-all 'self-say    <rest>))
  server-nick-change  (fn (ns/circe-format-all 'nick-change <rest>))
  server-join         (fn (ns/circe-format-all 'join        <rest>))
  server-part         (fn (ns/circe-format-all 'part        <rest>))
  server-quit         (fn (ns/circe-format-all 'quit        <rest>))
  server-topic        (fn (ns/circe-format-all 'topic       <rest>))
  server-quit-channel (fn (ns/circe-format-all 'quit        <rest>))
  server-mode-change  (fn (ns/circe-format-all 'mode-change <rest>))
  server-topic-time (fn (ns/circe-format-all 'topic-time    <rest>))
  )

;; cf: https://github.com/jorgenschaefer/circe/issues/298#issuecomment-262912703
;; Don't show names list upon joining a channel.
;;     this makes /names disable entirely though
;; (circe-set-display-handler "353" 'circe-display-ignore)
;; (circe-set-display-handler "366" 'circe-display-ignore)

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
  (mapcar #'(lambda (network) (circe-maybe-connect (car network)))
    circe-network-options)
  (ns/style-circe))

;; channel name in prompt
(add-hook 'circe-chat-mode-hook
  (fn (lui-set-prompt (ns/make-message (buffer-name) ""))))

;; prevent too long pastes/prompt on it:
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(load "lui-logging" nil t)
(enable-lui-logging-globally)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq fringes-outside-margins t
    right-margin-width 5
    left-margin-width 0
    word-wrap t
    wrap-prefix (ns/make-message "" ""))
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))

(use-package circe-notifications :config
  (autoload 'enable-circe-notifications "circe-notifications" nil t)
  (eval-after-load "circe-notifications" '(setq circe-notifications-watch-strings ns/circe-highlights))
  (add-hook 'circe-server-connected-hook 'enable-circe-notifications))

(defun ns/circe-unread-query-buffers ()
  (-intersection
    tracking-buffers
    (mapcar 'buffer-name (ns/buffers-by-mode 'circe-query-mode))))

(defcommand jump-irc ()
  (let ((irc-channels
          (mapcar 'buffer-name
            (ns/buffers-by-mode 'circe-channel-mode 'circe-query-mode))))
    (if (eq (length irc-channels) 0)
      (message "connect to irc first!")
      (ivy-read "channel: " irc-channels
        :action (lambda (option)
                  (interactive)
                  (counsel-switch-to-buffer-or-window option)
                  (evil-goto-line)
                  (evil-scroll-line-to-bottom nil) ; nil -> the current line
                  ;; this still needed?
                  ;; (ns/style-circe)
                  )))))

;; emacs freezes completely while pulling in the image fuckkkk
;; (require 'circe-display-images)
;; (setq circe-display-images-max-height 200)
;; (ns/bind-leader-mode 'circe-channel "i" 'circe-display-images-toggle-image-at-point)
;; (enable-circe-display-images)

(add-hook 'circe-channel-mode-hook 'ns/set-buffer-face-variable)
(advice-add #'ns/style :after #'ns/style-circe)
(defun ns/style-circe ()
  "Make chat pretty."
  (let*
    ((comment-fg (face-attribute 'font-lock-keyword-face :foreground))
      (default-fg (face-attribute 'default :foreground))
      (default-bg (face-attribute 'default :background))
      (highlight-fg (ns/color-tone default-fg 20 20))
      (fade-fg (ns/color-tone default-fg 35 40)))

    (set-face-attribute 'circe-server-face          nil  :foreground fade-fg)
    (set-face-attribute 'lui-time-stamp-face        nil  :foreground fade-fg)
    (set-face-attribute 'circe-prompt-face          nil  :foreground fade-fg)
    (set-face-attribute 'circe-my-message-face      nil  :foreground fade-fg)
    (set-face-attribute 'circe-highlight-nick-face  nil  :foreground highlight-fg)
    (set-face-attribute 'circe-originator-face      nil  :foreground highlight-fg)

    (defface circe-originator-fade-face
      `((t :foreground ,fade-fg))
      "circe actions and self-say characters"
      )

    ;; urls
    (set-face-attribute 'lui-button-face nil :foreground highlight-fg)
    (set-face-attribute 'lui-button-face nil :underline nil)

    (set-face-attribute 'circe-prompt-face nil :background nil)

    (ns/set-faces-monospace '(circe-originator-face circe-prompt-face circe-originator-fade-face))

    ;; apply the hook to everyone to update body font
    (dolist (b (ns/buffers-by-mode 'circe-channel-mode 'circe-query-mode))
      (with-current-buffer b (ns/set-buffer-face-variable)))))

;; todo: consider C-n, C-e as well.
;; todo: need query mode here too
(general-imap :keymaps 'circe-channel-mode-map "<up>" 'lui-previous-input)
(general-imap :keymaps 'circe-channel-mode-map "<down>" 'lui-next-input)
(general-nmap :keymaps 'circe-channel-mode-map "<up>" 'lui-previous-input)
(general-nmap :keymaps 'circe-channel-mode-map "<down>" 'lui-next-input)

(defun circe-command-NP (&optional ignored)
  (interactive "sAction: ")
  (circe-command-ME (concat "is now playing " (ns/shell-exec "music infoname"))))

(ns/bind
  "ai" 'connect-all-irc
  "ni" 'ns/jump-irc
  "nI" (fn! (counsel-switch-to-buffer-or-window "*circe-highlight*"))
  ;; ehhh
  "nr" 'ns/goto-last-highlight
  )

;; if this ever changes we're gonna break everything woo
(defun circe--irc-display-format (format target nick userhost event args)
  (let* ((target+name (circe--irc-display-target target nick args))
          (target (car target+name))
          (name (cdr target+name))
          (origin (if userhost
                    (format "%s (%s)" nick userhost)
                    (format "%s" nick))))
    (with-current-buffer (or target
                           (circe-server-last-active-buffer))
      (let ((circe--irc-format-server-numeric
              (if target
                ;; changing these two lines
                ;; (format "*** %s" format)
                ;; (format "*** [%s] %s" name format))))
                (format (make-message "*" "%s") format)
                (format (make-message name "%s") format))))
        (circe-display 'circe--irc-format-server-numeric
          :nick (or nick "(unknown)")
          :userhost (or userhost "server")
          :origin origin
          :event event
          :command event
          :target name
          :indexed-args args)))))
