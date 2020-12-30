;; -*- lexical-binding: t; -*-

;; todo:
;; alerts on DMs as well as channel highlights
;; don't alert on highlights if the window is focused and we are not idle

(use-package circe)

(setq
  ns/irc-nick "neeasade"
  circe-default-nick ns/irc-nick
  circe-default-user ns/irc-nick
  circe-default-realname ns/irc-nick)


(setq ns/circe-highlights `(,ns/irc-nick "neesade" "neese" "nessie" "bspwm" "emacs " "clojure" " nix "))

(setq-ns lui
  logging-directory (~ ".ircnew")
  time-stamp-position 'right-margin
  time-stamp-format "%H:%M"
  ;; fluid width windows
  fill-type nil
  )

(defun ns/circe-count-nicks ()
  (when (-contains-p '(circe-query-mode circe-channel-mode) major-mode)
    (length (circe-channel-nicks))))

(defun! ns/circe-count-nicks-message ()
  (message (format "there are %d people here." (ns/circe-count-nicks))))

(defun ns/init-circe ()
  (setq-ns circe
    ;; reduce-lurker-spam nil ;; hide part, join, quit
    default-quit-message ""
    default-part-message ""
    ;; todo: consider tracking channels somewhere else
    network-options
    `(
       ("Freenode"
         :nick ,ns/irc-nick
         :host "irc.freenode.net"
         :tls t
         :nickserv-password ,(pass "freenode")
         :channels (:after-auth
                     "#bspwm"
                     "#lobsters"
                     "#emacs"
                     "#nixers_net"
                     "#sway-devel"
                     "#k-slug"
                     "#LawrenceLUG"
                     "#qutebrowser"
                     "#qutebrowser-offtopic"
                     "##9fans"
                     "#clojure"
                     "#kisslinux"
                     "#distrotube"
                     "#clojure"
                     "#fennel"
                     ;; so noisy
                     ;; "#nixos"
                     "#nixos-chat"
                     "#nixos-emacs"
                     )
         )

       ("brettgilio"
         :nick ,ns/irc-nick
         :host "irc.brettgilio.com"
         :port 7097
         :tls t
         )

       ("tildechat"
         :nick ,ns/irc-nick
         :host "na.tilde.chat"
         :port 6697
         :tls t
         :nickserv-password ,(pass "tilde.chat")
         ;; https://tilde.chat/stats/
         :channels (:after-auth "#club" "#meta")
         )

       ("Cyberia"
         :nick ,ns/irc-nick
         :host "irc.cyberia.is"
         :port 6697
         :tls t
         :channels ("#cyberia" "#2f30")
         )

       ;; ("Nixers"
       ;;   :nick ,ns/irc-nick
       ;;   :host "irc.unix.chat"
       ;;   :port (6667 . 6697)
       ;;   :tls t
       ;;   :channels ("#unix"))

       ;; ("OFTC"
       ;;   :nick ,ns/irc-nick
       ;;   :host "irc.oftc.net"
       ;;   :port (6667 . 6697)
       ;;   :tls t
       ;;   :channels ("#bitlbee"))

       ;; dead?
       ;; ("eigenstate"
       ;;   :nick ,ns/irc-nick
       ;;   :host "irc.eigenstate.org"
       ;;   :port (6667 . 6697)
       ;;   :tls t
       ;;   :channels ("#myrddin"))

       ;; ("Bitlbee"
       ;;   :nick ,ns/irc-nick
       ;;   :host "localhost"
       ;;   )

       ("Rizon"
         :nick ,ns/irc-nick
         :host "irc.rizon.net"
         :port (6667 . 6697)
         :tls t
         :channels (:after-auth "#rice" "#code" "#leliana" "#etc")
         :nickserv-password ,(pass "rizon/pass")
         :nickserv-mask ,(rx bol "NickServ!service@rizon.net" eol)
         :nickserv-identify-challenge ,(rx bol "This nickname is registered and protected.")
         :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
         :nickserv-identify-confirmation ,(rx bol "Password accepted - you are now recognized." eol)
         )

       )))

;; auto fill/trim left to 8 columns
(defun ns/make-message (left body &optional left-face message-face)
  (concat
    (propertize
      (s-pad-left 8 " "
        (concat
          (s-left (if (> (length left) 8) 7 8) left)
          (when (> (length left) 8) "…")))
      'face
      (if left-face left-face 'circe-originator-face))
    (propertize (concat " " body)
      'face (if message-face message-face 'default))))

(defun ns/part-message (nick)
  "Generate a random part message for NICK."
  (let ((options
          '(
             "%s has left the party."
             "%s fell off a cliff."
             "%s drank too much and passed out."
             "%s fell into the lava."
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
             "%s's power went out."
             "%s is blasting off AGAINNnnnn...."
             "%s left to catch the dogecoin dip."
             "%s gave all their money to their brother day trader."
             )))
    (format (nth (random (- (length options) 1)) options)
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
          "%s hopped into the server. Kangaroo!!"
          "%s just showed up. Hold my beer."
          "Challenger approaching - %s has appeared!"
          "It's a bird! It's a plane! Nevermind, it's just %s."
          "It's %s! Praise the sun!"
          "Ha! %s has joined! You activated my trap card!"
          "Hey! Listen! %s has joined!"
          "%s has joined the server! It's super effective!"
          "Cheers, love! %s is here!"
          "%s is here, as the prophecy foretold."
          "%s has arrived. Party's over."
          "Ready player %s"
          "Hello. Is it %s you're looking for?"
          "%s has joined. Stay a while and listen!"
          "Roses are red, violets are blue, %s joined this server with you"
          "%s has joined the party."
          "%s is back from meatspace."
          "%s left to go think about HURD."
          )))
    (format
      (nth (random (- (length options) 1)) options)
      nick)))

(defun ns/circe-clear-reason (reason nick)
  "Clear out default REASONs for NICK leaving."
  (if (or
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
        (s-contains? "left: WeeChat" reason)
        (s-contains? "Quit: ERC" reason)
        ;; assume adverts for clients
        (s-contains? "http" reason)
        ) nil reason))

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
                 "EGGServ"
                 "EggServ"
                 "mockturtle"
                 "CuteServ"
                 "linkreader"
                 ) nick)
          (s-ends-with-p "bot" nick))

    ;; peek at last message hese
    (when (or
            (s-contains-p "http" circe-last-message)
            (-contains-p (string-to-list ",.!~[") (string-to-char circe-last-message))
            (s-starts-with-p "s/" circe-last-message)
            )
      (setq nick circe-last-nick)))

  ;; don't double print nicks
  ;; here is where we should pass type and maybe check against it

  (if (string= nick circe-last-nick)
    (setq nick "")
    (setq-local circe-last-nick nick)
    )

  (setq-local circe-last-message body)

  ;; todo: move highlight into it's own thing

  ;; highlight buffer
  (when (not (get-buffer "*circe-highlight*"))
    (generate-new-buffer "*circe-highlight*")
    (with-current-buffer "*circe-highlight*" (circe-highlight-mode)))

  (when (-first (fn (s-contains-p <> body t)) ns/circe-highlights)
    (let ((match (downcase (-first (fn (s-contains-p <> body t)) ns/circe-highlights)))
           (channel (buffer-name))
           (poster circe-last-nick))

      (when
        (and
          (not (string= channel (if (boundp 'circe-chat-target) "")))

          ;; don't self highlight!
          (not (string= circe-last-nick "me"))

          ;; don't care if some things are mentioned in their primary channel
          (not (and (string= "emacs " match) (string= "#emacs" channel)))
          (not (and (string= "clojure" match) (string= "#clojure-beginners" channel)))
          (not (and (string= "clojure" match) (string= "#clojure" channel)))
          (not (and (string= " nix " match) (string= "#nixos" channel)))
          (not (and (string= " nix " match) (string= "#nixos-chat" channel)))
          )

        (with-current-buffer "*circe-highlight*"
          (alert!
            (format "[%s] %s" poster body)
            ;; body
            :severity 'normal
            :title channel
            )

          (save-restriction
            (widen)
            (goto-char (point-min))
            (insert (format "%s:%s:%s\n" channel poster body)))))))

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

    (when (not (-contains-p '(join part quit) type))
      (setq nick (ns/circe-handle-say
                   (or nick "")
                   (or body ""))))

    ;; (setq nick (ns/circe-handle-say (or nick "") (or body "")))

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

(defun! ns/goto-highlight (input)
  (let (
         (buf (nth 0 (s-split ":" input)))
         (op (nth 1 (s-split ":" input)))
         (message (s-join ":" (cdr (cdr (s-split ":" input)))))
         )
    (counsel-switch-to-buffer-or-window buf)
    (goto-char (point-max))
    (search-backward message)
    ))

(defun! ns/goto-last-highlight ()
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
  :prefix "SPC"
  "nn" (fn!
         (->>
           (thing-at-point 'line)
           s-clean
           ns/goto-highlight)))

;; mabye
;; (defun ns/circe-map (type) (fn (ns/circe-format-all type <rest>)))
;; (ns/circe-map 'notice)

;; (macroexpand-1
;;   (quote (lambda (&rest rest) (ns/circe-format-all 'join rest))

;;     )

;;   )

;; (symbolp (intern "arst"))

(defmacro ns/set-circe-handler (kind target)
  `(setq
     ,(intern (format "circe-format-%s" kind))
     (lambda (&rest rest)
       (ns/circe-format-all ',target rest))))

(ns/set-circe-handler lurker-activity     say)
(ns/set-circe-handler rejoin              join)
(ns/set-circe-handler notice              notice)
(ns/set-circe-handler action              action)
(ns/set-circe-handler server-message      notice)
(ns/set-circe-handler self-action         action)
(ns/set-circe-handler say                 say)
(ns/set-circe-handler self-say            self-say)
(ns/set-circe-handler server-nick-change  nick-change)
(ns/set-circe-handler server-join         join)
(ns/set-circe-handler server-part         part)
(ns/set-circe-handler server-quit         quit)
(ns/set-circe-handler server-topic        topic)
(ns/set-circe-handler server-quit-channel quit)
(ns/set-circe-handler server-mode-change  mode-change)
(ns/set-circe-handler server-topic-time   topic-time)

;; (setq-ns circe-format
;;   server-lurker-activity (lambda (&rest args) (ns/circe-format-all 'say args))
;;   server-rejoin       (fn (ns/circe-format-all 'join <rest>))
;;   notice              (fn  (ns/circe-format-all 'notice      <rest>))
;;   action              (fn  (ns/circe-format-all 'action     <rest>))
;;   server-message      (fn  (ns/circe-format-all 'notice     <rest>))
;;   self-action         (fn  (ns/circe-format-all 'action     <rest>))
;;   say                 (fn  (ns/circe-format-all 'say        <rest>))
;;   self-say            (fn  (ns/circe-format-all 'self-say   <rest>))
;;   server-nick-change  (fn  (ns/circe-format-all 'nick-change <rest>))
;;   server-join         (fn  (ns/circe-format-all 'join       <rest>))
;;   server-part         (fn  (ns/circe-format-all 'part       <rest>))
;;   server-quit         (fn  (ns/circe-format-all 'quit       <rest>))
;;   server-topic        (fn  (ns/circe-format-all 'topic      <rest>))
;;   server-quit-channel (fn  (ns/circe-format-all 'quit       <rest>))
;;   server-mode-change  (fn  (ns/circe-format-all 'mode-change <rest>))
;;   server-topic-time (fn  (ns/circe-format-all 'topic-time   <rest>))
;;   )

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

;; options: nil, post-command, post-output, t (both)
;; note: this has the potential to get laggy with many irc buffers open
;; (setq lui-scroll-behavior t)
(setq lui-scroll-behavior nil)

(defun! connect-all-irc()
  (mapcar #'(lambda (network) (circe-maybe-connect (car network)))
    circe-network-options)
  ;; todo: this doesn't style because connecting takes some time -- need a way to wait for servers up/chans joined
  (ns/style-circe))

;; channel name in prompt

(defun ns/set-circe-prompt ()
  (lui-set-prompt (ns/make-message (buffer-name) "")))

(add-hook 'circe-chat-mode-hook 'ns/set-circe-prompt)

;; (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
;; (add-hook 'circe-query-mode-hook 'enable-lui-autopaste)

;; prevent too long pastes/prompt on it:
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
(add-hook 'circe-query-mode-hook 'enable-lui-autopaste)

;; paying this small cost a bunch of times means we don't have to
;; remember to call ns/style-circe later
(add-hook 'circe-channel-mode-hook 'ns/style-circe)
(add-hook 'circe-query-mode-hook 'ns/style-circe)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq fringes-outside-margins t
    right-margin-width 5
    left-margin-width 0
    word-wrap t
    wrap-prefix (ns/make-message "" ""))
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))

(defun ns/circe-unread-query-buffers ()
  (-intersection
    tracking-buffers
    (mapcar 'buffer-name (ns/buffers-by-mode 'circe-query-mode))))

(mapcar 'buffer-name
  (ns/buffers-by-mode 'circe-channel-mode 'circe-query-mode)

  )

(defun! ns/jump-irc ()
  (let* ((irc-channels
           (mapcar 'buffer-name
             (ns/buffers-by-mode 'circe-channel-mode 'circe-query-mode)))
          (irc-nicks
            (if (-contains-p irc-channels (buffer-name))
              ;; (circe-channel-nicks)
              '())))
    (if (eq (length irc-channels) 0)
      (message "connect to irc first!")
      (ivy-read "channel: " (append irc-channels irc-nicks)
        :action (lambda (option)
                  (interactive)
                  (if (-contains-p irc-nicks option)
                    (circe-command-QUERY option)
                    (when (get-buffer option)
                      (counsel-switch-to-buffer-or-window option)
                      (evil-goto-line)
                      (evil-scroll-line-to-bottom nil) ; nil -> the current line
                      )))))))

;; emacs freezes completely while pulling in the image fuckkkk
;; (require 'circe-display-images)
;; (setq circe-display-images-max-height 200)
;; (ns/bind-leader-mode 'circe-channel "i" 'circe-display-images-toggle-image-at-point)
;; (enable-circe-display-images)

(add-hook 'circe-channel-mode-hook 'ns/set-buffer-face-variable)
(add-hook 'circe-query-mode-hook 'ns/set-buffer-face-variable)

(defun! ns/style-circe ()
  "Make chat pretty."
  (let*
    ((comment-fg (face-attribute 'font-lock-keyword-face :foreground))
      (default-fg (face-attribute 'default :foreground))
      (default-bg (face-attribute 'default :background))
      (highlight-fg (ct/lessen 20 default-fg))
      (fade-fg (face-attribute 'font-lock-comment-face :foreground))
      )

    (set-face-attribute 'circe-server-face          nil  :foreground fade-fg)
    (set-face-attribute 'lui-time-stamp-face        nil  :foreground fade-fg)
    (set-face-attribute 'circe-prompt-face          nil  :foreground fade-fg)
    (set-face-attribute 'circe-my-message-face      nil  :foreground fade-fg)
    (set-face-attribute 'circe-highlight-nick-face  nil  :foreground highlight-fg)
    (set-face-attribute 'circe-originator-face      nil  :foreground highlight-fg)

    (defface circe-originator-fade-face
      `((t :foreground ,fade-fg))
      "circe actions and self-say characters")

    ;; urls
    (set-face-attribute 'lui-button-face nil :foreground highlight-fg)
    (set-face-attribute 'lui-button-face nil :underline nil)

    (set-face-attribute 'circe-prompt-face nil :background nil)

    (ns/set-faces-monospace '(circe-originator-face circe-prompt-face circe-originator-fade-face))

    ;; apply the hook to everyone to update body font
    (dolist (b (ns/buffers-by-mode 'circe-channel-mode 'circe-query-mode))
      (with-current-buffer b
        (ns/set-buffer-face-variable)
        ;; turn off quits and joins in high member count channels
        (setq-local circe-reduce-lurker-spam (> (ns/circe-count-nicks) 100)))))
  )

(defmacro ns/circe-bind (&rest binds)
  "Bind BINDS in normal/insert mode, in both channel and query buffers."
  `(progn
     (ns/inmap 'circe-channel-mode-map ,@binds)
     (ns/inmap 'circe-query-mode-map ,@binds)))

(ns/circe-bind (kbd "RET") 'lui-send-input)
(ns/circe-bind "<up>"      'lui-previous-input)
(ns/circe-bind "<down>"    'lui-next-input)
(ns/circe-bind (kbd "C-e") 'lui-previous-input)
(ns/circe-bind (kbd "C-n") 'lui-next-input)
(ns/circe-bind [(shift return)] (fn! (insert "\n")))

(defun circe-command-NP (&optional _)
  (interactive "sAction: ")
  (circe-command-ME (concat "is now playing " (ns/shell-exec "music infoname"))))

(defun circe-command-SHRUG (content)
  (circe-command-SAY (concat content " ¯\\_(ツ)_/¯")))

(defun circe-command-CS (content)
  (circe-command-MSG "chanserv" content))

(defun circe-command-EXEC (content)
  (->> content
    ns/shell-exec
    (s-split "\n")
    (cons (format "$ %s" content))
    (mapc 'circe-command-SAY)))

(defun circe-command-LIST (_)
  (irc-send-command (circe-server-process) "LIST"))

(ns/bind
  "ai" (fn!
         (ns/init-circe)
         (connect-all-irc))
  "ni" 'ns/jump-irc
  "nI" (fn! (counsel-switch-to-buffer-or-window "*circe-highlight*"))
  ;; ehhh
  ;; "nr" 'ns/goto-last-highlight
  )

(defun ns/last-face (point-current face-search)
  "Find the end of text last containing the face face-search"
  (let* ((point-change (- (previous-single-property-change point-current 'face) 1))
          (face-at-point (get-char-property point-change 'face))
          ;; 'face can return a list of faces, or just a single face
          (faces-at-point
            (if (eq nil face-at-point)
              (list nil)
              (if (listp face-at-point) face-at-point (list face-at-point)))))
    (if (< point-change 2)
      1
      ;; nil
      ;; (message (format "%s" face-search))
      (if (-contains-p faces-at-point face-search)
        point-change
        (ns/last-face point-change face-search))
      )))

(defun! ns/get-last-nick (&optional point)
  "Get the nick of whoever talked at point"
  (save-excursion
    (let ((next (ns/last-face (or point (point)) 'circe-originator-face)))
      (goto-char next)
      (if (s-blank-p (thing-at-point 'symbol))
        (ns/get-last-nick next)
        (thing-at-point 'symbol)
        ))))

(defun! ns/kill-all-irc ()
  ;; XXX this should be a let of some kind
  ;; doesn't appear to have any effect?
  ;; (fset 'yes-or-no-p (fn t))
  (mapcar 'kill-buffer (ns/buffers-by-mode 'circe-server-mode))
  ;; (fset 'yes-or-no-p 'y-or-n-p)
  )

;; todo: if this is region, quote the region rather than whole line
(defun! ns/circe-quote ()
  "quote whoever spoke at point -- conflict potential in the first 7 chars of a nick"

  (let* ((quote-start (+ 1 (ns/last-face (point) 'circe-originator-face)))
          (quote-end (point-at-eol))
          (quote-text (buffer-substring-no-properties quote-start quote-end))
          (sayer-display (ns/get-last-nick))
          (sayer
            (if (s-ends-with-p "…" sayer-display)
              (first
                (-filter
                  (fn (s-starts-with-p (substring sayer-display 0 7) <>))
                  (circe-channel-nicks)))
              sayer-display)))
    (goto-char (point-max))
    (if sayer
      ;; the propertize is so the text isn't read only
      (insert (propertize (format "<%s> %s" sayer (s-trim quote-text)) 'read-only nil))
      (insert (propertize (format "> %s" (s-trim quote-text)) 'read-only nil)))))

(ns/bind-mode 'circe-channel
  "qc" 'ns/circe-count-nicks-message
  "nq" 'ns/circe-quote
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
                (format (ns/make-message "*" "%s") format)
                (format (ns/make-message name "%s") format))))
        (circe-display 'circe--irc-format-server-numeric
          :nick (or nick "(unknown)")
          :userhost (or userhost "server")
          :origin origin
          :event event
          :command event
          :target name
          :indexed-args args)))))

(ns/comment
  ;; trying out treemacs idea -- want buflist but for circe buffers
  (use-package treemacs)

  ;; treemacs-define-* stuff isn't included ootb with the above?
  (require 'treemacs)

  (defun showcase--get-buffer-groups ()
    "Get the list of buffers, grouped by their major mode."
    (->> (buffer-list)
      (--reject (eq ?\ (aref (buffer-name it) 0)))
      (--group-by (buffer-local-value 'major-mode it)))
    )

  (defun showcase--get-buffer-groups ()
    "Get the list of buffers, grouped by their major mode."
    (let ((message-buffers (ns/buffers-by-mode 'circe-query-mode 'circe-channel-mode)))
      (-map
        (lambda (server-buffer)
          (cons
            (->> (prin1-to-string server-buffer)
              (s-split " ")
              (nth 1)
              (s-split ":")
              (nth 0)
              ;; (apply 'quote)
              (intern)
              )

            (-filter
              (lambda (message-buffer)
                (eq server-buffer
                  (buffer-local-value 'circe-server-buffer message-buffer)))
              message-buffers
              ))
          )
        (ns/buffers-by-mode 'circe-server-mode)
        )))

  (first
    (showcase--get-buffer-groups)

    ;; #<buffer irc.rizon.net:6697>
    )

  (defun showcase-visit-buffer (&rest _)
    "Switch to the buffer saved in node at point."
    (let* ((node (treemacs-current-button))
            (buffer (treemacs-button-get node :buffer)))
      (when (buffer-live-p buffer)
        (select-window (next-window))
        (switch-to-buffer buffer))))

  ;; todo: different buffer types -- activity, highlights, no activity
  (treemacs-define-leaf-node buffer-leaf
    (treemacs-as-icon "X " 'face 'font-lock-builtin-face)
    :ret-action #'showcase-visit-buffer
    :tab-action #'showcase-visit-buffer
    :mouse1-action
    (lambda (&rest args) (interactive) (showcase-visit-buffer args))
    )

  (treemacs-define-expandable-node buffer-group
    :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
    :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
    :query-function (treemacs-button-get (treemacs-current-button) :buffers)
    ;; todo: render action here is what should change
    :render-action
    (treemacs-render-node
      :icon treemacs-buffer-leaf-icon
      :label-form (buffer-name item)
      :state treemacs-buffer-leaf-state
      :face 'font-lock-string-face
      :key-form item
      :more-properties (:buffer item)))

  (treemacs-define-expandable-node buffers-root
    :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
    :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
    :query-function (showcase--get-buffer-groups)
    :render-action
    (treemacs-render-node
      :icon treemacs-icon-buffer-group-closed
      :label-form (symbol-name (car item))
      :state treemacs-buffer-group-closed-state
      :face 'font-lock-keyword-face
      :key-form (car item)
      :more-properties (:buffers (cdr item)))
    :root-marker t
    :root-label "Buffers"
    :root-face 'font-lock-type-face
    :root-key-form 'Buffers)

  (defun showcase-extension-predicate (project)
    t
    ;; (eq project
    ;;   (-> (treemacs-current-workspace)
    ;;     (treemacs-workspace->projects)
    ;;     (car)))
    )

  ;; testing with this
  (treemacs-define-project-extension
    :extension #'treemacs-BUFFERS-ROOT-extension
    :predicate #'showcase-extension-predicate
    :position 'top)

  ;; can't get this to work atm
  (treemacs-define-top-level-extension
    :extension #'treemacs-BUFFERS-ROOT-extension
    :position 'top
    )

  (treemacs-scope-storage)

  )


;; todo: maybe turn off all ctcp handling

;; (showcase--get-buffer-groups)
