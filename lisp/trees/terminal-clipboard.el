;; terminal clipboard syncing

;; clipetty works one way (emacs -> clip, not clip -> emacs)
(ns/use clipetty
  (setenv "SSH_TTY" nil)                  ; pretty much never want this?
  (global-clipetty-mode (if ns/term? t -1))) ; osc 52

(defun clipetty--emit (string)
  "Emit STRING, optionally wrapped in a DCS, to an appropriate tty."
  (let ((tmux    (getenv "TMUX" (selected-frame)))
         (term    (getenv "TERM" (selected-frame)))
         (ssh-tty (getenv "SSH_TTY" (selected-frame))))
    ;; foot's osc52 limit is like 2gb
    (if (or (string= term "foot") (<= (length string) clipetty--max-cut))
      (write-region
        (clipetty--dcs-wrap string tmux term ssh-tty)
        nil
        (clipetty--tty ssh-tty tmux)
        t
        0)
      (message "Selection too long for osc52 (length: %d)" (length string)))))

(defun ns/osc52-read ()
  ;; taken from xterm.el
  (let* ((type 'CLIPBOARD)
          (query (concat "\e]52;" (xterm--selection-char type) ";")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (xterm--query
        ;; Use ST as query terminator to get ST as reply terminator (bug#36879).
        (concat query "?\e\\")
        (list (cons query
                (lambda ()
                  ;; Read data up to the string terminator, ST.
                  (let (char last)
                    (while (and (setq char (read-char
                                             nil nil
                                             xterm-query-timeout))
                             (not (and (eq char ?\\)
                                    (eq last ?\e))))
                      (when last
                        (insert last))
                      (setq last char))))))
        'no-async)
      (base64-decode-region (point-min) (point-max))
      (decode-coding-region (point-min) (point-max) 'utf-8-unix t))))

(defun ns/get-clipboard ()
  ;; nb: osc52-read not working on windows alacritty
  ;; if wslg is stuck, wl-paste just hangs forever
  (llet [result (if ns/enable-wsl-p
                  (sh "timeout 1 wl-paste | dos2unix")
                  (ns/osc52-read))]
    (when-not (s-blank? result)
      result)))

;; nb: focus hook not working on dtach resume - workaround by connecting with emacsclient
(defun ns/sync-terminal-clipboard ()
  (interactive)
  (when (frame-focus-state)
    (when-let (clip (ns/get-clipboard))
      ;; (message (ns/str "killing " clip))
      (kill-new clip))))

(when ns/term?
  (add-function :after after-focus-change-function #'ns/sync-terminal-clipboard))

;; (remove-function after-focus-change-function #'ns/sync-terminal-clipboard)

;; (ns/bind "ip" (fn!! paste-gui (insert (sh "wl-paste | dos2unix"))))
(ns/bind "ip" (fn!! paste-gui (insert (ns/get-clipboard))))
