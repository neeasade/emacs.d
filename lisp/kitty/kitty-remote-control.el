;;; kitty-remote-control.el --- Kitty remote control  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ravi Kiran

;; Author: Ravi Kiran <aine.marina@gmail.com>
;; Keywords: terminals

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Remote control of kitty terminals

;;; Code:

(require 'json)
(require 'subr-x)

(defconst kitty-rc-min-version [0 19 3] "Minimum kitty version used in commands")
(defconst kitty-rc-command-prefix "\eP@kitty-cmd")
(defconst kitty-rc-command-suffix "\e\\")

(defun kitty-rc--construct-command-string (command payload min-version response)
  (concat
   kitty-rc-command-prefix
   (json-encode `(("cmd" . ,command)
                  ("version" . ,(or min-version kitty-rc-min-version))
                  ("no_response" . ,(not response))
                  ,@(and payload `(("payload" . ,payload)))))
   kitty-rc-command-suffix))

(defun kitty-rc--remote-control-response ()
  (let ((str "")
        prev-chr
        chr
        parsed
        payload)
    ;; The reply should be: \eP@kitty-cmd{"ok": true, "data": payload}\e\\
    (while (and (setq chr (xterm--read-event-for-query))
                (not (and (equal prev-chr ?\e) (equal chr ?\\))))
      (when prev-chr (setq str (concat str (string prev-chr))))
      (setq prev-chr chr))
    (setq parsed-data (json-parse-string str))
    (when (and (hash-table-p parsed-data) (eql (gethash "ok" parsed-data) t))
      (setq payload (gethash "data" parsed-data)))
    payload))

(defun kitty-rc-posted-command (command payload &optional min-version)
  (send-string-to-terminal
   (kitty-rc--construct-command-string command payload min-version nil)))

(defun kitty-rc-command (command payload handler &optional min-version)
  (xterm--query (kitty-rc--construct-command-string command payload min-version t)
    (list (cons kitty-rc-command-prefix
            (lambda ()
              (if-let* ((response-json (kitty-rc--remote-control-response)))
                (funcall handler response-json)
                (user-error "Kitty response failed")))))))

;; Applied commands
(defun kitty-rc-new-window (&optional os)
  "New kitty window (default OS, kitty if KITTY-WIN is non-nil)"
  (interactive "P")
  (kitty-rc-posted-command "new-window" `(("window_type" . ,(if os "os" "kitty")))))
(defun kitty-rc-focus-window (window-id)
  "Focus kitty window with id WINDOW-ID"
  (interactive "N")
  (kitty-rc-posted-command "focus-window" `(("match" . ,(format "id:%d" window-id)))))
(defun kitty-rc-launch-background (cmd)
  "Launch CMD (must be a vector) in kitty's background"
  (kitty-rc-posted-command "launch"
    `(("type" . "background")
       ("args" . ,cmd))))

;; Common commands
(defvar kitty-rc-browser '("firefox")
  "Browser arguments for kitty-rc-browse-url")
(defun kitty-rc-browse-url (url &rest args)
  "Browse URL by launching browser remotely"
  (kitty-rc-launch-background
   (apply #'vector
          (flatten-list
           (list kitty-rc-browser (browse-url-encode-url url) args)))))
(defun kitty-rc-browse-urls-remotely ()
  (interactive)
  (setq browse-url-browser-function #'kitty-rc-browse-url))

;; Clipboard handling: could be generalized
(defvar kitty-rc-clipboard-max-length 10000
  "Maximum length of characters copied to clipboard")
(defun kitty-rc-copy-to-clipboard (string &optional append use-primary)
  "Copy STRING to clipboard"
  (if-let* ((out-len (+ (* (length string) 3) 2))
            (allowed (< out-len kitty-rc-clipboard-max-length))
            (out-str (base64-encode-string (encode-coding-string string 'binary)))
            (pfx (concat "\e]52;" (if use-primary "p" "c") ";"))
            (sfx "\e\\"))
      (progn
        (unless append
          (send-string-to-terminal (concat pfx "!" sfx)))
        (send-string-to-terminal (concat pfx out-str sfx))
        (sit-for 0.1))
    (message "Selection too long to send to terminal")))

(defun kitty-rc-interprogram-cut-function (text)
  (when select-enable-primary
    (kitty-rc-copy-to-clipboard text nil t))
  (when select-enable-clipboard
    (kitty-rc-copy-to-clipboard text nil nil)))

(defun kitty-rc-set-interprogram-cut-function ()
  (interactive)
  (setq interprogram-cut-function #'kitty-rc-interprogram-cut-function))

(provide 'kitty-remote-control)
;;; kitty-remote-control.el ends here
