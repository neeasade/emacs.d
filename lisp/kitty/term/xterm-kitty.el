;;; xterm-kitty.el --- kitty terminal support

;; Copyright (C) 2021  Ravi Kiran

;; Author: Ravi Kiran <aine.marina@gmail.com>
;; Keywords:

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

;; Work-in-progress to support kitty terminal with all modifiers
;;   - still need to
;;     + ensure proper 24-bit color support
;;     + add all keys specified in private unicode space
;;     + figure out a way to split keybindings between kitty and non-kitty
;;       frames

;;; Code:

(require 'term/xterm)
(require 'kitty-keyboard-protocol)
(require 'kitty-remote-control)

(defun xterm-kitty-in-use (&optional frame)
  "Check whether FRAME is running under kitty terminal."
  (terminal-parameter frame 'kitty-window-id))

(defun xterm-kitty-make-binding-sequence (default key &rest modifiers)
  "Make a key vector for KEY with VECTORS suitable for binding with 'define-key' if xterm-kitty is active.

DEFAULT is the value to be returned if xterm-kitty is not active.
KEY is the key to be used. MODIFIERS is a list of modifiers, or
modifiers specified explicitly.

'define-key' uses 'event-convert-list' internally, which strips
off shift modifiers for alphabetic characters. The only way to
avoid it is to provide the key vector itself to 'define-key',
which this function explicitly creates. In that sense, this
function is almost equivalent to 'event-convert-list'."
  (if (xterm-kitty-in-use)
      (vector (kitty-kbp--add-modifier-list modifiers key))
    default))

(defun xterm-kitty--handle-escape-code (prompt)
  "Handle keycode; PROMPT is ignored"
  (let ((keyc (kitty-kbp--handle-escape-code prompt)))
    (pcase keyc
      (`(200 0 ?~) (xterm-translate-bracketed-paste nil))
      (`(0 0 ?I) (xterm-translate-focus-in nil))
      (`(0 0 ?O) (xterm-translate-focus-out nil))
      ((pred listp) (message "Unknown key: keycode %d, modifiers %d, suffix %s"
                             (car keyc) (cadr keyc) (string (caddr keyc))))
      (_ keyc))))

(defun xterm-kitty--setup-basic-keymap (kmap)
  ;; Terminal mouse handling
  (define-key kitty-kbp-basic-map "\e[200~" #'xterm-translate-bracketed-paste)
  (define-key kitty-kbp-basic-map "\e[I" #'xterm-translate-focus-in)
  (define-key kitty-kbp-basic-map "\e[O" #'xterm-translate-focus-out)
  ;; (xterm-kitty--insert-decode-table kmap)
  ;; (setq input-decode-map kitty-kbp-basic-map)
  (xterm--push-map kitty-kbp-basic-map kmap))

;; To do: debug the reason that the table method does not work
;;   - the table does not seem to be reflected in input-decode-map
;;   - the table method is needed for those packages which advice
;;     low-level functions such as read-char, e.g., multiple-cursors
(defvar xterm-kitty-use-table-method nil
  "Use table method to handle character map")

(defun xterm-kitty-apply-keyboard (&optional keymap alternate-keymap)
  "Apply keyboard defintion; optionally to KEYMAP and ALTERNATE-KEYMAP."
  (let* ((kmap (or keymap
                   (and (xterm-kitty-in-use) input-decode-map)))
         (alternate-kmap (or alternate-keymap
                             (and kmap
                                  (not keymap)
                                  key-translation-map))))
    (when kmap
      (unless keymap                    ; default keymap was used
        (kitty-kbp-setup-terminal t))
      (if xterm-kitty-use-table-method
          (xterm-kitty--setup-basic-keymap kmap)
        (define-key kmap kitty-kbp-escape-prefix #'xterm-kitty--handle-escape-code)))
    (when (and alternate-kmap)
      (xterm--push-map kitty-kbp-legacy-control-map alternate-kmap))))

(defun xterm-kitty-window-id (&optional terminal) ; public API
  (terminal-parameter terminal 'kitty-window-id))

(defun xterm-kitty--save-kitty-window-id (response-json)
  ;; (message "Saving window id...")
  (let (window-id)
    (mapc (lambda (os-win)
            (mapc (lambda (tab)
                    (mapc (lambda (win)
                            (let ((is-self (eq (gethash "is_self" win) t))
                                  (win-id (gethash "id" win)))
                              (when is-self
                                (if window-id
                                    (message "Multiple windows match: using %s, not %s" window-id win-id)
                                  (setq window-id win-id)))))
                          (gethash "windows" tab)))
                  (gethash "tabs" os-win)))
          (json-parse-string response-json))
    (set-terminal-parameter nil 'kitty-window-id window-id)))

(defun xterm-kitty-save-window-id ()
  "Save kitty window ID of current terminal"
  (kitty-rc-command "ls" nil #'xterm-kitty--save-kitty-window-id))

(defun xterm-kitty-focus (frame-or-window)
  "Set focus to terminal containing FRAME-OR-WINDOW

   FRAME-OR-WINDOW can be a terminal, a frame, or a window"
  (let* ((frame (if (windowp frame-or-window) (window-frame frame-or-window) frame-or-window))
         (kitty-window-id (xterm-kitty-window-id (frame-terminal frame))))
    ;; (message "Window id %d for %s" kitty-window-id frame)
    (if kitty-window-id
        (kitty-rc-focus-window kitty-window-id)
      (message "%s is not a kitty window" frame-or-window))))
(defun xterm-kitty-select-frame-set-input-focus-advice (old-function frame &optional no-record)
  (or (when (xterm-kitty-in-use frame)
        ;; (message "Switching to frame: %s" frame)
        (xterm-kitty-focus frame)
        (select-frame frame no-record))
      (funcall old-function frame no-record)))
(defun xterm-kitty-visible-window-advice (old-function &optional window minibuf all-frames)
  (when (xterm-kitty-in-use)
    ;; Terminal emacs thinks that only one frame is ever visible
    (funcall old-function window minibuf (if (eql all-frames 'visible) t all-frames))))

(defvar xterm-kitty--skip-select-frame-set-input-focus-advice nil
  "Set to true to disable setting input focus advice")
(defun xterm-kitty-add-select-frame-set-input-focus-advice (&optional print-message)
  "Advise SELECT-FRAME-SET-INPUT-FOCUS to handle xterm-kitty terminal windows"
  (interactive "p")
  (if xterm-kitty--skip-select-frame-set-input-focus-advice
      (when print-message
        (message "Input focus advice for selecting frames disabled"))
    (advice-add 'select-frame-set-input-focus :around #'xterm-kitty-select-frame-set-input-focus-advice)
    (advice-add 'next-window :around #'xterm-kitty-visible-window-advice)
    (advice-add 'previous-window :around #'xterm-kitty-visible-window-advice)
    (setq xterm-kitty--skip-select-frame-set-input-focus-advice t)))

(defun xterm-kitty-new-os-window ()
  "Open new xterm-kitty os window"
  (interactive)
  (when (xterm-kitty-in-use)
    (kitty-rc-new-window t)))

(defun terminal-init-xterm-kitty ()
  "Terminal initialization function for kitty"
  ;; Order is important here: window id must be saved prior to any
  ;; other xterm-kitty function calls in this function.
  (xterm-kitty-save-window-id)
  (xterm-kitty-apply-keyboard)

  ;; Standard xterm-like initialization
  (xterm-register-default-colors xterm-standard-colors)
  (tty-set-up-initial-frame-faces)

  ;; Steal private functions from term/xterm that kitty supports
  (xterm--query "\e]11;?\e\\"
                '(("\e]11;" .  xterm--report-background-handler)))
  (xterm--init-activate-get-selection)
  (xterm--init-activate-set-selection)
  (when xterm-set-window-title
    (xterm--init-frame-title))
  (xterm--init-bracketed-paste-mode)
  (xterm--init-focus-tracking)

  (run-hooks 'terminal-init-xterm-kitty-hook))

(provide 'term/xterm-kitty)
;;; xterm-kitty.el ends here
