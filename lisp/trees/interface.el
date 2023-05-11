;; -*- lexical-binding: t; -*-

(defun ns/pick (one &optional two)
  "Pick something from a list. accepts (prompt candidates) or (candidates)"
  (llet [(prompt candidates) (if two
                               (list (format "%s: " one) two)
                               (list "select: " one))]
    (completing-read prompt (-uniq candidates))))

(global-set-key (kbd "C-e") 'previous-line)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(ns/use vertico
  (vertico-mode t)

  (defun! ns/vertico-select-promptext ()
    (vertico--goto -1)
    (vertico-exit-input))

  (general-define-key
    :keymaps 'vertico-map
    (kbd "C-RET")  'ns/vertico-select-promptext
    (kbd "C-<return>") 'ns/vertico-select-promptext)

  (setq vertico-resize nil)             ; fixed-size

  (ns/face 'vertico-current :extend nil)
  (defun ns/set-vertico-count (&rest _)
    (setq vertico-count (/ (frame-height) 2)))
  (advice-add 'completing-read :before 'ns/set-vertico-count))

(ns/use marginalia (marginalia-mode t))

(ns/use consult
  (setq-ns consult-async-input-throttle 0.04
    consult-async-input-debounce 0.02))

(ns/use orderless
  ;; stolen directly from:
  ;; https://github.com/minad/consult/wiki#minads-orderless-configuration
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
        consult--tofu-char
        (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
      ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
      ((string-suffix-p "$" word)
        `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
      ;; File extensions
      ((and (or minibuffer-completing-file-name
              (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word))
        `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
    ;; Enable partial-completion for files.
    ;; Either give orderless precedence or partial-completion.
    ;; Note that completion-category-overrides is not really an override,
    ;; but rather prepended to the default completion-styles.
    ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
    completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                     ;; enable initialism by default for symbols
                                     (command (styles +orderless-with-initialism))
                                     (variable (styles +orderless-with-initialism))
                                     (symbol (styles +orderless-with-initialism)))
    orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
    orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                  #'orderless-affix-dispatch)))

(ns/use counsel)

(ns/use alert
  (setq alert-default-style
    (cond
      (ns/enable-windows-p 'toaster)
      (ns/enable-mac-p 'osx-notifier)
      (t 'libnotify)))

  ;; I could not get the (alert :persistent t keyword to work)
  (defun alert! (&rest alert-args)
    (let ((alert-fade-time 0))
      (apply 'alert alert-args))))

(ns/use which-key
  (setq-ns which-key
    idle-delay 1.5
    side-window-max-width 0.33
    sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(defun! ns/kill-other-buffers ()
  "Kill all other buffers."
  (->> (buffer-list)
    (-remove (lambda (b) (eq b (current-buffer))))
    (-map 'kill-buffer)))

(defun! ns/font-change ()
  (llet [current-size (/ (face-attribute 'default :height) 10)
          new-size (read-number (format "new size (current-size: %s): " current-size))]
    (ns/face 'default :height (* 10 new-size))))

(defun! ns/kill-buffers-missing-file ()
  "Kill buffers referencing a file that doesn't exist (EG, the file may have moved or been deleted)"
  (->> (buffer-list)
    (-keep
      (fn (llet [filename (buffer-local-value 'buffer-file-truename <>)]
            (when (and filename
                    (not (f-exists-p filename)))
              <>))))
    (-map #'kill-buffer)))

(defun! ns/kill-buffers-by-mode ()
  (->> (buffer-list)
    (-map (-partial 'buffer-local-value 'major-mode))
    (-uniq)
    (ns/pick "mode to kill")
    (intern)
    (ns/buffers-by-mode)
    (-map #'kill-buffer)))

(ns/bind "nd"
  (fn!! surf-dirs
    (llet [dir (ns/pick "directory"
                 (->> ns/cd-dirs
                   (-uniq)
                   (-filter (fn (s-equals-p (file-remote-p <>)
                                  (file-remote-p default-directory))))))]
      (if-not (eq major-mode 'shell-mode)
        (dired dir)
        (progn
          (goto-char (point-max))
          (insert (format "cd \"%s\"" (s-replace (or (file-remote-p dir) "") "" dir)))
          (comint-send-input))))))

(when-not window-system
  ;; (when running in a terminal)

  ;; this doesn't work in kitty (might be related to new esc code things)
  (comment (xterm-mouse-mode 1))
  ;; (xterm-mouse-mode nil)

  ;; these don't appear to be adding anything
  ;; (ns/use xclip (xclip-mode nil))
  ;; (ns/use clipetty (global-clipetty-mode t))

  ;; C-i and <tab> are equivalent in the terminal
  ;; (until kitty saves us all)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)

  (define-key evil-motion-state-map (kbd "C-i")
    'better-jumper-jump-forward))

(winner-mode 1)

(defun! ns/surf-urls ()
  "jump to url in current window text"

  ;; has a nice url regexp
  (require 'rcirc)

  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text))
          (urls (-map 'car urls)))
    (if urls
      (browse-url (ns/pick urls))
      (message "no urls!"))))

(ns/bind "nu" 'ns/surf-urls)

(ns/bind
  "/" (if (which "rg") 'consult-ripgrep 'consult-grep)


  "th" 'hl-line-mode

  ;; from the current dir down
  "?" (fn!! grep-here
        (funcall (if (which "rg") 'consult-ripgrep 'consult-grep)
          default-directory))

  "SPC" (fn!! (execute-extended-command nil))

  ;; windows
  "w" '(:ignore t :which-key "Windows")
  "wh" 'evil-window-left
  "wn" 'evil-window-down
  "we" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  "ww" 'other-window
  "wb" 'balance-windows-area

  "ws" (fn!! (split-window-horizontally)
         (evil-window-right 1))

  "wS" (fn!! (split-window-vertically)
         (evil-window-down 1))

  "wf" (fn!! (follow-mode)
         (delete-other-windows)
         (evil-window-vsplit))

  "wm" 'delete-other-windows ;; "window max"

  "wo" 'winner-undo
  "wi" 'winner-redo

  "a" '(:ignore t :which-key "Applications")
  "q" '(:ignore t :which-key "Query")

  ;; "b" '(:ignore t :which-key "Buffers")

  "bb" (fn!! surf-buffers
         (->> (ns/jump-file-candidates :buffers-without-files)
           (ns/pick "buffer")
           (ns/find-or-open)))

  "bm" (fn!! surf-buffers-mode
         (->> (ns/buffers-by-mode major-mode)
           (-map 'buffer-name)
           (ns/pick "buffer")
           (ns/find-or-open)))

  "br" 'revert-buffer

  "bd" (fn!! (kill-buffer nil))
  "bn" (fn!! buffer-same-name
         (let ((current-filename (f-filename (buffer-file-name (current-buffer)))))
           (->> (buffer-list)
             (-map 'buffer-name)
             (--filter (s-starts-with-p current-filename it))
             (ns/pick)
             (ns/find-or-open))))
  ;; "bK" 'ns/kill-other-buffers
  ;; "bk" 'kill-matching-buffers
  ;; "bm" 'ns/kill-buffers-by-mode

  "n" '(:ignore t :which-key "Jump")
  "nh" 'counsel-imenu
  )

(ns/use (deadgrep :host github :repo "Wilfred/deadgrep")
  (setq deadgrep-max-line-length 180)
  (ns/bind "ss" 'deadgrep)
  (general-nmap deadgrep-mode-map
    "RET" 'deadgrep-visit-result-other-window))

(defun! ns/helpful-or-dashdoc ()
  (cond
    ((eq 'emacs-lisp-mode major-mode)
      (helpful-callable (helpful--symbol-at-point)))
    ((eq 'clojure-mode major-mode) (cider-apropos-documentation))
    (ns/enable-dashdocs-p (ns/counsel-dash-word))
    (t (message "no doc option available!"))))

(ns/bind "nH" 'ns/helpful-or-dashdoc)
