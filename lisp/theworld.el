;;; theworld.el --- make the thing
;;; commentary:
;;; functions             | ns/asdf
;;; pred functions        | ns/asdf-p
;;; interactive functions | ns/asdf
;;; enable vars           | ns/enable-asdf-p
;;; vars                  | ns/asdf
;;; buffer local vars     | ns/enable-asdf
;;; code:

(setq
  ns/enable-windows-p (eq system-type 'windows-nt)
  ns/enable-linux-p (eq system-type 'gnu/linux)
  ns/enable-home-p (string= (system-name) "erasmus")
  ns/enable-docker-p (string= (getenv "USER") "emacser")
  ns/enable-work-p ns/enable-windows-p
  )

;; docker container user, still act trimmed/assume windows
(when ns/enable-docker-p
  (setq
    ns/enable-linux-p nil
    ns/enable-windows-p t
    ns/enable-work-p t
    ))

(setq ns/xrdb-fallback-values
  ;; for when we're away from $HOME.
  `(("*.background"         . ,(face-attribute 'default :background))
     ("Emacs.powerlinescale" . "1.1")
     ("Emacs.theme"          . "base16-grayscale-light")
     ("emacs.powerline"      . "bar")
     ("st.borderpx"          . "30")
     ("st.font"              . "Go Mono-10")
     ("st.font_variable"     . "Go-10")
     ))

;; master
(defmacro defconfig-base (label &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) nil
     ,@body))

;; commander
(defmacro defconfig (label &rest body)
  `(defconfig-base ,label
     (let ((config-name ,(prin1-to-string label)))
       (message (concat "loading " config-name "..."))
       (catch 'config-catch
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) nil)
         ,@body
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) t)
         ))))

;; guards!
(defmacro ns/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
    '(when t (throw 'config-catch (concat "config guard " config-name)))))

;; interactive
(defmacro defcommand (label args &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) ,args
     (interactive)
     ,@body))

(defconfig use-package
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (setq use-package-always-ensure t)
  )

(defconfig straight
  (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
         (bootstrap-version 2))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (setq straight-cache-autoloads t)
  )

(defconfig bedrock
  (use-package s)
  (use-package f)
  (use-package hydra)
  (use-package general)
  (use-package request)
  (require 'seq)

  (defmacro ns/shell-exec(command)
    "trim the newline from shell exec"
    `(replace-regexp-in-string "\n$" ""
       (shell-command-to-string ,command)))

  (defun ns/shell-exec-dontcare (command)
    (let* (
            (bufname (concat "*killme-shell" (number-to-string (random)) "*"))
            (junk-buffer (get-buffer-create bufname))
            )
      (shell-command command junk-buffer)
      (kill-buffer junk-buffer)))


  (defun mapcar* (f &rest xs)
    "MAPCAR for multiple sequences F XS."
    (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
        (apply 'mapcar* f (mapcar 'cdr xs)))))

  ;; setq namespace
  (defmacro setq-ns (namespace &rest lst)
    `(mapcar*
       (lambda (pair)
         (let ((key (car pair))
                (value (car (cdr pair))))
           (set
             (intern (concat (prin1-to-string ',namespace) "-" (prin1-to-string key)))
             (eval value)
             )))
       (seq-partition ',lst 2)
       ))

  (defun ~ (path)
    (concat (getenv (if ns/enable-windows-p "USERPROFILE" "HOME")) "/" path))

  ;; todo: take a look at general-describe-keybindings later
  ;; binding wrappers
  (defmacro ns/bind (&rest binds)
    `(general-define-key
       :states '(normal visual)
       :prefix "SPC"
       ,@binds
       ))

  (defmacro ns/bind-mode(keymaps &rest binds)
    `(general-define-key
       :prefix "SPC"
       :states '(visual normal)
       :keymaps ,keymaps
       ,@binds))

  (defmacro ns/bind-leader-mode (mode &rest binds)
    `(general-define-key
       :prefix ","
       :states '(visual normal)
       :keymaps (intern (concat (symbol-name ,mode) "-mode-map"))
       ,@binds))

  ;; this was removed
  ;; cf https://github.com/abo-abo/swiper/pull/1570/files#diff-c7fad2f9905e642928fa92ae655e23d0L4500
  (defun counsel-switch-to-buffer-or-window (buffer-name)
    "Display buffer BUFFER-NAME and select its window.

 This behaves as `switch-to-buffer', except when the buffer is
 already visible; in that case, select the window corresponding to
 the buffer."
    (let ((buffer (get-buffer buffer-name)))
      (if (not buffer)
        (shell buffer-name)
        (let (window-of-buffer-visible)
          (catch 'found
            (walk-windows (lambda (window)
                            (and (equal (window-buffer window) buffer)
                              (throw 'found (setq window-of-buffer-visible window))))))
          (if window-of-buffer-visible
            (select-window window-of-buffer-visible)
            (switch-to-buffer buffer))))))

  (defcommand find-or-open (filepath)
    "Find or open FILEPATH."
    (let
      ((filename (file-name-nondirectory filepath)))
      (if (get-buffer filename)
        (counsel-switch-to-buffer-or-window filename)
        (find-file filepath)
        )))

  ;; wrap passwordstore
  (defun pass (key)
    (ns/shell-exec (concat "pass " key)))

  (defun get-resource (name)
    "Get X resource value, with a fallback value NAME."
    (let ((default (cdr (assoc name ns/xrdb-fallback-values)))
           (result (if (executable-find "xrq")
                     (ns/shell-exec (format "xrq '%s' 2>/dev/null" name))
                     "")))
      (if (string= result "") default result)))

  (defun reload-init()
    "Reload init.el with straight.el."
    (interactive)
    (straight-transaction
      (straight-mark-transaction-as-init)
      (message "Reloading init.el...")
      (load user-init-file nil 'nomessage)
      (message "Reloading init.el... done.")))

  (let ((extend-file (~ "extend.el")))
    (when (file-exists-p extend-file)
      (eval-and-compile (load extend-file))))

  ;; a macro for when something is not on melpa yet (assumes github)
  (defmacro ns/use-package (name repo &rest config)
    `(progn
       (straight-use-package '(,(make-symbol (symbol-name name)) :host github :repo ,repo))
       ;; assume first arg is :config
       ,@(cdr config)))
  )

(defconfig util
  (use-package pcre2el)

  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defcommand what-line ()
    "Print the current line number (in the buffer) of point."
    (save-restriction
      (widen)
      (save-excursion
        (beginning-of-line)
        (1+ (count-lines 1 (point))))))

  ;; todo: this isn't working with anchors in other frames
  (defun ns/eww-browse-existing-or-new (url)
    "If eww is displayed, use that for URL, else open here."
    (if (get-buffer-window "*eww*" 0)
      (url-retrieve url 'eww-render
        (list url nil (get-buffer "*eww*")))
      (eww url)))

  (defun ns/color-is-light-p (name)
    (let*
      ((rgb (color-name-to-rgb name))
        (red (first rgb))
        (green (second rgb))
        (blue (third rgb))
        ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
        (yiq (+ (* red .299) (* green .587) (* blue .114))))
      (>= yiq 0.5)
      ))

  (defun ns/color-tone (name light dark)
    "tone name a percent based on if light or dark - generally want softer value for dark."
    (if (ns/color-is-light-p name)
      (color-darken-name name light)
      (color-lighten-name name dark)))

  (defun ns/what-face (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (defcommand what-major-mode ()
    "Reveal current major mode."
    (message "%s" major-mode))

  (defcommand what-minor-modes ()
    (message
      (format "%s"
        (delq nil
          (mapcar
            (lambda (x)
              (let ((car-x (car x)))
                (when (and (symbolp car-x) (symbol-value car-x))
                  x)))
            minor-mode-alist))))
    (ns/look-at-last-message)
    )

  (defun sudo-edit (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                   (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun ns/get-functions()
    (mapcar*
      (lambda(item)
        (s-chomp (s-chop-prefix "(defconfig " (car item))))
      (s-match-strings-all
        "^(defconfig [^ \(\)]+"
        (get-string-from-file (~ ".emacs.d/lisp/theworld.el")))))

  (defun ns/check-for-orphans()
    "Check to see if any defconfigs are missing from init."
    (let ((initfile (get-string-from-file (~ ".emacs.d/init.el"))))
      (mapcar
        (lambda(conf)
          (when (not (s-contains? conf initfile))
            (message (concat "orphaned function! " conf))))
        (ns/get-functions))))

  (defcommand jump-config()
    (ivy-read "config: " (ns/get-functions)
      :action
      (lambda (option)
        (interactive)
        (ns/find-or-open (~ ".emacs.d/lisp/theworld.el"))
        (goto-char (point-min))
        (re-search-forward (concat "defconfig " option))
        (ns/focus-line)
        )))

  (defcommand toggle-bloat()
    "toggle bloat in the current buffer"
    (if (not (bound-and-true-p company-mode))
      (progn
        (company-mode)
        (flycheck-mode)
        (font-lock-mode)
        (git-gutter-mode))
      (progn
        (company-mode -1)
        (flycheck-mode -1)
        (font-lock-mode 0)
        (git-gutter-mode 0))))

  (defun ns/toggle-bloat-global(toggle)
    "toggle global bloat - must be called on it's own"
    (if toggle
      (progn
        (global-company-mode)
        (global-flycheck-mode)
        (global-font-lock-mode)
        (when (not ns/enable-windows-p)
          (global-git-gutter-mode t)))
      (progn
        (global-company-mode -1)
        (global-flycheck-mode -1)
        (global-font-lock-mode 0)
        (global-git-gutter-mode 0))))

  (use-package simpleclip)

  (defcommand buffercurl ()
    "curl buffer from url grabbed from clipboard"

    (request
      (simpleclip-get-contents)
      :type "GET"
      :parser 'buffer-string
      :success
      (function*
        (lambda (&key data &allow-other-keys)
          (interactive)
          (insert data)))))

  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))

  (defcommand focus-line (&rest ignore)
    (evil-scroll-line-to-center (ns/what-line)))

  (defun ns/get-last-message()
    (with-current-buffer (get-buffer "*Messages*")
      (goto-char (point-max))
      (previous-line 1)
      (let ((beg (line-beginning-position 1))
             (end (line-beginning-position 2)))
        (buffer-substring beg end))))

  (defun ns/look-at-last-message()
    (interactive)
    (ns/find-or-open (~ ".emacs.d/lisp/scratch.el"))
    (goto-char (point-max))
    (insert "\n")
    (insert (ns/get-last-message))
    (previous-line 1)
    )

  (defun ns/parse-font (font)
    (let* ((parts (s-split "-" font))
            (family (first parts))
            (size (string-to-number (second parts))))
      ;; height is in 1/10th of pt
      `(:family ,family :height ,(* 10 size))))

  ;; (defmacro @ (input) (eval `(backquote ,input)))

  (defun ns/set-faces-variable (faces)
    (dolist (face faces)
      (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font_variable")))))

  (defun ns/set-faces-monospace (faces)
    (dolist (face faces)
      (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font")))))

  (defcommand set-buffer-face-variable ()
    (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font_variable")))
    (buffer-face-mode t))

  (defcommand set-buffer-face-monospace ()
    (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font")))
    (buffer-face-mode t))

  (ns/bind
    ;; reconsider these, moved from w -> q for query
    "qf" 'ns/what-face
    "qm" 'ns/what-major-mode
    "qi" 'ns/what-minor-modes
    "qq" 'ns/look-at-last-message

    ;; this should maybe be more generic ie mx history when not in shell
    "qh" 'counsel-shell-history

    "fE" 'sudo-edit
    "jc" 'ns/jump-config
    "tb" 'ns/toggle-bloat
    "iu" 'ns/buffercurl
    )
  )

(defconfig sanity
  (setq
    ;; todo: relook at this setting
    auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
    backup-directory-alist `(("." . ,(~ ".emacs.d/backups")))
    coding-system-for-read 'utf-8
    coding-system-for-write 'utf-8
    delete-old-versions -1
    global-auto-revert-mode t
    inhibit-startup-screen t
    initial-scratch-message ""
    ring-bell-function 'ignore
    sentence-end-double-space nil
    vc-follow-symlinks t ;; auto follow symlinks
    vc-make-backup-files t
    version-control t
    network-security-level 'high
    ;; ouch - todo: revisit this
    gc-cons-threshold 10000000
    frame-resize-pixelwise t

    ;; todo: not working for multi caps path case
    ;; eg /thing/Thing/README.md
    ;; doesn't complete at /thing/Thing/rea
    completion-ignore-case  t
    dabbrev-case-fold-search nil
    )

  ;; trim gui
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; cursor
  (show-paren-mode 1)
  (blink-cursor-mode 0)

  ;; custom
  (defconst custom-file (~ ".emacs.d/custom.el"))
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file 'noerr)

  ;; persistent session:
  ;; note: (desktop-clear) to clean/kill everything.
  (make-directory (~ ".emacs.desktop") t)
  (setq-ns desktop
    restore-eager 5
    auto-save-timeout 30
    path (list (~ ".emacs.desktop"))
    )

  ;; disabling in favor of recentf
  ;; (desktop-save-mode 1)

  (setq browse-url-browser-function 'browse-url-generic)

  (if ns/enable-windows-p
    (if (executable-find "qutebrowser")
      (setq browse-url-generic-program "qutebrowser")
      (setq browse-url-browser-function 'browse-url-default-windows-browser)
      )
    (setq browse-url-generic-program (getenv "BROWSER"))
    )

  ;; Removes *scratch* from buffer after the mode has been set.
  (add-hook 'after-change-major-mode-hook
    (lambda() (if (get-buffer "*scratch*") (kill-buffer "*scratch*"))))

  ;; disable semantic mode, this may bite me lets try it out
  (with-eval-after-load 'semantic
    (add-to-list 'semantic-inhibit-functions (lambda () t))
    )

  ;; set default to be handled by global bloat toggle
  (global-font-lock-mode 0)

  (fset 'yes-or-no-p 'y-or-n-p)

  (defcommand toggle-modeline ()
    (make-local-variable 'ns/modeline)

    (if mode-line-format
      (progn
        (setq ns/modeline mode-line-format)
        (setq mode-line-format nil))
      (setq mode-line-format ns/modeline))
    (redraw-frame))

  ;; don't ask to kill running processes when killing a buffer.
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  ;; don't popup buffers with output when launching things
  (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

  ;; save recent files
  (recentf-mode 1)
  (setq recentf-max-menu-items 300)
  (setq recentf-max-saved-items 300)

  (defun ns/save-files()
    (let ((inhibit-message t))
      (recentf-save-list)))

  (when ns/firstrun
    (run-at-time nil (* 5 60) 'ns/save-files))

  (setq whitespace-line-column 120)

  (defcommand insert-filename ()
    (insert (f-filename (buffer-file-name))))

  (defcommand insert-filepath ()
    (insert (buffer-file-name)))

  ;; a report toggle command for debugging on keybind
  (require 'profiler)
  (defcommand toggle-report ()
    (if (profiler-running-p)
      (progn
        (profiler-report)
        (profiler-stop))
      (profiler-cpu-start profiler-sampling-interval)))

  (ns/bind
    "js" (lambda() (interactive) (ns/find-or-open (~ ".emacs.d/lisp/scratch.el")))
    "jS" (lambda() (interactive) (ns/find-or-open (~ ".emacs.d/lisp/scratch.txt")))
    "jm" (lambda() (interactive) (counsel-switch-to-buffer-or-window  "*Messages*"))

    "t" '(:ignore t :which-key "Toggle")
    "tw" 'whitespace-mode
    "tn" 'linum-mode
    "tl" 'toggle-truncate-lines
    "ts" 'ns/style
    "ti" 'reload-init
    "tm" 'ns/toggle-modeline
    "tp" 'ns/toggle-report

    "i" '(:ignore t :which-key "Insert")
    "ic" 'insert-char
    "if" 'ns/insert-filename
    "ip" 'ns/insert-filepath
    )
  )

(defconfig elisp
  (ns/install-dashdoc "Emacs Lisp" 'emacs-lisp-mode-hook)
  (setq lisp-indent-function 'common-lisp-indent-function)

  (use-package helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h i") #'counsel-info-lookup-symbol)
    )

  (use-package eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)
    (ns/bind-leader-mode
      'emacs-lisp
      "er" 'eval-region
      "ei" 'eros-eval-last-sexp
      "ee" 'eros-eval-defun
      )
    ))

(defconfig evil
  (use-package evil
    ;; for evil-collection
    :init (setq evil-want-integration nil)
    :config (evil-mode 1)
    )

  (use-package evil-collection :config (evil-collection-init))

  (defun ns/zz-scroll (&rest optional)
    (let* ((scrollcount (/ (window-total-size) 7))
            (halfheight (/ (window-total-size) 2))
            (scrollcheck (- halfheight scrollcount)))
      (if (> (line-number-at-pos) scrollcheck)
        (evil-scroll-line-down scrollcount)
        )))

  (add-function :after (symbol-function 'evil-scroll-line-to-center) #'ns/zz-scroll)

  (general-evil-setup t)

  ;; defaults to fd/spacemacs-like config
  (use-package evil-escape :config (evil-escape-mode))
  (use-package evil-lion :config (evil-lion-mode))
  (use-package evil-commentary :config (evil-commentary-mode))
  (use-package evil-anzu :config (setq anzu-cons-mode-line-p nil)) ;; displays current match and total matches.
  (use-package evil-matchit :config (global-evil-matchit-mode 1))
  (use-package evil-numbers
    :config
    (general-nmap (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (general-nmap (kbd "C-c -") 'evil-numbers/dec-at-pt)
    )

  (use-package evil-fringe-mark
    :config
    (setq evil-fringe-mark-show-special nil)
    (global-evil-fringe-mark-mode t)
    )

  (use-package evil-goggles
    :config
    (setq evil-goggles-duration 0.100)
    (setq evil-goggles-pulse t)
    ;; fun visual vim mode
    (evil-goggles-mode 0)
    )

  (use-package evil-surround :config (global-evil-surround-mode 1))
  (use-package evil-embrace
    :config
    (general-define-key
      :states 'normal
      "c" (general-key-dispatch 'evil-change "s" #'embrace-change)
      "d" (general-key-dispatch 'evil-delete "s" #'embrace-delete))

    (general-define-key
      :states 'visual
      ;; `evil-change' is not bound in `evil-visual-state-map' by default but
      ;; inherited from `evil-normal-state-map'
      ;; if you don't want "c" to be affected in visual state, you should add this
      "c" #'evil-change
      "d" #'evil-delete
      "s" #'embrace-add
      )

    (evil-embrace-enable-evil-surround-integration))

  (use-package evil-snipe
    :config
    (setq evil-snipe-repeat-scope 'whole-visible)
    (setq evil-snipe-spillover-scope 'whole-visible)
    (evil-snipe-override-mode +1)
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-cx-install))

  ;; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)

  (defun djoyner/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  ;; todo: get this to hook
  ;; think it depends on gnu archive updating correctly.
  (use-package evil-org
    :commands evil-org-mode
    :after org
    :init (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (add-hook 'evil-org-mode-hook
      (lambda ()
        (evil-org-set-key-theme
          '(
             textobjects
             insert
             navigation
             additional
             shift
             todo
             heading
             )))))

  ;; persist marks
  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)

  ;; match qutebrowser fwd back
  (general-nmap
    "H" 'previous-buffer
    "L" 'next-buffer)

  (defcommand should-skip (buffername)
    (or
      ;; (member buffername '("scratch.el"))
      (s-starts-with? "*" buffername)
      (s-starts-with? "magit" buffername))
    )

  (defcommand maybe-next ()
    (when (ns/should-skip (buffer-name))
      (let ((temp (window-next-buffers)))
        (next-buffer)
        (set-window-next-buffers nil temp)
        )))

  (defcommand maybe-prev ()
    (when (ns/should-skip (buffer-name))
      (let ((temp (window-prev-buffers)))
        (previous-buffer)
        (set-window-prev-buffers nil temp)
        )))

  (advice-add #'next-buffer :after #'ns/maybe-next)
  (advice-add #'previous-buffer :after #'ns/maybe-prev)

  (general-nmap
    "]s" 'flyspell-goto-next-error
    "[b" 'evil-prev-buffer
    "]b" 'evil-next-buffer
    )

  (use-package avy
    :config

    (setq avy-all-windows 'all-frames)
    (setq avy-timeout-seconds 0.2)

    (general-mmap
      "z" 'avy-goto-char-timer)

    (general-nmap
      "s" 'avy-goto-char-timer
      "zz" 'evil-scroll-line-to-center
      )))

(defconfig flycheck
  (use-package flycheck
    :config
    ;; cf http://www.flycheck.org/en/latest/user/syntax-checks.html#check-automatically
    (setq-ns flycheck
      check-syntax-automatically (if ns/enable-windows-p
                                   '(save mode-enabled idle-change)
                                   '(save mode-enabled idle-change new-line))
      idle-change-delay 1
      )

    ;; (flycheck) disable jshint since we prefer eslint checking
    (setq-default
      flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    )

  ;; (ns/bind
  ;;   "e" '(:ignore t :which-key "Errors")
  ;;   "en" 'flycheck-next-error
  ;;   "ep" 'flycheck-previous-error
  ;;   )

  (general-nmap
    "]e" 'flycheck-next-error
    "[e" 'flycheck-previous-error
    ))

(defconfig treemacs
  (use-package treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  )

(defconfig company
  (use-package company
    :config
    (setq-ns company
      idle-delay (if ns/enable-windows-p 1 0)
      selection-wrap-around t
      tooltip-align-annotations t
      dabbrev-downcase nil
      dabbrev-ignore-case t
      tooltip-align-annotations t
      tooltip-margin 2
      global-modes '(not
                      org-mode
                      shell-mode
                      circe-chat-mode
                      circe-channel-mode
                      )
      tooltip-align-annotations t
      )

    ;; TODO: investigate tab handling like VS completely
    (define-key company-active-map [tab] 'company-complete)
    )

  (use-package company-quickhelp
    :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)
    )
  )

(defconfig editing
  (use-package editorconfig :config (editorconfig-mode 1))
  (setq tab-width 4)

  (use-package aggressive-indent
    :config
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
    )

  (use-package smartparens
    :init
    :config
    (add-to-list 'sp-ignore-modes-list 'circe-channel-mode)
    (smartparens-global-mode)
    )

  ;; from https://github.com/syl20bnr/spacemacs/blob/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/layers/%2Bdistributions/spacemacs-bootstrap/config.el
  ;; GPLv3
  (defvar spacemacs--indent-variable-alist
    ;; Note that derived modes must come before their sources
    '(((awk-mode c-mode c++-mode java-mode groovy-mode
         idl-mode java-mode objc-mode pike-mode) . c-basic-offset) (python-mode . python-indent-offset)
       (cmake-mode . cmake-tab-width)
       (coffee-mode . coffee-tab-width)
       (cperl-mode . cperl-indent-level)
       (css-mode . css-indent-offset)
       (elixir-mode . elixir-smie-indent-basic)
       ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
       (enh-ruby-mode . enh-ruby-indent-level)
       (erlang-mode . erlang-indent-level)
       (js2-mode . js2-basic-offset)
       (js3-mode . js3-indent-level)
       ((js-mode json-mode) . js-indent-level)
       (latex-mode . (LaTeX-indent-level tex-indent-basic))
       (livescript-mode . livescript-tab-width)
       (mustache-mode . mustache-basic-offset)
       (nxml-mode . nxml-child-indent)
       (perl-mode . perl-indent-level)
       (puppet-mode . puppet-indent-level)
       (ruby-mode . ruby-indent-level)
       (rust-mode . rust-indent-offset)
       (scala-mode . scala-indent:step)
       (sgml-mode . sgml-basic-offset)
       (sh-mode . sh-basic-offset)
       (typescript-mode . typescript-indent-level)
       (web-mode . web-mode-markup-indent-offset)
       (yaml-mode . yaml-indent-offset))
    "An alist where each key is either a symbol corresponding
      to a major mode, a list of such symbols, or the symbol t,
      acting as default. The values are either integers, symbols
      or lists of these.")

  (defun spacemacs//set-evil-shift-width ()
    "Set the value of `evil-shift-width' based on the indentation settings of the
      current major mode."
    (let ((shift-width
            (catch 'break
              (dolist (test spacemacs--indent-variable-alist)
                (let ((mode (car test))
                       (val (cdr test)))
                  (when (or (and (symbolp mode) (derived-mode-p mode))
                          (and (listp mode) (apply 'derived-mode-p mode))
                          (eq 't mode))
                    (when (not (listp val))
                      (setq val (list val)))
                    (dolist (v val)
                      (cond
                        ((integerp v) (throw 'break v))
                        ((and (symbolp v) (boundp v))
                          (throw 'break (symbol-value v))))))))
              (throw 'break (default-value 'evil-shift-width)))))
      (when (and (integerp shift-width)
              (< 0 shift-width))
        (setq-local evil-shift-width shift-width))))

  (add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

  ;; only trim whitespace on lines you edit
  (use-package ws-butler :config (ws-butler-global-mode))

  ;; to always trim it all
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; todo: call yas-describe-tables sometime
  (use-package yasnippet-snippets)
  (use-package yasnippet
    :config
    (yas-global-mode 1))

  (add-hook 'sh-mode-hook
    (lambda () (sh-electric-here-document-mode -1))))

(defconfig dashdocs
  (defmacro ns/install-dashdoc (docset mode-hook)
    "Install dash DOCSET if dashdocs enabled."
    (when (bound-and-true-p ns/enable-dashdocs-p)
      (if (helm-dash-docset-installed-p docset)
        `(progn
           (message (format "%s docset is already installed!" ,docset))
           (add-hook ,mode-hook (lambda() (setq-local counsel-dash-docsets '(,docset))))
           )
        `(progn
           (message (format "Installing %s docset..." ,docset))
           (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset))
           (add-hook ,mode-hook (lambda() (setq-local counsel-dash-docsets '(,docset))))
           ))))

  (ns/guard ns/enable-home-p)

  (use-package dash)
  (use-package counsel-dash
    :config
    (setq-ns counsel-dash
      min-length 2
      docsets-path (concat user-emacs-directory "docsets")
      browser-func 'ns/eww-browse-existing-or-new
      ))

  (defcommand counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash (thing-at-point 'word))))

  (ns/bind
    "jd" 'ns/counsel-dash-word)
  )

(defconfig-base style
  (interactive)
  ;; todo: an xresources theme that doesn't suck/covers extensions that base16 covers
  (use-package base16-theme)
  ;; https://github.com/waymondo/apropospriate-theme
  ;;(use-package ujelly-theme)

  (let ((theme (intern (get-resource "Emacs.theme"))))
    (when (boundp 'ns/loaded-theme)
      (disable-theme ns/loaded-theme))
    (load-theme theme)
    (setq ns/loaded-theme theme))

  (set-face-attribute 'fringe nil :background nil)
  (set-face-background 'font-lock-comment-face nil)

  ;; current frames
  (mapc (lambda(frame)
          (interactive)
          (set-frame-parameter frame 'internal-border-width
            (string-to-number (get-resource "st.borderpx")))
          (redraw-frame frame))
    (frame-list))

  ;; future frames
  (when (alist-get 'internal-border-width default-frame-alist)
    (setq default-frame-alist (assq-delete-all 'internal-border-width default-frame-alist)))
  (add-to-list 'default-frame-alist
    `(internal-border-width . ,(string-to-number (get-resource "st.borderpx"))))

  ;; this is only viable if can get it on internal window edges only (not right now)
  ;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
  ;; (fringe-mode (string-to-number (get-resource "st.borderpx")))

  ;; sync w/ term background
  (set-background-color (get-resource "*.background"))

  ;; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
    nil :foreground (face-attribute 'font-lock-comment-face :foreground))

  ;; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (set-face-attribute 'fringe nil :background nil)))

  ;; set font on current and future
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (set-frame-font (get-resource "st.font") nil t)

  (setq ns/colored-whitespace? nil)
  (defun color-whitespace-mode(&rest maybe)
    (when (not ns/colored-whitespace?)
      (set-face-attribute 'whitespace-space nil :background nil)
      (set-face-attribute 'whitespace-tab nil :background nil)
      (set-face-attribute 'whitespace-newline nil
        :foreground (face-attribute 'whitespace-space :foreground))
      (setq ns/colored-whitespace? t)
      )
    )

  (advice-add 'whitespace-mode :after #'color-whitespace-mode )
  ;; (advice-add 'whitespace-mode :after #'color-whitespace-mode )

  (use-package hl-todo
    :config
    (let* ((comment-color (face-attribute 'font-lock-comment-face :foreground))
            (highlight-color (ns/color-tone comment-color 30 30)))

      (setq hl-todo-keyword-faces
        `(("TODO" . ,highlight-color)
           ("todo" . ,highlight-color)
           ("NOTE" . ,highlight-color)
           ("note" . ,highlight-color)
           )))

    (global-hl-todo-mode)
    )

  ;; NO BOLD
  ;; (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
  (mapc (lambda (face)
          (set-face-attribute face nil
            :weight 'normal
            :slant 'normal
            :underline nil
            ;;:inherit nil
            ))
    (face-list))

  (use-package dimmer
    :config (setq dimmer-fraction 0.5)
    (dimmer-mode 0))

  ;; gross colors, but need something so we have a signifier in unique match case
  ;; todo: maybe fix gross colors
  ;; (set-face-attribute 'avy-lead-face nil :background (ns/color-tone (face-attribute 'default :background) 30 30))
  ;; (set-face-attribute 'avy-lead-face nil :foreground (ns/color-tone (face-attribute 'default :foreground) 30 30))

  (ns/spaceline)
  )

(defconfig spaceline
  (defun spacemacs/compute-powerline-height ()
    "Return an adjusted powerline height."
    (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                   powerline-scale 1)))
      (truncate (* scale (frame-char-height)))))

  (use-package spaceline
    :config
    (require 'spaceline-config)
    (setq-ns powerline
      scale (string-to-number (get-resource "Emacs.powerlinescale"))
      height (spacemacs/compute-powerline-height)
      default-separator (get-resource "Emacs.powerline")
      )

    ;; (set-face-attribute 'spaceline-highlight-face nil :background (face-attribute 'spaceline-evil-normal :background))

    ;; todo: make a circe segment

    ;; note to self: abandon this, look at switch-to-next, prev-buffer source, grab that and replace return
    (defun ns/next-buffer-name ()
      (-first (lambda (bufname) (not (ns/should-skip bufname)))
        (-map 'buffer-name (window-next-buffers))))

    (defun ns/prev-buffer-name ()
      (-first (lambda (bufname) (not (ns/should-skip bufname)))
        (-map 'buffer-name
          (-map 'first (window-prev-buffers)))))

    (spaceline-define-segment next-buffers
      "Docstring"
      ;; A single form whose value is the value of the segment.
      ;; It may return a string, an image or a list of such.
      (when t
        (concat
          (s-left 8 (ns/prev-buffer-name))
          " - "
          (s-left 8 (ns/next-buffer-name)))
        )
      :enabled t
      )

    ;; this is needed to set default for new buffers?
    (spaceline-spacemacs-theme)

    (spaceline-compile 'main
      '(
         anzu
         (remote-host projectile-root ">>" buffer-id buffer-modified)
         (flycheck-error flycheck-warning)
         process
         )
      '(
         ;; maybe
         (version-control :when active)
         (next-buffers :when active)
         (org-clock :when active)
         (org-pomodoro :when active)
         info-nodes
         ((line-column buffer-position)
           :separator " |" )
         (battery :when active)
         ))

    (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))

    ;; set the modeline for all existing buffers
    ;; todo: make this unset modeline on not matching spawn
    (defcommand refresh-all-modeline ()
      (dolist (buf (buffer-list))
        (when (not (s-starts-with-p "*spawn-shell" (buffer-name buf)))
          (with-current-buffer buf
            (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))))))

    (ns/refresh-all-modeline)
    (ns/bind "tM" 'ns/refresh-all-modeline)
    ))

(defconfig zoom
  (use-package zoom
    :config
    (setq zoom-size '(80 . 24))
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode 1)))

(defconfig org
  (use-package org
    :straight (:host github
                :repo "emacsmirror/org"
                :files ("lisp/*.el" "contrib/lisp/*.el"))

    :config
    (setq-ns org
      directory (~ "notes")
      agenda-files (list org-directory)
      default-notes-file  (concat org-directory "/notes.org")
      default-diary-file  (concat org-directory "/diary.org")
      default-habits-file  (concat org-directory "/habits.org")

      ellipsis "_"
      startup-indented t
      startup-folded t
      src-fontify-natively t
      startup-align-all-tables t

      ;; days before expiration where a deadline becomes active
      deadline-warn-days 14
      todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
         (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

      blank-before-new-entry '((heading . t) (plainlist-item . nil))
      tag-alist '(
                   ("test" . ?t)
                   ("endtest" . ?e)
                   )

      ;; clock
      clock-x11idle-program-name "x11idle"
      clock-idle-time 5
      clock-sound nil
      pomodoro-play-sounds nil
      pomodoro-keep-killed-pomodoro-time t
      pomodoro-ask-upon-killing nil

      ;; todo: consider note option here.
      log-done 'time

      ;; capture
      capture-templates
      '(
         ("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)

         ("b" "Blank" entry (file org-default-notes-file)
           "* %?\n%u")

         ("m" "Meeting" entry (file org-default-notes-file)
           "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)

         ("d" "Diary" entry (file+datetree org-default-diary-file)
           "* %?\n%U\n" :clock-in t :clock-resume t)

         ("D" "Daily Log" entry (file (~ "notes/daily-log.org"))
           "* %u %?\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)

         ("i" "Idea" entry (file org-default-notes-file)
           "* %? :IDEA: \n%u" :clock-in t :clock-resume t)

         ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
           "** NEXT %? \nDEADLINE: %t")
         )

      ;; current file or any of the agenda-files, max 9 levels deep
      refile-targets '(
                        (nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)
                        )
      )

    (ns/bind-leader-mode 'emacs-lisp "r" 'eros-eval-last-sexp)

    (add-hook
      'org-mode-hook
      (ns/bind-leader-mode
        'org
        "," 'org-ctrl-c-ctrl-c
        "t" 'org-todo
        "T" 'org-show-todo-tree
        "v" 'org-mark-element
        "a" 'org-agenda
        "l" 'evil-org-open-links
        "p" 'org-pomodoro
        "f" 'ns/org-set-active
        "b" 'ns/org-open-url
        ))

    ;; give us easy templates/tab completion like yasnippet and the like
    ;; form is '<<key><tab>', eg <s<tab> expands to src block
    ;; todo: reference what all this gives us: https://orgmode.org/manual/Easy-templates.html
    (require 'org-tempo)
    )

  (defcommand org-open-url() (browse-url (org-entry-get nil "url")))

  (defcommand org-set-active()
    (org-delete-property-globally "focus")
    (org-set-property "focus" "t")

    (setq ns/org-active-story (substring-no-properties (org-get-heading t t t t)))
    )

  ;; for externals to call into
  (defun ns/org-get-active()
    (if (not (bound-and-true-p ns/org-active-story))
      (progn
        (ns/org-goto-active)
        (ns/org-set-active))
      ns/org-active-story
      ))

  (defcommand org-goto-active()
    (ns/find-or-open org-default-notes-file)
    (goto-char (org-find-property "focus"))
    (org-show-context)
    (org-show-subtree)
    (ns/focus-line)
    )

  (use-package org-pomodoro
    :config
    (defun ns/toggle-music(action)
      (let ((command (concat (if ns/enable-home-p "player.sh" "mpc") " " action)))
        (shell-command command)))

    (add-hook 'org-pomodoro-started-hook
      (apply-partially #'ns/toggle-music "play"))

    (add-hook 'org-pomodoro-break-finished-hook
      (apply-partially #'ns/toggle-music "play"))

    (add-hook 'org-pomodoro-finished-hook
      (apply-partially #'ns/toggle-music "pause"))
    )

  (defcommand jump-org () (ns/find-or-open org-default-notes-file))

  ;; todo: make this insert at focused story?
  (defcommand make-org-link-to-here ()
    (insert (concat "[[file:" (buffer-file-name) "::"
              (number-to-string (line-number-at-pos)) "]]")))

  (defcommand insert-mark-org-links ()
    (setq ns/markers
      (append (cl-remove-if (lambda (m)
                              (or (evil-global-marker-p (car m))
                                (not (markerp (cdr m)))))
                evil-markers-alist)
        (cl-remove-if (lambda (m)
                        (or (not (evil-global-marker-p (car m)))
                          (not (markerp (cdr m)))))
          (default-value 'evil-markers-alist)))
      )

    ;; remove automatic marks
    (dolist (key '(40 41 94 91 93))
      (setq ns/markers (delq (assoc key ns/markers) ns/markers)))

    (insert (s-join "\n"
              (mapcar (lambda(mark)
                        (let ((file (buffer-file-name (marker-buffer mark)))
                               (linenumber
                                 (with-current-buffer (marker-buffer mark)
                                   (line-number-at-pos (marker-position mark)))))
                          (concat "[[file:" file "::" (number-to-string linenumber) "]]")))
                (mapcar 'cdr ns/markers)))))

  (ns/bind
    "oo" 'ns/org-goto-active
    "oc" 'org-capture
    "or" 'org-refile
    "ol" 'ns/make-org-link-to-here
    "om" 'ns/insert-mark-org-links

    ;; ehh
    "on" 'ns/jump-org
    )

  (add-hook 'org-mode-hook 'ns/set-buffer-face-variable)

  (advice-add #'ns/style :after #'ns/style-org)
  (defun ns/style-org ()
    (ns/set-faces-monospace '(org-block org-code org-table))

    (set-face-attribute 'org-block-begin-line nil :height 50)
    (set-face-attribute 'org-block-end-line nil :height 50)

    (let ((height (plist-get (ns/parse-font (get-resource "st.font")) :height)))
      (set-face-attribute 'org-level-1 nil :height (+ height 15) :weight 'semi-bold)
      (set-face-attribute 'org-level-2 nil :height (+ height 10) :weight 'semi-bold)
      (set-face-attribute 'org-level-3 nil :height (+ height 5) :weight 'semi-bold)
      (set-face-attribute 'org-level-4 nil :height height :weight 'semi-bold)
      ))

  ;; todo: into org agendas
  ;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
  )

(defconfig clojure
  (use-package clojure-mode)
  (use-package cider)
  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  ;; TODO: learn lispyville
  ;; (use-package lispy)

  (ns/bind-leader-mode
    'clojure
    "er" 'cider-eval-region
    "ei" 'cider-eval-last-sexp
    "eb" 'cider-evil-file
    )
  )

(defconfig nix
  (ns/guard ns/enable-home-p)
  (use-package nix-mode)
  )

(defconfig target-process
  (ns/guard ns/enable-work-p)
  (load (~ ".emacs.d/lib/targetprocess.el"))
  (advice-add #'ns/org-set-active :after #'tp-set-active)
  (ns/bind-leader-mode 'org "Q" 'tp-set-org-userstory)
  )

(defconfig interface
  ;; todo: into occur/search buffer solution for better finding when don't know what we're looking for
  (use-package ivy
    :config
    (setq-ns ivy
      re-builders-alist '((ivy-switch-buffer . ivy--regex-plus) (t . ivy--regex-fuzzy))
      initial-inputs-alist nil
      fixed-height-minibuffer t
      )

    ;; todo: this will also need a hook on frame focus now -- for when using emacs as term
    (add-hook 'window-configuration-change-hook 'dynamic-ivy-height)
    (defun dynamic-ivy-height()
      (setq ivy-height (/ (window-total-size) 2)))

    (dynamic-ivy-height)

    (ivy-mode 1)

    (ns/use-package prescient "raxod502/prescient.el" :config (ivy-prescient-mode))
    )

  ;; counsel
  (use-package counsel
    :config
    (use-package rg)
    (setq-ns counsel
      grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
      rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")

    ;; counsel-rg is crashing emacs on windows
    ;; (can't C-g/esc, can't send USR2 on windows)
    ;; (toggle-debug-on-error nil)
    ;; (toggle-debug-on-quit nil)
    )

  (use-package ranger
    :init (setq ranger-override-dired t)
    :config
    (setq-ns ranger
      show-literal nil
      show-hidden t
      cleanup-eagerly t
      )

    ;; call with eg 'dired-mode
    (defcommand kill-buffers-by-mode (mode)
      (mapc (lambda (buffer)
              (when (eq mode (buffer-local-value 'major-mode buffer))
                (kill-buffer buffer)))
        (buffer-list)))

    (defcommand kill-ranger-buffers ()
      (ns/kill-buffers-by-mode 'ranger-mode))

    (advice-add #'ranger-close :after #'ns/kill-ranger-buffers)

    (defcommand deer-with-last-shell ()
      (let ((current-buffer (buffer-name (current-buffer))))
        (if (or (s-match "\*spawn-shell.*" current-buffer)
              (s-match "\*shell-[1-9]\*" current-buffer))
          (setq ns/last-shell current-buffer)
          (setq ns/last-shell shell-pop-last-shell-buffer-name)))
      (deer))

    (ns/bind "d" 'ns/deer-with-last-shell)

    (defcommand open () (async-shell-command (format "xdg-open \"%s\"" (dired-get-file-for-visit))))
    (define-key ranger-normal-mode-map (kbd "RET") 'ns/open)
    )

  (defun my-resize-margins ()
    (let ((margin-size (if ns/center (/ (- (frame-width) 120) 2) 0)))
      (set-window-margins nil margin-size margin-size)))

  (defcommand toggle-margin ()
    (if (not (bound-and-true-p ns/center))
      (setq ns/center nil))

    (if ns/center
      (remove-hook 'window-configuration-change-hook #'my-resize-margins)
      (add-hook 'window-configuration-change-hook #'my-resize-margins))

    (setq ns/center (not ns/center))
    (my-resize-margins))

  (defcommand kill-current-buffer()
    (kill-buffer nil))

  (defcommand follow-mode ()
    (follow-mode)
    (delete-other-windows)
    (evil-window-vsplit))

  (ns/bind
    "/"   'counsel-rg
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" 'counsel-M-x

    ;; windows
    "w" '(:ignore t :which-key "Windows")
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'evil-window-delete
    "ww" 'other-window
    "wf" 'ns/follow-mode
    "wc" 'ns/toggle-margin

    "wm" 'delete-other-windows ;; window-max
    "wo" 'other-frame

    ;; Applications
    "a" '(:ignore t :which-key "Applications")

    "b" '(:ignore t :which-key "Buffers")
    "bd" 'ns/kill-current-buffer

    "j" '(:ignore t :which-key "Jump")
    "jd" 'counsel-imenu
    )

  (use-package alert
    :config (setq alert-default-style
              (if ns/enable-windows-p
                'toaster
                'libnotify
                )))

  (use-package which-key
    :config
    (setq-ns which-key
      idle-delay 1.5
      side-window-max-width 0.33
      sort-order 'which-key-key-order-alpha
      )
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)
    )



  (use-package ace-jump-buffer
    :config
    (defun dynamic-ajb-height()
      (setq ajb-max-window-height (/ (window-total-size) 2)))

    (dynamic-ajb-height)
    (add-hook 'window-configuration-change-hook 'dynamic-ajb-height)
    (setq ajb-sort-function 'bs--sort-by-recentf)

    ;; todo: kill buffers by regexp command
    (ns/bind
      "bs" 'ace-jump-buffer
      "bm" 'ace-jump-same-mode-buffers
      )
    )

  (defcommand kill-other-buffers ()
    "Kill all other buffers."
    (mapc 'kill-buffer
      (delq (current-buffer)
        (remove-if-not 'buffer-file-name (buffer-list)))))

  (ns/bind
    "bb" 'counsel-ibuffer
    "bK" 'ns/kill-other-buffers
    "bk" 'kill-matching-buffers
    )

  )

(defconfig music
  (ns/guard ns/enable-home-p)
  (use-package emms)

  (defun emms-start()
    (require 'emms-player-mpd)
    (setq-ns emms-player-mpd
      server-name "localhost"
      server-port "6600"
      music-directory (~ "Music")
      ;; server-password "mypassword"
      )

    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)

    ;; sync with mpd db, connect
    (emms-cache-set-from-mpd-all)
    (emms-player-mpd-connect)
    )

  (ns/bind "am" 'emms-start)
  )

(defconfig projectile
  (use-package projectile)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun get-project-files (project-root)
    (let* ((default-directory (expand-file-name project-root))
            (project-files-relative (s-split "\n" (shell-command-to-string counsel-git-cmd) t)))
      (mapcar (lambda (file) (concat default-directory file)) project-files-relative)))

  (defcommand jump-file ()
    (let* (
            (recent-files recentf-list)

            ;; bail out if we're not in a project
            ;; (project-root (condition-case nil (projectile-project-root) (error nil)))

            ;; (project-files
            ;;   (if project-root
            ;;     (get-project-files project-root)
            ;;     '()))

            (open-buffers
              ;; remove nils
              (-remove (lambda(file) (not file))
                (mapcar 'buffer-file-name (buffer-list))))

            (open-project-roots
              (-remove (lambda(file) (not file))
                (-distinct
                  (mapcar 'projectile-root-bottom-up
                    open-buffers))))

            (project-files (-flatten (mapcar 'get-project-files open-project-roots)))
            )

      (ivy-read "file: "
        (mapcar (lambda (s) (s-replace (~ "") "~/" s))
          (-distinct (append project-files open-buffers recent-files)))
        :action #'find-file)))

  (ns/bind "jf" 'ns/jump-file )
  )

(defconfig javascript
  ;; note: this is huge, takes a bit.
  (ns/install-dashdoc "JavaScript" 'web-mode-hook)

  (defun js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket)

  ;; use web-mode for .js files
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

  (use-package rjsx-mode)
  (use-package web-mode
    :config
    (add-hook 'web-mode-hook
      (lambda ()
        ;; short circuit js mode and just do everything in jsx-mode
        (if (equal web-mode-content-type "javascript")
          (web-mode-set-content-type "jsx")
          (message "now set to: %s" web-mode-content-type)))))

  (use-package prettier-js
    :config
    (when ns/enable-work-p
      (add-hook 'typescript-mode-hook 'prettier-js-mode)
      (add-hook 'web-mode-hook 'prettier-js-mode)
      (add-hook 'js-mode-hook 'prettier-js-mode)))

  ;; notes for using this
  ;; kill shx-mode
  ;; doesn't work with multiline input, or import command/multiple files
  (use-package nodejs-repl
    :config
    (ns/bind-leader-mode
      'nodejs-repl
      "er "'nodejs-repl-send-region
      "eb" 'nodejs-repl-load-file
      "ee" 'nodejs-repl-send-line
      "ei" 'nodejs-repl-send-last-expression
      ))
  )

(defconfig typescript
  (ns/install-dashdoc "TypeScript" 'typescript-mode-hook)
  (use-package tide
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      )

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    ;; formats the buffer before saving
    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    )
  )

(defconfig csharp
  (use-package csharp-mode)
  (use-package omnisharp)
  )

(defconfig git
  (use-package magit
    :config
    (setq-ns magit
      save-repository-buffers 'dontask
      repository-directories (list (~ "git"))
      )

    ;; https://magit.vc/manual/magit/Performance.html
    (when ns/enable-windows-p
      (setq-ns magit
        ;; diff perf
        diff-highlight-indentation nil
        diff-highlight-trailing nil
        diff-paint-whitespace nil
        diff-highlight-hunk-body nil
        diff-refine-hunk nil
        refresh-status-buffer nil
        )

      ;; don't show diff when committing --
      ;; means reviewing will have to be purposeful before
      (remove-hook 'server-switch-hook 'magit-commit-diff)
      ))

  (macroexpand-1
    '(ns/use-package magit-todos "alphapapa/magit-todos"
       :config
       (setq magit-todos-nice ns/enable-linux-p)
       (evil-define-key nil magit-todos-section-map "j" nil)
       (magit-todos-mode))
    )
  
  (use-package magit-svn :config
    (add-hook 'magit-mode-hook 'magit-svn-mode))

  (use-package evil-magit
    :config
    (evil-define-key
      evil-magit-state magit-mode-map "?" 'evil-search-backward))

  (use-package git-gutter-fringe
    :config
    (setq git-gutter-fr:side 'right-fringe)
    ;; fails when too many buffers open on windows
    (if ns/enable-linux-p (global-git-gutter-mode t))
    )

  (defhydra git-smerge-menu ()
    "
      movement^^^^               merge action^^           other
      ---------------------^^^^  -------------------^^    -----------
      [_n_]^^    next hunk       [_b_] keep base          [_u_] undo
      [_N_/_p_]  prev hunk       [_m_] keep mine          [_r_] refine
      [_j_/_k_]  move up/down    [_a_] keep all           [_q_] quit
      ^^^^                       [_o_] keep other
      ^^^^                       [_c_] keep current
      ^^^^                       [_C_] combine with next"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("N" smerge-prev)
    ("j" evil-next-line)
    ("k" evil-previous-line)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("c" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("r" smerge-refine)
    ("u" undo-tree-undo)
    ("q" nil :exit t))

  ;; define a minimal staging mode for when we're on windows.
  (when ns/enable-windows-p
    ;; WORKAROUND https://github.com/magit/magit/issues/2395
    (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
      "Mode for showing staged and unstaged changes."
      :group 'magit-status)
    (defun magit-staging-refresh-buffer ()
      (magit-insert-section (status)
        (magit-insert-untracked-files)
        (magit-insert-unstaged-changes)
        (magit-insert-staged-changes)))
    (defun magit-staging ()
      (interactive)
      (magit-mode-setup #'magit-staging-mode))
    )

  (defcommand git-status()
    (if ns/enable-windows-p (magit-staging) (magit-status))
    (if (> (frame-pixel-height) (frame-pixel-width))
      (delete-other-windows)))


  (use-package vdiff
    :config
    (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)

    (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" 'vdiff-close-fold)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" 'vdiff-close-all-folds)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" 'vdiff-open-fold)
    (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" 'vdiff-open-all-folds)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
    (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes)
    )

  (general-nmap
    "]g" 'git-gutter:next-hunk
    "[g" 'git-gutter:previous-hunk
    )

  ;; alias:
  (defcommand magit-history () (magit-log-buffer-file))

  (ns/bind
    "g" '(:ignore t :which-key "git")
    "gb" 'magit-blame
    "gl" 'magit-log-buffer-file
    "gm" 'git-smerge-menu/body
    "gd" 'vdiff-mode ; ,h for a hydra!
    "gs" 'ns/git-status
    "gh" 'ns/magit-history
    )
  )

(defconfig jump
  (use-package smart-jump
    :config
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher 'rg)
    (smart-jump-setup-default-registers)
    (ns/bind
      "j" '(:ignore t :which-key "Jump")
      "jj" 'smart-jump-go
      "jb" 'smart-jump-back
      )

    (advice-add #'smart-jump-go :after #'ns/focus-line)
    ))

;; todo: clean up the 'my' prefix?
(defconfig irc
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
          :channels (:after-auth "#github" "#bspwm" "#qutebrowser" "#emacs")
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
           :channels (:after-auth "#rice" "#code")
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
    "ji" 'ns/jump-irc
    ))

(defconfig pdf
  (ns/guard ns/enable-home-p)
  (use-package pdf-tools)
  )

(defconfig terraform
  (use-package terraform-mode)
  )

(defconfig twitter
  (ns/guard ns/enable-home-p)
  (use-package twittering-mode
    :commands (twittering-start)
    :init
    (add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode)))
    :config
    (setq-ns twittering
      use-master-password t
      icon-mode t
      use-icon-storage t
      ;; icon-storage-file (concat joe-emacs-temporal-directory "twittering-mode-icons.gz")
      convert-fix-size 52
      initial-timeline-spec-string '(":home")
      edit-skeleton 'inherit-any
      display-remaining t
      timeline-header  ""
      timeline-footer  ""
      status-format "%i  %S, %RT{%FACE[bold]{%S}} %@  %FACE[shadow]{%p%f%L%r}\n%FOLD[        ]{%T}\n"
      )

    ;; set the new bindings
    (bind-keys :map twittering-mode-map
      ("\\" . hydra-twittering/body)
      ("q" . twittering-kill-buffer)
      ("Q" . twittering-edit-mode)
      ("j" . twittering-goto-next-status)
      ("k" . twittering-goto-previous-status)
      ("h" . twittering-switch-to-next-timeline)
      ("l" . twittering-switch-to-previous-timeline)
      ("g" . beginning-of-buffer)
      ("G" . end-of-buffer)
      ("t" . twittering-update-status-interactive)
      ("X" . twittering-delete-status)
      ("RET" . twittering-reply-to-user)
      ("r" . twittering-native-retweet)
      ("R" . twittering-organic-retweet)
      ("d" . twittering-direct-message)
      ("u" . twittering-current-timeline)
      ("b" . twittering-favorite)
      ("B" . twittering-unfavorite)
      ("f" . twittering-follow)
      ("F" . twittering-unfollow)
      ("i" . twittering-view-user-page)
      ("/" . twittering-search)
      ("." . twittering-visit-timeline)
      ("@" . twittering-other-user-timeline)
      ("T" . twittering-toggle-or-retrieve-replied-statuses)
      ("o" . twittering-click)
      ("TAB" . twittering-goto-next-thing)
      ("<backtab>" . twittering-goto-previous-thing)
      ("n" . twittering-goto-next-status-of-user)
      ("p" . twittering-goto-previous-status-of-user)
      ("SPC" . twittering-scroll-up)
      ("S-SPC" . twittering-scroll-down)
      ("y" . twittering-push-uri-onto-kill-ring)
      ("Y" . twittering-push-tweet-onto-kill-ring)
      ("a" . twittering-toggle-activate-buffer)))

  (defhydra hydra-twittering (:color blue :hint nil)
    "
      ╭────────────┐
      Tweets                User                        Timeline     │ Twittering │
      ╭─────────────────────────────────────────────────────────────────┴────────────╯
      _k_  [_t_] post tweet      _p_  [_f_] follow                  ^_g_^      [_u_] update
      ^↑^  [_X_] delete tweet    ^↑^  [_F_] unfollow              ^_S-SPC_^    [_._] new
      ^ ^  [_r_] retweet         ^ ^  [_d_] direct message          ^^↑^^      [^@^] current user
      ^↓^  [_R_] retweet & edit  ^↓^  [_i_] profile (browser)   _h_ ←   → _l_  [_a_] toggle
      _j_  [_b_] favorite        _n_   ^ ^                          ^^↓^^
      ^ ^  [_B_] unfavorite      ^ ^   ^ ^                         ^_SPC_^
      ^ ^  [_RET_] reply         ^ ^   ^ ^                          ^_G_^
      ^ ^  [_T_] show Thread
      ^ ^  [_y_] yank url          Items                     Do
      ^ ^  [_Y_] yank tweet     ╭───────────────────────────────────────────────────────
      ^ ^  [_e_] edit mode        _<backtab>_ ← _o_pen → _<tab>_    [_q_] exit
      ^ ^   ^ ^                   ^         ^   ^ ^      ^     ^    [_/_] search
      --------------------------------------------------------------------------------
      "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("q"          twittering-kill-buffer)
    ("e"          twittering-edit-mode)
    ("j"          twittering-goto-next-status :color red)
    ("k"          twittering-goto-previous-status :color red)
    ("h"          twittering-switch-to-next-timeline :color red)
    ("l"          twittering-switch-to-previous-timeline :color red)
    ("g"          beginning-of-buffer)
    ("G"          end-of-buffer)
    ("t"          twittering-update-status-interactive)
    ("X"          twittering-delete-status)
    ("RET"        twittering-reply-to-user)
    ("r"          twittering-native-retweet)
    ("R"          twittering-organic-retweet)
    ("d"          twittering-direct-message)
    ("u"          twittering-current-timeline)
    ("b"          twittering-favorite)
    ("B"          twittering-unfavorite)
    ("f"          twittering-follow)
    ("F"          twittering-unfollow)
    ("i"          twittering-view-user-page)
    ("/"          twittering-search)
    ("."          twittering-visit-timeline)
    ("@"          twittering-other-user-timeline)
    ("T"          twittering-toggle-or-retrieve-replied-statuses)
    ("o"          twittering-click)
    ("<tab>"      twittering-goto-next-thing :color red)
    ("<backtab>"  twittering-goto-previous-thing :color red)
    ("n"          twittering-goto-next-status-of-user :color red)
    ("p"          twittering-goto-previous-status-of-user :color red)
    ("SPC"        twittering-scroll-up :color red)
    ("S-SPC"      twittering-scroll-down :color red)
    ("y"          twittering-push-uri-onto-kill-ring)
    ("Y"          twittering-push-tweet-onto-kill-ring)
    ("a"          twittering-toggle-activate-buffer))

  ;; (ns/bind "at" 'twittering-start)
  )

(defconfig slack
  (ns/guard ns/enable-home-p)
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t)
    (setq slack-prefer-current-team t)

    :config
    (when ns/enable-windows-p
      ;; https://github.com/yuya373/emacs-slack/issues/161
      (setq request-backend 'url-retrieve)
      (setq slack-request-timeout 50)
      )

    (slack-register-team
      :name (pass "slackteam")
      :default t
      :client-id (pass "slackid")
      :client-secret (pass "slack")
      :token (pass "slacktoken")
      :subscribed-channels '(general random)
      :full-and-display-names t
      )
    )

  ;; todo: where is slack-info/context for this bind
  (ns/bind-leader-mode
    'slack-info
    "u" 'slack-room-update-messages)

  (ns/bind-leader-mode
    'slack
    "c" 'slack-buffer-kill
    "ra" 'slack-message-add-reaction
    "rr" 'slack-message-remove-reaction
    "rs" 'slack-message-show-reaction-users
    "pl" 'slack-room-pins-list
    "pa" 'slack-message-pins-add
    "pr" 'slack-message-pins-remove
    "mm" 'slack-message-write-another-buffer
    "me" 'slack-message-edit
    "md" 'slack-message-delete
    "u" 'slack-room-update-messages
    "2" 'slack-message-embed-mention
    "3" 'slack-message-embed-channel
    )

  ;; todo: something for these maybe
  ;; "\C-n" 'slack-buffer-goto-next-message
  ;; "\C-p" 'slack-buffer-goto-prev-message)

  (ns/bind-leader-mode
    'slack-edit-message
    "k" 'slack-message-cancel-edit
    "s" 'slack-message-send-from-buffer
    "2" 'slack-message-embed-mention
    "3" 'slack-message-embed-channel
    )

  (ns/bind
    "as" 'slack-start))

(defconfig email
  (ns/guard ns/enable-home-p)
  ;; TODO
  )

(defconfig shell
  (require 'comint)

  (when ns/enable-linux-p
    (setq explicit-shell-file-name (getenv "SHELL")))

  (when (and ns/enable-windows-p (not ns/enable-docker-p))
    ;; todo: find out what provides first, we need that here
    (setq explicit-shell-file-name (car (s-split "\n" (ns/shell-exec "where bash"))))
    (setq explicit-bash.exe-args '("--login" "-i"))

    (setenv "PATH"
      (concat (~ "scoop/apps/git-with-openssh/current/usr/bin/") ";"
      (getenv "PATH"))))

  ;; cf https://stackoverflow.com/questions/25862743/emacs-can-i-limit-a-number-of-lines-in-a-buffer
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (setq comint-buffer-maximum-size 1000)
  (setq comint-prompt-read-only t)

  (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<down>") 'comint-next-input)

  (use-package shx
    :config
    (shx-global-mode 1)
    ;; todo: find a way to alias things
    ;; ie clear --> :clear, term, exit
    (defun shx-cmd-term (placeholder)
      (interactive)
      (let ((term (if ns/enable-windows-p "cmd" (getenv "TERMINAL")))
             ;; (default-directory (~ ""))
             )
        (shell-command (format "nohup %s &" term) nil nil))))

  (use-package shell-pop
    :config
    (setq-ns shell-pop
      window-position "top"
      window-size 33 ;; percent
      full-span t
      )

    ;; interactive shell-pop bound to spc t index shell
    (defun makepop (index)
      (let ((funcname (intern (concat "shell-pop-" (number-to-string index)))))
        (eval `(progn
                 (defun ,funcname () (interactive) (shell-pop ,index))
                 (ns/bind ,(concat "t" (number-to-string index)) ',funcname)))))

    ;; give us 1-9
    (mapc 'makepop (number-sequence 1 9))
    (ns/bind "'" 'shell-pop)

    ;; cf https://github.com/kyagi/shell-pop-el/issues/51
    (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

    ;; todo: make this use a fresh shell or something, maybe cleanup empty shells at some point
    (defcommand shell-pop-ranger-dir ()
      (let ((ranger-dir (expand-file-name default-directory)))
        (switch-to-buffer ns/last-shell)
        (shell-pop--cd-to-cwd-shell ranger-dir))
      ;; note: keep this outsite of let to close properly
      (ranger-kill-buffers-without-window)
      )

    (define-key ranger-mode-map (kbd "s") 'ns/shell-pop-ranger-dir)
    )

  ;; fix for term, ansi term
  ;; https://github.com/hlissner/emacs-doom-themes/issues/54
  (setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
  )

(defconfig jekyll
  (use-package jekyll-modes)
  )

(defconfig autohotkey
  (ns/guard ns/enable-windows-p)
  (use-package xahk-mode)
  )

(defconfig markdown
  ;; (use-package markdownmode)
  )

(defconfig restclient
  (use-package restclient
    :config
    (ns/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window
      )
    )

  (use-package company-restclient)
  )

(defconfig sql
  ;; todo
  ;; (ns/install-dashdoc "SQLite" ')

  ;; setup: https://github.com/kostafey/ejc-sql#install-jdbc-drivers
  ;; (use-package ejc-sql
  ;;   :config
  ;;   ;; test local sqlite
  ;; )
  )

(defconfig latex
  (ns/bind-leader-mode 'latex
    "\\"  'TeX-insert-macro                            ;; C-c C-m
    "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
    ";"   'TeX-comment-or-uncomment-region             ;; C-c ; or C-c :
    ;; TeX-command-run-all runs compile and open the viewer
    "a"   'TeX-command-run-all                         ;; C-c C-a
    "b"   'latex/build
    "k"   'TeX-kill-job                                ;; C-c C-k
    "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "m"   'TeX-insert-macro                            ;; C-c C-m
    "v"   'TeX-view                                    ;; C-c C-v
    )

  (ns/install-dashdoc "LaTeX" 'latex-mode-hook)

  ;; todo: https://github.com/The-Compiler/dotfiles/blob/543e48cd594750188dd3e935ef6dfd77f867ca71/spacemacs#L497
  ;; todo: this doesn't build?
  ;; ref: https://github.com/raxod502/straight.el/issues/240
  ;; (use-package company-auctex)
  )


(defconfig plantuml
  (use-package plantuml)
  (use-package flycheck-plantuml)
  )

(defconfig ledger
  (ns/guard ns/enable-home-p)
  (use-package ledger-mode)
  (use-package flycheck-ledger)
  (use-package evil-ledger
    :config
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)
    )
  )

(defconfig lsp
  (use-package lsp-ui)
  (use-package lsp-javascript-flow)
  (use-package lsp-javascript-typescript)

  (use-package cquery)


  (defun my-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))

  (defun my-js-hook nil
    (make-local-variable 'company-transformers)
    (push 'my-company-transformer company-transformers))

  (add-hook 'web-mode-hook 'my-js-hook)
  (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)

  (remove-hook 'web-mode-hook #'lsp-javascript-flow-enable)

  )

(defconfig search-engines
  (use-package engine-mode
    :config

    ;; bind spc s 'hotkey' to a search url with a label
    (defmacro bind-search (label url hotkey)
      `(progn
         (defengine ,label ,url)
         (ns/bind
           (concat "s" ,hotkey) (intern (concat "engine/search-" (prin1-to-string ',label))))))

    (bind-search google "https://google.com/search?q=%s" "s")
    (bind-search melpa "https://melpa.org/#/?q=%s" "m")
    (bind-search stack-overflow "https://stackoverflow.com/search?q=%s" "o")
    (bind-search github "https://github.com/search?ref=simplesearch&q=%s" "g")
    (bind-search youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" "y")
    (engine-mode t)
    ))

(defconfig filehooks
  (ns/guard ns/enable-home-p)

  (defvar *afilename-cmd*
    ;; todo: consider more here -- sxhkd, bspwmrc? ~/.wm_theme (if smart-load ever comes to fruition)
    `((,(~ ".Xresources") . "xrdb -merge ~/.Xresources")
       (,(~ ".Xmodmap") . "xmodmap ~/.Xmodmap"))
    "File association list with their respective command.")

  (defun my/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (setq filenames (mapcar 'car *afilename-cmd*))
    (dolist (file filenames)
      (let ((cmd (cdr (assoc file *afilename-cmd*))))
        (if (file-exists-p file)
          (when (equal (buffer-file-name) file)
            (shell-command cmd))
          (error "No such file %s" file)))))

  (add-hook 'after-save-hook 'my/cmd-after-saved-file)
  )

(defconfig emoji
  (use-package emojify
    :init (setq emojify-emoji-styles '(unicode github))
    :config
    (global-emojify-mode)
    (ns/bind "ie" 'emojify-insert-emoji)
    ))

(defconfig writing
  (ns/guard ns/enable-home-p)
  ;; todo
  ;; https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/
  ;; https://github.com/agzam/mw-thesaurus.el

  (use-package writeroom-mode)
  (add-hook 'writeroom-mode-hook 'flyspell-mode)

  (setq-default fill-column 80)
  (add-hook 'writeroom-mode-hook 'auto-fill-mode)
  ;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
  ;; There is no need for "^" as the regexp is matched at the beginning of line.
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

  ;; toggle focus?
  (ns/bind "tf" 'writeroom-mode)

  (use-package mw-thesaurus)
  (ns/bind-leader-mode 'org "q" 'mw-thesaurus--lookup-at-point)
  )

;; use shell frames as terminals.
;; todo: consider hiding this in window manager then popping it
;; in rather than making a frame at launch time
(defconfig terminal
  (defcommand stage-terminal ()
    (let ((default-directory (~ "")))
      (shell "*spawn-shell-staged*")
      (ns/toggle-modeline)
      (delete-window)
      ;; todo: find a way to set initial dirtrack to default-directory
      (dirtrack-mode)
      ))

  (ns/stage-terminal)

  (defcommand spawn-terminal ()
    (select-frame (make-frame))
    (switch-to-buffer (get-buffer "*spawn-shell-staged*"))
    (rename-buffer (concat "*spawn-shell-" (number-to-string (random)) "*"))
    (delete-other-windows)
    (set-window-fringes nil 0 0)
    (ns/stage-terminal)
    )

  (defcommand kill-spawned-shell (frame)
    (let ((windows (window-list frame)))
      (when (eq 1 (length windows))
        (let ((buffer (window-buffer (car windows))))
          (when (s-match "\*spawn-shell.*" (buffer-name buffer))
            (kill-buffer buffer))))))

  (ns/bind "at" 'ns/spawn-terminal)
  (add-hook 'delete-frame-hook 'ns/kill-spawned-shell))


(defconfig elfeed
  (ns/guard ns/enable-home-p)
  (use-package elfeed
    :config
    (ns/bind "af" 'elfeed)
    (setq elfeed-feeds
      '(
         "https://hnrss.org/newest?q=emacs"
         "http://pragmaticemacs.com/feed/"
         "http://xkcd.com/rss.xml"
         ))

    ;; Entries older than 2 weeks are marked as read
    (add-hook 'elfeed-new-entry-hook
      (elfeed-make-tagger :before "2 weeks ago"
        :remove 'unread))
    )

  (use-package elfeed-goodies
    :config (elfeed-goodies/setup))

  (use-package elfeed-org
    ;; todo - could then get feeds out of this file
    ))

(defconfig stackexchange
  (ns/guard ns/enable-home-p)
  (use-package sx
    :config
    (ns/bind "as" 'sx-tab-all-questions)
    ))

(defconfig reddit
  (ns/guard ns/enable-home-p)
  ;; todo: this needs some rice love
  ;; text wrapping comments don't align
  ;; could use some better keybinds, UX
  (use-package md4rd
    :config
    (ns/bind "ar" 'md4rd)
    (setq md4rd-subs-active
      '(unixporn emacs))

    (general-nmap md4rd-mode-map
      "q" 'ns/kill-current-buffer
      "o" 'md4rd-open
      "r" 'md4rd-reply
      "t" 'md4rd-widget-toggle-line
      ;; "<tab>" 'md4rd-widget-toggle-line
      )

    (add-hook 'md4rd-mode-hook 'ns/md4rd)
    (defun ns/md4rd ()
      ;; (setq wrap-prefix "         ")
      )))

(defconfig powershell
  (ns/guard ns/enable-windows-p)
  (use-package powershell))

(defconfig staging
  (use-package indent-guide
    :config

    (set-face-foreground 'indent-guide-face
      (face-attribute 'font-lock-keyword-face :foreground))

    ;; (set-face-foreground 'indent-guide-face (ns/color-tone (face-attribute 'default :background) 15 15))

    (setq indent-guide-char "|")
    (indent-guide-global-mode 0)
    )

  (defun xah-syntax-color-hex (toggle)
    "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
    (interactive)
    (eval
      (cons
        (if toggle 'font-lock-add-keywords 'font-lock-remove-keywords)
        '(nil
           '(("#[[:xdigit:]]\\{3\\}"
               (0 (put-text-property
                    (match-beginning 0)
                    (match-end 0)
                    'face (list :background
                            (let* (
                                    (ms (match-string-no-properties 0))
                                    (r (substring ms 1 2))
                                    (g (substring ms 2 3))
                                    (b (substring ms 3 4)))
                              (concat "#" r r g g b b))))))
              ("#[[:xdigit:]]\\{6\\}"
                (0 (put-text-property
                     (match-beginning 0)
                     (match-end 0)
                     'face (list :background (match-string-no-properties 0))))))))
      (font-lock-flush))
    )

  (defcommand toggle-colors ()
    (if (not (boundp 'ns/show-colors))
      (setq-local ns/show-colors nil))

    (setq-local ns/show-colors (not ns/show-colors))
    (xah-syntax-color-hex ns/show-colors))

  (ns/bind "tc" 'ns/toggle-colors)

  (defun xah-open-file-at-cursor ()
    "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2018-02-21"
    (interactive)
    (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                        (let ($p0 $p1 $p2
                               ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                               ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
                          (setq $p0 (point))
                          (skip-chars-backward $pathStops)
                          (setq $p1 (point))
                          (goto-char $p0)
                          (skip-chars-forward $pathStops)
                          (setq $p2 (point))
                          (goto-char $p0)
                          (buffer-substring-no-properties $p1 $p2))))
            ($path
              (replace-regexp-in-string
                "^file:///" "/"
                (replace-regexp-in-string
                  ":\\'" "" $inputStr))))
      (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
          (let (($x (xahsite-url-to-filepath $path)))
            (if (string-match "^http" $x )
              (progn (browse-url $x) t)
              (find-file $x)))
          (progn (browse-url $path)))
        (if ; not starting “http://”
          (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
          (let (
                 ($fpath (match-string 1 $path))
                 ($line-num (string-to-number (match-string 2 $path))))
            (if (file-exists-p $fpath)
              (progn
                (find-file $fpath)
                (goto-char 1)
                (forward-line (1- $line-num)))
              ;; (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath)) (find-file $fpath))
              nil
              ))
          (if (file-exists-p $path)
            (progn ; open f.ts instead of f.js
              (let (($ext (file-name-extension $path))
                     ($fnamecore (file-name-sans-extension $path)))
                (if (and (string-equal $ext "js")
                      (file-exists-p (concat $fnamecore ".ts")))
                  (find-file (concat $fnamecore ".ts"))
                  (find-file $path))))
            (if (file-exists-p (concat $path ".el"))
              (find-file (concat $path ".el"))
              ;; (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path)) (find-file $path))
              nil
              ))))))
  ;; todo: make this search back for url? ^

  (defcommand follow ()
    (if (not (xah-open-file-at-cursor))
      (if (string= major-mode "org-mode")
        (org-open-at-point)
        (smart-jump-go))))

  (ns/bind "jj" 'ns/follow)
  )

(defconfig c
  ;; note: depends on clang and cmake
  (use-package irony)
  (use-package flycheck-irony)

  ;; jump to it
  (defcommand recompile ()
    (let ((compilation-directory (projectile-project-root)))
      (ns/shell-exec-dontcare
        (format "cd '%s' && make clean" compilation-directory))
      (recompile)))

  (ns/bind "jk" 'ns/recompile)
  )

(defconfig lua
  (use-package lua-mode)
  ;; note: lua-mode comes with some repl stuff that might come in handy
  )

(defconfig graphiz
  (use-package graphviz-dot-mode
    :config
    ;; (ns/bind)
    (ns/bind-leader-mode 'graphviz-dot "," 'graphviz-dot-preview)))

(defconfig deadgrep
  (ns/use-package deadgrep "Wilfred/deadgrep"
    :config
    (ns/bind "ss" 'deadgrep)
    (setq deadgrep-max-line-length 180)
    (general-nmap deadgrep-mode-map
      "RET" 'deadgrep-visit-result-other-window)
    ))

(defconfig guix
  (ns/guard ns/enable-home-p)
  (use-package guix))

(defconfig elasticsearch
  (use-package es-mode))

;; todo: consider https://github.com/Bad-ptr/persp-mode.el
;; todo: consider https://scripter.co/accessing-devdocs-from-emacs/ instead of dashdocs

(provide 'theworld)

;;; theworld.el ends here
