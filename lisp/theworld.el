;;; theworld.el --- make the thing
;;; commentary:
;;; code:

(defmacro neeasade/load (&rest targets)
  `(mapc (lambda(target)
           (funcall (intern (concat "neeasade/" (prin1-to-string target))))
           )
     ',targets))

;; master
(defmacro defconfig-base (label &rest body)
  (cons 'defun
    (cons (intern (concat "neeasade/" (prin1-to-string label)))
      (cons () body)
      )))

;; commander
(defmacro defconfig (label &rest body)
  `(defconfig-base ,label
     (let ((config-name ,(prin1-to-string label)))
       (message (concat "loading " config-name "..."))
       (catch 'config-catch
         ,(cons 'progn body)
         ))))

;; guards!
(defmacro neeasade/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
    '(when t (throw 'config-catch (concat "config guard " config-name)))
    ))

(defun init-use-package()
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

(defun init-straight()
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

(defconfig helpers
  (use-package hydra)
  (use-package general)
  (use-package rg)
  (use-package dash)
  ;; todo: reconsider request at this level?
  (use-package request)
  (eval-and-compile (load "~/.emacs.d/lisp/helpers.el"))
  )

(defconfig interactive
  (use-package s)
  (load "~/.emacs.d/lisp/interactive.el")
  )

(defconfig sanity

  (setq
    auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
    backup-directory-alist `(("." . "~/.emacs.d/backups"))
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
    ;; ouch - todo: revisit this
    gc-cons-threshold 10000000
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
  (defconst custom-file "~/.emacs.d/custom.el")
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file)

  ;; persistent session:
  ;; note: (desktop-clear) to clean/kill everything.
  (make-directory "~/.emacs.desktop" t)
  (setq-ns desktop
    restore-eager 5
    auto-save-timeout 30
    path (list "~/.emacs.desktop")
    )

  (desktop-save-mode 1)

  (setq browse-url-browser-function 'browse-url-generic)

  (if sys/windows?
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

  (neeasade/bind
    "js" (lambda() (interactive) (neeasade/find-or-open "~/.emacs.d/lisp/scratch.el"))
    "jm" (lambda() (interactive) (counsel-switch-to-buffer-or-window  "*Messages*"))
    "ju" 'browse-url

    "tw" 'whitespace-mode
    "tn" 'linum-mode
    "tl" 'toggle-truncate-lines
    ;; todo: make a toggle
    ;; "tm" (lambda() (interactive) (setq mode-line-format nil))
    )
  )

(defconfig elisp
  (load "~/.emacs.d/vendor/le-eval-and-insert-results.el")

  ;; todo: move this to helpers
  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))

  (neeasade/install-dashdoc "Emacs Lisp")
  (setq lisp-indent-function 'common-lisp-indent-function)
  (defun neeasade/smart-elisp-eval()
    "eval total sexp by paragraph (jump ahead if not on blank line) or region if selected"
    (interactive)
    (if (use-region-p)
      ;; change this func, region doesn't matter
      (le::eval-and-insert-results)
      (progn
        (when (not (current-line-empty-p))
          ;; todo: ] keybind func
          )
        (le::eval-and-insert-results)
        )
      )
    )

  (neeasade/bind-leader-mode
    'emacs-lisp
    "er" 'eval-region
    "ei" 'le::eval-and-insert-results
    "eb" 'le::eval-and-insert-all-sexps
    )
  )

(defconfig evil
  (use-package evil
    ;; for evil-collection
    :init (setq evil-want-integration nil)
    :config (evil-mode 1)
    )

  (use-package evil-collection :config (evil-collection-init))

  ;; todo: check if we're near the height of the buffer and not scroll to top if so
  (defun neeasade/zz-scroll (&rest optional)
    (let* ((scrollcount (/ (window-total-size) 7))
            (halfheight (/ (window-total-size) 2))
            (scrollcheck (- halfheight scrollcount)))

      (if (> (line-number-at-pos) scrollcheck)
        (evil-scroll-line-down scrollcount)
        )
      )
    )

  (add-function :after (symbol-function 'evil-scroll-line-to-center) #'neeasade/zz-scroll)

  (general-evil-setup t)

  ;; defaults to fd/spacemacs-like config
  (use-package evil-escape :config (evil-escape-mode))
  (use-package evil-lion :config (evil-lion-mode))
  (use-package evil-commentary :config (evil-commentary-mode))
  (use-package evil-anzu) ;; displays current match and total matches.
  (use-package evil-matchit :config (global-evil-matchit-mode 1))
  (use-package evil-numbers
    :config
    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
    )

  (use-package evil-goggles
    :config
    (setq evil-goggles-duration 0.100)
    (setq evil-goggles-pulse t)
    ;; fun visual vim mode
    ;; todo: consider some sort of coding presentation mode func
    (evil-goggles-mode 0)
    )

  (use-package evil-surround :config (global-evil-surround-mode 0))

  ;; todo: checkout https://github.com/cute-jumper/evil-embrace.el
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
      ;; "c" #'evil-change
      ;; "d" #'evil-delete
      "s" #'embrace-add
      ;; "x" #'exchange-point-and-mark ; for expand-region
      )

    ;; (evil-embrace-evil-surround-integration)
    )
  ;;; ⇒ t

  ;; also https://github.com/cute-jumper/evil-embrace.el/issues/6
  ;; also: https://github.com/casouri/lunarymacs/blob/79f8eb90ce06371e87d9979de8d3607a52a648c6/star/basic/evil/config.el#L147

  ;; (use-package evil-snipe
  ;;   :config
  ;;   ;; todo: revisit these settings/evaluate https://github.com/hlissner/evil-snipe#search-scope
  ;;   ;; todo: resolve , because it's your leader -- overridden to jump back with this
  ;;   (setq evil-snipe-repeat-scope 'whole-buffer)
  ;;   (setq evil-snipe-spillover-scope 'whole-buffer)
  ;;   (evil-snipe-mode +1)
  ;;   (evil-snipe-override-mode +1)
  ;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  ;;   )

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
  )

(defconfig flycheck
  (use-package flycheck
    :config

    ;; (flycheck) disable jshint since we prefer eslint checking
    (setq-default
      flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    )

  (neeasade/bind
    ;; Applications
    "e" '(:ignore t :which-key "Errors")
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    )
  )

(defconfig treemacs
  (use-package treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  )

(defconfig company
  (use-package company
    :config
    (setq-ns company
      idle-delay (if sys/windows? 1 0)
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
         idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
       (python-mode . python-indent-offset)
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

  ;; some default indent preferences for different modes
  ;; note for the future: editorconfig is awesome.
  (setq sh-basic-offset 2)

  ;; only trim whitespace on lines you edit
  (use-package ws-butler :config (ws-butler-global-mode))

  ;; to always trim it all
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; todo: into yasnippet
  (use-package yasnippet)
  )

(defconfig dashdocs
  ;; doesn't work on windows - bind here for neeasade/install-dashdoc to ref
  (setq neeasade-dashdocs? sys/linux?)
  (neeasade/guard neeasade-dashdocs?)

  (use-package counsel-dash :config
    (setq helm-dash-docsets-path (concat user-emacs-directory "docsets"))
    (setq helm-dash-browser-func 'neeasade/eww-browse-existing-or-new)
    )

  ;; todo: this needs a counsel-dash-at-point/how to get point
  ;; ref: https://github.com/areina/helm-dash/blob/master/helm-dash.el#L584
  (neeasade/bind
    "jd" 'counsel-dash
    )
  )

(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                 powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defconfig style
  (interactive)
  ;; todo: an xresources theme that doesn't suck/covers extensions that base16 covers
  (use-package base16-theme)
  ;;(use-package ujelly-theme)

  (load-theme (intern (get-resource "Emacs.theme")))

  (set-face-attribute 'fringe nil :background nil)
  (set-face-background 'font-lock-comment-face nil)

  ;; todo: make this on all frames, not just current
  (set-frame-parameter (selected-frame) 'internal-border-width
    (string-to-number (get-resource "st.borderpx")))

  ;; this is only viable if can get it on internal window edges only (not right now)
  ;; http://emacsredux.com/blog/2015/01/18/customizing-the-fringes/
  ;; (fringe-mode (string-to-number (get-resource "st.borderpx")))

  ;; sync w/ term background
  (set-background-color (get-resource "*.background"))

  ;; assume softer vertical border by matching comment face
  (set-face-attribute 'vertical-border
    nil :foreground (face-attribute 'font-lock-comment-face :foreground))

  ;; after circe load
  ;; (set-face-attribute 'circe-server-face nil :foreground (face-attribute 'font-lock-comment-face :foreground))
  ;; circe-server-face

  ;; this doesn't persist across new frames even though the docs say it should
  (set-face-attribute 'fringe nil :background nil)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (set-face-attribute 'fringe nil :background nil)
      )
    )

  ;; set font on current and future
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (set-frame-font (get-resource "st.font") nil t)

  ;; (eval-after-load "whitespace-mode"
  ;; (defadvice org-add-props (ac check-faces activate)

  (defun color-whitespace-mode()
    (interactive)
    (message "hook hit")
    (set-face-attribute 'whitespace-space nil :background nil)
    (set-face-attribute 'whitespace-tab nil :background nil)
    (set-face-attribute 'whitespace-newline nil
      :foreground (face-attribute 'whitespace-space :foreground))
    )

  (advice-add #'color-whitespace-mode :after #'whitespace-mode)

  (use-package hl-todo
    :config
    (let* ((comment-color (face-attribute 'font-lock-comment-face :foreground))
            (highlight-color
              (if (neeasade/color-is-light-p comment-color)
                (color-darken-name comment-color 30)
                (color-lighten-name comment-color 30)
                )))

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

  (neeasade/spaceline)
  )

(defconfig spaceline
  (use-package spaceline
    :config
    (require 'spaceline-config)
    (setq-ns powerline
      scale (string-to-number (get-resource "Emacs.powerlinescale"))
      height (spacemacs/compute-powerline-height)
      default-separator (get-resource "Emacs.powerline")
      )

    ;; todo sync modeline background color?
    ;; other option is remove off color section/pick explicitly
    ;; (set-face-attribute 'spacemacs-normal-face nil :inherit 'mode-line)

    (set-face-attribute 'spaceline-highlight-face nil
      :background (face-attribute 'spaceline-evil-normal :background))

    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-evil-state-off)
    (spaceline-compile)
    )
  )

(defconfig feebleline
  ;; todo
  (load "~/.emacs.d/lib/feebleline.el")
  )

(defconfig zoom
  (use-package zoom
    :config
    (setq zoom-size '(80 . 24))
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode 1)
    )
  )

(defconfig dimmer
  (use-package dimmer
    :config (setq dimmer-fraction 0.5)
    (dimmer-mode)
    )
  )

(defconfig org
  (use-package org
    :straight (:host github
                :repo "emacsmirror/org"
                :files ("lisp/*.el" "contrib/lisp/*.el"))

    :config
    (setq-ns org
      directory "~/notes"
      agenda-files (list org-directory)
      default-notes-file  (concat org-directory "/notes.org")
      default-diary-file  (concat org-directory "/diary.org")
      default-habits-file  (concat org-directory "/habits.org")

      ellipsis "…"
      startup-indented t
      startup-folded t

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

         ("D" "Daily Log" entry (file "~/notes/daily-log.org")
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
    )

  (defun neeasade/org-open-url()
    (interactive)
    (browse-url (org-entry-get nil "url"))
    )

  (defun neeasade/org-set-active()
    (interactive)
    (org-delete-property-globally "focus")
    (org-set-property "focus" "t")

    (setq org-active-story (substring-no-properties (org-get-heading t t t t)))
    )

  ;; for externals to call into
  (defun neeasade/org-get-active()
    (if (not (bound-and-true-p org-active-story))
      (progn
        (neeasade/org-goto-active)
        (neeasade/org-set-active)
        )
      org-active-story
      )
    )

  (defun neeasade/org-goto-active()
    (interactive)
    (neeasade/find-or-open org-default-notes-file)
    (goto-char (org-find-property "focus"))
    (org-show-context)
    (org-show-subtree)
    (evil-scroll-line-to-center nil)
    )

  (add-hook
    'org-mode-hook
    (neeasade/bind-leader-mode
      'org
      "," 'org-ctrl-c-ctrl-c
      "t" 'org-todo
      "T" 'org-show-todo-tree
      "v" 'org-mark-element
      "a" 'org-agenda
      "l" 'evil-org-open-links
      "p" 'org-pomodoro
      "q" 'tp-set-org-userstory
      "f" 'neeasade/org-set-active
      "b" 'neeasade/org-open-url
      )
    )

  (use-package org-pomodoro
    :config
    (defun neeasade/toggle-music(action)
      ;; todo: see if this can turn into emms command
      (let ((command (concat (if sys/windows? "mpc" "player.sh") " " action)))
        (shell-command command)
        ))

    (add-hook 'org-pomodoro-started-hook
      (apply-partially #'neeasade/toggle-music "play"))

    (add-hook 'org-pomodoro-break-finished-hook
      (apply-partially #'neeasade/toggle-music "play"))

    (add-hook 'org-pomodoro-finished-hook
      (apply-partially #'neeasade/toggle-music "pause"))
    )

  (defun jump-org() (interactive) (neeasade/find-or-open "~/notes/notes.org" ))
  (neeasade/bind
    "oo" 'neeasade/org-goto-active
    "oc" 'org-capture
    "or" 'org-refile

    ;; ehh
    "on" 'jump-org
    )
  )

(defconfig clojure
  (use-package clojure-mode)
  (use-package cider)
  (neeasade/install-dashdoc "Clojure")

  ;; TODO: learn lispyville
  ;; (use-package lispy)

  (neeasade/bind-leader-mode
    'clojure
    "er" 'cider-eval-region
    "ei" 'cider-eval-last-sexp
    "eb" 'cider-evil-file
    )
  )

(defconfig nix
  (use-package nix-mode)
  )

(defconfig target-process
  (if enable-tp?
    (load "~/.emacs.d/lib/targetprocess.el")
    )
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

    (defun dynamic-ivy-height()
      (setq ivy-height (/ (window-total-size) 2)))

    (dynamic-ivy-height)
    (add-hook 'window-configuration-change-hook 'dynamic-ivy-height)

    (ivy-mode 1)

    (straight-use-package '(prescient :host github :repo "raxod502/prescient.el"))
    (ivy-prescient-mode)
    )

  ;; counsel
  (use-package counsel
    :config
    (setq-ns counsel
      grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
      rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."
      ag-base-command "ag --vimgrep --nocolor --nogroup %s"
      )

    ;; counsel-rg is crashing emacs on windows
    ;; (can't C-g/esc, can't send USR2 on windows)
    ;; (toggle-debug-on-error nil)
    ;; (toggle-debug-on-quit nil)
    )

  (use-package ranger
    :init (setq ranger-override-dired t)
    :config
    (setq-ns ranger-show literal nil hidden t)
    (neeasade/bind "d" 'deer)
    )

  (neeasade/bind
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

    "wm" 'delete-other-windows ;; window-max
    "wo" 'other-frame

    ;; Applications
    "a" '(:ignore t :which-key "Applications")

    "b" '(:ignore t :which-key "Buffers")
    "bd" '(kill-buffer nil)
    )

  (use-package alert
    :config (setq alert-default-style
              (if sys/windows?
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

  ;; jump to buffer with avy
  ;; todo: checkout avy-goto-char-timer
  (use-package ace-jump-buffer
    :config
    (defun dynamic-ajb-height()
      (setq ajb-max-window-height (/ (window-total-size) 2))
      )

    (dynamic-ajb-height)
    (add-hook 'window-configuration-change-hook 'dynamic-ajb-height)

    (setq ajb-sort-function 'bs--sort-by-recentf)

    (neeasade/bind
      "bb" 'counsel-ibuffer
      "bs" 'ace-jump-buffer
      "bm" 'ace-jump-same-mode-buffers
      )
    )
  )

(defconfig emms
  (neeasade/guard neeasade/home?)
  (use-package emms)

  (defun emms-start()
    (require 'emms-player-mpd)
    (setq-ns emms-player-mpd
      server-name "localhost"
      server-port "6600"
      music-directory "~/Music"
      ;; server-password "mypassword"
      )

    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)

    ;; sync with mpd db, connect
    (emms-cache-set-from-mpd-all)
    (emms-player-mpd-connect)
    )

  (neeasade/bind "am" 'emms-start)
  )

(defconfig projectile
  (use-package projectile)
  ;; (project-find-file-in)
  (neeasade/bind
    "p" '(:ignore t :which-key "projects")
    "pf" 'counsel-git
    )
  )

(defconfig javascript
  (neeasade/install-dashdoc "Javascript")

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
          (message "now set to: %s" web-mode-content-type))))

    )

  ;; todo: enable this sometimes if conf file found
  (use-package prettier-js)

  ;; notes for using this
  ;; kill shx-mode
  ;; doesn't work with multiline input, or import command/multiple files
  (use-package nodejs-repl
    :config
    (neeasade/bind-leader-mode
      'nodejs-repl
      "er "'nodejs-repl-send-region
      "eb" 'nodejs-repl-load-file
      "ee" 'nodejs-repl-send-line
      "ei" 'nodejs-repl-send-last-expression
      ))
  )

(defconfig typescript
  (neeasade/install-dashdoc "Typescript")
  (use-package tide
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      )

    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t)

    ;; formats the buffer before saving
    ;; (add-hook 'before-save-hook 'tide-format-before-save)

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    )
  )

(defconfig csharp
  (use-package csharp-mode)
  (use-package omnisharp)
  )

(defconfig git
  (use-package magit
    :config
    (setq magit-repository-directories (list "~/git"))

    ;; https://magit.vc/manual/magit/Performance.html
    (when sys/windows?
      (setq-ns magit
        ;; diff perf
        diff-highlight-indentation nil
        diff-highlight-trailing nil
        diff-paint-whitespace nil
        diff-highlight-hunk-body nil
        diff-refine-hunk nil

        ;;
        refresh-status-buffer nil
        )

      ;; don't show diff when committing --
      ;; means reviewing will have to be purposeful before
      (remove-hook 'server-switch-hook 'magit-commit-diff)

      ;; disable emacs VC
      (setq vc-handled-backends nil)
      )
    )

  (use-package magit-svn)
  (add-hook 'magit-mode-hook 'magit-svn-mode)

  (use-package evil-magit
    :config
    (evil-define-key
      evil-magit-state magit-mode-map "?" 'evil-search-backward)
    )

  (use-package git-gutter-fringe
    :config
    (setq git-gutter-fr:side 'right-fringe)
    ;; fails when too many buffers open on windows
    (if sys/linux? (global-git-gutter-mode t))
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

  (neeasade/bind
    "g" '(:ignore t :which-key "git")
    "gb" 'magit-blame
    "gl" 'magit-log-current
    "gm" 'git-smerge-menu/body
    )

  ;; define a minimal staging mode for when we're on windows.
  (when sys/windows?
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

  (if sys/windows?
    (neeasade/bind "gs" 'magit-staging)
    (neeasade/bind "gs" 'magit-status)
    )
  )

(defconfig jump
  (use-package smart-jump
    :config
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher 'rg)
    (smart-jump-setup-default-registers)
    (neeasade/bind
      "j" '(:ignore t :which-key "Jump")
      "jj" 'smart-jump-go
      "jb" 'smart-jump-back
      )
    )

  ;; (add-function :after (symbol-function 'smart-jump-go) #'evil-scroll-line-to-center)
  ;; todo:
  ;; (advice-add #'neeasade/org-set-active :after #'tp-set-active)
  )

(defconfig irc
  (neeasade/guard neeasade/home?)
  (use-package circe
    :config
    (setq neeasade/irc-nick "neeasade")
    (setq circe-network-options
      `(
         ("Freenode"
           :nick ,neeasade/irc-nick
           :host "irc.freenode.net"
           :tls t
           :nickserv-password ,(pass "freenode")
           :channels (:after-auth "#bspwm" "#qutebrowser")
           )

         ("Nixers"
           :nick ,neeasade/irc-nick
           :host "irc.unix.chat"
           :port (6667 . 6697)
           :tls t
           :channels ("#unix")
           )

         ("Rizon"
           :nick ,neeasade/irc-nick
           :host "irc.rizon.net"
           :port (6667 . 6697)
           :tls t
           :channels (:after-auth "#rice" "#code")
           :nickserv-password ,(pass "rizon/pass")
           :nickserv-mask ,(rx bol "NickServ!service@rizon.net" eol)
           :nickserv-identify-challenge ,(rx bol "This nickname is registered and protected.")
           :nickserv-identify-command "PRIVMSG NickServ :IDENTIFY {password}"
           :nickserv-identify-confirmation ,(rx bol "Password accepted - you are now recognized." eol)
           )
         ))
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
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

  (defun connect-all-irc()
    (interactive)
    (mapcar
      '(lambda (network) (circe-maybe-connect (car network)))
      circe-network-options)
    )

  ;; channel name in prompt
  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (defun my-circe-prompt ()
    (lui-set-prompt
      (concat (propertize (concat (buffer-name) ">") 'face 'circe-prompt-face) " ")))

  ;; prevent too long pastes/prompt on it:
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

  ;; hide part, join, quit
  (setq circe-reduce-lurker-spam t)

  (setq circe-format-say "{nick:-8s} {body}")

  (load "lui-logging" nil t)
  (setq lui-logging-directory "~/.irc")
  (enable-lui-logging-globally)

  (setq
    lui-time-stamp-position 'right-margin
    lui-time-stamp-format "%H:%M")

  (add-hook 'lui-mode-hook 'my-circe-set-margin)
  (defun my-circe-set-margin ()
    (setq right-margin-width 5)
    (setq left-margin-width 0)
    )

  ;; fluid width windows
  (setq
    lui-time-stamp-position 'right-margin
    lui-fill-type nil)

  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq
      fringes-outside-margins t
      right-margin-width 5
      word-wrap t
      wrap-prefix "    ")
    (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)
    )

  (use-package circe-notifications
    :config
    (autoload 'enable-circe-notifications "circe-notifications" nil t)
    (eval-after-load "circe-notifications"
      '(setq circe-notifications-watch-strings
         ;; example: '("people" "you" "like" "to" "hear" "from")))
         '("neeasade")))

    (add-hook 'circe-server-connected-hook 'enable-circe-notifications)
    )

  (defun neeasade/jump-irc()
    (interactive)
    (let ((irc-channels
            (remove-if-not
              (lambda (s) (s-match "#.*" s))
              (mapcar 'buffer-name (buffer-list))
              )))
      (if (eq (length irc-channels) 0)
        (message "connect to irc first!")
        (ivy-read "channel: " irc-channels
          :action (lambda (option) (counsel-switch-to-buffer-or-window option)))
        )
      )
    )

  (neeasade/bind
    "ai" 'connect-all-irc
    "ji" 'neeasade/jump-irc
    )
  )

(defconfig pdf
  (neeasade/guard neeasade/home?)
  (use-package pdf-tools)
  )

(defconfig terraform
  (use-package terraform-mode)
  )

(defconfig twitter
  (neeasade/guard neeasade/home?)
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

  (neeasade/bind "at" 'twittering-start)
  )

(defconfig slack
  (neeasade/guard neeasade/home?)
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t)
    (setq slack-prefer-current-team t)

    :config
    (when sys/windows?
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
  (neeasade/bind-leader-mode
    'slack-info
    "u" 'slack-room-update-messages)

  (neeasade/bind-leader-mode
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

  (neeasade/bind-leader-mode
    'slack-edit-message
    "k" 'slack-message-cancel-edit
    "s" 'slack-message-send-from-buffer
    "2" 'slack-message-embed-mention
    "3" 'slack-message-embed-channel
    )

  (neeasade/bind
    "as" 'slack-start)
  )

(defconfig email
  (neeasade/guard neeasade/home?)
  ;; TODO
  (nop)
  )

(defconfig shell

  (add-hook 'sh-mode-hook
    (lambda () (sh-electric-here-document-mode -1)))

  ;; consider arrow function here
  ;; https://superuser.com/questions/139815/how-do-you-run-the-previous-command-in-emacs-shell
  (when sys/linux?
    (setq explicit-shell-file-name (getenv "SHELL"))
    )

  (when (and sys/windows? (not sys/docker?))
    (setq explicit-shell-file-name (neeasade/shell-exec "where bash"))
    (setq explicit-bash.exe-args '("--login" "-i"))
    )

  ;; https://stackoverflow.com/questions/25862743/emacs-can-i-limit-a-number-of-lines-in-a-buffer
  (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

  (use-package shx :config (shx-global-mode 1))

  (use-package shell-pop
    :config

    ;; https://github.com/kyagi/shell-pop-el/issues/51
    (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

    (setq-ns shell-pop
      window-position "top"
      window-size 33 ;; percent
      full-span t
      )

    (neeasade/bind "'" 'shell-pop)

    ;; interactive shell-pop bound to spc t index shell
    (defun makepop(index)
      (let ((funcname (intern (concat "shell-pop-" (number-to-string index)))))
        (eval `(progn
                 (defun ,funcname () (interactive) (shell-pop ,index))
                 (neeasade/bind ,(concat "t" (number-to-string index)) ',funcname)
                 )
          )
        )
      )

    ;; give us 1-9
    (mapc 'makepop (number-sequence 1 9))
    )

  (neeasade/bind-mode '(shell)
    ;; todo: winner-undo/unshell pop
    "d" 'deer
    )

  ;; todo: These binds don't work why
  ;; todo: def advice after quite ranger last history shell-pop/use same shell
  ;; (progn ranger-history-ring)
  (neeasade/bind-mode
    '(ranger)
    "s" 'shell-pop
    )

  (general-define-key
    :states '(visual normal)
    :keymaps '(ranger)
    "s" 'shell-pop
    )

  ;; only for term, ansi term
  ;; https://github.com/hlissner/emacs-doom-themes/issues/54
  (setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
  )

(defconfig eshell
  ;; todo: https://www.reddit.com/r/emacs/comments/6y3q4k/yes_eshell_is_my_main_shell/
  )

(defconfig jekyll
  (use-package jekyll-modes)
  )

(defconfig autohotkey
  (neeasade/guard sys/windows?)
  (use-package xahk-mode)
  )

(defconfig markdown
  ;; (use-package markdownmode)
  )

(defconfig restclient
  (use-package restclient
    :config
    (neeasade/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window
      )
    )

  (use-package company-restclient)
  )

(defconfig sql
  ;; todo
  (neeasade/install-dashdoc "SQLite")
  )

(defconfig latex
  (neeasade/bind-leader-mode 'latex
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

  (neeasade/install-dashdoc "LaTeX")

  ;; todo: this doesn't build?
  ;; ref: https://github.com/raxod502/straight.el/issues/240
  ;; (use-package company-auctex)
  )


(defconfig plantuml
  (use-package plantuml)
  (use-package flycheck-plantuml)
  )

(defconfig ledger
  (neeasade/guard neeasade/home?)
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
  )

(defconfig search-engines
  (use-package engine-mode
    :config

    ;; bind spc s 'hotkey' to a search url with a label
    (defmacro bind-search (label url hotkey)
      `(progn
         (defengine ,label ,url)
         (neeasade/bind
           (concat "s" ,hotkey) (intern (concat "engine/search-" (prin1-to-string ',label)))
           )
         )
      )

    (bind-search google "https://google.com/search?q=%s" "s")
    (bind-search melpa "https://melpa.org/#/?q=%s" "m")
    (bind-search stack-overflow "https://stackoverflow.com/search?q=%s" "o")
    (bind-search github "https://github.com/search?ref=simplesearch&q=%s" "g")
    (bind-search youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" "y")
    (engine-mode t)
    )
  )

(defconfig filehooks
  (neeasade/guard neeasade/home?)

  (defvar *afilename-cmd*
    ;; todo: consider more here -- sxhkd, bspwmrc? ~/.wm_theme (if smart-load ever comes to fruition)
    `((,(neeasade/homefile ".Xresources") . "xrdb -merge ~/.Xresources")
       (,(neeasade/homefile ".Xmodmap") . "xmodmap ~/.Xmodmap"))
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
    :config (global-emojify-mode)
    ))

;; todo: consider https://github.com/Bad-ptr/persp-mode.el
;; todo: consider https://scripter.co/accessing-devdocs-from-emacs/ instead of dashdocs

(provide 'theworld)

;;; theworld.el ends here
