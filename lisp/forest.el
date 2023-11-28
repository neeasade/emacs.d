;; -*- lexical-binding: t; -*-
;;; forest.el --- Take a walk through the parentheses.
;;; Commentary:
;;; 🌲🌲👺🌲🌲🌲
;;; Code:

(ns/defconfig elisp
  (ns/install-dashdoc "Emacs Lisp" 'emacs-lisp-mode-hook)
  ;; 'common-lisp-indent-function

  (setq lisp-indent-function 'common-lisp-indent-function)

  (ns/use helpful
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key))

  (ns/use eros
    (setq eros-eval-result-duration 20)
    (eros-mode 1)

    (defun! ns/smart-elisp-eval ()
      (if (use-region-p)
        (eval-region (region-beginning) (region-end))
        (if (s-blank-p (s-trim (thing-at-point 'line)))
          (eros-eval-last-sexp nil)
          (eros-eval-defun nil))))

    (defun! ns/eval-and-replace ()
      "Replace the preceding sexp with its value."
      (backward-kill-sexp)
      (condition-case nil
        (prin1 (eval (read (current-kill 0)))
          (current-buffer))
        (error (message "Invalid expression")
          (insert (current-kill 0)))))

    (ns/bind-mode 'emacs-lisp
      "e" 'ns/smart-elisp-eval
      "E" 'eval-print-last-sexp
      ))

  ;; note: elsa needs cask to do anything:
  (when (which "cask")
    (ns/use elsa)
    (ns/use flycheck-elsa)
    (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup)))

(ns/defconfig flycheck
  (ns/use flycheck
    ;; cf http://www.flycheck.org/en/latest/user/syntax-checks.html#check-automatically
    (setq-ns flycheck
      check-syntax-automatically (if ns/enable-windows-p
                                   '(save mode-enabled idle-change)
                                   '(save mode-enabled idle-change new-line))

      idle-change-delay 1
      global-modes '(not circe-channel-mode circe-query-mode emacs-lisp-mode)
      ))

  ;; disable jshint since we prefer eslint checking
  (setq-default
    flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (ns/use flycheck-pos-tip (flycheck-pos-tip-mode t)))

(ns/defconfig corfu
  (ns/use (corfu :host github :repo "minad/corfu" :files ("*.el" "extensions/*.el"))
    (setq tab-always-indent 'complete)

    (setq
      corfu-quit-no-match 'separator
      corfu-auto t
      corfu-auto-delay 0.1)

    (apply 'evil-collection-translate-key
      'insert '(corfu-map)
      ns/evil-collection-keys)

    (require 'corfu-popupinfo)
    (corfu-popupinfo-mode)
    (setq corfu-popupinfo-delay '(0.2 . 0.1)))

  ;; todo: check this out
  (ns/use cape))

(ns/defconfig dashdocs
  ;; todo: checkout consult-dash
  (ns/use counsel-dash)

  (setq-ns counsel-dash
    min-length 0
    docsets-path (~ ".local/share/Zeal/Zeal/docsets"))

  (make-directory counsel-dash-docsets-path t)

  (defun! ns/counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash "")))

  ;; doesn't work, dired/switch
  ;; (ns/bind "nd" 'ns/counsel-dash-word)
  )

(ns/defconfig python
  (ns/install-dashdoc "Python 3" 'python-mode-hook)
  (ns/use elpy)
  (when (which "pyflakes")
    (ns/use flycheck-pyflakes)))

(ns/defconfig clojure
  (ns/use clojure-mode)
  (ns/use cider)

  ;; (ns/use clj-refactor)

  (setq cider-eval-result-duration 20)

  (ns/inmap 'cider-repl-mode-map (kbd "C-e") 'cider-repl-previous-input)
  (ns/inmap 'cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)

  ;; https://docs.cider.mx/cider/caveats.html#_injecting_dependencies_and_leiningen_pedantic_abort_mode
  (when ns/enable-work-p
    (setq cider-inject-dependencies-at-jack-in nil))

  (setq cljr-suppress-middleware-warnings t)

  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  (defun! ns/smart-cider-eval ()
    (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (cider-eval-last-sexp nil)
        (cider-eval-defun-at-point nil))))

  (ns/bind-mode 'clojure
    "e" 'ns/smart-cider-eval
    "E" 'cider-eval-print-last-sexp)

  (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
  (add-to-list 'interpreter-mode-alist '("joker" . clojure-mode))

  (when (which "joker")
    (ns/use flycheck-joker  (require 'flycheck-joker)))

  (when (which "clj-kondo")
    (ns/use flycheck-clj-kondo))

  (defun! ns/babashka-default-connect ()
    (cider-connect-clj '(:host "localhost" :port 1667))))

(ns/defconfig projectile
  (ns/use projectile)

  (ns/bind "nt" 'projectile-toggle-between-implementation-and-test)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun ns/get-project-files (project-root)
    (llet (default-directory (expand-file-name project-root)
            project-files-relative (s-split "\n"
                                     (s-replace (char-to-string ?\0) "\n"
                                       (shell-command-to-string
                                         counsel-git-cmd
                                         )) t))

      (-map (fn (concat default-directory <>)) project-files-relative)))

  (defun ns/all-project-files (open-buffers)
    (-flatten
      (-map 'ns/get-project-files
        (-remove (lambda (file) (not file))
          (-map 'projectile-root-bottom-up open-buffers)))))

  (defun ns/jump-file-candidates (&rest wants)
    (llet (sources
            (list
              ;; XXX these keys are used in dmenu_switcher
              :buffers-without-files
              (->> (buffer-list)
                (--filter (not (buffer-file-name it)))
                (--remove (eq 'dired-mode (buffer-local-value 'major-mode it)))
                (-map 'buffer-name)
                (-remove (-partial 's-starts-with-p "*spawn-shell-"))
                ;; there appear to be "special buffers" in the buffers-without-files set
                ;; using this hack to get the "right" special buffers
                ;; (derived from the completion used for evil's :b)
                (-intersection (internal-complete-buffer "" nil t)))

              :buffers-with-files (->> (buffer-list)
                                    (-map 'buffer-file-name)
                                    (-remove 'not) ; remove nils
                                    (-map #'consult--fast-abbreviate-file-name))

              :project-files (-when-let (current-file-name (buffer-file-name (current-buffer)))
                               (-map #'consult--fast-abbreviate-file-name
                                 (ns/all-project-files (list current-file-name))))

              ;; (ns/get-project-files (current-file))
              ;; (if ns/enable-linux-p (ns/all-project-files open-buffers) (ns/get-project-files (current-file)))

              :recentf (-map #'consult--fast-abbreviate-file-name recentf-list)))

      (llet [selected (or wants '(:recentf :project-files :buffers-with-files))
              results (->> selected
                        (-map (-partial #'plist-get sources))
                        (-flatten)
                        (-uniq)
                        (--filter
                          ;; if a file is not remote, ensure it exists
                          ;; (f-exists-p is slow on remote files)
                          (or (file-remote-p it)
                            (f-exists-p it))))]

        (if (-contains-p selected :buffers-without-files)
          (append results (plist-get sources :buffers-without-files))
          results))))

  (ns/bind
    "ne" (fn!! surf-files (find-file (ns/pick "file" (ns/jump-file-candidates))))
    ;; todo: fix this in dired/broken there
    "nE" (fn!! surf-project-files (find-file (ns/pick "file" (ns/jump-file-candidates :project-files))))))

(ns/defconfig javascript
  ;; note: this is huge, takes a bit.
  (ns/install-dashdoc "JavaScript" 'web-mode-hook)

  (ns/comment
    (defun js-jsx-indent-line-align-closing-bracket ()
      "Workaround sgml-mode and align closing bracket with opening bracket"
      (save-excursion
        (beginning-of-line)
        (when (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset))))

    (advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket))

  ;; use web-mode for .js files

  (ns/file-mode "js" 'web-mode)
  (ns/file-mode "tsx" 'web-mode)

  (ns/use rjsx-mode)
  (defun ns/webhook ()
    ;; (if (string-equal "tsx" (file-name-extension buffer-file-name))
    ;;   (if (equal web-mode-content-type "javascript")
    ;;     (progn
    ;;       (web-mode-set-content-type "jsx")
    ;;       (setup-tide-mode)))
    ;;   (message "now set to: %s" web-mode-content-type))
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  (ns/use web-mode
    (add-hook 'web-mode-hook 'ns/webhook))

  (ns/use prettier-js
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'js-mode-hook 'prettier-js-mode)))

(ns/defconfig typescript
  (ns/install-dashdoc "TypeScript" 'typescript-mode-hook)

  (ns/use tide
    (defun! setup-tide-mode ()
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode)

    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    ))

(ns/defconfig markdown
  (ns/use markdown-mode)
  (ns/file-mode "md" 'markdown-mode)
  (ns/file-mode "markdown" 'markdown-mode)

  (general-define-key
    :states '(normal)
    :keymaps 'markdown-mode-map
    (kbd "<tab>") 'markdown-cycle)

  (defun ns/style-markdown ()
    (ns/set-faces-monospace '(markdown-code-face markdown-comment-face))

    (when (called-interactively-p 'any)
      (ns/set-buffers-face-variable (ns/buffers-by-mode 'markdown-mode))))

  (add-hook 'markdown-mode-hook 'ns/set-buffer-face-variable))

(ns/defconfig restclient
  (ns/use restclient
    (ns/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window)))

(ns/defconfig sql
  (ns/install-dashdoc "SQLite" 'sql-mode)

  ;; setup: https://github.com/kostafey/ejc-sql#install-jdbc-drivers
  ;; (ns/use ejc-sql
  ;;   
  ;;   ;; test local sqlite
  ;; )
  )

(ns/defconfig latex
  (ns/install-dashdoc "LaTeX" 'latex-mode-hook))

(ns/defconfig plantuml
  (ns/use plantuml-mode)
  (ns/use flycheck-plantuml))

(ns/defconfig ledger
  (ns/use ledger-mode)
  (ns/use flycheck-ledger)
  (ns/use evil-ledger
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

(ns/defconfig lsp
  (ns/use lsp-mode)
  (ns/use lsp-ui)

  ;; (ns/use eglot)

  (defun ns/lsp-cleanup ()
    "lsp-format and imports on save"
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'ns/lsp-cleanup))

(ns/defconfig search-engines
  (ns/use engine-mode
    ;; bind spc s 'hotkey' to a search url with a label
    (defmacro bind-search (hotkey label url)
      `(progn
         (defengine ,label ,url)
         (ns/bind
           (concat "s" ,hotkey)
           (intern (concat "engine/search-" (prin1-to-string ',label))))))

    (bind-search "m" melpa "https://melpa.org/#/?q=%s")
    (bind-search "o" stack-overflow "https://stackoverflow.com/search?q=%s")
    (bind-search "g" github "https://github.com/search?ref=simplesearch&q=%s")
    (bind-search "y" youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
    (engine-mode t)))

(ns/defconfig filehooks
  (setq ns/filename-cmd
    `(
       ;; ,(~ ".Xresources") "xrdb -merge ~/.Xresources && pkill -x --signal USR1 xst"
       ,(~ ".Xresources") "xrdb -merge ~/.Xresources"
       ,(~ ".Xmodmap") "xmodmap ~/.Xmodmap"
       ,(~ "bin/theme") "theme -c"
       ;; eventually
       ;; ,@(->> (f-files (~ ".dotfiles/wm/.templates"))
       ;;     (-mapcat (fn (list <> (format "ltheme %s" (f-base <>))))))
       ))

  (defun ns/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (-map (-lambda ((file cmd))
            (when (equal (buffer-file-name) file)
              (sh-toss cmd)))
      (-partition 2 ns/filename-cmd)))

  (add-hook 'after-save-hook 'ns/cmd-after-saved-file))

(ns/defconfig c
  (ns/install-dashdoc "C" 'c-mode-hook)

  ;; note: depends on clang and cmake
  ;; (ns/use irony)
  ;; fyi:
  ;; (irony-install-server)

  (ns/use flycheck-irony)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (defun! ns/recompile ()
    (let ((compilation-directory (projectile-project-root)))
      (sh (format "cd '%s' && make clean" compilation-directory))
      (recompile))))

(ns/defconfig graphviz
  (ns/use graphviz-dot-mode
    (ns/bind-leader-mode 'graphviz-dot "," 'graphviz-dot-preview)
    (ns/bind-mode 'graphviz-dot "e" 'graphviz-dot-preview))

  ;; use dot in org mode
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)))

  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  (defun ns/org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))         ;; don't ask for dot

  (setq org-confirm-babel-evaluate 'ns/org-confirm-babel-evaluate)

  (defun! ns/refresh-images-org ()
    (org-toggle-inline-images)
    (org-toggle-inline-images)))

(ns/defconfig server
  (require 'server)

  (when ns/enable-windows-p
    (setq server-auth-dir (~e "server")
      server-name "emacs-server-file"))

  (when-not (server-running-p)
    (server-start)))

(ns/defconfig common-lisp
  (ns/use slime)
  (setq inferior-lisp-program (which "sbcl"))
  (setq slime-contribs '(slime-fancy))

  (defun! slime-eval-last-sexp-overlay ()
    (destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))

  (ns/install-dashdoc "Common Lisp" 'lisp-mode-hook)

  (defun! ns/smart-slime-eval ()
    (if (use-region-p)
      (slime-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (slime-eval-last-sexp-overlay)
        (save-excursion (end-of-defun) (slime-eval-last-sexp-overlay)))))

  ;; common-lisp-mode -> lisp-mode
  (ns/bind-mode 'lisp "e" 'ns/smart-slime-eval))

(ns/defconfig scripting
  (add-to-list 'interpreter-mode-alist '("elisp" . emacs-lisp-mode))

  ;; helper for unpacking args provided by elisp script as shebang
  ;; use: (ns/let-script-args (named named2) body)
  (defmacro ns/let-script-args (args &rest content)
    `(let (,@(-map
               (fn (list (nth <> args)
                     (nth <> ns-args)))
               (number-sequence 0 (- (length args) 1))))
       ,@content))

  (defun ns/make-lines (list)
    "Transform a LIST of things into something that can be newline iterated by a shell script."
    (->> list
      (-map 'ns/str)
      (-map 's-clean)
      (s-join "\n"))))

(ns/defconfig funtext
  (defun studlify-string (s)
    (with-temp-buffer
      (insert s)
      (studlify-buffer)
      (buffer-string)))

  (defun ns/make-char-table (name upper lower)
    "Make a char table for a certain kind of character"
    (set name
      (let ((str (make-string 127 0)))
        (dotimes (i 127)
          (aset str i i))
        (dotimes (i 26)
          (aset str (+ i ?A) (+ i upper))
          (aset str (+ i ?a) (+ i lower)))
        str)))

  (->> '((?𝙰 ?𝚊 monospace)
          (?Ａ ?ａ widechar)
          (?𝔄 ?𝔞 gothic)
          (?𝓐 ?𝓪 cursive))
    (-map
      (-lambda ((upper lower label))
        (llet [char-table-name (intern (format "ns/%s-char-table" label))
                fn-name (intern (format "ns/text-to-%s" label))]
          (ns/make-char-table char-table-name upper lower)
          (eval
            `(defun ,fn-name (beg end) (interactive "r")
               (translate-region beg end ,char-table-name))))))))

(ns/defconfig adoc
  (ns/use adoc-mode)
  (ns/use ox-asciidoc)

  (defun ns/style-adoc ()
    (ns/set-faces-monospace '(adoc-code-face adoc-comment-face))

    (when (called-interactively-p 'any)
      (ns/set-buffers-face-variable (ns/buffers-by-mode 'adoc-mode))))

  (add-hook 'markdown-mode-hook 'ns/set-buffer-face-variable))

(ns/defconfig resources
  (setq ns/resource-table
    (let ((default-font (-when-let (dfont (face-attribute 'default :font))
                          (when-not (eq 'unspecified dfont)
                            (font-get dfont :name)))))
      (-ht
        "panel.height" "24"
        "emacs.theme" "myron-mcfay"
        "font.mono.spec" default-font
        "font.variable.spec" default-font)))

  (defun get-resource (name)
    (ht-get ns/resource-table name))

  (defun ns/update-resource-font (key font)
    "Update the fallback font for xrdb value"
    (when (and font (not (s-blank-p font))
            (find-font (font-spec :name font)))
      (ht-set ns/resource-table key font)
      t))

  (defun ns/refresh-resources ()
    (interactive)
    (if (which "theme")
      (--map (ht-set ns/resource-table it
               (sh "theme -q '%s' 2>/dev/null" it))
        (ht-keys ns/resource-table))
      (progn
        (-first (-partial 'ns/update-resource-font "font.mono.spec")
          (--map (format "%s-14" it)
            '("Go Mono"
               "Menlo"
               "Source Code Pro"
               "Noto Sans Mono"
               "Lucida Console"
               "Dejavu Sans Mono")))
        (-first (-partial 'ns/update-resource-font "font.variable.spec")
          (--map (format "%s-14" it)
            '("Charter"
               "Noto Serif"
               "Lucida Console"
               "Dejavu Sans"
               "Menlo"))))))

  (ns/refresh-resources))

(ns/defconfig rice-integrations
  (defun ns/make-border-color (c)
    (--> c
      (ct-iterate it 'ct-pastel
        (lambda (c)
          (> (ct-distance it c) 20)))
      (ct-iterate it 'ct-edit-lab-l-inc
        (lambda (c) (ct-is-light-p c 75)))))

  (defun ns/set-btag-colors ()
    (->> (if (-contains-p '(myron-kobo myron-grayscale) (first custom-enabled-themes))
           (list "#c6007f" "#007c00" "#0065c8" "#6a6d6e") ; from mcfay
           (-map 'myron-get (list :primary :strings :assumed :faded)))
      (-map 'ns/make-border-color)
      (--map (substring it 1))
      (-map-indexed (lambda (i c) (sh (format "btags set ^%s color %s" (+ 1 i) c)))))
    nil)

  (ns/bind "iq" (fn!! insert-qb-region (sh "qb_userscript paste_selected")))
  (ns/bind "it"
    (fn!! insert-theme-key
      (->> (sh "theme -k")
        (s-split "\n")
        (ns/pick)
        (insert))))

  ;; used in window move scripts
  (defalias 'evil-window-north 'evil-window-up)
  (defalias 'evil-window-south 'evil-window-down)
  (defalias 'evil-window-east 'evil-window-right)
  (defalias 'evil-window-west 'evil-window-left))

(ns/defconfig macos-integrations
  (defun! ns/toggle-music-play ()
    (sh (format "macos-vol setvol %s" ns/macos-vol)))
  (defun! ns/toggle-music-pause ()
    (setq ns/macos-vol (sh "macos-vol get"))
    (sh "macos-vol setvol 0"))

  ;; adding the (t . emacs) so we don't open in textedit and stuff when using ns/follow
  (setq org-file-apps
    '((auto-mode . emacs)
       (directory . emacs)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'" . default)
       (t . emacs)))

  (when (string= (which "ls") "/bin/ls")
    (setq dired-listing-switches "-al")) ; default

  (when (which "/run/current-system/sw/bin/bash")
    (setq explicit-shell-file-name "/run/current-system/sw/bin/bash")))

(ns/defconfig go (ns/use go-mode))
(ns/defconfig pdf (ns/use pdf-tools))

(ns/defconfig minor-langs
  ;; pulling in these modes for syntax highlighting basically
  ;; they get grouped in a defconfig b/c minor/stable
  (ns/use nix-mode)
  (ns/use powershell)
  (ns/use terraform-mode)
  (ns/use yaml-mode)
  (ns/use ahk-mode) ; autohotkey
  (ns/use dockerfile-mode))

;; big bois
;; having them listed like this gives ns/jump-config something to search for
(ns/defconfig blog        (shut-up-load (~e "lisp/trees/blog.el")))
(ns/defconfig doomline    (shut-up-load (~e "lisp/trees/doomline.el")))
(ns/defconfig editing     (shut-up-load (~e "lisp/trees/editing.el")))
(ns/defconfig evil        (shut-up-load (~e "lisp/trees/evil.el")))
(ns/defconfig follow-dwim (shut-up-load (~e "lisp/trees/follow.el")))
(ns/defconfig git         (shut-up-load (~e "lisp/trees/git.el")))
(ns/defconfig interface   (shut-up-load (~e "lisp/trees/interface.el")))
(ns/defconfig irc         (shut-up-load (~e "lisp/trees/irc.el")))
(ns/defconfig minibuffer         (shut-up-load (~e "lisp/trees/minibuffer.el")))
(ns/defconfig buffers-and-windows         (shut-up-load (~e "lisp/trees/buffers-and-windows.el")))
(ns/defconfig dired       (shut-up-load (~e "lisp/trees/dired.el")))
(ns/defconfig org         (shut-up-load (~e "lisp/trees/org.el")))
(ns/defconfig org-capture (shut-up-load (~e "lisp/trees/org-capture.el")))
(ns/defconfig org-pim     (shut-up-load (~e "lisp/trees/org-pim.el")))
(ns/defconfig org-pim-export     (shut-up-load (~e "lisp/trees/org-pim-export.el")))

(ns/defconfig sanity      (shut-up-load (~e "lisp/trees/sanity.el")))
(ns/defconfig shell       (shut-up-load (~e "lisp/trees/shell.el")))
(ns/defconfig staging     (shut-up-load (~e "lisp/trees/staging.el")))
(ns/defconfig style       (shut-up-load (~e "lisp/trees/style.el")))
(ns/defconfig util        (shut-up-load (~e "lisp/trees/util.el")))

(provide 'forest)

;;; forest.el ends here
