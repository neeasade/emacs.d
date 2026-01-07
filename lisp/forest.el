;; -*- lexical-binding: t; -*-
;;; forest.el --- Take a walk through the parentheses.
;;; Commentary:
;;; 🌲🌲👺🌲🌲🌲
;;; Code:

(ns/defconfig elisp
  (ns/install-dashdoc "Emacs Lisp" 'emacs-lisp-mode-hook)
  ;; 'common-lisp-indent-function
  (defun! ns/byte-compile-and-jump-to-error ()
    (byte-compile-file (buffer-file-name))
    (with-current-buffer "*Compile-Log*"
      (goto-char (point-max))
      (compilation-previous-error 1)
      (compile-goto-error)))

  (ns/bind-leader-mode 'emacs-lisp "c" 'ns/byte-compile-and-jump-to-error)

  (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'load-file)

  (setq lisp-indent-function 'common-lisp-indent-function)

  (ns/use helpful
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key))

  (ns/use eros
    (setq eros-eval-result-duration 20)
    (eros-mode 1)

    (defun! ns/smart-elisp-eval ()
      (llet [result (if (use-region-p)
                      (eval-region (region-beginning) (region-end))
                      (if (s-blank-p (s-trim (thing-at-point 'line)))
                        (eros-eval-last-sexp nil)
                        (eros-eval-defun nil)))]
        (if (and (functionp result)
              (s-starts-with? "ns/conf-" (pr-str result)))
          (funcall result))))

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
      "E" 'eval-print-last-sexp))

  (ns/use package-lint)

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
      ;; corfu-auto-delay 0.1
      )

    (apply 'evil-collection-translate-key 'insert '(corfu-map) ns/evil-collection-keys)

    (global-corfu-mode t)

    (require 'corfu-popupinfo)
    (setq corfu-popupinfo-delay '(0.2 . 0.1))
    (corfu-popupinfo-mode t))


  ;; can be removed with emacs 31
  (when ns/term?
    (ns/use corfu-terminal (corfu-terminal-mode)))

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
  ;; todo: something like spc r to jack in, or open repl buffer if already connected
  (ns/use clojure-mode)
  (ns/use cider)

  ;; speed in clojure repl
  (setq cider-repl-use-clojure-font-lock nil)

  ;; (ns/use clj-refactor)

  (setq cider-eval-result-duration 20)

  (ns/inmap 'cider-repl-mode-map (kbd "C-e") 'cider-repl-previous-input)
  (ns/inmap 'cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)

  ;; https://docs.cider.mx/cider/caveats.html#_injecting_dependencies_and_leiningen_pedantic_abort_mode
  ;; (setq cider-inject-dependencies-at-jack-in nil)

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

  (ns/file-mode "bb" 'clojure-mode)
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
  (add-to-list 'interpreter-mode-alist '("joker" . clojure-mode))

  (when (which "joker")
    (ns/use flycheck-joker  (require 'flycheck-joker)))

  (when (which "clj-kondo")
    (ns/use flycheck-clj-kondo))

  (defun! ns/babashka-default-connect ()
    ;; todo: indicate a repl is already open here?
    (let ((proc 'ns/bb-server-process))
      (when-not (and (boundp proc)
                  (eq 'run (process-status (symbol-value proc))))
        (set proc (start-process "bb-repl" "*bb-nrepl-server*" "bb" "--nrepl-server")))
      (sit-for 0.3))                    ; racey
    (cider-connect-clj '(:host "localhost" :port 1667)))

  ;; for scittle nrepl https://github.com/babashka/scittle/tree/main/doc/nrepl
  (cider-register-cljs-repl-type 'sci-js "(+ 1 2 3)")

  (defun mm/cider-connected-hook ()
    (when (eq 'sci-js cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))

  (add-hook 'cider-connected-hook #'mm/cider-connected-hook))

(ns/defconfig projectile
  ;; todo: try to replace this with builtin?
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

  (defun f-img? (f)
    ;; this was faster than shelling out to mimetype etc
    (when-let (ext (f-ext f))
      (-contains-p '("jpeg" "png" "jpg" "gif")
        (downcase ext))))

  (defun ns/jump-file-candidates (&rest wants)
    (llet (sources
            (list
              ;; XXX these keys are used in dmenu_switcher
              :buffers-without-files (fn (->> (buffer-list)
                                           (--filter (not (buffer-file-name it)))
                                           (--remove (eq 'dired-mode (buffer-local-value 'major-mode it)))
                                           (-map 'buffer-name)
                                           (-remove (-partial 's-starts-with-p "*spawn-shell-"))
                                           ;; there appear to be "special buffers" in the buffers-without-files set
                                           ;; using this hack to get the "right" special buffers
                                           ;; (derived from the completion used for evil's :b)
                                           (-intersection (internal-complete-buffer "" nil t))))

              :buffers-with-files (fn (->> (buffer-list)
                                        (-keep 'buffer-file-name)))
              :project-files (fn (ns/all-project-files (list default-directory)))
              :project-files-all (fn (ns/all-project-files (list default-directory)))
              ;; (ns/get-project-files (current-file))
              ;; (if ns/enable-linux-p (ns/all-project-files open-buffers) (ns/get-project-files (current-file)))

              :recentf (fn recentf-list)))

      (llet [selected (or wants '(:recentf :project-files :buffers-with-files))
              results (->> selected
                        (--mapcat (funcall (plist-get sources it)))
                        (-uniq)
                        (-remove 'f-img?)
                        (--remove (s-ends-with? "org_archive" (f-filename it)))
                        ;; todo fix this later - strips all results from buffers-without-files
                        ;; (--filter
                        ;;   ;; if a file is not remote, ensure it exists
                        ;;   ;; (f-exists-p is slow on remote files)
                        ;;   (or (file-remote-p it)
                        ;;     (f-exists-p it)))
                        )]
        results)))

  (ns/bind
    ;; todo: comething to consider: mixing in org headings here
    "ne" (fn!! surf-files (ns/find-files "file" (ns/jump-file-candidates)))
    "nE" (fn!! surf-project-files (ns/find-files "file" (ns/jump-file-candidates :project-files)))))

(fn!! surf-files-cwd ()
  (ns/pick (sh "rg" "--files" default-directory)))

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

  (add-hook 'markdown-mode-hook 'markdown-toggle-fontify-code-blocks-natively))

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
       ;; bit aggressive
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
  (add-to-list 'interpreter-mode-alist '("elispp" . emacs-lisp-mode))

  ;; helper for unpacking args provided by elisp script as shebang
  ;; use: (ns/let-script-args (named named2) body)
  (defmacro ns/let-script-args (args &rest content)
    `(let (,@(-map
               (fn (list (nth <> args)
                     (nth <> *command-line-args*)))
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

  (defalias 'ns/text-to-stud 'studlify-region)
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
  (ns/use ox-asciidoc))



(ns/defconfig rice-integrations
  (defun ns/make-border-color (c)
    (-> c
      (ct-aiterate 'ct-pastel (> (ct-distance C0 C) 20))
      (ct-aiterate 'ct-edit-lab-l-inc (ct-is-light-p C 75))))

  (defun ns/set-btag-colors ()
    (->> (if (-contains-p '(myron-kobo myron-grayscale) (first custom-enabled-themes))
           (list "#c6007f" "#007c00" "#0065c8" "#6a6d6e") ; from mcfay
           (-map 'myron-get (list :primary :strings :assumed :faded)))
      (-map 'ns/make-border-color)
      (--map (substring it 1))
      (-map-indexed (lambda (i c) (sh (format "btags set ^%s color %s" (+ 1 i) c)))))
    nil)

  ;; "insert region"
  (ns/bind "ir" (fn!! insert-qb-region (sh "qb_userscript paste_selected")))

  (ns/bind "it" (fn!! insert-theme-key (insert (ns/pick (s-lines (sh "theme -k"))))))

  ;; used in window move scripts
  (defalias 'evil-window-north 'evil-window-up)
  (defalias 'evil-window-south 'evil-window-down)
  (defalias 'evil-window-east 'evil-window-right)
  (defalias 'evil-window-west 'evil-window-left)

  ;; if these keys are reaching emacs, use em
  (ns/inmap 'general-override-mode-map
    (kbd "M-n") #'evil-window-down
    (kbd "M-e") #'evil-window-up
    (kbd "M-l") #'evil-window-right
    (kbd "M-h") #'evil-window-left)

  (ns/inmap 'general-override-mode-map
    (kbd "M-i") (fn!! switch-one (tab-bar-select-tab 1))
    (kbd "M-o") (fn!! switch-two (tab-bar-select-tab 2))
    (kbd "M-u") (fn!! switch-three (tab-bar-select-tab 3))
    (kbd "M-1") (fn!! switch-one (tab-bar-select-tab 1))
    (kbd "M-2") (fn!! switch-two (tab-bar-select-tab 2))
    (kbd "M-3") (fn!! switch-three (tab-bar-select-tab 3)))

  ;; used in box
  (defun ns/string-width (s)
    ;; string-width lies?, so we divide
    (/ (string-pixel-width s)
      (string-pixel-width "═")))

  ;; dunst_logger
  (defun ns/message-buffer (buffer-name message)
    (let ((messages-buffer-name buffer-name)
           (inhibit-message t))           ; don't show in modeline
      (message message))))

(ns/defconfig macos-integrations
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

(ns/defconfig llm
  (ns/use (shell-maker :type git :host github :repo "xenodium/shell-maker" :files ("*.el"))
    (defun shell-maker-welcome-message (_)))

  (ns/use (chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
    (setq chatgpt-shell-anthropic-key (pass "anthropic api key"))
    (setq chatgpt-shell-google-key (pass "gemini_api_key"))
    (chatgpt-shell-google-load-models)
    (chatgpt-shell-ollama-load-models)
    (setq-default chatgpt-shell-model-version "claude-3-7-sonnet-latest")

    (setq-default chatgpt-shell-system-prompt 2) ; the "programming" prompt
    (ns/bind "nf"
      (fn!! find-chatgpt-shell
        (if-let (b (first (ns/buffers-by-mode 'chatgpt-shell-mode)))
          (switch-to-buffer b)
          (chatgpt-shell)))))

  (defun ns/chatgpt-shell-save-transcript ()
    (when (eq major-mode 'chatgpt-shell-mode)
      (llet [dest (~ "logs" "llm" chatgpt-shell-model-version (format-time-string "%F-%T-transcript.md"))
              frontmatter (ns/str "# a chat with " chatgpt-shell-model-version " saved at " (format-time-string "%F-%T"))]
        (f-mkdir-full-path (f-dirname dest))

        (setq-local shell-maker--file "/dev/shm/llm.txt")
        (chatgpt-shell-save-session-transcript)
        (->> (slurp "/dev/shm/llm.txt")
          (s-replace-regexp (chatgpt-shell--prompt-regexp) "## User: ")
          ;; the duplication is on purpose for code block handling
          (s-replace "<shell-maker-end-of-prompt>\n`" "## LLM: \n`")
          (s-replace "<shell-maker-end-of-prompt>\n" "## LLM: ")
          (s-replace "\n\n## User: " "\n## User: ")
          (s-replace "## User: clear" "")
          (ns/str frontmatter "\n\n")
          (spit dest)))))

  (advice-add 'comint-clear-buffer :before 'ns/chatgpt-shell-save-transcript)

  ;; todo: check this var later for custom prompt messing
  ;; chatgpt-shell-system-prompts

  ;; considering:
  ;; comint is shell, chatgpt-shell
  ;; (ns/face '(comint-highlight-input comint-highlight-prompt)
  ;;   :foreground (myron-get :faded)
  ;;   :background nil)
  )

(ns/defconfig whisper
  (ns/use (whisper :host github :repo "natrys/whisper.el")
    (setq-ns whisper
      ;; nb: smaller = faster
      ;; https://github.com/ggml-org/whisper.cpp#memory-usage
      model "base"
      ;; model "small"
      language "en"
      ;; use-threads (/ (num-processors) 2)
      ;; server-mode 'local
      server-mode nil

      insert-text-at-point t            ; nil for preview window
      server-port 8080
      -temp-file "/dev/shm/emacs-whisper.wav"))

  ;; talk-to-bot button via chatgpt shell
  (defun! ns/send-whisper-message ()
    (when (eq major-mode 'chatgpt-shell-mode)
      (shell-maker-submit)))

  (add-hook 'whisper-after-insert-hook #'ns/send-whisper-message)

  (defun! ns/whisper-llm-chat ()
    ;; (switch-to-buffer (first (ns/buffers-by-mode 'chatgpt-shell-mode)))
    (save-excursion
      (with-current-buffer (first (ns/buffers-by-mode 'chatgpt-shell-mode))
        (chatgpt-shell-interrupt t)
        (goto-char (point-max))
        (whisper-run))))


  ;; insert at point
  (ns/bind "r" 'whisper-run))

(ns/defconfig minor-langs
  ;; pulling in these modes for syntax highlighting basically
  ;; they get grouped in a defconfig b/c minor/stable
  (ns/use nix-mode)
  (ns/use powershell)
  (ns/use terraform-mode)
  (ns/use yaml-mode)
  (ns/use ahk-mode)                     ; autohotkey
  (ns/use dockerfile-mode))

(ns/defconfig blog-syntax
  ;; modes for src block syntax, only used in blog ssg
  (ns/use dockerfile-mode)
  (ns/use restclient)
  (ns/use nix-mode)
  (ns/use clojure-mode))

(ns/defconfig frog-jump
  (ns/use frog-jump-buffer)

  (ns/bind "u" 'frog-jump-buffer)
  (ns/bind "U" 'frog-jump-buffer-other-window)

  (setq frog-jump-buffer-default-filter
    'frog-jump-buffer-filter-file-buffers
    ;; 'frog-jump-buffer-filter-same-project
    ;; 'frog-jump-buffer-filter-recentf
    ;; 'ns/jump-file-candidates
    )

  ;; (setq frog-menu-avy-padding)
  (setq frog-menu-avy-keys '(?a ?r ?s ?t ?g ?k ?n ?e ?i ?o))
  (setq frog-jump-buffer-max-buffers (length frog-menu-avy-keys))
  (setq frog-jump-buffer-include-current-buffer nil)

  (when (fboundp 'myron-get)
    (setq frog-jump-buffer-posframe-parameters
      `(;; cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html
         (background-color . ,(myron-get :background :weak))

         (foreground-color . ,(myron-get :primary :weak))
         (left . 0.0)
         )))

  ;;   ;; (set-face-attribute 'avy-lead-face nil :box (myron-get :faded))
  ;;   (set-face-attribute 'avy-lead-face nil :box nil))

  (defun frog-menu-type ()
    "Return variable `frog-menu-type' to use."
    (if ns/term?
      'avy-side-window
      'avy-posframe))
  )

(ns/defconfig kkp
  (ns/use kkp
    (defun kkp-translate-aliased-keys (original-fun &rest args)
      "Advise kkp--translate-terminal-input to translate keys like C-i (hard
aliased to TAB by emacs) into novel unambiguous symbols, bindable with
e.g. (define-key (kbd (\"<C-i>\")) ...)."
      (let ((terminal-input (car args)))
        ;; The KKP escape sequence for e.g. C-i is "\e[105;5u".
        ;; The input to the translator function is the part *after* "\e[".
        (cond ((equal terminal-input '(?1 ?0 ?5 ?\; ?5 ?u)) [C-i])
          ((equal terminal-input '(?1 ?0 ?9 ?\; ?5 ?u)) [C-m])
          (t (apply original-fun args)))))

    ;; kkp.el maps the kkp escape prefix to a function which pops the rest of the escape sequence
    ;; chars directly from read-event and feeds them to this function, partially sidestepping emacs'
    ;; usual keymap system. Advise that function.
    (advice-add 'kkp--translate-terminal-input :around #'kkp-translate-aliased-keys)

    (global-kkp-mode +1)))

;; big bois
;; having them listed like this gives ns/jump-config something to search for
(ns/defconfig blog                (shut-up-load (~e "lisp/trees/blog.el")))
(ns/defconfig doomline            (shut-up-load (~e "lisp/trees/doomline.el")))
(ns/defconfig editing             (shut-up-load (~e "lisp/trees/editing.el")))
(ns/defconfig evil                (shut-up-load (~e "lisp/trees/evil.el")))
(ns/defconfig follow-dwim         (shut-up-load (~e "lisp/trees/follow.el")))
(ns/defconfig git                 (shut-up-load (~e "lisp/trees/git.el")))
(ns/defconfig interface           (shut-up-load (~e "lisp/trees/interface.el")))
(ns/defconfig minibuffer          (shut-up-load (~e "lisp/trees/minibuffer.el")))
(ns/defconfig buffers-and-windows (shut-up-load (~e "lisp/trees/buffers-and-windows.el")))
(ns/defconfig dired               (shut-up-load (~e "lisp/trees/dired.el")))
(ns/defconfig org                 (shut-up-load (~e "lisp/trees/org.el")))
(ns/defconfig org-capture         (shut-up-load (~e "lisp/trees/org-capture.el")))
(ns/defconfig org-pim             (shut-up-load (~e "lisp/trees/org-pim.el")))
(ns/defconfig org-pim-export      (shut-up-load (~e "lisp/trees/org-pim-export.el")))

(ns/defconfig sanity      (shut-up-load (~e "lisp/trees/sanity.el")))
(ns/defconfig shell       (shut-up-load (~e "lisp/trees/shell.el")))
(ns/defconfig staging     (shut-up-load (~e "lisp/trees/staging.el")))
(ns/defconfig style       (shut-up-load (~e "lisp/trees/style.el")))
(ns/defconfig util        (shut-up-load (~e "lisp/trees/util.el")))

