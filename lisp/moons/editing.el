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
  (lambda () (sh-electric-here-document-mode -1)))