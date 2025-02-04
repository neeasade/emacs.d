;; config the minad stack

(global-set-key (kbd "C-e") 'previous-line)

(defun ns/find-files (prompt files)
  "Light wrapper around consult read with file set (history based on prompt)"
  (find-file
    (consult--read files
      :prompt (format "%s: " prompt)
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history)))

(defun ns/pick (one &optional two)
  "Pick something from a list. accepts (prompt candidates) or (candidates)"
  (llet [(prompt candidates) (if two
                               (list (format "%s: " one) two)
                               (list "select: " one))]
    (completing-read prompt (-uniq candidates))))

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
    (setq vertico-count (round (/ (frame-height) 2))))

  (advice-add 'completing-read :before 'ns/set-vertico-count))

(ns/use marginalia (marginalia-mode t))

(ns/use consult
  (setq
    consult-async-input-throttle 0.04
    consult-async-input-debounce 0.02)

  (setq consult-ripgrep-args
    ;; add "--hidden"
    "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden"))

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

