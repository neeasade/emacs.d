;; evil-collection
(setq evil-want-keybinding nil)

(general-evil-setup t)

(use-package evil
  ;; evil-collection
  :init (setq evil-want-integration nil)
  :config (evil-mode 1)
  (general-nmap "N" 'evil-join))

;; disable: some of the binds get in the way of our colemak remappings.
(use-package evil-collection :config
  (defun ns/nek-rotation (_mode mode-keymaps &rest _rest)
    (evil-collection-translate-key 'normal mode-keymaps
      "n" "j"
      "e" "k"
      "j" "e"
      "k" "n"
      "K" "N"
      ))
  (add-hook 'evil-collection-setup-hook #'ns/nek-rotation)

  (evil-collection-init))

(defun ns/zz-scroll (&rest optional)
  (let* ((scrollcount (/ (window-total-size) 7))
          (halfheight (/ (window-total-size) 2))
          (scrollcheck (- halfheight scrollcount)))
    (if (> (line-number-at-pos) scrollcheck)
      (evil-scroll-line-down scrollcount)
      )))

(advice-add #'evil-scroll-line-to-center :after #'ns/zz-scroll)

(advice-add 'evil-search-next :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

(advice-add 'evil-search-previous :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

;; for reference, alteratively tried:
;; https://github.com/noctuid/general.el#mapping-under-non-prefix-keys
;; but it's very laggy/intensive by comparison (measured in the profiler)
(setq-default evil-escape-key-sequence "tn")
(use-package evil-escape :config (evil-escape-mode))

(defun set-in-evil-states (key def maps)
  (while maps
    (define-key (pop maps) key def)))

(defun set-in-navigation-evil-states (key def)
  (set-in-evil-states key def (list evil-motion-state-map
                                evil-normal-state-map
                                evil-visual-state-map)))

(define-key evil-motion-state-map "k" 'evil-search-next)
(define-key evil-motion-state-map "K" 'evil-search-previous)
(set-in-navigation-evil-states "n" 'evil-next-line)
(set-in-navigation-evil-states "e" 'evil-previous-line)

(use-package evil-lion
  :config
  (evil-define-key 'normal prog-mode-map
    (kbd "g l") 'evil-lion-left
    (kbd "g L") 'evil-lion-right)

  (evil-define-key 'visual prog-mode-map
    (kbd "g l") 'evil-lion-left
    (kbd "g L") 'evil-lion-right))

(use-package evil-commentary :config (evil-commentary-mode))
(use-package evil-anzu :config (setq anzu-cons-mode-line-p nil)) ;; displays current match and total matches.
(use-package evil-matchit :config (global-evil-matchit-mode 1))

(ns/use-package evil-numbers "janpath/evil-numbers"
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; this is nice, but I don't use marks often.
;; (use-package evil-fringe-mark
;;   :config (setq evil-fringe-mark-show-special nil)
;;   (global-evil-fringe-mark-mode t))

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
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-repeat-scope 'whole-line)
  (setq evil-snipe-spillover-scope 'whole-line)
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

;; persist marks
(add-to-list 'desktop-locals-to-save 'evil-markers-alist)

;; match qutebrowser fwd back
(general-nmap
  "H" 'previous-buffer
  "L" 'next-buffer)

(defun! ns/should-skip (buffername)
  (or
    ;; (member buffername '("scratch.el"))
    (s-starts-with? "*" buffername)
    (s-starts-with? "magit" buffername))
  )

(defun! ns/maybe-next ()
  (when (ns/should-skip (buffer-name))
    (let ((temp (window-next-buffers)))
      (next-buffer)
      (set-window-next-buffers nil temp))))

(defun! ns/maybe-prev ()
  (when (ns/should-skip (buffer-name))
    (let ((temp (window-prev-buffers)))
      (previous-buffer)
      (set-window-prev-buffers nil temp))))

(advice-add #'next-buffer :after #'ns/maybe-next)
(advice-add #'previous-buffer :after #'ns/maybe-prev)

(general-nmap
  "]s" 'flyspell-goto-next-error
  "[b" 'evil-prev-buffer
  "]b" 'evil-next-buffer
  )

(general-nmap "s" 'avy-goto-char-timer)

;; break a bad habit by nop'ing :b
;; (evil-ex-define-cmd "b" nil)
