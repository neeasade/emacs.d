;; -*- lexical-binding: t; -*-

;; TODO make a toggle for this
;; evil-ex-visual-char-range

;; this happens here to appease evil-collection
(ns/use magit  (require 'magit))

;; evil-collection
(setq evil-want-keybinding nil)

(general-evil-setup t)

(ns/use evil
  ;; evil-collection
  :init (setq evil-want-integration t)
   (evil-mode 1)
  (general-nmap "N" 'evil-join))

;; disable: some of the binds get in the way of our colemak remappings.
(ns/use evil-collection 
  (defun ns/nek-rotation (_mode mode-keymaps &rest _rest)
    (evil-collection-translate-key 'normal mode-keymaps
      "e" "k"
      "k" "n"
      "n" "j"
      "j" "e"

      "E" "K"
      "K" "N"
      "N" "J"
      "J" "E"
      )

    ;; todo: test diffing
    ;; I guess translate key doesn't work in control keybindings?
    (evil-collection-define-key 'motion 'diff-mode-map
      (kbd "C-n") 'diff-hunk-next
      (kbd "C-e") 'diff-hunk-prev)

    (evil-collection-define-key 'normal 'diff-mode-map
      (kbd "C-n") 'diff-hunk-next
      (kbd "C-e") 'diff-hunk-prev)
    )

  (add-hook 'evil-collection-setup-hook #'ns/nek-rotation)

  (evil-collection-init)
  (global-evil-collection-unimpaired-mode 0))

(defun ns/zz-scroll (&rest _)
  (when (not (-contains-p '(circe-channel-mode circe-query-mode) major-mode))
    (let* ((scrollcount (/ (window-total-size) 7))
            (halfheight (/ (window-total-size) 2))
            (scrollcheck (- halfheight scrollcount)))
      (when (> (line-number-at-pos) scrollcheck)
        ;; this is sometimes broken? always scrolling
        ;; (evil-scroll-line-down scrollcount)
        nil
        ))))

(advice-add #'recenter :after #'ns/zz-scroll)

;; for reference, alteratively tried:
;; https://github.com/noctuid/general.el#mapping-under-non-prefix-keys
;; but it's very laggy/intensive by comparison (measured in the profiler)
(setq-default evil-escape-key-sequence "tn")

(ns/use (evil-escape :host github :repo "hlissner/evil-escape")
   (evil-escape-mode))

(defun set-in-evil-states (key def maps)
  (while maps
    (define-key (pop maps) key def)))

(defun set-in-navigation-evil-states (key def)
  (set-in-evil-states key def (list evil-motion-state-map
                                evil-normal-state-map
                                evil-visual-state-map)))


;; with the isearch backend, can change the keybind here
(evil-select-search-module 'evil-search-module 'evil-search)
(define-key evil-motion-state-map "k" 'evil-ex-search-next)
(define-key evil-motion-state-map "K" 'evil-ex-search-previous)

(set-in-navigation-evil-states "n" 'evil-next-line)
(set-in-navigation-evil-states "e" 'evil-previous-line)

(ns/use evil-lion
  (evil-define-key 'normal prog-mode-map
    (kbd "g l") 'evil-lion-left
    (kbd "g L") 'evil-lion-right)

  (evil-define-key 'visual prog-mode-map
    (kbd "g l") 'evil-lion-left
    (kbd "g L") 'evil-lion-right))

(ns/use evil-commentary  (evil-commentary-mode))
(ns/use evil-anzu 
  (setq anzu-cons-mode-line-p nil) (global-anzu-mode 1))

(ns/use evil-matchit  (global-evil-matchit-mode 1))

(ns/use (evil-numbers :host github :repo "janpath/evil-numbers")
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; this is nice, but I don't use marks often.
;; (ns/use evil-fringe-mark
;;    (setq evil-fringe-mark-show-special nil)
;;   (global-evil-fringe-mark-mode t))

(ns/use evil-goggles
  (setq evil-goggles-duration 0.100)
  (setq evil-goggles-pulse t)
  ;; fun visual vim mode
  (evil-goggles-mode 0)
  )

(ns/use evil-surround  (global-evil-surround-mode 1))

(ns/use evil-embrace
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

(ns/use evil-snipe
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-repeat-scope 'whole-line)
  (setq evil-snipe-spillover-scope 'whole-line)
  (evil-snipe-override-mode +1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(ns/use evil-exchange
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
    (s-starts-with? "magit" buffername)
    (with-current-buffer (get-buffer buffername) (eq major-mode 'dired-mode))
    )
  )

;; (defun! ns/maybe-next ()
;;   (when (ns/should-skip (buffer-name))
;;     (next-buffer)))

;; (defun! ns/maybe-prev ()
;;   (when (ns/should-skip (buffer-name))
;;     (previous-buffer)))

;; fucking what -- why did I do the temp shit here
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
  ;; this is not a thing.
  ;; "[s" 'flyspell-goto-prev-error
  "]s" 'flyspell-goto-next-error
  "[b" 'evil-prev-buffer
  "]b" 'evil-next-buffer
  )

(general-nmap "s" 'avy-goto-char-timer)

;; break a bad habit by nop'ing :b
;; (evil-ex-define-cmd "b" nil)

(ns/use better-jumper
  ;; (with-eval-after-load 'evil-maps
  ;;   (define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
  ;;   (define-key evil-motion-state-map (kbd "<C-i>") 'evil-jump-forward))

  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

  (setq-ns better-jumper
    context 'buffer ;; buffer or window
    new-window-behavior 'copy ;; copy or empty - new window context
    use-evil-jump-advice t ;; any evil jumps from elsewhere will be synced
    use-savehist nil ;; save when using 'buffer context
    )

  (better-jumper-mode +1)

  (add-hook 'better-jumper-post-jump-hook 'recenter))

;; (ns/use undo-tree
;;   
;;   (require 'undo-tree)
;;   (global-undo-tree-mode)
;;   (evil-set-undo-system 'undo-tree))

;; undo-tree seems to have a weird garbage collection thing going on
;; freezes emacs
(ns/use undo-fu)
(evil-set-undo-system 'undo-fu)
