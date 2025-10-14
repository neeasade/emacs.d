;; -*- lexical-binding: t; -*-

;; this happens here to appease evil-collection
(ns/use magit)

;; evil-collection
(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(setq evil-collection-outline-bind-tab-p t)

(general-evil-setup t)

(ns/use evil
  (setq evil-ex-visual-char-range t) ; make search/replace in visual blocks exact, rather than lines
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  (general-nmap "N" 'evil-join)

  ;; we are redefining here to undo this commit:
  ;; https://github.com/emacs-evil/evil/commit/0ad84c52169068021ec3372bf52503631f2261de
  ;; which breaks evil-escape: https://github.com/emacsorphanage/evil-escape/issues/7
  (evil-define-command evil-execute-macro (count macro)
    "Execute keyboard macro MACRO, COUNT times.
When called with a non-numerical prefix \
\(such as \\[universal-argument]),
COUNT is infinite. MACRO is read from a register
when called interactively."
    :keep-visual t
    :suppress-operator t
    :repeat evil-repeat-execute-macro
    (interactive
      (let (count macro register)
        (setq count (cond ((null current-prefix-arg) 1)
                      ((numberp current-prefix-arg) current-prefix-arg)
                      (t 0))
          register (or evil-this-register (read-char)))
        (cond
          ((or (eq register ?:)
             (and (eq register ?@) (eq evil-last-register ?:)))
            (setq macro #'evil-ex-repeat
              evil-last-register ?:))
          ((eq register ?@)
            (unless evil-last-register
              (user-error "No previously executed keyboard macro."))
            (setq macro (evil-get-register evil-last-register t)))
          (t
            (setq macro (evil-get-register register t)
              evil-last-register register)))
        (list count macro)))
    (cond
      ((functionp macro)
        (evil-repeat-abort)
        (if (zerop count)
          (while t (funcall macro))
          (dotimes (_ (or count 1)) (funcall macro))))
      ((or (and (not (stringp macro))
             (not (vectorp macro)))
         (member macro '("" [])))
        ;; allow references to currently empty registers
        ;; when defining macro
        (unless evil-this-macro (user-error "No previous macro")))
      (t
        (condition-case err
          (evil-with-single-undo
            (combine-after-change-calls
              (execute-kbd-macro macro count)
              (setq this-command 'evil-execute-macro))) ; For repeatability
          ;; enter Normal state if the macro fails
          (error
            (evil-normal-state)
            (signal (car err) (cdr err))))))))

(ns/use evil-collection

  (setq ns/evil-collection-keys
    `(;; jekn
       "e" "k"
       "k" "n"
       "n" "j"
       "j" "e"

       "E" "K"
       "K" "N"
       "N" "J"
       "J" "E"

       ,(kbd "C-e") ,(kbd "C-k")
       ,(kbd "C-k") ,(kbd "C-n")
       ,(kbd "C-n") ,(kbd "C-j")
       ,(kbd "C-j") ,(kbd "C-e")))

  (defun ns/nek-rotation (_mode mode-keymaps &rest _rest)
    (apply 'evil-collection-translate-key 'visual
      '(magit-status-mode-map magit-log-mode-map)
      ns/evil-collection-keys)

    (apply 'evil-collection-translate-key 'normal
      (->> mode-keymaps
        (--remove (eq it 'magit-status-mode-map))
        (--remove (eq it 'magit-log-mode-map)))
      ns/evil-collection-keys))

  (add-hook 'evil-collection-setup-hook #'ns/nek-rotation)
  (evil-collection-init))

(defun ns/zz-scroll (&rest _)
  (when (and (not (-contains-p '(circe-channel-mode circe-query-mode shell-mode) major-mode)))
    ;; lift the gaze a little
    (when (> (line-number-at-pos) 6)
      (condition-case nil
        (scroll-up 6)
        (error nil)))))

;; (defun ns/zz-scroll (&rest _))

(advice-add #'recenter :after #'ns/zz-scroll)

;; for reference, alteratively tried:
;; https://github.com/noctuid/general.el#mapping-under-non-prefix-keys
;; but it's very laggy/intensive by comparison (measured in the profiler)
(setq-default evil-escape-key-sequence "tn")

;; note: https://github.com/emacs-evil/evil/commit/0ad84c52169068021ec3372bf52503631f2261de
;; breaks tn in macro use, you removed the (let (pre-command-hook post-command-hook)) in evil.el
(ns/use (evil-escape :host github :repo "emacsorphanage/evil-escape")
  (evil-escape-mode))

(defun set-in-evil-states (key def maps)
  (while maps
    (define-key (pop maps) key def)))

(defun set-in-navigation-evil-states (key def)
  (set-in-evil-states key def (list evil-motion-state-map
                                evil-normal-state-map
                                evil-visual-state-map)))

;; commenting out -- want to see if setting  search backend before  evil-collection fixes (edit: it does not)

;; note: these are associated with the evil-search search module, set above with
;; (evil-select-search-module 'evil-search-module 'evil-search)
;; with the isearch backend, can change the keybind here to evil-search-{next,previous}
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
    (kbd "g L") 'evil-lion-right)

  (evil-lion-mode))

(ns/use evil-commentary  (evil-commentary-mode))
(ns/use evil-anzu 
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode 1))

(ns/use evil-matchit (global-evil-matchit-mode 1))

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
  (evil-goggles-mode 0))

(ns/use evil-surround (global-evil-surround-mode 1))

(ns/use evil-embrace
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)
  (general-define-key
    :states 'visual
    ;; `evil-change' is not bound in `evil-visual-state-map' by default but
    ;; inherited from `evil-normal-state-map'
    ;; if you don't want "c" to be affected in visual state, you should add this
    "c" #'evil-change
    "d" #'evil-delete
    "s" #'embrace-add
    ))

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

(ns/persist evil-markers-alist)

;; match qutebrowser fwd back
(general-nmap
  "H" 'previous-buffer
  "L" 'next-buffer)

(defun ns/should-skip (&optional win buf _)
  (let ((buffername (buffer-name buf))
         (file-name (buffer-file-name buf)))
    (or
      (eq (current-buffer) buf)
      (s-starts-with? "*" buffername)
      (s-starts-with? " *" buffername)
      (s-ends-with? ".org_archive" file-name)
      (s-starts-with? "magit" buffername)
      (eq 'dired-mode (buffer-local-value 'major-mode buf)))))

(setq ; why does no one use this feature? it's so great
  switch-to-next-buffer-skip #'ns/should-skip
  switch-to-prev-buffer-skip #'ns/should-skip)

(ns/use avy
  ;; (setq avy-keys '(?a ?r ?s ?t ?g ?k ?n ?e ?i ?o))
  (setq avy-all-windows 'all-frames)

  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(general-nmap
  ;; "[s" 'flyspell-goto-prev-error ; not a thing
  "]s" 'flyspell-goto-next-error
  ;; "s" 'avy-goto-char-timer

  ;; todo: this has a conflicting feel with surround (visual jump around)
  "s" 'avy-goto-char-2
  )

;; break a bad habit by nop'ing :b - we have a mapping to "bb" elsewhere
;; update: habit broken, but occasionally still a legit need for this
;; (evil-ex-define-cmd "b" nil)

(ns/use better-jumper
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)

  ;; (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward)

  (setq-ns better-jumper
    context 'buffer           ; buffer or window
    new-window-behavior 'copy ; copy or empty - new window context
    use-evil-jump-advice t    ; any evil jumps from elsewhere will be synced
    use-savehist t            ; save when using 'buffer context
    ignored-file-patterns '("COMMIT_EDITMSG$") ; remove TAGS from the filter
    )

  (better-jumper-mode t)

  (add-hook 'better-jumper-post-jump-hook 'recenter))

(ns/use undo-tree
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

;; undo-tree seems to have a weird garbage collection thing going on
;; freezes emacs
;; (ns/use undo-fu (evil-set-undo-system 'undo-fu))
