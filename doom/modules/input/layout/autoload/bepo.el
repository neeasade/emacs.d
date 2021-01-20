;;; input/keymaps/autoload/bepo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ns/rotate-jekn (keymaps)
  (dolist (keymap keymaps)
    (evil-collection-translate-key nil keymap
      "e" "k"
      "k" "n"
      "n" "j"
      "j" "e"

      "E" "K"
      "K" "N"
      "N" "J"
      "J" "E"

      (kbd "C-e") (kbd "C-k")
      (kbd "C-k") (kbd "C-n")
      (kbd "C-n") (kbd "C-j")
      (kbd "C-j") (kbd "C-e")

      (kbd "C-M-e") (kbd "C-M-k")
      (kbd "C-M-k") (kbd "C-M-n")
      (kbd "C-M-n") (kbd "C-M-j")
      (kbd "C-M-j") (kbd "C-M-e")
      )))


;;;###autoload
(defun ns/rotate-evil (&optional cr-style)
  "Remap evil-{normal,operator,motion,...}-state-map
  to be more natural with Bépo keyboard layout.
See `doom-bepo-cr-rotation-style' for the meaning of CR-STYLE."
  (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-visual-state-map)
    "e" "k"
    "k" "n"
    "n" "j"
    "j" "e"

    "E" "K"
    "K" "N"
    "N" "J"
    "J" "E"
    )

  (evil-collection-translate-key nil '(evil-insert-state-map)
    ;; todo: shift keysets (C-S-*) ?
    (kbd "C-e") (kbd "C-k")
    (kbd "C-k") (kbd "C-n")
    (kbd "C-n") (kbd "C-j")
    (kbd "C-j") (kbd "C-e")
    )


  ;; <> as direct access
  ;; (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map)
  ;;   "«" "<"
  ;;   "»" ">")

  ;; " è replaces ^0 to go at BOL
  ;; (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map)
  ;;   "è" "^"
  ;;   "È" "0")

  ;; [W] -> [É]
  ;; [C-W] -> [W]
  ;; (evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-operator-state-map)
  ;;   "é" "w"
  ;;   "É" "W"
  ;;   "w" (kbd "C-w")
  ;;   "W" (kbd "C-w C-w"))
  )

;;;###autoload
(defun doom-bepo-rotate-collection-keymaps-h-builder (cr-style)
  "Build a hook that remaps evil-collection customizations to be more natural
  with Bépo keyboard layout, according to CR-STYLE (see `doom-bepo-cr-rotation-style')."
  (let* ((cr-style (or cr-style 'ergodis))
          (doom-bepo-hook (lambda (_mode mode-keymaps &rest _rest)
                            (dolist (keymap mode-keymaps)
                              (evil-collection-translate-key '(normal motion visual) keymap
                                "e" "k"
                                "k" "n"
                                "n" "j"
                                "j" "e"

                                "E" "K"
                                "K" "N"
                                "N" "J"
                                "J" "E")

                              (evil-collection-translate-key '(insert) keymap
                                ;; todo: shift keysets (C-S-*) ?
                                (kbd "C-e") (kbd "C-k")
                                (kbd "C-k") (kbd "C-n")
                                (kbd "C-n") (kbd "C-j")
                                (kbd "C-j") (kbd "C-e")
                                )


                              ;; <> en direct
                              ;; (evil-collection-translate-key '(normal motion visual) keymap
                              ;;   "«" "<"
                              ;;   "»" ">")

                              ;; è pour aller au début de ligne
                              ;; (evil-collection-translate-key '(normal motion visual) keymap
                              ;;   "è" "^"
                              ;;   "È" "0")

                              ;; [W] -> [É]
                              ;; [C-W] -> [W]
                              ;; (evil-collection-translate-key '(normal motion operator visual) keymap
                              ;;   "é" "w"
                              ;;   "É" "W"
                              ;;   "w" (kbd "C-w")
                              ;;   "W" (kbd "C-w C-w"))

                              ))))
    doom-bepo-hook))
