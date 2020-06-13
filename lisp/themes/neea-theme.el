;; -*- lexical-binding: t; -*-
;; see file:../trees/colors.el

;; kinds of colors we want to create:
;; fg, bg
;; 2 faded fg levels (modeline, then comments)
;; 2 accents + fades on accents

;; my thinking right now is that we'll derive 'lightness' from the foreground color, then tilt
;; create accent colors based on directions in the lab space (note: maybe also provide a way to give
;; and accent color)

;; additionally if we steal accent colors from somewhere we can snipe the hue and then use that as
;; our base with foreground (new color is HSL, accent H, accent S, foreground L)

(require 'base16-theme)
(require 'color)

(setq ns/theme-white-point
  ;; note: ICC is https://en.wikipedia.org/wiki/ICC_profile
  ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
  ;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
  color-d65-xyz ;; | Noon Daylight: Television, sRGB color space (standard assumption)
  ;; color-d75-xyz ;; | North sky Daylight
  )

;; saving this one as a 'safe' fallback
(defun ns/color-derive-accent-safe (origin mod)
  (ns/color-lch-transform origin
    (lambda (L C H)
      (list
        (+ L mod)
        ;; (- C (/ mod 2))
        (- C mod)
        H))))

(defun ns/color-derive-accent (origin mod)
  (ns/color-iterate origin
    (lambda (c)
      (ns/color-lch-transform c
        (lambda (L C H)
          (list
            (+ L 2)
            (- C 2)
            ;; (+ L mod)
            ;; (- C (/ mod 2))
            ;; (- C mod)
            H))))

    (lambda (c)
      (> (ns/color-name-distance
           ;; todo: consider passing hue correction here
           origin c)
        (* 1.5  mod)
        ;; mod
        ))))

(let*
  ;; from the lab light theme
  ((foreground  "#5A5E65")
    (background  "#F2F5F8")
    (accent1
      (ns/color-lab-transform foreground
        (lambda (L A B)
          (list
            (+ L 5)
            ;; going towards green, away from red
            (- A (* 0.5 (+ A 100)))
            ;; going towards blue, away from yello
            (- B (* 0.7 (+ B 100)))
            ))))

    (accent2
      (ns/color-lab-transform foreground
        (lambda (L A B)
          (list
            ;; (+ L 10)
            (+ L 5)
            ;; going towards green, away from red
            (- A (* 0.6 (+ A 100)))
            ;; going towards yellow, away from blue
            (+ B (* 0.6 (- 200 (+ B 100))))
            ))))

    ;; todo: revisit numbers here
    (accent1_ (ns/color-derive-accent accent1 10))
    (accent1__ (ns/color-derive-accent accent1_ 10))

    (accent2_ (ns/color-derive-accent accent2 10))
    (accent2__ (ns/color-derive-accent accent2_ 10))

    (foreground_ (ns/color-lab-lighten foreground 20))
    (foreground__ (ns/color-lab-lighten foreground_ 6))
    )

  (setq ns/theme
    (ht
      (:foreground foreground)
      (:foreground_ foreground_)
      (:foreground__ foreground__)

      (:background background)

      (:accent1 accent1)
      (:accent1_ accent1_)
      (:accent1__ accent1__)

      (:accent2 accent2)
      (:accent2_ accent2_)

      (:accent2__
        ;; (ns/color-lab-lighten accent2__ -10)
        accent2__
        )))

  ;; note: here is the place for lighting and gamma correction functions
  ;; todo: investigate the different color blending options for transforms here

  ;; take the chroma of everything up a bit (away from "gray")
  (setq ns/theme
    (ht-transform ns/theme
      (lambda (c)
        (ns/color-lch-transform c
          (lambda (L C H) (list L (+ C 5) H))))))

  ;; (ns/color-tint-with-light "#cccccc" ns/theme-white-point ns/theme-white-point)

  ;; (setq ns/theme
  ;;   (ht-transform ns/theme
  ;;     (lambda (c)
  ;;       ;; (ns/color-lab-lighten)
  ;;       (ns/color-tint-with-light
  ;;         c
  ;;         ns/theme-white-point
  ;;         ;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space (standard assumption)
  ;;         ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
  ;;         color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
  ;;         ;; color-d75-xyz ;; | North sky Daylight
  ;;         ))))

  )

;; todo: this theme should also handle org outline levels colors explicitly
(deftheme neea)

(base16-theme-define 'neea
  (list
    ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
    ;; what the emacs base16 theme package follows.

    ;; guidelines location: http://chriskempson.com/projects/base16/
    ;; I've also noted some faces I care about

    :base00 (ht-get ns/theme :background) ;; Default Background

    ;; ivy-current-match background, isearch match background, inactive modeline background
    :base01 (color-darken-name (ht-get ns/theme :background) 7) ;; Lighter Background (Used for status bars)

    ;; font-comment-delimiter, region, active modeline background
    :base02 (ht-get ns/theme :accent1__) ;; Selection Background

    :base03 (ht-get ns/theme :foreground__) ;; Comments, Invisibles, Line Highlighting
    :base04 (ht-get ns/theme :foreground_)  ;; Dark Foreground (Used for status bars)

    :base05 (ht-get ns/theme :foreground) ;; Default Foreground, Caret, Delimiters, Operators

    :base06 (ht-get ns/theme :foreground_) ;; Light Foreground (Not often used)

    :base07 (ht-get ns/theme :foreground_) ;; Light Background (Not often used)

    ;; org-todo, variables
    :base08 (ht-get ns/theme :accent2) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

    ;; ivy-current-match foreground
    :base09 (ht-get ns/theme :foreground) ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

    :base0A (ht-get ns/theme :foreground__) ;; Classes, Markup Bold, Search Text Background

    ;; font-lock-string-face
    ;; this should maybe be accent1__
    :base0B (ht-get ns/theme :accent2__) ;; Strings, Inherited Class, Markup Code, Diff Inserted

    :base0C (ht-get ns/theme :foreground_) ;; Support, Regular Expressions, Escape Characters, Markup Quotes

    ;; prompt, function-name, search match foreground
    :base0D (ht-get ns/theme :accent1) ;; Functions, Methods, Attribute IDs, Headings

    ;; keyword-face, org-date
    :base0E (ht-get ns/theme :accent1_) ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

    :base0F (ht-get ns/theme :foreground) ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
    ))

(provide-theme 'neea)

(provide 'neea-theme)
;;; neea-theme.el ends here
