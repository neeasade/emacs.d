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
(ns/colors)

;; the definition of 'white' as displayed on the screen your viewing it on
;; picture you are a photographer, taking pictured in different lighting conditions -- sunlight and
;; incandescent lighting conditions are very different, for example.
;; well here, your monitor is like the photograph, and this point represents "white" on your screen.
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

(defun ns/color-derive-accent-left (origin mod)
  (ns/color-iterate origin
    (lambda (c)
      (-> c

        ;; (ns/color-lch-transform
        ;;   (lambda (L C H)
        ;;     (list L (* C 0.9) H)))
        (ns/color-pastel 0.9 1.3)
        )
      )

    (lambda (c)
      (> (ns/color-name-distance
           ;; todo: consider passing hue correction here
           origin c)
        mod
        ))))

(defun ns/color-derive-accent-right (origin mod)
  (ns/color-iterate origin
    (lambda (c)
      (-> c

        ;; (ns/color-lch-transform
        ;;   (lambda (L C H)
        ;;     (list L (* C 0.9) H)))
        (ns/color-pastel 0.9 1.1)
        ;; (ns/color-pastel 0.9 1.2)
        )
      )

    (lambda (c)
      (> (ns/color-name-distance
           ;; todo: consider passing hue correction here
           origin c)
        mod
        ))))

(defun ns/theme-enforce-contrast (c background foreground)
  (let* ((main-ratio (ns/color-contrast-ratio foreground background))
          (c-ratio (ns/color-contrast-ratio c background))
          ;; (bg-is-light (ns/color-is-light-p background))
          )
    (ns/color-iterate
      c
      (fn (color-darken-name <> 0.5))
      ;; (fn (> ratio (ns/color-contrast-ratio <> background)))
      (fn (>=
            ;; ratio
            (ns/color-contrast-ratio <> background)
            (* 0.75 main-ratio)
            ))
      )))

;; (let ((foreground  "#5A5E65")
;;        (background "#EEF0F3"))
;;   (ns/theme-enforce-contrast "#dddddd" background foreground))


(let*
  ;; fg bg from the lab light theme by MetroWind.
  ((foreground  "#5A5E65")
    (background "#EEF0F3")

    (accent1
      (ns/color-lab-transform foreground
        (lambda (L A B)
          (list
            (+ L 3)
            ;; L
            ;; going towards green, away from red
            ;; (- A (* 0.5 (+ A 100)))
            ;; to red
            (+ A (* 0.3 (- 200 (+ A 100))))
            ;; going towards blue, away from yellow
            (- B (* 0.9 (+ B 100)))
            ;; (+ A (* 0.3 (- 200 (+ A 100))))
            ))))

    (accent2
      (ns/color-lab-transform foreground
        (lambda (L A B)
          (list
            ;; (+ L 10)
            ;; (- L 2)
            (+ L 3)
            ;; going towards green, away from red
            (- A (* 0.9 (+ A 100)))
            ;; (+ A (* 0.8 (- 200 (+ A 100))))
            ;; (- A (* 0.0 (+ A 100)))
            ;; going towards yellow, away from blue
            ;; (- B (* 0.9 (+ B 100)))
            (+ B (* 0.4 (- 200 (+ B 100))))
            ))))

    ;; todo: revisit numbers here
    (accent1_ (ns/color-derive-accent-left accent1 8))

    (accent1__
      (->  accent1_
        (ns/color-derive-accent-left 4)
        (ns/color-lab-darken 10)
        (color-desaturate-name 10)
        ))


    (accent2_ (ns/color-derive-accent-right accent2 10))

    ;; the special care here is because accent2__ is for strings
    ;; and strings are everywhere
    (accent2__ (ns/color-derive-accent-right accent2_ 8))
    (accent2__ (ns/color-lab-lighten accent2__ 2))
    (accent2__ (color-desaturate-name accent2__ 10))

    ;; (foreground_
    ;;   (ns/color-iterate foreground
    ;;     (fn (ns/color-lab-lighten <> 1))
    ;;     (fn (> (ns/color-name-distance <> foreground) 10)))
    ;;   )

    ;; (foreground__
    ;;   (ns/color-iterate foreground_
    ;;     (fn (ns/color-lab-lighten <> 1))
    ;;     (fn (> (ns/color-name-distance <> foreground_) 10)))
    ;;   )

    (foreground_ (ns/color-derive-accent-right foreground 10))
    (foreground__ (ns/color-derive-accent-right foreground_ 4))
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
      (:accent2__ accent2__)))

  ;; note: here is the place for lighting and gamma correction functions
  ;; todo: investigate the different color blending options for transforms here

  ;; (setq ns/theme
  ;;   (ht-transform-kv ns/theme
  ;;     (lambda (k v)
  ;;       (if (s-starts-with-p ":accent" (prin1-to-string k))
  ;;         ;; (ns/theme-enforce-contrast v background foreground)
  ;;         (color-desaturate-name v 5)
  ;;         v))))

  ;; correlate this with screen brightness -- the lower you turn it you will want to turn this down
  ;; todo: for this to be accurate you must be sure of the initial color adjustments in sRBG
  ;; that implies gamma correction/measure in an enviroment controlled or similar to the one described at
  ;; https://en.wikipedia.org/wiki/SRGB

  ;; white point const meanings
  ;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space (standard assumption)
  ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
  ;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
  ;; color-d75-xyz ;; | North sky Daylight

  ;; when outside, or with really low brightness, try out these transforms:
  ;; (->>
  ;;   (fn (ns/color-tint-with-light <> ns/theme-white-point color-d55-xyz))
  ;;   ;; (fn (ns/color-tint-with-light <> ns/theme-white-point color-d50-xyz))
  ;;   (ht-transform-v ns/theme)
  ;;   (setq ns/theme))
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
    ;; :base01 (ht-get ns/theme :foreground__) ;; Lighter Background (Used for status bars)

    ;; font-comment-delimiter, region, active modeline background
    :base02 (ht-get ns/theme :accent1__) ;; Selection Background

    :base03 (ht-get ns/theme :foreground__) ;; Comments, Invisibles, Line Highlighting
    :base04 (ht-get ns/theme :foreground_)  ;; Dark Foreground (Used for status bars)

    :base05 (ht-get ns/theme :foreground) ;; Default Foreground, Caret, Delimiters, Operators

    :base06 (ht-get ns/theme :foreground_) ;; Light Foreground (Not often used)

    :base07 (ht-get ns/theme :foreground_) ;; Light Background (Not often used)

    ;; org-todo, variables
    ;; :base08 (ht-get ns/theme :accent2) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
    :base08 (ht-get ns/theme :accent2_) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

    ;; ivy-current-match foreground
    :base09 (ht-get ns/theme :foreground) ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

    ;; :base0A (ns/color-lab-darken (ht-get ns/theme :accent1__) 4)
    :base0A (ht-get ns/theme :accent1__)
    ;; Classes, Markup Bold, Search Text Background

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
