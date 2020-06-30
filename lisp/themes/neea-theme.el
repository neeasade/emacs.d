;; -*- lexical-binding: t; -*-
;; see file:../trees/colors.el for some notes on color spaces/resources
;; big thanks to belak for https://github.com/belak/base16-emacs

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

(defun ns/color-derive-accent-left (origin mod)
  (ns/color-iterate origin
    (lambda (c)
      (-> c
        ;; (ns/color-lch-transform (lambda (L C H) (list L (* C 0.9) H)))
        (ns/color-pastel 0.9 1.3)))
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
        ;; (ns/color-lch-transform (lambda (L C H) (list L (* C 0.9) H)))
        (ns/color-pastel 0.9 1.1)))

    (lambda (c)
      (> (ns/color-name-distance
           ;; todo: consider passing hue correction here
           origin c)
        mod
        ))))

(let*
  (
    ;; the most important color:
    (background
      (ns/color-lab-to-name
        '(97
           ;; 0
           -10
           8
           )
        ns/theme-white-point))

    ;; foreground and faded foreground will be contrast ratio based:
    (foreground
      (ns/color-iterate background
        (fn (ns/color-lab-darken <> 0.5))
        (fn (> (ns/color-contrast-ratio <> background)
              3.9
              ))))

    (foreground_
      (ns/color-iterate background
        (fn (ns/color-lab-darken <> 0.5))
        (fn (> (ns/color-contrast-ratio <> background)
              2
              ))))

    ;; accents are where the real fun is -- play in LAB space with foreground being initial starting point
    (accent1
      (ns/color-lab-transform foreground
        (lambda (L A B)
          (list
            (+ L 28)
            ;; L
            ;; going towards green, away from red
            ;; (- A (* 0.5 (+ A 100)))
            ;; to red
            (+ A (* 0.8 (- 200 (+ A 100))))
            ;; going towards blue, away from yellow
            ;; (- B (* 0.9 (+ B 100)))
            B
            ;; (+ A (* 0.3 (- 200 (+ A 100))))
            ))))

    (accent2
      (-> foreground
        (ns/color-lab-transform
          (lambda (L A B)
            (list
              ;; (+ L 10)
              ;; (- L 2)
              (+ L 2)
              ;; going towards green, away from red
              (- A (* 0.9 (+ A 100)))
              ;; (+ A (* 0.8 (- 200 (+ A 100))))
              ;; (- A (* 0.0 (+ A 100)))
              ;; going towards yellow, away from blue
              ;; (- B (* 0.9 (+ B 100)))
              (+ B (* 0.4 (- 200 (+ B 100))))
              )))
        (ns/color-derive-accent-right 10)
        ))

    (accent1_ (ns/color-derive-accent-left accent1 4))

    ;; strings:
    (accent2_ (ns/color-derive-accent-right accent2 8))

    ;; active BG (selections)
    (background+
      (ns/color-iterate accent2_
        (fn (ns/color-lab-lighten <> 0.5) )
        (fn (< (ns/color-contrast-ratio <> background)
              1.4
              ))))
    )

  (setq ns/theme
    (ht
      (:foreground foreground)
      (:foreground_ foreground_)

      (:background background)
      (:background+ background+)

      (:accent1 accent1)
      (:accent1_ accent1_)

      (:accent2 accent2)
      (:accent2_ accent2_)))

  ;; perform transforms to accent colors:
  (setq ns/theme
    (ht-transform-kv ns/theme
      (lambda (k v)
        (cond
          ((s-starts-with-p ":accent" (prin1-to-string k))
            ;; (ns/theme-enforce-contrast v background foreground)
            ;; (color-desaturate-name v 30)
            ;; (ns/color-lch-transform v (lambda (L C H) (list L (* C 0.7) H)))
            v
            )
          (t v)))))

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

  (deftheme neea)
  (base16-theme-define 'neea
    (list
      ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
      ;; what the emacs base16 theme package follows.

      ;; guidelines location: http://chriskempson.com/projects/base16/
      ;; I've also noted some faces I care about

      :base00 background ;; Default Background

      ;; ivy-current-match background, isearch match foreground, inactive modeline background
      ;; :base01 (color-darken-name (ht-get ns/theme :background) 7) ;; Lighter Background (Used for status bars)
      :base01 background+ ;; Lighter Background (Used for status bars)

      ;; font-comment-delimiter, region, active modeline background
      :base02 background+ ;; Selection Background
      ;; :base02 accent2_ ;; Selection Background

      :base03 foreground_ ;; Comments, Invisibles, Line Highlighting
      :base04 foreground_ ;; Dark Foreground (Used for status bars)
      :base05 foreground  ;; Default Foreground, Caret, Delimiters, Operators
      :base06 foreground_ ;; Light Foreground (Not often used)
      :base07 foreground_ ;; Light Background (Not often used)

      ;; org-todo, variables
      ;; :base08 accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      :base08 accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

      ;; ivy-current-match foreground
      :base09 foreground ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

      ;; types
      :base0A accent1 ;; Classes, Markup Bold, Search Text Background

      ;; font-lock-string-face
      :base0B accent2_ ;; Strings, Inherited Class, Markup Code, Diff Inserted

      :base0C foreground_ ;; Support, Regular Expressions, Escape Characters, Markup Quotes

      ;; prompt, function-name, search match foreground
      :base0D accent1 ;; Functions, Methods, Attribute IDs, Headings

      ;; keyword-face, org-date
      :base0E accent1_ ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

      :base0F foreground_ ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
      )))

(provide-theme 'neea)
(provide 'neea-theme)
;;; neea-theme.el ends here
