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

;; LAB definition for color.el:
;; L: Luminance 0-100
;; A: Green -100 <--> 100 Red
;; B: Blue  -100 <--> 100 Yellow

(let*
  (
    ;; the most important color:
    ;; (background (ns/color-lab-to-name '(94 10 0)))
    (background "#EEF0F3")
    (background (ns/color-lab-darken "#EEF0F3" 2))

    ;; foreground and faded foreground will be contrast ratio based:
    (foreground (ns/color-tint-ratio background background 3.4))
    (foreground_ (ns/color-tint-ratio background background 2.1))

    (accent-rotations
      (let*
        ((color-start
           (ns/color-hsluv-transform
             foreground_
             (lambda (H S L)
               (list
                 ;; 346
                 ;; (- 346 90)
                 ;; 256

                 ;; 0
                 270
                 ;; 40
                 ;; (+ 253.642696368751 120)
                 ;; 253.642696368751


                 75

                 L
                 ))))

          ;; (interval (degrees-to-radians 60))

          ;; (interval 72)
          ;; (interval 90)
          ;; (interval 120)
          (interval -45)

          ;; (interval 60)
          ;; (interval 90)
          ;; (interval 30)
          )

        (list
          color-start

          ;; 2 sets of complements by an initial offset:
          ;; (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ (degrees-to-radians 180) H))))
          ;; (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ interval H))))
          ;; (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ interval (degrees-to-radians 180) H))))

          ;; LCH space (hue is at 90 degree offsets here rather than 60)
          (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ (* 1 interval) H))))
          (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ (* 2 interval) H))))
          (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ (* 3 interval) H))))
          (ns/color-lch-transform color-start (lambda (L C H) (list L C (+ (* 4 interval) H))))

          ;; HSLuv space test
          (ns/color-hsluv-transform color-start (lambda (H S L) (list (+ (* 1 interval) H) S L)))
          (ns/color-hsluv-transform color-start (lambda (H S L) (list (+ (* 2 interval) H) S L)))
          (ns/color-hsluv-transform color-start (lambda (H S L) (list (+ (* 3 interval) H) S L)))
          (ns/color-hsluv-transform color-start (lambda (H S L) (list (+ (* 4 interval) H) S L)))
          (ns/color-hsluv-transform color-start (lambda (H S L) (list (+ (* 5 interval) H) S L)))
          )))

    ;; (accent1  (nth 0 accent-rotations))
    ;; (accent1_ (nth 1 accent-rotations))
    ;; (accent2  (nth 0 accent-rotations))
    ;; (accent2_ (nth 1 accent-rotations))

    (accent1  (nth 0 accent-rotations))
    (accent1_ (nth 1 accent-rotations))
    (accent2  (nth 2 accent-rotations))
    (accent2_ (nth 4 accent-rotations))

    ;; active BG (selections)
    (background+
      (ns/color-iterate
        accent1_
        (fn (ns/color-lab-lighten <> 0.1) )
        (fn (< (ns/color-contrast-ratio <> background)
              ;; 1.3

              1.2
              ;; 1.03
              ;; 1.15
              ))))

    (background_
      (-> background
        (ns/color-lch-transform
          (lambda (L C H)
            (list L C
              ;; steal hue from accent1_
              (third (apply 'color-lab-to-lch
                       (ns/color-name-to-lab accent1_))))))
        (ns/color-hsluv-transform
          (lambda (H S L) (list H S (- L 4))))))

    (background__
      (ns/color-hsluv-transform
        background_
        (lambda (H S L) (list H S (- L 6)))))

    ;; (background_ (ns/color-tint-ratio-reverse background background 1.05))
    ;; (background__ (ns/color-tint-ratio-reverse accent1_ background 1.12))

    (background+ background__)
    )

  (setq ns/theme
    (ht
      (:foreground foreground)          ; regular text
      (:foreground_ foreground_)        ; comments
      (:foreground+ background)         ; foreground of a focused/highlighted thing

      (:background background)          ; regular canvas
      (:background_ background_)        ; emphasis?
      (:background__ background__)      ; inactive modeline
      (:background+ background+)        ; background of a focused/highlighted thing (also active modeline)

      (:accent1 accent1)                ; identifiers
      (:accent1_ accent1_)              ; builtins
      (:accent2 accent2)                ; types
      (:accent2_ accent2_)              ; strings
      ))

  ;; perform transforms to accent colors:
  (setq ns/theme
    (ht-transform-kv ns/theme
      (lambda (k v)
        (cond
          ((s-starts-with-p ":accent" (prin1-to-string k))
            ;; messing around:
            ;; (ns/color-lch-transform v (lambda (L C H) (list L (* C 1.5) H)))
            ;; (ns/color-hsluv-transform v (lambda (H S L) (list H (* S 1.0) (* L 0.95))))

            ;; ensure all colors have some minimum contrast ratio
            (ns/color-tint-ratio v background 2.2)

            ;; don't tweak anything
            ;; v

            ;; conform
            ;; foreground
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

  ;; shorten all the colors, because they are also used in EG org exports
  (setq ns/theme (ht-transform-v ns/theme 'ns/color-shorten)))

;; (setq ns/theme ns/theme-melon)
;; (setq ns/theme ns/theme-soft)

(deftheme neea)
(base16-theme-define 'neea
  (ht-with-context ns/theme
    (list
      ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
      ;; what the emacs base16 theme package follows.

      ;; guidelines location: http://chriskempson.com/projects/base16/
      ;; I've also noted some faces I care about

      :base00 :background ;; Default Background

      ;; ivy-current-match background, isearch match foreground, inactive modeline background
      :base01 :background+ ;; Lighter Background (Used for status bars)
      ;; :base01 :background__ ;; Lighter Background (Used for status bars)

      ;; region, active modeline background
      :base02 :background+ ;; Selection Background

      :base03 :foreground_ ;; Comments, Invisibles, Line Highlighting
      :base04 :foreground_ ;; Dark Foreground (Used for status bars)
      :base05 :foreground  ;; Default Foreground, Caret, Delimiters, Operators
      :base06 :foreground_ ;; Light Foreground (Not often used)
      :base07 :foreground_ ;; Light Background (Not often used)

      ;; org-todo, variables
      ;; :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
      :base08 :accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

      ;; ivy-current-match foreground
      :base09 :foreground ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

      ;; types
      ;; :base0A :accent1 ;; Classes, Markup Bold, Search Text Background
      :base0A :accent2 ;; Classes, Markup Bold, Search Text Background

      ;; strings
      :base0B :accent2_ ;; Strings, Inherited Class, Markup Code, Diff Inserted

      ;; :base0C :foreground_  ;; Support, Regular Expressions, Escape Characters, Markup Quotes
      :base0C :accent1_ ;; Support, Regular Expressions, Escape Characters, Markup Quotes

      ;; prompt, function-name, search match foreground
      :base0D :accent1 ;; Functions, Methods, Attribute IDs, Headings

      ;; keyword-face, org-date
      :base0E :accent1_ ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

      :base0F :foreground_ ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
      )))

(ns/comment
  ;; dump contrast ratios of all colors against background
  (ht-to-plist
    (ht-transform-kv
      ns/theme
      ;; ns/theme-melon
      (lambda (k v)
        (message
          (format "%s: %s"
            (prn k)
            (prn
              (ns/color-contrast-ratio
                (ht-get ns/theme :background)
                ;; (ht-get ns/theme-melon :background)
                v
                )))))))

  )



(provide-theme 'neea)
(provide 'neea-theme)
;;; neea-theme.el ends here
