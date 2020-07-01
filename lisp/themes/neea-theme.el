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

;; (face-attribute 'default :foreground)
;; "#6af87bbf6957"


;; save themes
(setq ns/theme-melon
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
	    (:accent2_ "#44048cdd4404"
        :accent2 "#28ef740328ef"
        :accent1_ "#eb7e614e9cdc"
        :accent1 "#edad125370dc"
        :background+ "#876ad14982d3"
        :background "#e989fcb0e774"
        :foreground_ "#6af87bbf6957"
        :foreground "#2cc03bc32bad")))

(setq ns/theme-soft
  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data
      (:accent2_ "#a8e969a00000"
        :accent2 "#9e145777cceb"
        :accent1_ "#0000919c94c5"
        :accent1 "#0000919c94c5"
        :background+ "#ffffca0bffff"
        :background "#fffff98bffff"
        :foreground_ "#92728c539259"
        :foreground "#586652b35849" )))

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
        ;; (ns/color-lch-transform (lambda (L C H) (list
        ;;                                           (+ L 0.5)
        ;;                                           (+ C 1)
        ;;                                           ;; (+ (degrees-to-radians 1) H)
        ;;                                           )))

        (ns/color-pastel 0.9 1.1)
        ))

    (lambda (c)
      (> (ns/color-name-distance
           ;; todo: consider passing hue correction here
           origin c)
        mod
        ))))

;; LAB definition for color.el:
;; L: Luminance 0-100
;; A: Green -100 <--> 100 Red
;; B: Blue  -100 <--> 100 Yellow

(let*
  (
    ;; the most important color:
    (background
      (ns/color-lab-to-name
        '(100 10 0)
        ns/theme-white-point)

      )

    ;; foreground and faded foreground will be contrast ratio based:
    (foreground
      (ns/color-tint-ratio background background 2.8)
      ;; (ns/color-iterate background
      ;;   (fn (ns/color-lab-darken <> 0.5))
      ;;   (fn (> (ns/color-contrast-ratio <> background)
      ;;         3.9
      ;;         )))
      )

    (foreground_
      (ns/color-tint-ratio background background 1.8)
      ;; (ns/color-iterate background
      ;;   (fn (ns/color-lab-darken <> 0.5))
      ;;   (fn (> (ns/color-contrast-ratio <> background)
      ;;         2
      ;;         )))
      )

    (accent1
      (ns/color-lch-transform foreground
        (lambda (L C H)
          (list
            ;; (first (ns/color-name-to-lab foreground_))
            40
            ;; L
            ;; (* 2 C)
            C
            (degrees-to-radians 270)
            ))))

    (accent2
      (-> foreground
        (ns/color-lch-transform
          (lambda (L C H)
            (list
              ;; L
              40
              ;; (first (ns/color-name-to-lab foreground_))
              ;; (* 2 C)
              C
              (degrees-to-radians 90)
              )))))

    ;; (accent1_ (ns/color-derive-accent-left accent1 4))
    (accent1_
      (ns/color-lch-transform accent1
        (lambda (L C H)
          (list
            L
            (* C 3)
            H
            )
          )
        )
      ;; (ns/color-derive-accent-right accent1 10)
      )

    ;; strings:
    (accent2_
      (ns/color-lch-transform accent2
        (lambda (L C H)
          (list
            L
            (* C 3)
            H
            )
          )
        )

      ;; (ns/color-derive-accent-right accent2 10)
      )

    ;; active BG (selections)
    (background+
      ;; (ns/color-tint-ratio accent2 background 1.2)

      (ns/color-iterate
        accent2
        (fn (ns/color-lab-lighten <> 0.5) )
        (fn (< (ns/color-contrast-ratio <>
                 background
                 )
              1.3
              ;; 1.1
              )))
      )
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
  )

(setq ns/theme ns/theme-melon)

(deftheme neea)
(base16-theme-define 'neea
  (list
    ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
    ;; what the emacs base16 theme package follows.

    ;; guidelines location: http://chriskempson.com/projects/base16/
    ;; I've also noted some faces I care about

    :base00 (ht-get ns/theme :background) ;; Default Background

    ;; ivy-current-match background, isearch match foreground, inactive modeline background
    ;; :base01 (color-darken-name (ht-get ns/theme :background) 7) ;; Lighter Background (Used for status bars)
    :base01 (ht-get ns/theme :background+)  ;; Lighter Background (Used for status bars)

    ;; font-comment-delimiter, region, active modeline background
    :base02 (ht-get ns/theme :background+)  ;; Selection Background

    :base03 (ht-get ns/theme :foreground_) ;; Comments, Invisibles, Line Highlighting
    :base04 (ht-get ns/theme :foreground_) ;; Dark Foreground (Used for status bars)
    :base05 (ht-get ns/theme :foreground)  ;; Default Foreground, Caret, Delimiters, Operators
    :base06 (ht-get ns/theme :foreground_) ;; Light Foreground (Not often used)
    :base07 (ht-get ns/theme :foreground_) ;; Light Background (Not often used)

    ;; org-todo, variables
    ;; :base08 accent2 ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
    :base08 (ht-get ns/theme :accent2) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

    ;; ivy-current-match foreground
    :base09 (ht-get ns/theme :foreground) ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

    ;; types
    :base0A (ht-get ns/theme :accent1) ;; Classes, Markup Bold, Search Text Background

    ;; font-lock-string-face
    :base0B (ht-get ns/theme :accent2_) ;; Strings, Inherited Class, Markup Code, Diff Inserted

    :base0C (ht-get ns/theme :foreground_)  ;; Support, Regular Expressions, Escape Characters, Markup Quotes

    ;; prompt, function-name, search match foreground
    :base0D (ht-get ns/theme :accent1) ;; Functions, Methods, Attribute IDs, Headings

    ;; keyword-face, org-date
    :base0E (ht-get ns/theme :accent1_) ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

    :base0F (ht-get ns/theme :foreground_)  ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
    ))

(provide-theme 'neea)
(provide 'neea-theme)
;;; neea-theme.el ends here
