;; -*- lexical-binding: t; -*-

;; cf:
;; https://en.wikipedia.org/wiki/CIELAB_color_space
;; LAB only: https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants
;; https://peteroupc.github.io/colorgen.html
;; http://colorizer.org/
;; https://github.com/yurikhan/yk-color/blob/master/yk-color.el
;; https://www.htmlcsscolor.com/hex/9ABCDD
;; https://www.w3.org/TR/WCAG20/#relativeluminancedef
;; emacs shipped color.el

;; HSL - Hue Saturation Luminance -- all 0.0 -> 1.0
;; HSV - Hue Saturation Value Hue in radians, SV is 0.0 -> 1.0

;; notes for hue:
;; Hue is an angle from red at 0 to yellow to green to cyan to blue to magenta to red
;; angle mapping in hue in degrees: (HSL space)
;; note: for HUE in color.el these are all within the 1.0 range/collapsed
;; 0   - 60 red to yellow
;; 60  - 120 yellow to green
;; 120 - 180 green to cyan
;; 180 - 240 cyan to blue
;; 240 - 300 blue to magenta
;; 300 - 360 magenta to red

;; LAB
;; - L*, or lightness of a color (how bright that color appears in comparison to white), is 0 or
;;   greater and 100 or less, where 0 is black and 100 is white.
;; - a* is a coordinate of the red/green axis (positive points to red, negative to green).
;; - b* is a coordinate of the yellow/blue axis (positive points to yellow, negative to blue).

;; LCH
;; - Lightness (L*) remains unchanged.
;; - Chroma (C*) is the distance of the color from the "gray" line.
;; - Hue (h, an angle)(12) ranges from magenta at roughly 0 to red to yellow to green to cyan to blue to magenta.

;; sRGB vs linear RGB
;; sRGB is kinda like declaring intent wrt a standard white point? (LAB makes this explicit)

;; Pastels
;; pastel colors belong to a pale family of colors, which, when described in the HSV color space,
;; have high value and low saturation.

;; THEME THOUGHTS
;; kinds of colors we want to create:
;; fg, bg

;; 2 faded fg levels (modeline, then comments)
;; 2 accents -- each with strong and weak versions
;;      the export might be just still be gradient style (it's easier to reason about emphasis if the star is a middle color, I think)

;; my thinking right now is that we'll derive 'lightness' from the foreground color -- and then tint
;; the hue to get our accent colors, and then from there tweak properties to get the 'weaker' and
;; 'stronger' variants of the colors.

;; additionally if we steal accent colors from somewhere we can snipe the hue and then use that as
;; our base with foreground (new color is HSL, accent H, accent S, foreground L)

;; I really like this color, lets see some properties of it.
;; #9abcdd

(require 'base16-theme)

(defun ns/make-color-helpers ()
  "Contain the help."
  (defun ns/color-contrast-ratio (c1 c2)
    (let ((rl1 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c1))))
           (rl2 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c2)))))
      (/ (+ 0.05 (max rl1 rl2))
        (+ 0.05 (min rl1 rl2)))))

  (defun ns/color-iterate (start op condition)
    "Do OP on START color until CONDITION is met or op has no effect."
    (let ((color start))
      (while (and (not (funcall condition color))
               (not (string= (funcall op color) color)))
        (setq color (funcall op color))) color))

  ;; TODO:
  (defun ns/color-iterate-collection (start op condition)
    "Do OP on START color until CONDITION is met or op has no effect (return all steps)."
    (let ((color start))
      (while (and (not (funcall condition color))
               (not (string= (funcall op color) color)))
        (setq color (funcall op color)))
      color))

  (defun ns/color-name-to-lab (name &optional white-point)
    "Transform NAME into LAB colorspace with some lighting assumption."
    (-as-> name <>
      (color-name-to-rgb <>)
      (apply 'color-srgb-to-xyz <>)
      (append <> (list white-point))
      (apply 'color-xyz-to-lab <>)))

  (defun ns/color-lab-to-name (lab &optional white-point)
    (->> (append lab (list white-point))
      (apply 'color-lab-to-xyz)
      (apply 'color-xyz-to-srgb)
      ;; when pulling it out we might die
      (-map 'color-clamp)
      (apply 'color-rgb-to-hex)))

  (defun ns/color-tint-with-light (name w1 w2)
    "convert a color wrt white points W1 and W2 through the lab colorspace"
    (ns/color-lab-to-name (ns/color-name-to-lab name w1) w2))

  (defun ns/color-name-distance (c1 c2)
    ;; note: there are 3 additional optional params to cie-de2000: compensation for
    ;; {lightness,chroma,hue} (all 0.0-1.0)
    ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
    (color-cie-de2000
      (ns/color-name-to-lab c1)
      (ns/color-name-to-lab c2)))

  ;; todo: rgb to srgb?
  )

(ns/make-color-helpers)

(setq ns/theme-white-point
  ;; note: ICC is https://en.wikipedia.org/wiki/ICC_profile
  ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
  ;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
  color-d65-xyz ;; | Noon Daylight: Television, sRGB color space (standard assumption)
  ;; color-d75-xyz ;; | North sky Daylight
  )

(defun ns/color-derive-accent (origin mod)
  (-as-> origin <>
    (ns/color-name-to-lab <> ns/theme-white-point)
    (apply 'color-lab-to-lch <>)
    (apply (lambda (L C H)
             (list
               (+ L mod)
               (- C (/ mod 2))
               H)) <>)
    (apply 'color-lch-to-lab <>)
    (ns/color-lab-to-name <> ns/theme-white-point)))

(add-to-list 'custom-theme-load-path (~ ".emacs.d/lisp/themes"))

(let*
  ;; from the lab light theme
  ((foreground  "#5A5E65")
    (background  "#EEF0F3")

    (accent1
      (-as-> foreground <>
        (ns/color-name-to-lab <> ns/theme-white-point)
        (apply (lambda (L A B)
                 (list
                   (+ L 10)
                   ;; green
                   (- A (* 0.5 (+ A 100)))
                   ;; blue
                   (- B (* 0.5 (+ B 100)))
                   )) <>)

        (ns/color-lab-to-name <> ns/theme-white-point)))

    (accent2
      (-as-> foreground <>
        (ns/color-name-to-lab <> ns/theme-white-point)
        (apply (lambda (L A B)
                 (list
                   (+ L 10)
                   ;; going to negative 100 (green)
                   (- A (* 0.5 (+ A 100)))
                   ;; going to positive 100 (yellow)
                   (+ B (* 0.5 (- 200 (+ B 100))))
                   )) <>)

        (ns/color-lab-to-name <> ns/theme-white-point)))

    (accent1_ (ns/color-derive-accent accent1 10))
    (accent1__ (ns/color-derive-accent accent1_ 10))
    (accent2_ (ns/color-derive-accent accent2 10))
    (accent2__ (ns/color-derive-accent accent2_ 10))

    (foreground_ (ns/color-derive-accent foreground 20))
    (foreground__ (ns/color-derive-accent foreground_ 10))
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
      (:accent2__ accent2__)
      )))

;; #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (
;; :foreground "#5A5E65"
;; :foreground_ "#94c6902d86f9"
;; :foreground__ "#c25cc6d3cedf"
;; :background "#EEF0F3"
;; :accent1 "#00008f61cf98"
;; :accent1_ "#834cc303f71f"
;; :accent1__ "#d1b4f987ffff"
;; :accent2 "#1fed897317c5"
;; :accent2_ "#6e9bbd935d1f"
;; :accent2__ "#b258f39a9fc7" ...

;; ns/theme

;; #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (
;; foreground "#5A5E65"
;; foreground_ "#94c6902d86f9"
;; foreground__ "#c25cc6d3cedf"
;; background "#EEF0F3"
;; accent1 "#00008f61cf98"
;; accent1_ "#5653a8cde341"
;; accent1__ "#834cc303f71e"
;; accent2 "#1fed897317c5"
;; accent2_ "#4b25a3443c34"
;; accent2__ "#6e9bbd925d1f"

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

    ;; font-comment delimiter face, region, active modeline background
    :base02 (ht-get ns/theme :accent1_) ;; Selection Background

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
