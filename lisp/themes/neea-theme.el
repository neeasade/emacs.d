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

(color-name-to-rgb "#9abcdd")
;; => (0.6039215686274509 0.7372549019607844 0.8666666666666667)

(apply 'color-rgb-to-hsl (color-name-to-rgb "#9abcdd"))
;; (0.582089552238806 0.4962962962962964 0.7352941176470589)

(apply 'color-rgb-to-hsv (color-name-to-rgb "#9abcdd"))
;; (3.6573765220896104 0.30316742081447967 0.8666666666666667)

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

;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
;; color-d75-xyz ;; | North sky Daylight
;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space

;; misc colors
;; "#bb40cf15e4de"
;; /* #ebe5ff */
;; /* #d7cdff */
;; "#00C5E0"

(setq ns/theme
  (let*
    ((foreground  "#5A5E65" )
      (background  "#EEF0F3")
      ;; (accent1 "#d7cdff")
      (accent1 "#aee19a9affff")

      (comp (apply 'color-rgb-to-hex (color-complement accent1)))
      (accent2
        (ns/iterate-color comp
          ;; accent1
          (fn (color-lighten-name <> 2))
          (fn (< (ns/color-contrast accent1 <>) 3))
          ;; (fn (< (ns/color-name-distance accent1 <>) 20))
          )))

    (ht
      (:foreground foreground)
      (:background background)

      ;; (:foreground-fade (ns/color-lessen 50 foreground))
      (:foreground-fade (ns/color-lessen 50 foreground))
      (:faded-fg (ns/color-lessen 50 foreground))
      ;; (:focused-fg (ns/color-lessen 10 accent1))

      (:accent1 accent1)
      (:accent1-1 (ns/color-weird accent1 10))
      (:accent1-2 (ns/color-weird accent1 15))

      ;; todo: handle the fade/alts here better
      (:accent2 accent2)
      (:accent2-1 (ns/color-weird accent2 10))
      (:accent2-2 (ns/color-weird accent2 15))

      ;; note: ICC is https://en.wikipedia.org/wiki/ICC_profile
      (:white-point
        color-d65-xyz
        ;; color-d50-xyz | Horizon Light. ICC profile PCS
        ;; color-d55-xyz | Mid-morning / Mid-afternoon Daylight
        ;; color-d65-xyz | Noon Daylight: Television, sRGB color space
        ;; color-d75-xyz | North sky Daylight
        ))))

(deftheme neea)
(base16-theme-define 'neea
  (list
    ;; The comments on the sections here are from the base16 styling guidelines, not necessarily
    ;; what the emacs base16 theme package follows.

    ;; guidelines location: http://chriskempson.com/projects/base16/
    ;; I've also noted some faces I care about

    :base00 background ;; Default Background

    ;; ivy-current-match background
    :base01 (ns/fade-min-contrast star 100 2) ;; Lighter Background (Used for status bars)

    :base02 (ns/fade-min-contrast star 100 2) ;; Selection Background

    :base03 (ns/fade-color foreground 20) ;; Comments, Invisibles, Line Highlighting
    :base04 foreground                    ;; Dark Foreground (Used for status bars)

    :base05 foreground                    ;; Default Foreground, Caret, Delimiters, Operators

    :base06 foreground                    ;; Light Foreground (Not often used)

    :base07 background ;; Light Background (Not often used)

    ;; org-todo
    :base08 (ns/fade-color star 15) ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted

    ;; ivy-current-match foreground
    :base09 foreground              ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

    :base0A (ns/fade-color star 20) ;; Classes, Markup Bold, Search Text Background

    ;; font-lock-string-face
    :base0B star       ;; Strings, Inherited Class, Markup Code, Diff Inserted

    :base0C foreground ;; Support, Regular Expressions, Escape Characters, Markup Quotes

    ;; prompt, function-name
    :base0D (ns/bolden-color star 30) ;; Functions, Methods, Attribute IDs, Headings

    ;; keyword-face, org-date
    :base0E star       ;; Keywords, Storage, Selector, Markup Italic, Diff Changed

    :base0F foreground ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
    ))

(provide-theme 'base16-neeatheme)

(provide 'neea-theme)
;;; neea-theme.el ends here
