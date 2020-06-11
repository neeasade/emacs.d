;; -*- lexical-binding: t; -*-

;; this is a great resource: https://peteroupc.github.io/colorgen.html

;; see this table for white point references and what they are meant to represent in LAB:
;; https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants

;; emacs ships with the following white space consts (annotated with info from linked table):
;; color-d50-xyz | Horizon Light. ICC profile PCS
;; color-d55-xyz | Mid-morning / Mid-afternoon Daylight
;; color-d65-xyz | Noon Daylight: Television, sRGB color space
;; color-d75-xyz | North sky Daylight

;; Hue is an angle from red at 0 to yellow to green to cyan to blue to magenta to red
;; angle mapping in hue in degrees: (HSL space)
;; note: for HUE in color.el these are all within the 1.0 range/collapsed
;; 0   - 60 red to yellow
;; 60  - 120 yellow to green
;; 120 - 180 green to cyan
;; 180 - 240 cyan to blue
;; 240 - 300 blue to magenta
;; 300 - 360 magenta to red

;; 180 - 240 -- this will be our favored hue maybe? cyan to blue

(require 'base16-theme)

;; aim for minimum contrast ratios of 7:1 (between background and foreground)?
(defun ns/color-contrast (c1 c2)
  (let ((rl1 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c1))))
         (rl2 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c2)))))
    (/ (+ 0.05 (max rl1 rl2))
      (+ 0.05 (min rl1 rl2)))))

(defun ns/iterate-color (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (let ((color start))
    (while (and (not (funcall condition color))
             (not (string= (funcall op color) color)))
      (setq color (funcall op color))) color))

(defun ns/color-name-to-lab (name)
  "Transform NAME into LAB colorspace with some lighting assumption."
  (-as-> name <>
    (color-name-to-rgb <>)
    (apply 'color-srgb-to-xyz <>)
    (append <> (list (ht-get ns/theme :white-point)))
    (apply 'color-xyz-to-lab <>)))

(defun ns/color-lab-to-name (lab)
  (->> (append lab (list (ht-get ns/theme :white-point)))
    (apply 'color-lab-to-xyz)
    (apply 'color-xyz-to-srgb)
    (apply 'color-rgb-to-hex)))

;; tests
(->> "#ccddee" color-name-to-rgb (apply 'color-srgb-to-lab)
  ;; (apply 'color-lab-to-srgb)
  ;; (apply 'color-rgb-to-hex)
  (ns/color-lab-to-name)
  )


(defun ns/color-name-distance (c1 c2)
  ;; note: there are 3 additional optional params to cie-de2000: compensation for {lightness,chroma,hue}
  ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
  (color-cie-de2000
    (ns/color-name-to-lab c1)
    (ns/color-name-to-lab c2)))

;; (ns/color-name-distance "#000000" "#ffffff")
;; the builtin:
;; (color-distance "#000000" "#ffffff")

;; (ns/iterate-color "#ffffff"
;;   (lambda (c) (color-darken-name c 0.5))
;;   (lambda (color)
;;     (>
;;       ;; (ns/color-name-distance "#ffffff" color)
;;       (ns/color-contrast "#ffffff" color)
;;       19
;;       ;; 6.8
;;       )))

;; some thoughts:
;; contrast ratio as measurement is really only good as luminance meatric
;; color distance is useful if we're doing weird things maybe?

(setq ns/theme
  (let*
    ((foreground 'todo)
      (background 'todo)
      (accent1 'todo)
      (accent2 'todo))

    (ht
      (:foreground foreground)
      (:background background)

      (:faded 'todo)

      (:star background)
      (:star-1 background)
      (:star-2 background)

      (:moon background)
      (:moon-1 background)
      (:moon-2 background)

      (:white-point
        color-d65-xyz
        ;; color-d50-xyz | Horizon Light. ICC profile PCS
        ;; color-d55-xyz | Mid-morning / Mid-afternoon Daylight
        ;; color-d65-xyz | Noon Daylight: Television, sRGB color space
        ;; color-d75-xyz | North sky Daylight
        ))))

(ht-get ns/theme :white-point)

;; (apply 'color-rgb-to-hsl (color-name-to-rgb "#ccddee"))

(apply 'color-srgb-to-lab (color-name-to-rgb "#ccddee"))

;; HSL - Hue Saturation Luminance -- all 0.0 -> 1.0
;; HSV - Hue Saturation Value Hue in radians, SV is 0.0 -> 1.0

;; (defun ns/tint-to-color (name target percent)
;;   "tint NAME to TARGET color by PERCENT of all HSL properties"
;;   ;; maybe todo: have tint accept some generic color property
;;   (let* (
;;           ;; (is-light (ns/color-is-light-p (ns/theme 'foreground)))
;;           (tint-function (if is-light 'color-darken-name 'color-lighten-name))
;;           (luminance (third (apply 'color-rgb-to-hsl (color-name-to-rgb name))))
;;           ;; original (tints to black or white instead of fg bg):
;;           ;; (l-percent (* percent (if is-light luminance (- 1 luminance))))


;;           (bg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'background)))))
;;           (fg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'foreground)))))
;;           (l-percent (* percent (if is-light (- luminance bg-l) (- fg-l luminance))))
;;           )
;;     (funcall tint-function name l-percent)))

;; enforce a minimum contrast

;; (defun ns/fade-min-contrast (name percent contrast)
;;   (ns/contrast-color (ns/fade-color name percent) contrast 'ns/bolden-color))

;; (defun ns/bolden-min-contrast (name percent contrast)
;;   (ns/contrast-color (ns/bolden-color name percent) contrast 'ns/fade-color))

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
