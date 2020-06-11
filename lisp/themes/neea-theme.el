;; -*- lexical-binding: t; -*-

;; this is a great resource: https://peteroupc.github.io/colorgen.html

;; see this table for white point references and what they are meant to represent in LAB:
;; https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants

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
      (message color)
      (setq color (funcall op color))) color))

;; iterate color, but return all inbetween steps
(defun ns/iterate-color (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (let ((color start))
    (while (and (not (funcall condition color))
             (not (string= (funcall op color) color)))
      (message color)
      (setq color (funcall op color))) color))

(defun ns/color-name-to-lab (name &optional white-point)
  "Transform NAME into LAB colorspace with some lighting assumption."
  (-as-> name <>
    (color-name-to-rgb <>)
    (apply 'color-srgb-to-xyz <>)
    (append <> (list (or white-point (ht-get ns/theme :white-point))))
    (apply 'color-xyz-to-lab <>)))

(defun ns/color-lab-to-name (lab &optional white-point)
  (->> (append lab (list (or white-point (ht-get ns/theme :white-point))))
    (apply 'color-lab-to-xyz)
    (apply 'color-xyz-to-srgb)
    (-map 'color-clamp)
    (apply 'color-rgb-to-hex)))

(defun ns/color-tint-with-light (name w1 w2)
  "convert a color wrt white points W1 and W2 through the lab colorspace"
  (ns/color-lab-to-name (ns/color-name-to-lab name w1) w2))

(-map
  (lambda (percent)
    (ns/iterate-color
      "#00C5E0"
      (lambda (c) (ns/color-tint-with-light c
                    color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
                    color-d75-xyz ;; | North sky Daylight
                    ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
                    ;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space
                    ))
      (lambda (c) (>
                    (ns/color-name-distance c "#00C5E0")
                    ;; (ns/color-contrast c "#00C5E0")
                    percent))))
  '(3 6 9 12 15)
  ;; '(1 1.1 1.2 1.3 1.4)
  ;; '(2 4 6 8)
  )

(defun ns/color-weird (color percent)
  (ns/iterate-color
    color
    (lambda (c)
      (color-lighten-name c 1)
      ;; ns/color-tint-with-light c ;; color-d50-xyz ;; | Horizon Light. ICC profile PCS
      ;; color-d55-xyz ;; | Mid-morning / Mid-afternoon Daylight
      ;; color-d75-xyz ;; | North sky Daylight
      ;; color-d65-xyz ;; | Noon Daylight: Television, sRGB color space
      )
    (lambda (c) (>
                  (ns/color-name-distance c color)
                  ;; (ns/color-contrast c "#00C5E0")
                  percent))))

;; '(3 6 9 12 15)
;; '(1 1.1 1.2 1.3 1.4)
;; '(2 4 6 8)




("#0000c7d1ffff" "#0000c7d1ffff" "#0000ebdbffff" "#0000f4a5ffff" "#0000fd2dffff")

("#0000c7d1ffff" "#0000c7d1ffff" "#0000c7d1ffff" "#0000c7d1ffff")

("#0000c7d1ffff" "#0000c7d1ffff" "#0000ebdbffff" "#0000f4a5ffff" "#0000fd2dffff")

;; ("#0000c6c1ed38" "#0000c7d3fa17" "#0000eb39ffff" "#0000f4dcffff" "#0000fd3fffff")

(ns/color-tint-with-light   "#bb40cf15e4de" color-d50-xyz color-d55-xyz )

(ns/color-name-distance
  "#c4a1cdebd89a"
  ;; "#bb40cf15e4de"
  ;; "#b051d04bf19e"
  "#00C5E0")

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

;; some thoughts:
;; contrast ratio as measurement is really only good as luminance meatric
;; color distance is useful if we're doing weird things maybe?


;; from our automata tiling repos
;; /* #ebe5ff */
;; /* #eef0f3 */
;; /* #d7cdff */

(color-darken-name "#d7cdff" 10)

"#aee19a9affff"

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

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :accent1 "#aee19a9affff" :accent1-1 "#cb89be6cfffe" :accent1-2 "#dbe9d2e4fffe" :accent2 "#618079df0000" :accent2-1 "#7a1098930000" :accent2-2 "#8658a7ed0000" :white-point (0.950455 1.0 1.088753) ...))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :focused-fg "#8567ff" :accent1 "#aee19a9affff" :accent1-1 "#cb89be6cfffe" :accent1-2 "#dbe9d2e4fffe" :accent2 "#618079df0000" :accent2-1 "#7a1098930000" :accent2-2 "#8658a7ed0000" :white-point (0.950455 1.0 1.088753) ...))

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :focused-fg "#8567ff" :accent1 "#aee19a9affff" :accent1-1 "#bb29a9f4ffff" :accent1-2 "#e831e23efffe" :accent2 "#618079df0000" :accent2-1 "#6dc889390000" :accent2-2 "#71e08e570000" :white-point (0.950455 1.0 1.088753) ...))


#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :focused-fg "#8567ff" :accent1 "#aee19a9affff" :accent1-1 "#87dd9ed3ffff" :accent1-2 "#4905a2b6ffff" :accent2 "#618079df0000" :accent2-1 "#593d7b0b266e" :accent2-2 "#53b57bac3445" :white-point (0.950455 1.0 1.088753) ...))

;; #005f87
;; 'color00' : ['#eeeeee', '255'],
;; \       'color01' : ['#af0000', '124'],
;; \       'color02' : ['#008700', '28'],
;; \       'color03' : ['#5f8700', '64'],
;; \       'color04' : ['#0087af', '31'],
;; \       'color05' : ['#878787', '102'],
;; \       'color06' : ['#005f87', '24'],
;; \       'color07' : ['#444444', '238'],
;; \       'color08' : ['#bcbcbc', '250'],
;; \       'color09' : ['#d70000', '160'],
;; \       'color10' : ['#d70087', '162'],
;; \       'color11' : ['#8700af', '91'],
;; \       'color12' : ['#d75f00', '166'],
;; \       'color13' : ['#d75f00', '166'],
;; \       'color14' : ['#005faf', '25'],
;; \       'color15' : ['#005f87', '24'],
;; \       'color16' : ['#0087af', '31'],
;; \       'color17' : ['#008700', '28'],

#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :focused-fg "#ae9aff" :accent1 "#d7cdff" :accent1-1 "#b958d13cffff" :accent1-2 "#9322d473ffff" :accent2 "#71e18e560000" :accent2-1 "#68598fae2e51" :accent2-2 "#61f590663e3d" :white-point (0.950455 1.0 1.088753) ...))

"#d7cdff" "#c07cd0c4ffff" "#9568d4e2ffff" :
#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (:foreground "#5A5E65" :background "#EEF0F3" :faded-fg "#c2c4c8" :focused-fg "#ae9aff" :accent1 "#d7cdff" :accent1-1 "#c07cd0c4ffff" :accent1-2 "#9568d4e2ffff" :accent2 "#71e18e560000" :accent2-1 "#691d8fb1276e" :accent2-2 "#60ae90be3ae6" :white-point (0.950455 1.0 1.088753) ...))

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
