;; -*- lexical-binding: t; -*-
;; color utilities

;; cf:
;; https://en.wikipedia.org/wiki/CIELAB_color_space
;; LAB only: https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants
;; https://peteroupc.github.io/colorgen.html
;; http://colorizer.org/
;; https://github.com/yurikhan/yk-color/blob/master/yk-color.el
;; https://www.htmlcsscolor.com/hex/9ABCDD
;; https://www.w3.org/TR/WCAG20/#relativeluminancedef
;; emacs shipped color.el

;; todo: consider cieCAM02
;; https://en.wikipedia.org/wiki/CIECAM02

;; HSL - Hue Saturation Luminance -- all 0.0 -> 1.0
;; HSV - Hue Saturation Value Hue in radians, SV is 0.0 -> 1.0

;; notes for hue:
;; Hue is an angle from red at 0 to yellow to green to cyan to blue to magenta to red
;; angle mapping in hue in degrees: (HSL space)
;; note: for HUE in color.el these are all within the 1.0 range/collapsed

;; to see these colors inline here with rainbow-mode:
;; (setq rainbow-x-colors t)
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

;; todo: consider: longen colors before operations on names, just generally

(defun ns/color-format (color)
  (format "#%s" (substring color -6 nil)))

(defun ns/color-greaten (percent color)
  (ns/color-shorten
    (if (ns/color-is-light-p color)
      (color-lighten-name color percent)
      (color-darken-name color percent))))

(defun ns/color-lessen (percent color)
  (ns/shorten-color
    (if (ns/color-is-light-p color)
      (color-darken-name color percent)
      (color-lighten-name color percent))))

;; optionally transform #<12> to #<6>
(defun ns/color-shorten (color)
  "COLOR #HHHHHHHHHHHH to #HHHHHH"
  (if (= (length color) 7)
    color
    (-as-> color C
      (color-name-to-rgb C)
      `(color-rgb-to-hex ,@C 2)
      (eval C))))

(defun ns/color-longen (color)
  "COLOR #HHHHHH to #HHHHHHHHHHHH"
  (if (= (length color) 7)
    (let* ((convert (lambda (p1 p2) (/ (string-to-number (substring color p1 p2) 16) 255.0)))
            (red (funcall convert 1 3))
            (green (funcall convert 3 5))
            (blue (funcall convert 5 7)))
      (color-rgb-to-hex red green blue))
    color))

(defun ns/color-is-light-p (name)
  ;; todo: maybe check the L in lab instead for this
  (let*
    ((rgb (color-name-to-rgb name))
      (red (first rgb))
      (green (second rgb))
      (blue (third rgb))
      ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
      (yiq (+ (* red .299) (* green .587) (* blue .114))))
    (>= yiq 0.5)))

(defun ns/color-tone (name light dark)
  "tone name a percent based on if light or dark - generally want softer value for dark."
  (if (ns/color-is-light-p name)
    (color-darken-name name light)
    (color-lighten-name name dark)))

(defun ns/color-contrast-ratio (c1 c2)
  (let ((rl1 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c1))))
         (rl2 (third (apply 'color-rgb-to-hsl (color-name-to-rgb c2)))))
    (/ (+ 0.05 (max rl1 rl2))
      (+ 0.05 (min rl1 rl2)))))

(defun ns/color-iterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (let ((color start)
         (iterations 0))
    (while (and (not (funcall condition color))
             (not (string= (funcall op color) color))
             (< iterations 10000))
      (setq iterations (+ iterations 1))
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

(defun ns/color-lab-transform (color transform)
  "Generate an accent color from COLOR using TRANSFORM, a LAB colorspace function."
  (-as-> color <>
    (ns/color-name-to-lab <> ns/theme-white-point)
    (apply transform <>)
    (ns/color-lab-to-name <> ns/theme-white-point)))

(defun ns/color-lab-lighten (c value)
  (ns/color-lab-transform c
    (lambda (L A B) (list (+ L value) A B))))

(defun ns/color-lch-transform (c transform)
  (ns/color-lab-transform c
    (lambda (L A B)
      (apply 'color-lch-to-lab
        (apply transform (color-lab-to-lch L A B))))))

(defun ns/color-hsl-transform (c transform)
  (->> (color-name-to-rgb c)
    (apply 'color-rgb-to-hsl)
    (apply transform)
    (apply 'color-hsl-to-rgb)
    (apply 'color-rgb-to-hex)))

;; todo: rgb to srgb/some form of gamma correction?
;; maybe steal a little from https://github.com/yurikhan/yk-color/blob/master/yk-color.el
