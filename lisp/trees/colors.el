;; -*- lexical-binding: t; -*-
;; color utilities
;; the main thing this file provides are color transformation functions
;; these accept lambdas for operating on hex colors in different color spaces

;; for information, cf:
;; https://en.wikipedia.org/wiki/CIELAB_color_space
;; LAB only: https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants
;; https://peteroupc.github.io/colorgen.html
;; http://colorizer.org/
;; https://github.com/yurikhan/yk-color/blob/master/yk-color.el
;; https://www.w3.org/TR/WCAG20/#relativeluminancedef
;; https://www.24a11y.com/2019/color-theory-and-contrast-ratios/
;; emacs shipped color.el


;; notes for hue:
;; Hue is an angle from red at 0 to yellow to green to cyan to blue to magenta to red
;; angle mapping in hue in degrees: (HSL space)
;; note: for HUE in color.el these are all within the 1.0 range/collapsed

;; LCH space hue is different:
;; 0 red
;; 90 yellow
;; 180 green
;; 270 blue

(ns/comment
  ;; play with LCh hue:
  (ns/color-lch-transform "#cccccc"
    (lambda (L C H)
      (list 50 90 180)
      (list 70 90 90)))
  )

;; NOTE: hue is different in LCH and HSL
;; in lch going from 4 primary colors red yellow green blue, along 90 degrees

;; hue visualized (degrees):
;; HSL with .5 saturation and .5 luminance
;; and LCH -> d65 lab with 50 sat and luminance
;; 0   red      "#bfff3fff3fff"
;; 60  yellow   "#bfffbfff3fff"
;; 120 green    "#3fffbfff3fff"
;; 180 cyan     "#3fffbfffbfff"
;; 240 blue     "#3fff3fffbfff"
;; 300 magenta  "#bfff3fffbfff"
;; 360 red      "#bfff3fff3fff"

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
;; https://en.wikipedia.org/wiki/SRGB
;; note: the rgb conversion functions in HSLuv lib handle linear transformation of rgb colors

(require 'color)
(use-package hsluv)

(defun ns/color-greaten (percent color)
  (ns/color-shorten
    (if (ns/color-is-light-p color)
      (color-lighten-name color percent)
      (color-darken-name color percent))))

(defun ns/color-lessen (percent color)
  (ns/color-shorten
    (if (ns/color-is-light-p color)
      (color-darken-name color percent)
      (color-lighten-name color percent))))

(defun ns/color-tint-ratio (c against ratio)
  (ns/color-iterate c
    (if (ns/color-is-light-p against)
      (fn (ns/color-lab-darken <> 0.1))
      (fn (ns/color-lab-lighten <> 0.1)))

    (fn (> (ns/color-contrast-ratio <> against) ratio))))

(ns/comment
  ;; testing
  (ns/color-tint-ratio "#eeeeee" "#ffffff" 20.0)

  (ns/color-contrast-ratio "#222222" "#000000")

  (ns/color-contrast-ratio "#000000" "#111111" )

  (ns/color-tint-ratio "#000000" "#111111" 3.0)

  (ns/color-tint-ratio "#222222" "#000000" 21.0)

  )

(defun ns/color-shorten (color)
  "optionally transform COLOR #HHHHHHHHHHHH to #HHHHHH"
  (if (= (length color) 7)
    color
    (-as-> color C
      (color-name-to-rgb C)
      `(color-rgb-to-hex ,@C 2)
      (eval C))))

(defun ns/color-is-light-p (name)
  (> (first (ns/color-name-to-lab name))
    65                                 
    )

  ;; (> (->> name (color-name-to-rgb) (apply 'color-rgb-to-hsl) (third))
  ;;   0.65
  ;;   )

  ;; old way:
  ;; (let*
  ;;   ((rgb (color-name-to-rgb name))
  ;;     (red (first rgb))
  ;;     (green (second rgb))
  ;;     (blue (third rgb))
  ;;     ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
  ;;     (yiq (+ (* red .299) (* green .587) (* blue .114))))
  ;;   (>= yiq 0.5))
  )

(defun ns/color-contrast-ratio (c1 c2)
  ;; cf https://peteroupc.github.io/colorgen.html#Contrast_Between_Two_Colors
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

(defun ns/color-name-to-lab (name &optional white-point)
  "Transform NAME into LAB colorspace with some lighting assumption."
  (-as-> name <>
    (color-name-to-rgb <>)
    (apply 'color-srgb-to-xyz <>)
    (append <> (list (or white-point color-d65-xyz)))
    (apply 'color-xyz-to-lab <>)))

(defun ns/color-lab-to-name (lab &optional white-point)
  (->> (append lab (list (or white-point color-d65-xyz)))
    (apply 'color-lab-to-xyz)
    (apply 'color-xyz-to-srgb)
    ;; when pulling it out we might die
    (-map 'color-clamp)
    (apply 'color-rgb-to-hex)))

(defun ns/color-lab-change-whitepoint (name w1 w2)
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
  (->> color
    (ns/color-name-to-lab)
    (apply transform)
    (ns/color-lab-to-name)))

(defun ns/color-lab-lighten (c value)
  (ns/color-lab-transform c
    (lambda (L A B) (list (+ L value) A B))))

(defun ns/color-lab-darken (c value)
  (ns/color-lab-transform c
    (lambda (L A B) (list (- L value) A B))))

(defun ns/color-lch-transform (c transform)
  "Perform a transformation in the LAB LCH space. LCH values are {0-100, 0-100, 0-360}"
  ;; color-lab-to-lch returns a form with H in radians.
  ;; we do some hamfisted handling here for a consistent expectation.
  (ns/color-lab-transform c
    (lambda (L A B)
      (apply 'color-lch-to-lab
        (let ((result (apply transform
                        (append
                          (-take 2 (color-lab-to-lch L A B))
                          (list (radians-to-degrees (third (color-lab-to-lch L A B))))))))
          (append
            (-take 2 result)
            (list (degrees-to-radians (mod (third result) 360.0))))))))

  ;; (ns/color-lab-transform c
  ;;   (lambda (L A B)
  ;;     (apply 'color-lch-to-lab
  ;;       (apply transform (color-lab-to-lch L A B)))))
  )

(defun ns/color-hsl-transform (c transform)
  "Tweak C in the HSL colorspace. Transform gets HSL in values {0-360,0-100,0-100}"
  (->> (color-name-to-rgb c)
    ;; all ranges 0-1
    (apply 'color-rgb-to-hsl)
    ((lambda (hsl)
       (apply transform
         (list
           (* 360.0 (first hsl))
           (* 100.0 (second hsl))
           (* 100.0 (third hsl))))))
    ;; from transformed to what color.el expects
    ((lambda (hsl)
       (list
         (/ (first hsl) 360.0)
         (/ (second hsl) 100.0)
         (/ (third hsl) 100.0))))
    (-map 'color-clamp)
    (apply 'color-hsl-to-rgb)
    (apply 'color-rgb-to-hex)))

(ns/comment
  (ns/color-hsl-transform "#ccddcc"
    (lambda (H S L)
      (message (format "got: %s %s %s" H S L))
      ;; (list H S L)
      ;; (list H 100 L)
      (list H 40 40)
      )))

(defun ns/color-hsluv-transform (c transform)
  "Tweak a color in the HSLuv space. HSL rangeis {}"
  (apply 'color-rgb-to-hex
    (-map 'color-clamp
      (hsluv-hsluv-to-rgb
        (let ((result (apply transform (-> c ns/color-shorten (hsluv-hex-to-hsluv)))))
          (list
            (mod (first result) 360.0)
            (second result)
            (third result)))))))

(defun ns/color-pastel (c &optional Smod Vmod)
  "Make a color more pastel in the hsl space"
  ;; https://en.wikipedia.org/wiki/Pastel_(color)
  ;; pastel colors belong to a pale family of colors, which, when described in the HSV color space,
  ;; have high value and low saturation.
  (ns/color-hsl-transform c
    (lambda (H S L)
      (list
        H
        (* S (or Smod 0.9))
        (* L (or Vmod 1.1))
        ))))
