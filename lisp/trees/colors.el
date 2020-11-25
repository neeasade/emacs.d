;; -*- lexical-binding: t; -*-
;; color utilities
;; cf: https://notes.neeasade.net/color-spaces.html

;; sRGB vs linear RGB
;; sRGB is kinda like declaring intent wrt a standard white point? (LAB makes this explicit)
;; https://en.wikipedia.org/wiki/SRGB
;; note: the rgb conversion functions in HSLuv lib handle linear transformation of rgb colors

(require 'color)
(use-package hsluv)

(defun ns/color-shorten (color)
  "optionally transform COLOR #HHHHHHHHHHHH to #HHHHHH"
  (if (= (length color) 7)
    color
    (-as-> color C
      (color-name-to-rgb C)
      `(color-rgb-to-hex ,@C 2)
      (eval C))))

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

(defun ns/color-is-light-p (name)
  (> (first (ns/color-name-to-lab name)) 65))

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

(defun ns/color-iterations (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect - return all intermediate steps."
  (let ((colors (list start))
         (iterations 0))
    (while (and (not (funcall condition (-last-item colors)))
             (not (string= (funcall op (-last-item colors)) (-last-item colors)))
             (< iterations 10000))
      (setq iterations (+ iterations 1))
      (setq colors (-snoc colors (funcall op (-last-item colors)))))
    colors))

(defun ns/color-iterate (start op condition)
  "Do OP on START color until CONDITION is met or op has no effect."
  (-last-item (ns/color-iterations start op condition)))

(defun ns/color-tint-ratio (c against ratio)
  (ns/color-iterate c
    (if (ns/color-is-light-p against)
      (fn (ns/color-lab-darken <> 0.1))
      (fn (ns/color-lab-lighten <> 0.1)))
    (fn (> (ns/color-contrast-ratio <> against) ratio))))

(defun ns/color-luminance-srgb (color)
  ;; cf https://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef
  (let ((rgb (-map
               (lambda (part)
                 (if (<= part 0.03928)
                   (/ part 12.92)
                   (expt (/ (+ 0.055 part) 1.055) 2.4)))
               (color-name-to-rgb color))))
    (+
      (* (nth 0 rgb) 0.2126)
      (* (nth 1 rgb) 0.7152)
      (* (nth 2 rgb) 0.0722))))

(defun ns/color-contrast-ratio (c1 c2)
  ;; cf https://peteroupc.github.io/colorgen.html#Contrast_Between_Two_Colors
  (let ((rl1 (ns/color-luminance-srgb c1))
         (rl2 (ns/color-luminance-srgb c2)))
    (/ (+ 0.05 (max rl1 rl2))
      (+ 0.05 (min rl1 rl2)))))

(defun ns/color-lab-change-whitepoint (name w1 w2)
  "convert a color wrt white points W1 and W2 through the lab colorspace"
  (ns/color-lab-to-name (ns/color-name-to-lab name w1) w2))

(defun ns/color-name-distance (c1 c2)
  ;; note: there are 3 additional optional params to cie-de2000: compensation for
  ;; {lightness,chroma,hue} (all 0.0-1.0)
  ;; https://en.wikipedia.org/wiki/Color_difference#CIEDE2000
  (apply 'color-cie-de2000 (-map 'ns/color-name-to-lab (list c1 c2))))

;; transformers
(defun ns/color-lab-transform (color transform)
  "Generate an accent color from COLOR using TRANSFORM, a LAB colorspace function."
  (->> color
    (ns/color-name-to-lab)
    (apply transform)
    (ns/color-lab-to-name)))

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
            ;; (-map (lambda (p) (* 100.0 p)) (-take 2 result))
            (-take 2 result)
            (list (degrees-to-radians (mod (third result) 360.0)))))))))

(defun ns/color-hsl-transform (c transform)
  "Tweak C in the HSL colorspace. Transform gets HSL in values {0-360,0-100,0-100}"
  (->> (color-name-to-rgb c)
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

(defun ns/color-hsluv-transform (c transform)
  "Tweak a color in the HSLuv space. S,L range is {0-100}"
  (apply 'color-rgb-to-hex
    (-map 'color-clamp
      (hsluv-hsluv-to-rgb
        (let ((result (apply transform (-> c ns/color-shorten hsluv-hex-to-hsluv))))
          (list
            (mod (first result) 360.0)
            (second result)
            (third result)))))))

;; better names, probably:
(defalias 'ns/color-transform-hsl 'ns/color-hsl-transform)
(defalias 'ns/color-transform-hsluv 'ns/color-hsluv-transform)
(defalias 'ns/color-transform-lch 'ns/color-lch-transform)
(defalias 'ns/color-transform-lab 'ns/color-lab-transform)

;; individual property tweaks:
(defun ns/color-transform-hsl-h (c func) (ns/color-transform-hsl c (lambda (H S L) (list (funcall func H) S  L))))
(defun ns/color-transform-hsl-s (c func) (ns/color-transform-hsl c (lambda (H S L) (list H (funcall func S) L))))
(defun ns/color-transform-hsl-l (c func) (ns/color-transform-hsl c (lambda (H S L) (list H S (funcall func L)))))

(defun ns/color-transform-hsluv-h (c func) (ns/color-transform-hsluv c (lambda (H S L) (list (funcall func H) S  L))))
(defun ns/color-transform-hsluv-s (c func) (ns/color-transform-hsluv c (lambda (H S L) (list H (funcall func S) L))))
(defun ns/color-transform-hsluv-l (c func) (ns/color-transform-hsluv c (lambda (L C H) (list L C (funcall func H)))))

(defun ns/color-transform-lch-l (c func) (ns/color-transform-lch c (lambda (L C H) (list (funcall func L) C  H))))
(defun ns/color-transform-lch-c (c func) (ns/color-transform-lch c (lambda (L C H) (list L (funcall func C) H))))
(defun ns/color-transform-lch-h (c func) (ns/color-transform-lch c (lambda (L C H) (list L C (funcall func H)))))

(defun ns/color-transform-lab-l (c func) (ns/color-transform-lab c (lambda (L A B) (list (funcall func L) A  B))))
(defun ns/color-transform-lab-c (c func) (ns/color-transform-lab c (lambda (L A B) (list L (funcall func A) B))))
(defun ns/color-transform-lab-h (c func) (ns/color-transform-lab c (lambda (L A B) (list L A (funcall func B)))))

(defun ns/color-getter (c transform getter)
  (let ((return))
    (apply transform
      (list c
        (lambda (&rest _)
          (setq return (funcall getter _))
          _)))
    return))

(defun ns/color-get-lab-l (c) (ns/color-getter c 'ns/color-transform-lab 'first))
(defun ns/color-get-lab-a (c) (ns/color-getter c 'ns/color-transform-lab 'second))
(defun ns/color-get-lab-b (c) (ns/color-getter c 'ns/color-transform-lab 'third))

(defun ns/color-get-hsl-h (c) (ns/color-getter c 'ns/color-transform-hsl 'first))
(defun ns/color-get-hsl-s (c) (ns/color-getter c 'ns/color-transform-hsl 'second))
(defun ns/color-get-hsl-l (c) (ns/color-getter c 'ns/color-transform-hsl 'third))

(defun ns/color-get-hsluv-h (c) (ns/color-getter c 'ns/color-transform-hsluv 'first))
(defun ns/color-get-hsluv-s (c) (ns/color-getter c 'ns/color-transform-hsluv 'second))
(defun ns/color-get-hsluv-l (c) (ns/color-getter c 'ns/color-transform-hsluv 'third))

(defun ns/color-get-lch-l (c) (ns/color-getter c 'ns/color-transform-lch 'first))
(defun ns/color-get-lch-c (c) (ns/color-getter c 'ns/color-transform-lch 'second))
(defun ns/color-get-lch-h (c) (ns/color-getter c 'ns/color-transform-lch 'third))

;; other color functions:
(defun ns/color-lab-lighten (c value)
  (ns/color-transform-lab-l c (-partial '+ value)))

(defun ns/color-lab-darken (c value)
  (ns/color-transform-lab-l c (-rpartial '- value)))

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

(defun ns/color-gradient (step start end &optional with-ends)
  "create a gradient from"
  (if with-ends
    `(,start
       ,@(-map
           (fn (eval `(color-rgb-to-hex ,@<> 2)))
           (color-gradient
	           (color-name-to-rgb start)
	           (color-name-to-rgb end)
	           (- step 2)))
       ,end)
    (-map
      (fn (eval `(color-rgb-to-hex ,@<> 2)))
      (color-gradient
	      (color-name-to-rgb start)
	      (color-name-to-rgb end)
	      step))))

;; make colors within our normalized transform functions:
(defun ns/color-make-color-meta (transform properties)
  (apply transform
    (list "#cccccc"                     ; throwaway
      (lambda (&rest _) properties))))

(defun ns/color-make-hsl (H S L) (ns/color-make-color-meta 'ns/color-hsl-transform (list H S L)))
(defun ns/color-make-hsluv (H S L) (ns/color-make-color-meta 'ns/color-hsluv-transform (list H S L)))
(defun ns/color-make-lab (L A B) (ns/color-make-color-meta 'ns/color-lab-transform (list L A B)))
(defun ns/color-make-lch (L C H) (ns/color-make-color-meta 'ns/color-lch-transform (list L C H)))

