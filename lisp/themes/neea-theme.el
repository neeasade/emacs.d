(require 'base16-theme)

;; use true color here
(defun ns/color-is-light-p (name)
  (let* ((rgb (color-name-to-rgb name))
          (red (first rgb))
          (green (second rgb))
          (blue (third rgb))
          ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
          (yiq (+ (* red .299) (* green .587) (* blue .114))))
    (>= yiq 0.5)))

(defun ns/convert-color (name)
  "NAME #HHHHHH to #HHHHHHHHHHHH"
  (let* ((convert (lambda (p1 p2) (/ (string-to-number (substring name p1 p2) 16) 255.0)))
          (red (funcall convert 1 3))
          (green (funcall convert 3 5))
          (blue (funcall convert 5 7)))
    (color-rgb-to-hex red green blue)))

;; bold and fade are working as expected don't @ them
;; fade here means tinted to the background
(defun ns/fade-color (name percent)
  (let* (
          ;; is foreground light
          (is-light (ns/color-is-light-p (ns/theme 'foreground)))
          (tint-function (if is-light 'color-darken-name 'color-lighten-name))
          (luminance (third (apply 'color-rgb-to-hsl (color-name-to-rgb name))))
          ;; original (tints to black or white instead of fg bg):
          ;; (l-percent (* percent (if is-light luminance (- 1 luminance))))

          (bg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'background)))))
          (fg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'foreground)))))
          (l-percent (* percent (if is-light (- luminance bg-l) (- fg-l luminance))))
          )
    (funcall tint-function name l-percent)))

;; bolden here means tinted to the foreground
(defun ns/bolden-color (name percent)
  (let* (
          (is-light (ns/color-is-light-p (ns/theme 'background)))
          (tint-function (if is-light 'color-darken-name 'color-lighten-name))
          (luminance (third (apply 'color-rgb-to-hsl (color-name-to-rgb name))))
          ;; original:
          ;; (l-percent (* percent (if is-light luminance (- 1 luminance))))
          (bg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'background)))))
          (fg-l (third (apply 'color-rgb-to-hsl (color-name-to-rgb (ns/theme 'foreground)))))
          (l-percent (* percent (if is-light (- luminance bg-l) (- fg-l luminance))))
          )
    (funcall tint-function name l-percent)))

;; enforce a minimum contrast
(defun ns/contrast-color (name contrast op)
  (let ((result name))
    (while (and
             (not (string= (funcall op result 1) result))
             (<
               ;; here: need some way to make sure a color is above the background
               (color-cie-de2000
                 (apply 'color-srgb-to-lab (color-name-to-rgb result))
                 (apply 'color-srgb-to-lab (color-name-to-rgb (ns/theme 'background)))
                 )
               contrast))
      (setq result (funcall op result 1)))
    result))

(color-cie-de2000
  (apply 'color-srgb-to-lab (color-name-to-rgb "#111111111111" ))
  (apply 'color-srgb-to-lab (color-name-to-rgb "#cccccccccccc" )))

(defun ns/fade-min-contrast (name percent contrast)
  (ns/contrast-color (ns/fade-color name percent) contrast 'ns/bolden-color))

(defun ns/bolden-min-contrast (name percent contrast)
  (ns/contrast-color (ns/bolden-color name percent) contrast 'ns/fade-color))

(defun ns/theme (key)
  (pcase key
    ('foreground (ns/convert-color "#444444"))
    ('background (ns/convert-color "#eeeeee"))
    ('star (ns/convert-color "#9999cc"))
    ;; ('background (ns/convert-color "#1e1e1e"))
    ;; ('foreground (ns/convert-color "#a7a7a7"))
    ;; ('star (ns/convert-color "#a7a7a7"))
    ))


(let* (
        (background  (ns/theme 'background))
        (foreground (ns/theme 'foreground))
        (star (ns/theme 'star))
        (theme
          `(
             ;; note: fade values might need to change based on light/dark emthasis color, fg bg
             :base00 ,background                           ;; Default Background

             ;; you'll want to match these
             :base01 ,(ns/fade-min-contrast star 100 2) ;; Lighter Background (Used for status bars)
             :base02 ,(ns/fade-min-contrast star 100 2) ;; Selection Background

             :base03 ,(ns/fade-color foreground 20)        ;; Comments, Invisibles, Line Highlighting
             :base04 ,foreground                           ;; Dark Foreground (Used for status bars)
             :base05 ,foreground                           ;; Default Foreground, Caret, Delimiters, Operators
             :base06 ,foreground                           ;; Light Foreground (Not often used)
             :base07 ,background                           ;; Light Background (Not often used)
             :base08 ,(ns/fade-color star 15)              ;; Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
             :base09 ,foreground                           ;; Integers, Boolean, Constants, XML Attributes, Markup Link Url

             :base0A ,(ns/fade-color star 20)              ;; Classes, Markup Bold, Search Text Background

             :base0B ,star                                 ;; Strings, Inherited Class, Markup Code, Diff Inserted
             :base0C ,foreground                           ;; Support, Regular Expressions, Escape Characters, Markup Quotes

             ;; prompt
             :base0D ,(ns/bolden-color star 30)            ;; Functions, Methods, Attribute IDs, Headings

             :base0E ,star                                 ;; Keywords, Storage, Selector, Markup Italic, Diff Changed
             :base0F ,foreground                           ;; Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>
             )))

  (deftheme neea)
  (base16-theme-define 'neea theme)
  )

(provide-theme 'base16-neeatheme)

(provide 'neea-theme)
;;; neea-theme.el ends here
