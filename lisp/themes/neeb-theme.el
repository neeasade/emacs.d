;; -*- lexical-binding: t; -*-
;; see file:../trees/colors.el for some notes on color spaces/resources
;; big thanks to belak for https://github.com/belak/base16-emacs

(require 'base16-theme)
(require 'color-tools)

(defun ns/neeb-get-accents (background foreground foreground_)
  ;; return a list accent1, accent1_, accent2, accent2_

  ;; "#34D3EC"
  ;; "#3BB570"
  ;; "#9A71AD"
  ;; "#1BEBD1"
  ;; "#00F4B7"
  ;; "#FE0D51"

  (-->
    (ct/rotation-hsluv
      (->
        ;; (ct/make-hsluv 265 60 40)
        ;; "#5AAF00"
        ;; "#0F9908"
        "#FE0D51"

        (ct/transform-hsluv-l
          ;; (ct/get-hsluv-l foreground_)
          (ct/get-hsluv-l
            foreground_
            ;; (ht-get ns/theme :foreground_)
            )
          )
        )

      ;; "#009404"

      ;; (ct/make-lab 55 50 50)
      ;; 60
      (/ 360 5)
      ;; (/ 360 6)
      ;; (/ 360 3)
      ;; (/ 360 10)
      )

    ;; ("#fefd47476766" "#9b828f57096b" "#0a41a12a7048" "#0c3b9a14baa5" "#d6cc4c1dfe7d")

    ;; ("#fefd9999a4a3" "#c819b8c41378" "#14adcf3491b4" "#177cc645ef3d" "#e2e79aeefeca")


    (-map (fn (ns/nth <> it))
      '(
         -1
         0
         -2
         2
         )
      )

    (-map
      (fn (ct/tint-ratio <> background 4.5 ))
      it)
    ))

(let*
  (
    (background
      ;; (ct/make-lab 98 70 80)
      ;; (ct/make-lab 88 -10 -10)
      ;; (ct/make-lab 88 -5 5)
      ;; (ct/make-lab 88 5 -5)
      ;; (ct/make-lab 20 -10 0)
      ;; (ct/make-lab 30 -5 0)
      (ct/make-lab 25 40 10)

      )

    (foreground (ct/tint-ratio background background 8.5))
    (foreground_ (ct/tint-ratio background background 5.5))

    (accents (ns/neeb-get-accents background foreground foreground_))
    (accent1  (nth 0 accents))
    (accent1_ (nth 1 accents))
    (accent2  (nth 2 accents))
    (accent2_ (nth 3 accents))

    ;; active BG (selections)
    ;; take an accent color, fade it until you reach a minimum contrast against foreground_
    (background+ "#cccccc")

    ;; new idea: these could be contrast based as well in relation to foreground
    (background_
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 7)))))

    (background__
      (-> background
        (ct/transform-lch-h (ct/get-lch-h accent2))
        (ct/transform-lch-l (ct/get-lch-l foreground))
        ((lambda (c) (ct/tint-ratio foreground c 6)))))

    )

  (setq ns/theme
    (ht
      (:foreground foreground)          ; regular text
      (:foreground_ foreground_)        ; comments
      (:foreground+ foreground)         ; foreground of a focused/highlighted thing
      ;; (:foreground+ background)         ; foreground of a focused/highlighted thing

      (:background background)          ; regular canvas
      (:background_ background_)        ; emphasis?
      (:background__ background__)      ; inactive modeline
      (:background+ background+)  ; background of a focused/highlighted thing (also active modeline)

      (:accent1 accent1)                ; identifiers
      (:accent1_ accent1_)              ; builtins
      (:accent2 accent2)                ; types
      (:accent2_ accent2_)              ; strings
      ))

  (ht-set ns/theme :background+
    (ct/iterate
      ;; accent1_
      accent2_
      ;; (ct/transform-lch-c accent1_ (-partial '* 0.5))
      ;; (ct/transform-lch-c accent2 (lambda (_) 33))
      'ct/lab-darken
      (fn (> (ct/contrast-ratio <> foreground_) 4.0))
      ;; (fn (> (ct/contrast-ratio <> foreground_) 3.5))
      )
    )

  ;; (ht-set ns/theme :background+ accent1)

  ;; (ht-set ns/theme :foreground_
  ;;   (ct/tint-ratio
  ;;     (ct/transform-hsl accent2 (lambda (h s l) (list h 80 70)))
  ;;     background
  ;;     4.5
  ;;     )
  ;;   ;; (ct/transform-hsluv-s 50)
  ;;   )


  (ht-set ns/theme :foreground_ (ct/transform-lch-c foreground_ 80))

  ;; shorten all the colors, because they are also used in EG org exports
  (setq ns/theme (ht-transform-v ns/theme 'ct/shorten)))

(deftheme neeb)
(base16-theme-define 'neeb
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

(provide-theme 'neeb)
(provide 'neeb-theme)
;;; neeb-theme.el ends here
