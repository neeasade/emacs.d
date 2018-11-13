    (ns/guard ns/enable-home-p)
  (use-package twittering-mode
    :commands (twittering-start)
    :init
    (add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode)))
    :config
    (setq-ns twittering
	     use-master-password t
	     icon-mode t
	     use-icon-storage t
	     ;; icon-storage-file (concat joe-emacs-temporal-directory "twittering-mode-icons.gz")
	     convert-fix-size 52
	     initial-timeline-spec-string '(":home")
	     edit-skeleton 'inherit-any
	     display-remaining t
	     timeline-header  ""
	     timeline-footer  ""
	     status-format "%i  %S, %RT{%FACE[bold]{%S}} %@  %FACE[shadow]{%p%f%L%r}\n%FOLD[        ]{%T}\n"
	     )

    ;; set the new bindings
    (bind-keys :map twittering-mode-map
	       ("\\" . hydra-twittering/body)
	       ("q" . twittering-kill-buffer)
	       ("Q" . twittering-edit-mode)
	       ("j" . twittering-goto-next-status)
	       ("k" . twittering-goto-previous-status)
	       ("h" . twittering-switch-to-next-timeline)
	       ("l" . twittering-switch-to-previous-timeline)
	       ("g" . beginning-of-buffer)
	       ("G" . end-of-buffer)
	       ("t" . twittering-update-status-interactive)
	       ("X" . twittering-delete-status)
	       ("RET" . twittering-reply-to-user)
	       ("r" . twittering-native-retweet)
	       ("R" . twittering-organic-retweet)
	       ("d" . twittering-direct-message)
	       ("u" . twittering-current-timeline)
	       ("b" . twittering-favorite)
	       ("B" . twittering-unfavorite)
	       ("f" . twittering-follow)
	       ("F" . twittering-unfollow)
	       ("i" . twittering-view-user-page)
	       ("/" . twittering-search)
	       ("." . twittering-visit-timeline)
	       ("@" . twittering-other-user-timeline)
	       ("T" . twittering-toggle-or-retrieve-replied-statuses)
	       ("o" . twittering-click)
	       ("TAB" . twittering-goto-next-thing)
	       ("<backtab>" . twittering-goto-previous-thing)
	       ("n" . twittering-goto-next-status-of-user)
	       ("p" . twittering-goto-previous-status-of-user)
	       ("SPC" . twittering-scroll-up)
	       ("S-SPC" . twittering-scroll-down)
	       ("y" . twittering-push-uri-onto-kill-ring)
	       ("Y" . twittering-push-tweet-onto-kill-ring)
	       ("a" . twittering-toggle-activate-buffer)))

  (defhydra hydra-twittering (:color blue :hint nil)
    "
      ╭────────────┐
      Tweets                User                        Timeline     │ Twittering │
      ╭─────────────────────────────────────────────────────────────────┴────────────╯
      _k_  [_t_] post tweet      _p_  [_f_] follow                  ^_g_^      [_u_] update
      ^↑^  [_X_] delete tweet    ^↑^  [_F_] unfollow              ^_S-SPC_^    [_._] new
      ^ ^  [_r_] retweet         ^ ^  [_d_] direct message          ^^↑^^      [^@^] current user
      ^↓^  [_R_] retweet & edit  ^↓^  [_i_] profile (browser)   _h_ ←   → _l_  [_a_] toggle
      _j_  [_b_] favorite        _n_   ^ ^                          ^^↓^^
      ^ ^  [_B_] unfavorite      ^ ^   ^ ^                         ^_SPC_^
      ^ ^  [_RET_] reply         ^ ^   ^ ^                          ^_G_^
      ^ ^  [_T_] show Thread
      ^ ^  [_y_] yank url          Items                     Do
      ^ ^  [_Y_] yank tweet     ╭───────────────────────────────────────────────────────
      ^ ^  [_e_] edit mode        _<backtab>_ ← _o_pen → _<tab>_    [_q_] exit
      ^ ^   ^ ^                   ^         ^   ^ ^      ^     ^    [_/_] search
      --------------------------------------------------------------------------------
      "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("q"          twittering-kill-buffer)
    ("e"          twittering-edit-mode)
    ("j"          twittering-goto-next-status :color red)
    ("k"          twittering-goto-previous-status :color red)
    ("h"          twittering-switch-to-next-timeline :color red)
    ("l"          twittering-switch-to-previous-timeline :color red)
    ("g"          beginning-of-buffer)
    ("G"          end-of-buffer)
    ("t"          twittering-update-status-interactive)
    ("X"          twittering-delete-status)
    ("RET"        twittering-reply-to-user)
    ("r"          twittering-native-retweet)
    ("R"          twittering-organic-retweet)
    ("d"          twittering-direct-message)
    ("u"          twittering-current-timeline)
    ("b"          twittering-favorite)
    ("B"          twittering-unfavorite)
    ("f"          twittering-follow)
    ("F"          twittering-unfollow)
    ("i"          twittering-view-user-page)
    ("/"          twittering-search)
    ("."          twittering-visit-timeline)
    ("@"          twittering-other-user-timeline)
    ("T"          twittering-toggle-or-retrieve-replied-statuses)
    ("o"          twittering-click)
    ("<tab>"      twittering-goto-next-thing :color red)
    ("<backtab>"  twittering-goto-previous-thing :color red)
    ("n"          twittering-goto-next-status-of-user :color red)
    ("p"          twittering-goto-previous-status-of-user :color red)
    ("SPC"        twittering-scroll-up :color red)
    ("S-SPC"      twittering-scroll-down :color red)
    ("y"          twittering-push-uri-onto-kill-ring)
    ("Y"          twittering-push-tweet-onto-kill-ring)
    ("a"          twittering-toggle-activate-buffer))

  ;; (ns/bind "at" 'twittering-start)
