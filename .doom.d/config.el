;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


(set-popup-rules!
  '(("^\\*info\\*" :slot 2 :side left :width 85 :quit nil)))

(after! dante
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))



(setq user-full-name "Thanawat Techaumnuaiwit")
(after! mu4e
  (setq!  mu4e-get-mail-command "mbsync -c ~/.config/mbsync/mbsyncrc -a"))
(set-email-account! "ucsb"
                    '((user-mail-address      . "thanawat@ucsb.edu")
                      (smtpmail-smtp-user     . "thanawat@ucsb.edu")
                      (mu4e-drafts-folder     . "/ucsb/[acc1].Drafts")
                      (mu4e-refile-folder     . "/ucsb/[acc1].Archive")
                      (mu4e-sent-folder       . "/ucsb/[acc1].Sent Mail")
                      (mu4e-trash-folder      . "/ucsb/[acc1].Trash")
                      (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                      (smtpmail-default-smtp-server . "smtp.gmail.com")
                      (smtpmail-smtp-server . "smtp.gmail.com")
                      (smtpmail-smtp-service . 587)
                      (mu4e-compose-signature . "---\nThanawat Techaumnuaiwit"))
                    t)

(set-email-account! "personal-gmail"
  '((mu4e-sent-folder       . "/personal-gmail/[acc2].Sent Mail")
    (mu4e-drafts-folder     . "/personal-gmail/[acc2].Drafts")
    (mu4e-trash-folder      . "/personal-gmail/[acc2].Trash")
    (mu4e-refile-folder     . "/personal-gmail/[acc2].Archive")
    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
    (smtpmail-smtp-user     . "thanatechaumnuaiwit@gmail.com")
    (user-mail-address      . "thanatechaumnuaiwit@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nThanawat Techaumnuaiwit"))
  nil)

;; (setq doom-font (font-spec :family "Mononoki Nerd Font" :size 18))
(setq doom-font (font-spec :family "Hasklug Nerd Font Mono" :size 18))
(after! pretty-code
  (setq +pretty-code-hasklig-font-name "Hasklug Nerd Font"))
(setq tab-width 2)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)
;; Makes visual-lines work better
(setq visual-fill-column-center-text t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; source https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
;; I'm not very well versed in elisp, so I'm not sure what's happening
(defun my/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(map! "C-c o o" 'my/open-with)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))
;; (use-package ewal-doom-themes
;;   :config (progn
;;             (load-theme 'ewal-doom-vibrant t)
;;             (enable-theme 'ewal-doom-vibrant)))
(setq doom-theme 'doom-nord)

;; explcitly set the frametitle because otherwise the frame title would show weird characters
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format "%b - Doom Emacs")

(setq evil-escape-key-sequence "fd")
(map! :leader
      :desc "rss" "o s" #'=rss)
;; Make evil-mode up/down operate in screen lines instead of logical lines
;; (define-key evil-motion-state-map "j" 'evil-next-visual-line)
;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
;; (define-key evil-visual-state-map "j" 'evil-next-visual-line)
;; (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(setq org-directory "~/org/")

(after! org
  ;; TODO refactor!
  (setq org-capture-templates (append org-capture-templates
    '(("h" "Homework" entry (file "~/org/homework.org" ) "* TODO %?\n  %i\n") ("b" "Blog idea" entry (file "~/org/blog-ideas.org" ) "* TODO %?\n  %i\n"))))
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-re-reveal-root "/home/thanawat/reveal.js/")
  ;; (add-to-list 'org-modules 'org-habit)
  ;; Enables notifications for org-agenda
  (require 'appt)
  (require 'notifications)
  (setq appt-time-msg-list nil)    ;; clear existing appt list
  (setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
  (setq
    appt-message-warning-time '15  ;; send first warning 15 minutes before appointment
    appt-display-mode-line nil     ;; don't show in the modeline
    appt-display-format 'window)   ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function ct/appt-display-native))

  (appt-activate 1)                ;; activate appointment notification
  ; (display-time) ;; Clock in modeline
  (defun ct/appt-display-native (min-to-app new-time msg)
    (notifications-notify
           :title (format "Event in %s minutes" min-to-app) ; Title
           :body (format "%s" msg)
           :urgency 'normal
           ))
  ;; Agenda-to-appointent hooks
  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  )

(use-package! anki-editor
  :config
  (setq anki-editor-create-decks t))

(map! :localleader
      :map org-mode-map
      (:prefix ("k" . "Anki")
        :desc "Push" "p" 'anki-editor-push-notes
        :desc "Retry" "r" 'anki-editor-retry-failure-notes
        :desc "Insert" "n" 'anki-editor-insert-note
        (:prefix ("c" . "Cloze")
          :desc "Dwim" "d" 'anki-editor-cloze-dwim
          :desc "Region" "r" 'anki-editor-cloze-region
          )
        )
 )

(use-package org-journal
  :config
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-enable-agenda-integration t)
  )
(map! :leader
      :desc "New scheduled entry" "n j J" #'org-journal-new-scheduled-entry)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . mixed-pitch-mode)
  :hook (nov-mode . visual-line-mode)
  :hook (nov-mode . visual-fill-column-mode)
  :config
  (setq nov-text-width t)
  (setq nov-variable-pitch nil))

(use-package! elfeed
  :config
    (setq elfeed-search-filter "@1-month-ago +unread +daily")
    (defun elfeed-v-mpv (url)
    "Watch a video from URL in MPV"
    (async-shell-command (format "mpv \"%s\"" url)))

    (defun elfeed-view-mpv (&optional use-generic-p)
    "Youtube-feed link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
        (cl-loop for entry in entries
        do (elfeed-untag entry 'unread)
        when (elfeed-entry-link entry)
        do (elfeed-v-mpv it))
        (mapc #'elfeed-search-update-entry entries)
        (unless (use-region-p) (forward-line))))

    (define-key elfeed-search-mode-map (kbd "M-v") 'elfeed-view-mpv)
    )
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t))

(after! cc-mode
  (setq c-basic-offset 2)
  (setq tab-width 2))

(setq python-shell-interpreter "python3"
     flycheck-python-pycompile-executable "python3")

(add-hook! 'rainbow-mode-hook
(hl-line-mode (if rainbow-mode -1 +1)))

