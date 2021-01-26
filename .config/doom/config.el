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

(setq user-full-name "Thanawat Techaumnuaiwit")
(after! mu4e
  (setq!  mu4e-get-mail-command "mbsync -c ~/.config/mbsync/mbsyncrc -a")
  (setq mu4e-change-filenames-when-moving t)
  )
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
                      (mu4e-compose-signature . "Best,\n Thanawat Techaumnuaiwit"))
                    t)

(set-email-account! "personal-gmail"
  '((mu4e-sent-folder       . "/personal-gmail/[acc2].Sent Mail")
    (mu4e-drafts-folder     . "/personal-gmail/[acc2].Drafts")
    (mu4e-trash-folder      . "/personal-gmail/[acc2].Trash")
    (mu4e-refile-folder     . "/personal-gmail/[acc2].Archive")
    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
    (smtpmail-default-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-user     . "thanatechaumnuaiwit@gmail.com")
    (user-mail-address      . "thanatechaumnuaiwit@gmail.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "You can find me at https://thanawat.xyz\n---\nThanawat Techaumnuaiwit"))
  nil)

(setq doom-font (font-spec :family "Hasklug Nerd Font Mono" :size 18))
(after! ispell
  (setq ispell-dictionary "en"))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'nil)
;; Makes visual-lines work better
(setq visual-fill-column-center-text t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-material)

;; explcitly set the frametitle because otherwise the frame title would show weird characters
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format "%b - Doom Emacs")

(setq evil-escape-key-sequence "fd")
(map! :leader
      :desc "rss" "o s" #'=rss)
;;Make evil-mode up/down operate in screen lines instead of logical lines
;; (define-key evil-motion-state-map "j" 'evil-next-visual-line)
;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
 ;;Also in visual mode
;; (define-key evil-visual-state-map "j" 'evil-next-visual-line)
;; (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(after! org
  (setq org-directory "~/org/"
      org-agenda-files '("~/org/gtd/inbox.org" "~/org/gtd/tickler.org" "~/org/gtd/gtd.org")
      org-re-reveal-root "/home/thanawat/reveal.js/"
      org-export-with-toc nil
      org-hide-emphasis-markers t
      org-log-into-drawer t
      org-log-done 'time
      org-export-with-section-numbers nil)
)

(after! org
  (setq org-capture-templates
    '(("t" "Todos" entry (file+headline "gtd/inbox.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
      ("T" "Tickler" entry (file+headline "gtd/tickler.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
        )
    )


  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  ;; (setq org-latex-listings 'minted
  ;;     org-latex-packages-alist '(("" "minted"))
  ;;     org-latex-pdf-process
  ;;     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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

;; (use-package! ob-mermaid
;;   :config
;; (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc"))
(use-package! mermaid-mode
  :mode "\\.mmd\\'"
  :config
  (setq mermaid-mmdc-location "~/custom_packages/node_modules/.bin/mmdc"))

;; (use-package! org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-label-truncate t
;;         org-roam-server-label-truncate-length 60
        ;; org-roam-server-label-wrap-length 20))
(use-package! org-roam
  :init
    (setq org-roam-dailies-directory "daily/"
          org-roam-db-gc-threshold most-positive-fixnum
           org-id-link-to-org-use-id t))

(after! org
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
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
)

(map! :leader
      :prefix ("a" . "Personal Kbds")
      :desc "Add word to dictionary" "w" #'add-word-to-dictionary)

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
    (setq elfeed-search-filter "@1-week-ago +unread +daily")
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
    (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
    )

(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t))

(after! cc-mode
  (setq c-basic-offset 2)
  (setq tab-width 2))

(setq python-shell-interpreter "python3"
     flycheck-python-pycompile-executable "python3")

(add-hook! 'rainbow-mode-hook
(hl-line-mode (if rainbow-mode -1 +1)))

(map! :map org-present-mode-keymap
        :g [C-right] #'org-present-next
        :g [C-left]  #'org-present-prev
        )
(after! org-tree-slide (setq org-tree-slide-never-touch-face t))

(set-popup-rules!
  '(("^\\*info\\*" :slot 2 :side left :width 85 :quit nil)))

(display-battery-mode)

(use-package! org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
    org-ref-default-bibliography `,(list (concat org-directory "roam/biblio.bib"))
    reftex-default-bibliography  `,(list (concat org-directory "roam/biblio.bib")))
  )
(use-package! org-roam-bibtex
  :after org-roam

  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))

  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "lit/${slug}"
           :head ,(concat
                   "#+title:${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* Notes"
                   ":PROPERTIES:\n"
                   ":Custom_ID: ${=key=}\n"
                   ":URL: ${url}\n"
                   ":AUTHOR: ${author-abbrev}\n"
                   ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   ":NOTER_PAGE: \n"
                   ":END:\n")
           :unnarrowed t)))
)
(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/org/roam/lit"
        bibtex-completion-bibliography "~/org/roam/biblio.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )))

(after! dante
  (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))

;; Opens video file in mpv
;; using openwith for this is a kind of bloated solution, however it works
(use-package! openwith
  :after-call pre-command-hook
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.mp4\\'" "mpv" (file)) ("\\.webm\\'" "mpv" (file)) ("\\.mkv\\'" "mpv" (file))))
  )

(after! circe
(set-irc-server! "Freenode"
  `(
    :host "irc.freenode.net"
    :tls t
    :port 6697
    :nick "thiskappaisgrey"
    :nickserv-nick "thiskappaisgrey"
    :nickserv-password (lambda (&rest _) (+pass-get-secret "irc/thiskappaisgrey"))
    )))
