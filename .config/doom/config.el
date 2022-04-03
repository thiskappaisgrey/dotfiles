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
(add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e/")
(setq enable-local-variables t)

(setq doom-font (font-spec :family "Hasklug Nerd Font Mono" :size 18))
(after! ispell
  (setq ispell-dictionary "en"))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)
;; Makes visual-lines work better
(setq visual-fill-column-center-text t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-oceanic-next)

;; explcitly set the frametitle because otherwise the frame title would show weird characters
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format "%b - Doom Emacs")

(set-popup-rules!
  '(("^\\*info\\*" :slot 2 :side left :width 85 :quit nil)))

(display-battery-mode)

;; (use-package dashboard
;;   :init      ;; tweak dashboard config before loading it
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;;   (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   (setq dashboard-center-content nil) ;; set to 't' for centered content
;;   (setq dashboard-items '((recents . 5)
;;                           (agenda . 5 )
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (registers . 5)))

;;   :config
;;   (dashboard-setup-startup-hook)
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;             (bookmarks . "book")))
;;   )

(setq evil-escape-key-sequence "fd")
(map! :leader
      :desc "rss" "o s" #'=rss)
;;Make evil-mode up/down operate in screen lines instead of logical lines
;; (define-key evil-motion-state-map "j" 'evil-next-visual-line)
;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
 ;;Also in visual mode
;; (define-key evil-visual-state-map "j" 'evil-next-visual-line)
;; (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
(setq avy-keys '(?a ?s ?e ?t ?g ?y ?n ?i ?o ?h))

(map!
  :map smartparens-mode-map
  ;; smartparens maps (navigation ops)
  :nvie "C-M-f" #'sp-forward-sexp
  :nvie "C-M-b" #'sp-backward-sexp
  :nvie "C-M-u" #'sp-backward-up-sexp
  :nvie "C-M-d" #'sp-down-sexp
  ;; smartparens maps (split join slurp barf)
  :nie "M-s" #'sp-split-sexp
  :nie "M-j" #'sp-join-sexp
  :nvie "C->" #'sp-forward-slurp-sexp
  :nvie "C-<" #'sp-forward-barf-sexp
  :nvie "C-{" #'sp-backward-slurp-sexp
  :nvie "C-}" #'sp-backward-barf-sexp)

(after! org
  (setq org-directory "~/org/"
      org-agenda-files '("~/org/gtd/inbox.org" "~/org/gtd/tickler.org" "~/org/gtd/gtd.org" "~/org/gtd/habits.org")
      org-re-reveal-root "/home/thanawat/reveal.js/"
      org-export-with-toc nil
      org-hide-emphasis-markers t
      org-log-into-drawer t
      org-log-done 'time
      org-export-with-section-numbers nil)
  (add-to-list 'org-modules 'org-habit t)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
)

(after! org
  (setq org-capture-templates
        '(("t" "Todos" entry (file+headline "gtd/inbox.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
          ("T" "Tickler" entry (file+headline "gtd/tickler.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
          ("r" "Resources" entry (file+headline "gtd/resources.org" "Inbox") "* TODO %?" :prepend t)
          ("e" "Emacs + Vim tricks" entry (file+headline "emacs-tips.org" "Inbox") "* TODO %?" :prepend t)
          ;; copied from doom source code
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          )
        )

  (use-package! ox-extra
    :config
    (ox-extras-activate '(ignore-headlines latex-header-blocks))
    )
  (use-package! ox-latex
    :init
    ;; code here will run immediately
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)

    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil)))
  ;;(setq org-latex-packages-alist '(("margin=0.5in" "geometry")))
  ;; (setq org-latex-packages-alist '(("" "booktabs")))
  (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted")))
  ;;     org-latex-pdf-process
  ;;     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;       "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
           :time-grid t  ; Items that appear on the time grid
           :todo "TODAY")  ; Items that have this TODO keyword
          (:name "Important"
           :priority "A")
          ;; Set order of multiple groups at once
          (:name "Shopping"
           :tag "shopping")
          (:name "Recipes"
           ;; Multiple args given in list with implicit OR
           :tag ("food"))
          (:name "Habits"
           :habit t)
          (:name "School"
           :tag "school")
          (:name "coding"
           :tag "coding")

          (:name "next"
           :tag "next"
           :scheduled nil)
          ;; Groups supply their own section names when none are given
          (:todo "WAITING" :order 8)  ; Set order of this section
          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
           ;; Show this group at the end of the agenda (since it has the
           ;; highest number). If you specified this group last, items
           ;; with these todo keywords that e.g. have priority A would be
           ;; displayed in that group instead, because items are grouped
           ;; out in the order the groups are listed.
           :order 9)
          (:priority<= "B"
           ;; Show this section after "Today" and "Important", because
           ;; their order is unspecified, defaulting to 0. Sections
           ;; are displayed lowest-number-first.
           :order 1)
          ;; After the last group, the agenda will display items that didn't
          ;; match any of these groups, with the default order position of 99
          ))

  :config (org-super-agenda-mode))
(after! (org-agenda org-super-agenda)
  (setq! org-super-agenda-header-map (make-sparse-keymap))
  (setq! org-agenda-custom-commands '(("h" "my custom agenda view"
                                       ((alltodo "" ((org-agenda-overriding-header "")
                                                     (org-super-agenda-groups
                                                      '(
                                                        (:name "Important"
                                                         :priority "A")
                                                        (:name "Projects"
                                                         :todo "PROJ"
                                                         :children t
                                                         :order 1)
                                                        (:name "To Process"
                                                         :file-path "inbox\\.org"
                                                         :order 2)
                                                        (:name "School"
                                                         :tag "school"
                                                         :order 3)
                                                        (:order-multi (2 (:name "Shopping for Food items"
                                                                          ;; Boolean AND group matches items that match all subgroups
                                                                          :and (:tag "shopping"))
                                                                         (:name "Food and cooking"
                                                                          ;; Multiple args given in list with implicit OR
                                                                          :tag ("food" "cooking"))))
                                                        (:discard (:anything t))
                                                        )))))
                                       )))

  )
(map! :leader "a" #'org-agenda)
(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(use-package! anki-editor
  :config
  (setq anki-editor-create-decks t))

;; (map! :localleader
;;       :map org-mode-map
;;       (:prefix ("k" . "Anki")
;;         :desc "Push" "p" 'anki-editor-push-notes
;;         :desc "Retry" "r" 'anki-editor-retry-failure-notes
;;         :desc "Insert" "n" 'anki-editor-insert-note
;;         (:prefix ("c" . "Cloze")
;;           :desc "Dwim" "d" 'anki-editor-cloze-dwim
;;           :desc "Region" "r" 'anki-editor-cloze-region
;;           )
;;         )
;;  )

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
  ;; (map! :leader
  ;;       :prefix "r"
  ;;       :desc "org-roam" "l" #'org-roam-buffer-toggle
  ;;       :desc "org-roam-node-insert" "i" #'org-roam-node-insert
  ;;       :desc "org-roam-node-find" "f" #'org-roam-node-find
  ;;       :desc "org-roam-ref-find" "r" #'org-roam-ref-find
  ;;       :desc "org-roam-show-graph" "g" #'org-roam-show-graph
  ;;       :desc "org-roam-capture" "c" #'org-roam-capture
  ;;       :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  ;; (setq org-roam-directory (concat org-directory "roam")
  ;;       org-roam-db-gc-threshold most-positive-fixnum
  ;;       org-id-link-to-org-use-id t)
  ;; (add-to-list 'display-buffer-alist
  ;;              '(("\\*org-roam\\*"
  ;;                 (display-buffer-in-direction)
  ;;                 (direction . right)
  ;;                 (window-width . 0.33)
  ;;                 (window-height . fit-window-to-buffer))))
  :config

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
            :immediate-finish t
           :unnarrowed t)
          ("l" "lit" entry
           "* %?"
           :if-new (file+head "lit/${slug}.org"
                              "#+title: ${title}\n")
            :clock-in
            :clock-keep
            :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))


  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-db-location (expand-file-name "roam/org-roam.db" org-directory))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))
  ;; (set-company-backend! 'org-mode '(company-capf))
  )

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

(setq  org-cite-csl-styles-dir "~/Zotero/styles"
       citar-bibliography "~/org/roam/biblio.bib"
       citar-notes-paths '("~/org/roam/lit")
       org-cite-global-bibliography '("~/org/roam/biblio.bib")
       )

(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :config

  (setq org-recur-finish-done t
        org-recur-finish-archive t))
(map! :map org-recur-mode-map
        :after org-recur
        :g "C-c d" #'org-recur-finish)

(map! :map org-recur-agenda-mode-map
        :after org-recur
        :g "C-c d" #'org-recur-finish)

;; Sets the languagetool java class path to the correct place
;;(setq langtool-java-classpath (concat (shell-command-to-string "nix eval --raw nixos.languagetool") "/share/*"))
(let ((server (executable-find "languagetool-commandline")))
    (if server (setq langtool-bin server)))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . mixed-pitch-mode)
  :hook (nov-mode . visual-line-mode)
  :hook (nov-mode . visual-fill-column-mode)
  :config
  (setq nov-text-width t)
  (setq nov-variable-pitch nil))

(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +unread +daily")
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  )
(defun elfeed-v-mpv (url)
  "Watch a video from URL in MPV"
  (async-shell-command (format "mpv \"%s\"" url)))

(defun my/elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (elfeed-v-mpv it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))
(map! :map elfeed-search-mode-map
      :after elfeed
      :g "M-v" #'my/elfeed-view-mpv
      )

;;(use-package! lsp-ui
 ;; :config
  ;;(setq lsp-ui-sideline-show-hover t))
;; (use-package! lsp
;;  :config
;;   (setq lsp-enable-symbol-highlighting 'nil))

(after! cc-mode
  (setq c-basic-offset 2)
  (setq tab-width 2))

(setq python-shell-interpreter "python3"
      flycheck-python-pycompile-executable "python3")
;;(use-package! lsp-python-ms
  ;;:init
  ;;(setq lsp-python-ms-executable (executable-find "python-language-server")))

(add-hook! 'rainbow-mode-hook
(hl-line-mode (if rainbow-mode -1 +1)))

;; (after! dante
;;   (add-to-list 'flycheck-disabled-checkers 'haskell-hlint))
(after! lsp-haskell
  (setq lsp-haskell-server-path "haskell-language-server"))

(use-package! scad-mode
  :mode "\\.scad$")

(use-package! kbd-mode
  :mode ("\\.kbd\\'" . kbd-mode))

(use-package! graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(map! :map org-present-mode-keymap
        :g [C-right] #'org-present-next
        :g [C-left]  #'org-present-prev
        )
(after! org-tree-slide (setq org-tree-slide-never-touch-face t))

;; Opens video file in mpv
;; using openwith for this is a kind of bloated solution, however it works
(use-package! openwith
  :after-call pre-command-hook
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.mp4\\'" "mpv" (file)) ("\\.webm\\'" "mpv" (file)) ("\\.mkv\\'" "mpv" (file))))
  )

(after! circe
(set-irc-server! "irc.libera.chat"
  `(
    :tls t
    :port 6697
    :nick "thiskappaisgrey"
    :nickserv-nick "thiskappaisgrey"
    :channels ("#haskell-language-server" "#emacs")
    :nickserv-password (lambda (&rest _) (+pass-get-secret "irc/libera.chat"))
    )))
