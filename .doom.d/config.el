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

(setq user-full-name "Thanawat Techaumnuaiwit"
      user-mail-address "thanawat@ucsb.edu")
(set-email-account! "umail"
                    '((user-mail-address      . "thanawat@ucsb.edu")
                      (smtpmail-smtp-user     . "thanawat@ucsb.edu")
                      (user-full-name         . "Thanawat Techaumnuaiwit")
                      (smtpmail-smtp-server   . "smtp.gmail.com")
                      (smtpmail-smtp-service  . 587)
                      (smtpmail-stream-type   . starttls)
                      (smtpmail-debug-info    . t)
                      (mu4e-drafts-folder     . "/Drafts")
                      (mu4e-refile-folder     . "/Archive")
                      (mu4e-sent-folder       . "/Sent Items")
                      (mu4e-trash-folder      . "/Deleted Items")
                      (mu4e-update-interval   . 1800)))

(setq doom-font (font-spec :family "Hasklug Nerd Font Mono" :size 18))
(after! pretty-code
  (setq +pretty-code-hasklig-font-name "Hasklug Nerd Font"))
(setq tab-width 2)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package! all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(setq doom-theme 'doom-nord)

(setq evil-escape-key-sequence "fd")

(setq org-directory "~/org/")

(after! org
  (add-to-list 'org-capture-templates '("h" "Homework" entry (file "~/org/homework.org" ) "* TODO %?\n  %i\n  %a"))
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-re-reveal-root "~/reveal.js/")
  )

(use-package ob-mermaid
  :config
  (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc"))

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

(use-package! ein
  :config
  (setq ein:output-area-inlined-images t))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

(after! elfeed
  (setq elfeed-search-filter "@1-month-ago +unread"))
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! lsp-mode
  (setq lsp-vetur-format-options-tab-size 4))

(after! cc-mode
  (setq c-basic-offset 2)
  (setq tab-width 2))

(setq python-shell-interpreter "python3"
     flycheck-python-pycompile-executable "python3")
