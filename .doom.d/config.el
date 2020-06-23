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

(setq doom-theme 'doom-nord)

;; explcitly set the frametitle because otherwise the frame title would show weird characters
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format "%b - Doom Emacs")

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
  (add-to-list 'org-modules 'org-habit)
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

(after! org-roam
  (setq org-id-link-to-org-use-id t))
(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(after! org-journal
  (setq org-journal-file-type 'weekly)
  )

(use-package! ein
  :config
  (setq ein:output-area-inlined-images t))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places"))
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  )

(after! elfeed
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

(after! lsp-mode
  (setq lsp-vetur-format-options-tab-size 4))

(after! cc-mode
  (setq c-basic-offset 2)
  (setq tab-width 2))

 (setq python-shell-interpreter "python3"
      flycheck-python-pycompile-executable "python3")

(after! rgb
(add-hook! 'rainbow-mode-hook
(hl-line-mode (if rainbow-mode -1 +1)))
)
