(setq inhibit-startup-message t)
(require 'use-package)
(setq straight-use-package-by-default t)
(use-package restart-emacs
:init
(setq restart-emacs-restore-frames t)
)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(load-theme 'wombat)

;; TODO copy over the right font
(set-face-attribute 'default nil
		       :font "Hasklug Nerd Font Mono"
		       :height 170)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
;; treat all themes as safe so I don't get promts everytime I switch theme
  (setq custom-safe-themes t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package vertico
:init
(vertico-mode))

(use-package savehist
:straight nil
 :init
 (savehist-mode))

(use-package marginalia
:after vertico
:custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package general
    :config
    ;;(general-evil-setup t)

    (general-create-definer tt/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")
  )
  
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(tt/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  ;;"tw" 'whitespace-mode
  "tt" '(load-theme :which-key "choose theme"))

(use-package evil
    :init
    (setq evil-want-keybinding t)
    :config
    (evil-mode 1)

)
  (use-package evil-collection
    :after evil
    :disabled  
    ;; :init
    ;; (evil-collection-init 'dired)
    )
  (use-package evil-easymotion
    :after evil
    :config
    ;; TODO fix this
    (evilem-default-keybindings "C-;"))
  (use-package evil-org
    :after org
    :hook (org-mode . evil-org-mode)
    :init
    (setq evil-org-use-additional-insert t)
    :config
    (require 'evil-org)
    (require 'evil-org-agenda)
    (evil-org-set-key-theme '(navigation insert textobjects additional calendar))

    (evil-org-agenda-set-keys))
(use-package evil-escape
  :straight  (:host github :repo "hlissner/evil-escape")
  :hook (evil-mode . evil-escape-mode)
  :init
   (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
	;;evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
	evil-escape-key-sequence "fd"
	evil-escape-delay 0.15)
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
(doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil)
  )

(use-package org
;; TODO add a straight recipe to get the latest org-version
  :straight nil
  :defer t
  :config
  (setq org-modules
	'(org-crypt
	  org-habit
	  org-tempo))
    (setq org-directory "~/org/"
	org-agenda-files '("~/org/gtd/inbox.org" "~/org/gtd/tickler.org" "~/org/gtd/gtd.org" "~/org/gtd/habits.org")
	;;org-re-reveal-root "/home/thanawat/reveal.js/"
	org-export-with-toc nil
	org-hide-emphasis-markers t
	org-log-into-drawer t
	org-log-done 'time
	org-export-with-section-numbers nil)
    (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
    (setq org-capture-templates
      '(("t" "Todos" entry (file+headline "gtd/inbox.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	("T" "Tickler" entry (file+headline "gtd/tickler.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	("r" "Resources" entry (file+headline "gtd/resources.org" "Inbox") "* TODO %?" :prepend t)
	("e" "Emacs + Vim tricks" entry (file+headline "emacs-tips.org" "Inbox") "* TODO %?" :prepend t)))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
)
