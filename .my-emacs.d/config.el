  (setq inhibit-startup-message t)
  ;; TODO figure out how to calculate this path
  (add-to-list 'Info-directory-list (expand-file-name "share/info" (file-name-directory (shell-command-to-string "printf %s \"$(dirname $(realpath $(which emacs)))\""))))
  (require 'use-package)
  (setq straight-use-package-by-default t)
  (use-package restart-emacs
  :init
  (setq restart-emacs-restore-frames t)
  )
  ;; TODO Add the "nix-store" info path to the search directory

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  (menu-bar-mode -1)            ; Disable the menu bar

   ;; TODO copy over the right font
   (set-face-attribute 'default nil
			  :font "Hasklug Nerd Font Mono"
			  :height 170)

  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;;(setq custom-safe-themes t)
    (load-theme 'doom-one t)
  ;; treat all themes as safe so I don't get promts everytime I switch theme
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

    (use-package dashboard
      :ensure t
      :config
      (dashboard-setup-startup-hook)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

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
    ;; TODO figure out how to configure
  (use-package orderless
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  ;; TODO Add style dispatchers
    (setq completion-styles '(orderless)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles partial-completion)))))
  ;; TODO make sure to bind the consult commands
  (use-package consult
    :after vertico
    :bind ("C-s" . consult-line)

    ;; TODO customize project root function
    )

  (use-package corfu
    ;; Optional customizations
     :custom
     (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
     (corfu-auto t)                 ;; Enable auto completion
     (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
     (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; You may want to enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since dabbrev can be used globally (M-/).
    :init
    (corfu-global-mode))
    ;; Add extensions
    (use-package cape
    ;; TODO I might rebind these later
    :bind (("C-c p p" . completion-at-point) ;; capf
	    ("C-c p t" . complete-tag)        ;; etags
	    ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	    ("C-c p f" . cape-file)
	    ("C-c p k" . cape-keyword)
	    ("C-c p s" . cape-symbol)
	    ("C-c p a" . cape-abbrev)
	    ("C-c p i" . cape-ispell)
	    ("C-c p l" . cape-line)
	    ("C-c p w" . cape-dict)
	    ("C-c p \\" . cape-tex)
	    ("C-c p _" . cape-tex)
	    ("C-c p ^" . cape-tex)
	    ("C-c p &" . cape-sgml)
	    ("C-c p r" . cape-rfc1345))
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

    ;; Use dabbrev with Corfu!
  (use-package dabbrev
    :straight nil
    ;; Swap M-/ and C-M-/
    :bind (("M-/" . dabbrev-completion)
	   ("C-M-/" . dabbrev-expand)))

  ;; A few more useful configurations...
  (use-package emacs
    :straight nil
    :init
    ;; TAB cycle if there are only few candidates
    (setq completion-cycle-threshold 3)

    ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
    ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable indentation+completion using the TAB key.
    ;; `completion-at-point' is often bound to M-TAB.
    (setq tab-always-indent 'complete))

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
    "f" '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file"))

  (tt/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    ;;"tw" 'whitespace-mode
    "tt" '(load-theme :which-key "choose theme")
    "tv" '(visual-line-mode :which-key "line wrapping")

  )

  (tt/leader-keys
      "b" '(:ignore t :which-key "buffers")
      "bb" '(switch-to-buffer :which-key "switch to buffer")
      "bd" '(kill-this-buffer :which-key "kill this buffer")
      "bk" '(kill-buffer :which-key "kill buffer")
  )
  (tt/leader-keys "w" '(evil-window-map :which-key "window"))

      (use-package evil
	:init
	(setq evil-want-keybinding nil)
	:config
	(evil-mode 1)

    )
      (use-package evil-collection
	:after evil
	;;:disabled  
	 :init
	 (evil-collection-init '(calendar dired calc info))
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
	(evil-org-set-key-theme '(navigation insert textobjects additional calendar return))
	(evil-org-agenda-set-keys))
    (use-package evil-escape
      :straight  (:host github :repo "hlissner/evil-escape")
      :after evil
  ;;    :hook (evil-mode . evil-escape-mode)
      :init
	(evil-escape-mode)
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
      :straight (:type built-in)
      :defer t
      :config
      ;;(setq org-modules
	  ;;  '(org-crypt
	   ;;   org-habit
	    ;;  org-tempo))
	(setq org-directory "~/org/"
	    org-agenda-files '("~/org/gtd/inbox.org" "~/org/gtd/tickler.org" "~/org/gtd/gtd.org" "~/org/gtd/habits.org")
	    ;;org-re-reveal-root "/home/thanawat/reveal.js/"
	    org-export-with-toc nil
	    org-hide-emphasis-markers t
	    org-log-into-drawer t
	    org-log-done 'time
	    org-ellipsis " ▾"
	    org-export-with-section-numbers nil
	    )
	(setq org-agenda-window-setup 'current-window)

	(setcar (nthcdr 4 org-emphasis-regexp-components) 10)
	(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
	(setq org-capture-templates
	  '(("t" "Todos" entry (file+headline "gtd/inbox.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	    ("T" "Tickler" entry (file+headline "gtd/tickler.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	    ("r" "Resources" entry (file+headline "gtd/resources.org" "Inbox") "* TODO %?" :prepend t)
	    ("e" "Emacs + Vim tricks" entry (file+headline "emacs-tips.org" "Inbox") "* TODO %?" :prepend t)))
    ;; TODO I might remove this to use tempel package instead
	(require 'org-tempo)
	(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
	(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
	(add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
    )

  (use-package org-modern
    :after org
    :hook ((org-mode . org-modern-mode) (org-agenda-finalize . org-modern-agenda))
    )

  (use-package org-appear
    :after org
    :straight (:type git :host github :repo "awth13/org-appear")
    :hook (org-mode . org-appear-mode))

  (use-package org-recur
    :hook ((org-mode . org-recur-mode)
	 (org-agenda-mode . org-recur-agenda-mode))
    ;; TODO make this specific to org-recur mode
    :config

    (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)
    (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)
    (setq org-recur-finish-done t
	org-recur-finish-archive t)
    )
