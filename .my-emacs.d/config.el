;; Make Emacs startup faster
(setq gc-cons-threshold (* 50 1000 1000))
;; profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

;; use the gcmh package to make startup faster
(straight-use-package '(gcmh :type git :host gitlab :repo "koral/gcmh"))
(gcmh-mode 1)

(setq inhibit-startup-message t)
;; TODO figure out how to calculate this path
(add-to-list 'Info-directory-list (expand-file-name "share/info" (file-name-directory (shell-command-to-string "printf %s \"$(dirname $(realpath $(which emacs)))\""))))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
;; TODO Add the "nix-store" info path to the search directory
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
(setq native-comp-async-report-warnings-errors nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
  (require 'setup)
  ;; copied from
  ;; (defun tt/filter-straight-recipe (recipe)
  ;;   (let* ((plist (cdr recipe))
  ;; 	 (name (plist-get plist :straight)))
  ;;     (cons (if (and name (not (equal name t)))
  ;; 	      name
  ;; 	    (car recipe))
  ;; 	  (plist-put plist :straight nil))))
  (setup-define :pkg
    (lambda (&rest recipe)
           ;; `(straight-use-package ',(tt/filter-straight-recipe recipe)))
`(unless (straight-use-package ',recipe)
       ,(setup-quit)))
    :documentation "Install RECIPE via straight.el"
    :shorthand #'cadr)


  (setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
		     (require ',(setup-get 'feature))
		     ,@body)))
	(dolist (feature (if (listp features)
			     (nreverse features)
			   (list features)))
	  (setq body `(with-eval-after-load ',feature ,body)))
	body))
    :documentation "Load the current feature after FEATURES."
    :indent 1)

  (setup-define :disabled
    (lambda ()
      `,(setup-quit))
    :documentation "Always stop evaluating the body.")

  (setup-define :delay
    (lambda (&rest time)
      `(run-with-idle-timer ,(or time 1)
			    nil ;; Don't repeat
			    (lambda () (require ',(setup-get 'feature)))))
    :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(let ((normal-keybindings '(("?" "meow-keypad-describe-key") (1 "meow-expand-1") (2 "meow-expand-2") (3 "meow-expand-3") (4 "meow-expand-4") (5 "meow-expand-5") (6 "meow-expand-6") (7 "meow-expand-7") (8 "meow-expand-8") (9 "meow-expand-9") (0 "meow-expand-0") ("-" "negative-argument") (";" "meow-reverse") ("," "meow-inner-of-thing") ("." "meow-bounds-of-thing") ("<" "meow-beginning-of-thing") (">" "meow-end-of-thing") ("a" "my-meow-append") ("A" "meow-open-below") ("b" "meow-back-word") ("B" "meow-back-symbol") ("c" "meow-change") ("C" "meow-comment") ("d" "meow-delete") ("D" "meow-backward-delete") ("e" "meow-next-word") ("E" "meow-next-symbol") ("f" "meow-find") ("g" "meow-cancel-selection") ("G" "meow-grab") ("h" "meow-right") ("H" "meow-right-expand") ("i" "meow-prev") ("I" "meow-prev-expand") ("j" "meow-join") ("k" "meow-kill") ("l" "meow-line") ("L" "meow-goto-line") ("m" "meow-pop-selection") ("M" "meow-sync-grab") ("n" "meow-next") ("N" "meow-next-expand") ("o" "meow-left") ("O" "meow-left-expand") ("p" "meow-block") ("P" "meow-to-block") ("q" "meow-quit") ("r" "meow-replace-save") ("R" "meow-swap-grab") ("s" "meow-insert") ("S" "meow-open-above") ("t" "meow-till-expand") ("T" "meow-indent") ("u" "meow-undo") ("U" "meow-undo-in-selection") ("v" "meow-visit") ("w" "meow-mark-word") ("W" "meow-mark-symbol") ("x" "meow-save") ("X" "meow-clipboard-save") ("y" "meow-yank") ("Y" "meow-clipboard-yank") ("$" "repeat") ("/" "meow-search") ("'" "repeat") ("~" "meow-query-replace-regexp") ("%" "meow-query-replace") ("<escape>" "ignore")))
      (motion-keybindings '(("<escape>" "ignore") ("n" "meow-next") ("i" "meow-prev")))
      (leader-keybindings '(("e" "dispatch: C-x C-e" "") ("u" "meow-universal-argument" "") ("w w" "other-window" "") ("w v" "split-window-right" "") ("w s" "split-window-below" "") ("w o" "delete-other-windows" "") ("w d" "delete-window" "") ("b" "switch-to-buffer" "") ("n" "dispatch: H-n" "") ("i" "dspatch: H-i" ""))))
(setup (:pkg meow)
  ;; (:option
  ;;  meow-cheatsheet-layout meow-cheatsheet-layout-norman
  ;;  sentence-end-double-space 'nil)

  (defun my-meow-append ()
    (interactive)
    (unless (region-active-p) (forward-char 1))
    (if meow--temp-normal
	(progn
	  (message "Quit temporary normal mode")
	  (meow--switch-state 'motion))
      (meow--direction-forward)
      (when (bound-and-true-p delete-selection-mode)
	(meow--cancel-selection))
      (meow--switch-state 'insert)))
  (defun meow-setup ()
    (let ((parse-def (lambda (x)
		       (cons (format "%s" (car x))
			     (if (string-prefix-p "dispatch:" (cadr x))
				 (string-trim (substring (cadr x) 9))
			       (intern (cadr x)))))))
      ;; "normal-keybindings" and the like are defined in org-tables
      (apply #'meow-normal-define-key (mapcar parse-def normal-keybindings))
      (apply #'meow-motion-overwrite-define-key (mapcar parse-def motion-keybindings))
      (apply #'meow-leader-define-key (mapcar parse-def leader-keybindings))))
  (require 'meow)
  (setq      meow-cheatsheet-layout meow-cheatsheet-layout-norman
	     sentence-end-double-space 'nil)


  (meow-setup)
  (meow-global-mode 1)
  )

;; (meow-global-mode 1)

)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; TODO copy over the right font
(set-face-attribute 'default nil
		    :font "Hasklug Nerd Font Mono"
		    :height 170)

(setup (:pkg doom-themes)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
(load-theme 'doom-one t)
(setq custom-safe-themes t)

(setup (:pkg dashboard)
  (dashboard-setup-startup-hook)
  ;; ;; TODO only set this when I start in daemon!!
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(setq tab-bar-new-tab-choice "*dashboard*")
;;(global-set-key (kbd "C-z") tab-bar-prefix-map)



(setup (:pkg popper)
  (:bind "C-*"    popper-toggle-latest
	 "M-*"    popper-cycle
	 "C-M-*"  popper-toggle-type)
  (setq popper-reference-buffers
	'("\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  help-mode
	  compilation-mode

	  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
	  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
	  "^\\*term.*\\*$"   term-mode   ;term as a popup
	  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
	  ))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setup (:pkg vertico )
  (vertico-mode))
(setup (:pkg all-the-icons-completion)  (:with-hook marginalia-mode (:hook all-the-icons-completion-marginalia-setup)))
(setup (:pkg marginalia)
  (:option
   marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode 1))
(setup savehist
  (setq history-length 25)
  (savehist-mode 1))


;; TODO figure out how to configure
(setup (:pkg orderless)
  (:option completion-styles '(orderless)
	   completion-category-defaults nil
	   completion-fcategory-overrides '((file (styles partial-completion))))
  (require 'orderless))

(setup (:pkg embark)
  (:global
   "C-."  embark-act
   "M-."  embark-dwim
   "C-h B"  embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command))

(setup (:pkg consult)
  ;; (require 'consult)
  (:global "C-s" consult-line
	   "C-c f f" find-file
	   "C-c f r" consult-recent-file)
  (:when-loaded
    (consult-customize consult-recent-file :preview-key (kbd "M-.")))
  )	 ;;"C-M-j" persp-switch-to-buffer*)

;; (:with-map minibuffer-local-map
;;   (:bind "C-r" consult-history))

(setup (:pkg corfu)
  ;; Optional customizations
  (:option
   corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
   corfu-auto  t               ;; Enable auto completion
   corfu-separator ?\s          ;; Orderless field separator
   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
   corfu-scroll-margin 5        ;; Use scroll margin
   )
  (:load-after cape

    ;; (:with-map corfu-map
    ;;   (:bind
    ;;    "TAB"  corfu-next
    ;;    [tab]  corfu-next
    ;;    "S-TAB"  corfu-previous
    ;;    [backtab]  corfu-previous))
    ;; add a list of corfu extensions
    (add-to-list 'load-path "~/.my-emacs.d/straight/build/corfu/extensions/")
    (require 'corfu-history)
    (require 'corfu-info)
    (require 'corfu-indexed)
    (corfu-global-mode)))


(setup (:pkg kind-icon)
  (:load-after corfu
    (:option kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    (require 'kind-icon)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))
;; icons for corfu
;; Add extensions
(setup (:pkg cape)
  ;; (:load-after corfu)
  ;; TODO I might rebind these later

  ;; (:global "C-c p p"  completion-at-point ;; capf
  ;; 	 "C-c p t"  complete-tag       ;; etags
  ;; 	 "C-c p d"  cape-dabbrev        ;; or dabbrev-completion
  ;; 	 "C-c p f"  cape-file
  ;; 	 "C-c p k"  cape-keyword
  ;; 	 "C-c p s"  cape-symbol
  ;; 	 "C-c p a"  cape-abbrev
  ;; 	 "C-c p i"  cape-ispell
  ;; 	 "C-c p l"  cape-line
  ;; 	 "C-c p w"  cape-dict
  ;; 	 "C-c p \\" cape-tex
  ;; 	 "C-c p &"  cape-sgml
  ;; 	 "C-c p r"  cape-rfc1345)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; corfu needs cape to work, so I eagerly load it
  (require 'cape)
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
;; A few more useful configurations...
(setq completion-cycle-threshold 3)

;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(setup (:pkg org :type built-in)
  (:also-load org-tempo)
  ( :bind "C-c a"  org-agenda
    "C-c x" org-capture
    "M-y"  org-metaleft
    "M-o"  org-metaright
    "M-n"  org-metadown
    "M-i"  org-metaup)
    (setq org-modules
	  '(org-crypt
	    org-habit
	    org-bookmark
	    org-eshell
	    org-irc))
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
    ;; WORKFLOW stuff
    (setq org-capture-templates
	  '(("t" "Todos" entry (file+headline "gtd/inbox.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	    ("T" "Tickler" entry (file+headline "gtd/tickler.org" "Inbox") "* TODO %?\n%i\n%a" :prepend t)
	    ("r" "Resources" entry (file+headline "gtd/resources.org" "Inbox") "* TODO %?" :prepend t)
	    ("e" "Emacs + Vim tricks" entry (file+headline "emacs-tips.org" "Inbox") "* TODO %?" :prepend t)))
    ;; TODO I might remove this to use tempel package instead
    (setq
     org-refile-targets '(("~/org/gtd/gtd.org"  :maxlevel . 2)
			  ("~/org/gtd/inbox.org"  :maxlevel . 1)
			  ("~/org/gtd/someday.org"  :maxlevel . 1)
			  ("~/org/gtd/resources.org"  :maxlevel . 2))
     org-refile-use-outline-path t
     )
    ;;TODO Move this somewhere else
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (emacs-lisp . t)
       (dot . t))) ; this line activates dot

    (setq org-confirm-babel-evaluate 'nil)
    )

(setup org-tempo
  (:when-loaded
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))))

(setup (:pkg org-modern)
  (:hook-into org-mode)
  (:with-hook org-agenda-finalize-hook (:hook org-modern-agenda))
  )

(setup (:pkg org-appear :host github :repo "awth13/org-appear")
	     (:hook-into org-mode))

(setup (:pkg org-recur)
  (:hook-into org-mode)
  (:with-hook org-agenda-mode (:hook org-recur-agenda-mode))
  (:bind "C-c d" org-recur-finish)
  (:with-mode org-recur-agenda-mode (:bind "C-c d" org-recur-finish))
  (:option 
   org-recur-finish-done t
   org-recur-finish-archive t)
  )

(setup  (:pkg org-super-agenda)
  (:hook-into org-agenda-mode)
  (setq org-todo-keywords
	'((sequence
	   "TODO(t)"  ; A task that needs doing & is ready to do
	   "PROJ(p)"  ; A project, which usually contains other tasks
	   "LOOP(r)"  ; A recurring task
	   "STRT(s)"  ; A task that is in progress
	   "WAIT(w)"  ; Something external is holding up this task
	   "HOLD(h)"  ; This task is paused/on hold because of me
	   "IDEA(i)"  ; An unconfirmed and unapproved task or notion
	   "|"
	   "DONE(d)"  ; Task successfully completed
	   "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
	  ))
  (setq org-agenda-skip-scheduled-if-done t
	org-agenda-skip-deadline-if-done t
	org-agenda-include-deadlines t
	org-agenda-block-separator nil
	org-agenda-tags-column 100 ;; from testing this seems to be a good value
	org-agenda-compact-blocks t)
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
  (setq
   org-super-agenda-header-map (make-sparse-keymap)
   org-agenda-custom-commands '(("h" "my custom agenda view"
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
				 )
				("r" "Need to do + Want to do"
				 ((tags-todo "need|want"  ((org-agenda-overriding-header "Need to do + Want to do")
							   (org-super-agenda-groups
							    '(
							      (:name "Need to do"
								     :tag "need")
							      (:name "Want to do"
								     :tag "want")
							      (:discard (:anything t))
							      ))))))))

  )

(setup (:pkg org-roam)
  (:option
  org-roam-directory (file-truename "~/org/roam/")
  org-roam-completion-everywhere t
  org-roam-db-location "~/org/roam/org-roam.db"
  org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
  )
  ;; TODO do some stuf
  (:bind "C-c n l"  org-roam-buffer-toggle
	 "C-c n f"  org-roam-node-find
	 "C-c n g"  org-roam-graph
	 "C-c n i"  org-roam-node-insert
	 "C-c n c"  org-roam-capture
	 ;; Dailies
	 "C-c n j"  org-roam-dailies-capture-today)
  (:hook org-roam-db-autosync-mode))

;; Opens video file in mpv
(setup (:pkg openwith)
  (:option openwith-associations '(("\\.mp4\\'" "mpv" (file)) ("\\.webm\\'" "mpv" (file)) ("\\.mkv\\'" "mpv" (file)) ("\\.pdf\\'" "zathura" (file))))
  (openwith-mode t))

(setup (:pkg flycheck)
  (:hook-into lsp-mode))
(setup (:pkg lsp-mode)
  (:option
   lsp-headerline-breadcrumb-enable nil
   lsp-completion-provider :none)
  (:hook-into cc-mode haskell-mode)
  (defun tt/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))) ;; Configure orderless
  (:with-mode lsp-completion-mode (:hook tt/lsp-mode-setup-completion))

  )

(setup (:pkg lsp-ui)
  (:hook-into lsp-mode)
  (:option
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-hover nil
   lsp-ui-doc-position 'bottom)
  (:hook lsp-ui-doc-show))
;; dap is dependent on lsp-treemacs
(setup (:pkg dap-mode)
  (:option lsp-enable-dap-auto-configure nil)
  (:hook dap-ui-mode dap-tooltip-mode dap-node-setup))
;;:init
;; (dap-ui-mode 1)
;; (dap-tooltip-mode 1)
;; (require 'dap-node)
;; (dap-node-setup))
(setup (:pkg hl-todo)
  (:option  hl-todo-keyword-faces
	    '(("TODO"   . "#FF0000")
	      ("FIXME"  . "#FF0000")
	      ("DEBUG"  . "#A020F0")
	      ("GOTCHA" . "#FF4500")
	      ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode))

(setup (:pkg tree-sitter))
(setup (:pkg tree-sitter-langs))

(setup (:pkg ccls)
  (:hook-into c-mode c++-mode)
  (:hook lsp))

(setup (:pkg graphviz-dot-mode)
  (:option graphviz-dot-indent-width 4))

(setup (:pkg nix-mode)
  (:file-match "\\.nix\\'"))
