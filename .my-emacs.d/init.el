;;; init.el -*- lexical-binding: t; -*-

;;(setq user-emacs-directory "~/.dotfiles/home/emacs")

;(setq debug-on-error t)

;; Make Emacs startup faster
(setq gc-cons-threshold (* 50 1000 1000))


;; setup straight
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
(straight-use-package 'use-package)
;; use the gcmh package to make startup faster
(straight-use-package '(gcmh :type git :host gitlab :repo "koral/gcmh"))
(gcmh-mode 1)

;; profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
(setq native-comp-async-report-warnings-errors nil)


(org-babel-load-file (concat user-emacs-directory "/config.org"))
;; set gc-cons back
;; (setq gc-cons-threshold (* 2 1000 1000))
