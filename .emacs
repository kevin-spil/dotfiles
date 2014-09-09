(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(add-to-list 'load-path "~/.emacs.d/plugins")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'redo+)
(require 'helm)
(require 'erlang)
(require 'git-gutter-fringe+)
					; Add tab config
(setq tab-width 4)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
(require 'dash)
(require 'indent-of-doom)
(setq my-doom '(
    (all . (
        ((and (prev 'ends-on "[") (current 'starts-with "]")) (prev 'indent))
        ((and (prev 'ends-on "{") (current 'starts-with "}")) (prev 'indent))
        ((and (prev 'ends-on "(") (current 'starts-with ")")) (prev 'indent))

        ((current 'starts-with "]" "}" ")") (prev 'indent -1))
        ((prev 'ends-on "[" "{" "(")        (prev 'indent 1))
        ((prev 'ends-on ",")        (prev 'indent))
    ))
    (erlang-mode . (
        ((prev 'ends-on "->" "fun" "of") (prev 'indent 1))
        ((prev 'ends-on ";") (prev 'indent -1))
        ((current 'ends-on "end") (prev 'indent -1))
    ))
	))
(add-hook 'erlang-mode-hook (lambda ()
    (setq tab-width 4)
    (indent-of-doom-mode t)
    (setq-local doom-indent-fallback t)
    (setq-local doom-use-tab-cycle nil)
))
(nyan-mode t)
(nyan-start-animation)
(setq nyan-wavy-trail t)
(global-linum-mode t)
(git-gutter-fr+-minimal)
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Autocomplete / Yasnippet settings START
 (require 'auto-complete-config)
;; Add auto complete to these modes
(add-to-list 'ac-modes 'erlang-mode)
(add-to-list 'ac-modes 'elixir-mode)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'web-mode)

;; Autocomplete default config
(ac-config-default)
;; Use auto complete menu
(setq ac-use-menu-map t)
;; Show menu instantly
(setq ac-auto-show-menu 0.0)
;; Show help menu in 0.5 sec
(setq ac-quick-help-delay 0.5)
;; Add yasnippets to menu
(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
    (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
      (add-to-list 'ac-sources 'ac-source-yasnippet))
;;; Autocomplete / Yasnippet settings END

;; Flymake Settings.
(require 'flymake)

; define auto erlang mode for these files/extensions.
(add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))

; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (i . "../include") (i . "../deps") (i . "../../") (i . "../../../deps") (i . "~/libraries")))

; define where put beam files.
(setq erlang-compile-outdir "../ebin")
(setq flymake-log-level 3)

(defun flymake-compile-script-path (path)
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

(defun flymake-syntaxerl ()
  (flymake-compile-script-path "~/.emacs.d/scripts/syntaxerl/syntaxerl"))

(add-hook 'erlang-mode-hook
  '(lambda()
     (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))

     ;; should be the last.
     (flymake-mode 1)
))
