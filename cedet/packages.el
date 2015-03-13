;;; packages.el --- cedet Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar cedet-packages
  '(cedet function-args ggtags helm-gtags semantic cpputils-cmake)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cedet-excluded-packages '()
  "List of packages to exclude.")

(defun cedet/init-cedet ()
  (use-package cedet-devel-load
    ;; :defer t
    :demand
    :commands (cedet-devel-load cedet-contrib-load)
    :load-path ("private/cedet/cedet" "private/cedet/cedet/contrib")
    :config (use-package cedet-contrib-load
              :demand)
    ))

(defun cedet/init-semantic ()
  (use-package semantic
    ;; :defer t
    :commands semantic-mode
    ;; :idle (progn
    ;;         (semantic-mode 1)
    ;;         (global-srecode-minor-mode 1)
    ;;         (global-ede-mode t))
    :init
    (progn
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semanticdb-minor-mode)
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semantic-idle-scheduler-mode)
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semantic-idle-local-symbol-highlight-mode)
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semantic-idle-completions-mode)
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semantic-idle-summary-mode)
      ;;   (add-to-list 'semantic-default-submodes
      ;;                'global-semantic-stickyfunc-mode)
      (add-hook 'c++-mode-hook 'semantic-mode))
    :config
    (progn
      (use-package semantic/bovine/el)
      (use-package semantic/canned-configs)
      ;;(semantic-load-enable-gaudy-code-helpers)
      (setq semantic-default-submodes
            '(global-semantic-idle-scheduler-mode
              global-semanticdb-minor-mode
              ;; global-semantic-idle-summary-mode
              ;; global-semantic-mru-bookmark-mode
              ;; global-cedet-m3-minor-mode
              ;; global-semantic-decoration-mode
              global-semantic-stickyfunc-mode
              ;; global-semantic-idle-completions-mode
              ))

      (semantic-mode 1)
      ;; This enables parsing of header files.
      (setq semantic-idle-work-update-headers-flag t)

      (when (and (eq window-system 'x)
                 (locate-library "imenu"))
        (add-hook 'semantic-init-hook
                  (lambda ()
                    (condition-case nil
                        (imenu-add-to-menubar
                         semantic-load-imenu-string)
                      (error nil)))))
      (setq semantic-idle-work-parse-neighboring-files-flag t)

      (setq semantic-clang-binary "/usr/local/opt/ccache/libexec/clang++-3.5.1")
      (use-package semantic/ia)
      (use-package semantic/sb)
      (use-package semantic/bovine)
      (use-package semantic/bovine/c)
      (use-package semantic/bovine/gcc)
      (use-package semantic/bovine/clang)
      (use-package semantic/decorate/include)
      (use-package semantic/lex-spp)
      (use-package eassist)

      (use-package cedet-global)
      (when (cedet-gnu-global-version-check t)
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode))

      (setq semantic-ectags-program "/usr/local/bin/ctags")
      (semanticdb-enable-cscope-databases :noerror)
      (ignore-errors
        (when (cedet-ectag-version-check t)
          (semantic-load-enable-primary-ectags-support)
          (semantic-load-enable-secondary-ectags-support)))

      ;; SRecode
      ;;(use-package srecode/compile)
      ;;(global-srecode-minor-mode 1)

      (add-to-list
       'semantic-lex-c-preprocessor-symbol-file
       "/usr/local/lib/gcc/4.9/gcc/x86_64-apple-darwin14.0.0/4.9.2/include/stddef.h")
      (semantic-add-system-include "/usr/local/include/")
      (semantic-add-system-include "/usr/include/")
      (semantic-add-system-include "/usr/local/include/c++/4.9.2/")
      (add-to-list 'company-c-headers-path-system "/usr/local/include/c++/4.9.2/")

      (global-ede-mode t)
      ;; (spacemacs|hide-lighter ede-dired-minor-mode)
      (ede-enable-generic-projects)
      ;;(add-to-list 'company-backends 'company-semantic)

      (setq ;; use gdb-many-windows by default
       gdb-many-windows t

       ;; Non-nil means display source file containing the main routine at startup
       gdb-show-main t)

      (setq compilation-disable-input t)
      (setq compilation-scroll-output t)
      (setq mode-compile-always-save-buffer-p t))
    ))

(defun cedet/init-function-args ()
  (use-package function-args
    :defer t
    :init (fa-config-default)
    :config (progn
              (set-default 'semantic-case-fold t)
              (spacemacs|diminish function-args-mode " ⓕ" " fa"))
    ))

(defun cedet/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p
                         'c-mode 'c++-mode 'java-mode 'asm-mode)
                    (ggtags-mode 1)))))
    :config (eval-after-load 'company-mode
              (progn
                (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
                (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
                (add-to-list 'company-backends (company-mode/backend-with-yas 'company-gtags))
                (spacemacs|diminish ggtags-mode " ⓖ" " gg")))))

(defun cedet/init-helm-gtags ()
  (use-package helm-gtags
    :defer t
    :pre-load (progn
                (setq helm-gtags-prefix-key "<SPC>s"
                      helm-gtags-ignore-case t
                      helm-gtags-auto-update t
                      helm-gtags-use-input-at-cursor t
                      helm-gtags-pulse-at-cursor t
                      helm-gtags-suggested-key-mapping nil))
    :init (progn
            (add-hook 'c-mode-hook 'helm-gtags-mode)
            (add-hook 'c++-mode-hook 'helm-gtags-mode)
            (add-hook 'asm-mode-hook 'helm-gtags-mode)
            (helm-gtags-mode t)
            ;; key bindings --
            (evil-leader/set-key
              "sgs" 'helm-gtags-select
              "sgt" 'helm-gtags-dwim
              "sgp" 'helm-gtags-pop-stack
              "sgN" 'helm-gtags-previous-history
              "sgn" 'helm-gtags-next-history)
            (spacemacs|diminish helm-gtags-mode " ⓖt" " gt"))
    ))

(defun cedet/init-cpputils-cmake ()
  (use-package cpputils-cmake
    :defer t
    :commands (cppcm-get-exe-path-current-buffer  cppcm-reload-all)
    :init (progn
            (add-hook 'c-mode-common-hook
                      (lambda ()
                        (if (derived-mode-p 'c-mode 'c++-mode)
                            (cppcm-reload-all)))))
    :config
    (progn
      ;; OPTIONAL, somebody reported that they can use this package with Fortran
      (add-hook 'c-mode-common-hook (lambda ()
                                      (cppcm-reload-all)))

      ;; ;; OPTIONAL, avoid typing full path when starting gdb
      ;; (global-set-key (kbd "C-c C-g")
      ;;                 '(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))

      ;; OPTIONAL, some users need specify extra flags forwarded to compiler
                                        ;(setq cppcm-extra-preprocss-flags-from-user '("-Iinclude/"))

      ;; (add-hook 'c-mode-common-hook
      ;;           (lambda ()
      ;;             (if (derived-mode-p 'c-mode 'c++-mode)
      ;;                 (if  (not (or (string-match "^/usr/local/include/.*" buffer-file-name)))
      ;;                     (cppcm-reload-all))
      ;;               )))

      (setq cppcm-get-executable-full-path-callback
            (lambda (path type tgt-name)
              ;; path is the supposed-to-be target's full path
              ;; type is either add_executabe or add_library
              ;; tgt-name is the target to built. The target's file extension is stripped
              (message "cppcm-get-executable-full-path-callback called => %s %s %s" path type tgt-name)
              (let ((dir (file-name-directory path))
                    (file (file-name-nondirectory path)))
                (cond
                 ((string= type "add_executable")
                  (setq path (concat dir "bin/" file)))
                 ;; for add_library
                 (t (setq path (concat dir "lib/" file)))
                 ))
              ;; return the new path
              (message "cppcm-get-executable-full-path-callback called => path=%s" path)
              path))
      (setq cppcm-write-flymake-makefile nil)

      ;; (setq cppcm-debug t)
      )
    ))
