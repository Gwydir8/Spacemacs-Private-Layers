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
    :config (use-package cedet-contrib-load :demand)
    ))

(defun cedet/init-semantic ()
  (use-package semantic
    ;; :defer t
    :commands semantic-mode
    :init
    (progn
      (add-hook 'c-mode-hook 'semantic-mode)
      (add-hook 'c++-mode-hook 'semantic-mode))
    :config
    (progn

      (use-package ede)
      (use-package semantic/bovine/el)
      (use-package semantic/canned-configs)
      (semantic-load-code-helpers-1)
      ;;(semantic-load-enable-gaudy-code-helpers)
      (setq semantic-default-submodes
            '(global-semantic-idle-scheduler-mode
              global-semanticdb-minor-mode
              global-semantic-idle-summary-mode
              global-semantic-mru-bookmark-mode
              global-cedet-m3-minor-mode
              ;; global-semantic-decoration-mode
              global-semantic-stickyfunc-mode
              global-semantic-idle-completions-mode
              ))

     (semantic-mode 1)
     ;; Increase the delay before doing slow work to 2 minutes.
     (setq semantic-idle-scheduler-work-idle-time 120)

     ;; Increase the delay before activation
     (setq semantic-idle-scheduler-idle-time 10)
     ;; Don't reparse really big buffers.
     (setq semantic-idle-scheduler-max-buffer-size 100000)
     ;; Increase the delay before doing slow work to 2 minutes.
     (setq semantic-idle-scheduler-work-idle-time 120)


      ;; This enables parsing of header files.
      (setq semantic-idle-work-update-headers-flag t)
      (setq semantic-idle-work-parse-neighboring-files-flag t)


      (when (and (eq window-system 'x)
                 (locate-library "imenu"))
        (add-hook 'semantic-init-hook
                  (lambda ()
                    (condition-case nil
                        (imenu-add-to-menubar
                         semantic-load-imenu-string)
                      (error nil)))))

      (use-package semantic/ia)
      (use-package semantic/sb)
      (use-package semantic/bovine)
      (use-package semantic/bovine/c)

      ;; (semantic-gcc-setup "gcc-4.9")
      ;;(use-package semantic/bovine/gcc)
      (use-package semantic/bovine/clang)
      (use-package semantic/decorate/include)
      (use-package semantic/lex-spp)
      (use-package eassist)

      (setq semantic-clang-binary "/usr/local/opt/ccache/libexec/clang++-3.6")
      (semantic-clang-activate)

      (setq ede-locate-setup-options '(ede-locate-base))

      (use-package cedet-global)
      (setq cedet-global-command "/usr/local/bin/global")
      (when (cedet-gnu-global-version-check t)
       (add-to-list 'ede-locate-setup-options 'ede-locate-global)
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode))

      ;; cscope
      (use-package cedet-cscope)
     (when (cedet-cscope-version-check t)
       (add-to-list 'ede-locate-setup-options 'ede-locate-cscope)
       (semanticdb-enable-cscope-databases :noerror))

      ;; ctags
      (setq semantic-ectags-program "/usr/local/bin/ctags")
      (ignore-errors
        (when (cedet-ectag-version-check t)
          (semantic-load-enable-all-ectags-support)))

      (use-package cedet-idutils)
     (when (cedet-idutils-version-check t)
       (add-to-list 'ede-locate-setup-options 'ede-locate-idutils))

      ;; SRecode
      (setq srecode-map-save-file (concat spacemacs-cache-directory "srecode-map.el"))
      (use-package srecode/compile)
      ;; (global-srecode-minor-mode 1)
      (add-hook 'c-mode-hook 'srecode-minor-mode)
      (add-hook 'c++-mode-hook 'srecode-minor-mode)

     ;;throttle semanticdb
     (setq-mode-local c-mode
                      semanticdb-find-default-throttle
                      '(project
                        unloaded
                        system
                        recursive))

     (setq-mode-local c++-mode
                      semanticdb-find-default-throttle
                      '(project
                        unloaded
                        system
                        recursive))



      (semantic-add-system-include "/usr/local/include/boost/" 'c++-mode)
      (semantic-add-system-include "/usr/local/include/")
      (semantic-add-system-include "/usr/include/")

      ;;gcc
      (semantic-add-system-include "/usr/local/include/c++/4.9.2/")
      (add-to-list 'company-c-headers-path-system "/usr/local/include/c++/4.9.2/")

      ;; clang
      ;; llvm 3.5.1
      (semantic-add-system-include "/usr/local/Cellar/llvm/3.5.1/include/c++/v1")
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file
       "/usr/local/Cellar/llvm/3.5.1/lib/clang/3.5.1/include/stddef.h")
      (add-to-list 'company-c-headers-path-system "/usr/local/Cellar/llvm/3.5.1/include/c++/v1/")
      ;;llvm 3.6
      (semantic-add-system-include "/usr/local/opt/llvm36/lib/llvm-3.6/include/c++/v1")
      (add-to-list 'company-c-headers-path-system "/usr/local/opt/llvm36/lib/llvm-3.6/include/c++/v1")
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/local/Cellar/llvm36/3.6.0/lib/llvm-3.6/lib/clang/3.6.0/include/stddef.h")


      ;; (global-ede-mode t)
      ;; (spacemacs|hide-lighter ede-dired-minor-mode)
      (ede-enable-generic-projects)
      (add-to-list 'ede-locate-setup-options 'ede-locate-base)

      ;; use gdb-many-windows by default
      (setq gdb-many-windows t)

      ;; Non-nil means display source file containing the main routine at startup
      (setq gdb-show-main t)

      (setq compilation-disable-input t)
      (setq compilation-scroll-output t)
      (setq mode-compile-always-save-buffer-p t))
      (setq semanticdb-default-save-directory (concat spacemacs-cache-directory "semanticdb/"))
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

      ;; (setq cppcm-get-executable-full-path-callback
      ;;       (lambda (path type tgt-name)
      ;;         ;; path is the supposed-to-be target's full path
      ;;         ;; type is either add_executabe or add_library
      ;;         ;; tgt-name is the target to built. The target's file extension is stripped
      ;;         (message "cppcm-get-executable-full-path-callback called => %s %s %s" path type tgt-name)
      ;;         (let ((dir (file-name-directory path))
      ;;               (file (file-name-nondirectory path)))
      ;;           (cond
      ;;            ((string= type "add_executable")
      ;;             (setq path (concat dir "bin/" file)))
      ;;            ;; for add_library
      ;;            (t (setq path (concat dir "lib/" file)))
      ;;            ))
      ;;         ;; return the new path
      ;;         (message "cppcm-get-executable-full-path-callback called => path=%s" path)
      ;;         path))
       (setq cppcm-write-flymake-makefile nil)

      ;; (setq cppcm-debug t)
      )
    ))
