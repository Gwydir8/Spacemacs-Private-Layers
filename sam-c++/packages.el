;;; packages.el --- sam-c++ Layer packages File for Spacemacs
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

(defvar sam-c++-packages
  '(cc-mode
    cmake-mode
    flycheck
    srefactor)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun sam-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
      (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))
    :config
    (progn
      (require 'compile)
      (add-hook 'c-mode-hook '(lambda ()
                                ;; (c-toggle-auto-newline t)
                                (linum-mode)))
      (add-hook 'c++-mode-hook '(lambda ()
                                  ;; (c-toggle-auto-newline t)
                                (linum-mode)))
      (add-to-list 'company-c-headers-path-system "/usr/local/include/c++/4.9.2/")
      )
    ))

(defun sam-c++/init-srefactor ()
  (use-package srefactor
    :defer t
    :if (not (version< emacs-version "24.4"))
    :init
    (progn
      (evil-leader/set-key-for-mode 'c-mode
        "mr" 'srefactor-refactor-at-point)
      (evil-leader/set-key-for-mode 'c++-mode
        "mr" 'srefactor-refactor-at-point))))

(defun sam-c++/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :init
    (setq auto-mode-alist
          (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist))))

(defun sam-c++/init-flycheck ()
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode))
