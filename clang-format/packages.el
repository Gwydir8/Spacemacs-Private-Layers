;;; packages.el --- clang-format Layer packages File for Spacemacs
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

(defvar clang-format-packages
  '(clang-format)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar clang-format-excluded-packages '()
  "List of packages to exclude.")

(defun clang-format/init-clang-format ()
  "Initialize my package"
  (use-package clang-format
    :commands clang-format-region
    :init
    (progn (evil-leader/set-key-for-mode 'cc-mode "cf" 'clang-format-region)
           (evil-leader/set-key-for-mode 'cc-mode "cb" 'clang-format-buffer)
           (evil-leader/set-key-for-mode 'c++-mode "cf" 'clang-format-region)
           (evil-leader/set-key-for-mode 'c++-mode "cb" 'clang-format-buffer)
           (evil-leader/set-key-for-mode 'objc-mode "cf" 'clang-format-region)
           (evil-leader/set-key-for-mode 'objc-mode "cb" 'clang-format-buffer))))
