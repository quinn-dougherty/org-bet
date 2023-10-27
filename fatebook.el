;;; fatebook.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Quinn Dougherty
;;
;; Author: Quinn Dougherty <quinnd@riseup.net>
;; Maintainer: Quinn Dougherty <quinnd@riseup.net>
;; Created: October 26, 2023
;; Modified: October 26, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/qd/fatebook
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'json)
(require 'url)

(defvar fatebook-base-url "https://fatebook.io/api/v0/")

(provide 'fatebook)
;;; fatebook.el ends here
