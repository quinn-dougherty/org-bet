;;; manifold.el --- Bet on anything -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Quinn Dougherty
;;
;; Author: Quinn Dougherty <quinnd@riseup.net>
;; Maintainer: Quinn Dougherty <quinnd@riseup.net>
;; Created: October 24, 2023
;; Modified: October 24, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/quinn-dougherty/manifold
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'json)
(require 'url)

(defvar manifold-markets-base-url "https://manifold.markets/api/v0/")

(defun manifold-markets-get-request (endpoint)
  "Make a GET request to the Manifold Markets API. ENDPOINT is a string."
  (let ((url (concat manifold-markets-base-url endpoint))
        buffer)
    (setq buffer (url-retrieve-synchronously url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (buffer-string))))

(defun manifold-markets-post-request (endpoint data)
  "Make a POST request to the Manifold Markets API.
ENDPOINT is the API endpoint. DATA is the data to be sent as JSON."
  (let ((url (concat manifold-markets-base-url endpoint))
        (url-request-method "POST")
        (url-request-data (json-encode data))
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        buffer)
    (setq buffer (url-retrieve-synchronously url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (buffer-string))))

(defun manifold-markets-parse-json (json-string)
  "Parse the JSON response from Manifold Markets API."
  (json-read-from-string json-string))

(defun manifold-markets-get-managrams ()
  "Get available instruments from Manifold Markets."
  (let ((response (manifold-markets-request "managrams")))
    (manifold-markets-parse-json response)))

(defun manifold-markets-post-managram (user-id amount &optional message)
  "Send a managram to someone"
  )








;; above is gpt4's take, below is me transcribing ideas from karthink's gptel package


(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(require 'url)
(require 'json)
(require 'map)
(require 'text-property-search)

(defgroup org-manifold nil
  "Bet on anything, in emacs."
  :group 'hypermedia)

(defcustom manifold-host "manifold.markets/api/v0/"
  "The api host queried by org-manifold"
  :group 'org-manifold
  :type 'string)

(defcustom org-manifold-api-key #'org-manifold-api-key-from-auth-source
  "An OpenAI API key (string).

Can also be a function of no arguments that returns an API
key (more secure)."
  :group 'org-manifold
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

(defcustom org-manifold-pre-response-hook nil
  "Hook run before populating manifold's response to current buffer.

This hook is called in the buffer from which the market or bet was sent to chatgpt.
Note: this hook only runs if the request succeeds."
  :group 'org-manifold
  :type 'hook)

(defcustom org-manifold-post-response-hook nil
  "Hook run after inserting Manifold's response into the current buffer.

This hook is called in the buffer from which the prompt was sent
to Manifold. Note: this hook runs even if the request fails."
  :group 'org-manifold
  :type 'hook)


(provide 'manifold)
;;; manifold.el ends here
