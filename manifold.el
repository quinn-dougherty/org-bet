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
;; Package-Requires: ((emacs "27.1"))
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
(require 'map)
(require 'text-property-search)
(require 'auth-source)


;; test: https://dev.manifold.markets/mirrorbot/kalshi-will-the-government-be-shut


(defvar manifold-base-url "dev.manifold.markets/api/v0/")
(defvar manifold-api-key "76877963-22ef-48b3-8b46-1a1a5975d49b")

(defun manifold-parse-json (json-string)
  "Parse the JSON response from Manifold Markets API."
  (json-read-from-string json-string))


(defun manifold-get-request (endpoint)
  "Make a GET request to the Manifold Markets API. ENDPOINT is a string."
  (let ((url (concat "https://" manifold-base-url endpoint))
        buffer)
    (setq buffer (url-retrieve-synchronously url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (buffer-string))))

(defun get-manifold-api-key ()
  "Retrieve the Manifold API key from authinfo.gpg."
  (let ((credential (auth-source-search :max 1
                                        :host manifold-base-url
                                        :user "apikey"
                                        :type 'netrc
                                        :require '(:password))))
    (if credential
        (let ((key (plist-get (car credential) :password)))
          (if key
              key
            (error "No API key found for dev.manifold.markets/api/v0 in authinfo.gpg")))
      (error "No credentials found for dev.manifold.markets/api/v0 in authinfo.gpg"))))

;; Tassilo's (actually works)
(defun manifold--api-key-fn (&optional host)
  (let* ((host (or host "dev.manifold.markets"))
         (credentials (let ((auth-source-creation-prompts '((secret . "Enter API key for %h: "))))
                        (auth-source-search :host host
                                            :user "apikey"
                                            :type 'netrc
                                            :max 1))))
    (let ((secret (plist-get (car credentials) :secret)))
      (unless secret
        (error "You need to configure an API key for Manifold. See https://github.com/sonofhypnos/fatebook.el#user-content-storing-your-api-keys"))
      secret)))
(funcall (manifold--api-key-fn "dev.manifold.markets/api/v0"))


(defun manifold-post-request (endpoint data)
  "Make a POST request to the Manifold Markets API.
ENDPOINT is the API endpoint. DATA is the data to be sent as JSON."
  (let ((url (concat "https://" manifold-base-url endpoint))
        (url-request-method "POST")
        (url-request-data (json-encode data))
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat "Key " manifold-api-key))))
        buffer)
    (setq buffer (url-retrieve-synchronously url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (buffer-string))))


;; test
(defun manifold--api-key-fn (&optional host)
  (let* ((host (or host "dev.manifold.markets"))
         (credentials (let ((auth-source-creation-prompts '((secret . "Enter API key for %h: "))))
                        (auth-source-search :host host
                                            :user "apikey"
                                            :type 'netrc
                                            :max 1))))
    (let ((secret (plist-get (car credentials) :secret)))
      (unless secret
        (error "You need to configure an API key for Manifold. See https://github.com/sonofhypnos/fatebook.el#user-content-storing-your-api-keys"))
      secret)))
(funcall (manifold--api-key-fn "dev.manifold.markets/api/v0"))

(defun get-contract-id-from-slug (marketSlug)
  "Get contractId from marketSlug."
  (let* ((endpoint (format "slug/%s" marketSlug))
         (response (manifold-get-request endpoint))
         (json-response (json-read-from-string response))
         (contractId-pair (assoc 'id json-response)))
    (when (consp contractId-pair) ;; Check if contractId-pair is a cons cell
      (cdr contractId-pair))))

(defun get-manifold-api-key ()
  "Retrieve the Manifold API key from authinfo.gpg."
  (let ((credential (auth-source-search :max 1
                                        :host manifold-base-url
                                        :user "apikey"
                                        :type 'netrc
                                        :require '(:password))))
    (if credential
        (let ((key (plist-get (car credential) :password)))
          (if key
              key
            (error "No API key found for dev.manifold.markets/api/v0 in authinfo.gpg")))
      (error "No credentials found for dev.manifold.markets/api/v0 in authinfo.gpg"))))
(get-manifold-api-key)

(defun manifold-place-bet (amount slug outcome &optional limitProb expiresAt)
  "Place a bet on the Manifold Markets API."
  (let* ((contractId (get-contract-id-from-slug slug)) ; Retrieve contract ID first
         ;; Now create the data with the retrieved contract ID
         (data `(("amount" . ,amount)
                 ("contractId" . ,contractId)
                 ("outcome" . ,outcome)
                 ,@(when limitProb `(("limitProb" . ,limitProb)))
                 ,@(when expiresAt `(("expiresAt" . ,expiresAt)))))
         (response (manifold-post-request "bet" data)))
    response))

;; test
(get-contract-id-from-slug "kalshi-will-the-government-be-shut")
(manifold-place-bet 1 "kalshi-will-the-government-be-shut" "NO")


(defun manifold-get-managrams ()
  "Get available instruments from Manifold Markets."
  (let ((response (manifold-request "managrams")))
    (manifold-parse-json response)))




;; above is gpt4's take, below is me transcribing ideas from karthink's gptel package


(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))


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
