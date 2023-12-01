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

;; test: https://dev.manifold.markets/mirrorbot/kalshi-will-the-government-be-shut

(defvar manifold-markets-base-url "https://dev.manifold.markets/api/v0/")

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


;; (let ((url-request-method "POST")
;;       (url-request-extra-headers '(("Content-Type" . "application/json")))
;;       (url-request-data (json-encode `((amount . ,amount)
;;                                       (contractId . ,contractId)
;;                                       (outcome . ,outcome)
;;                                       (limitProb . ,limitProb)  ; Only include if provided
;;                                       (expiresAt . ,expiresAt))))  ; Only include if provided
;;       (url "https://api.example.com/v0/bet"))  ; Replace with the actual API endpoint

;;   (url-retrieve url (lambda (status)
;;                       ;; Handle the response here
;;                       (when (plist-get status :error)
;;                         (error "Error in request: %S" status))
;;                       ;; Process the response body
;;                       (let ((response (buffer-string)))
;;                         ;; Do something with the response
;;                         ))))
(defvar manifold-markets-api-key "76877963-22ef-48b3-8b46-1a1a5975d49b")

(defun manifold-markets-place-bet (amount slug outcome &optional limitProb expiresAt)
  "Place a bet on the Manifold Markets API."
  (let* ((contractId (get-contract-id-from-slug slug)) ; Retrieve contract ID first
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")
                                      ("Authorization" . "Key 76877963-22ef-48b3-8b46-1a1a5975d49b")))
         ;; Now create the data with the retrieved contract ID
         (url-request-data (json-encode `(("amount" . ,amount)
                                          ("contractId" . ,contractId)
                                          ("outcome" . ,outcome)
                                          ,@(when limitProb `(("limitProb" . ,limitProb)))
                                          ,@(when expiresAt `(("expiresAt" . ,expiresAt))))))
         (url "https://dev.manifold.markets/api/v0/bet"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let ((response (json-read)))
        (kill-buffer)
        response))))

(get-contract-id-from-slug "kalshi-will-the-government-be-shut")
(manifold-markets-place-bet 1 "kalshi-will-the-government-be-shut" "YES")
;; NOTE

;; (defun manifold-markets-place-bet (amount contractId outcome &optional limitProb expiresAt)
;;   "Place a bet on Manifold Markets."
;;   (let ((data `(("amount" . ,amount)
;;                 ("contractId" . ,contractId)
;;                 ("outcome" . ,outcome)
;;                 ,@(when limitProb `(("limitProb" . ,limitProb)))
;;                 ,@(when expiresAt `(("expiresAt" . ,expiresAt))))))
;; (manifold-markets-post-request "/v0/bet" data)))


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



(cdr '(id . "K4x5nvjCv6GMJqxiPgyp"))
(cdr (get-contract-id-from-slug
      "kalshi-will-the-government-be-shut"))

(defun get-contract-id-from-slug (marketSlug)
  "Get contractId from marketSlug."
  (let ((url (format "https://dev.manifold.markets/api/v0/slug/%s" marketSlug)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read)) ;; Read the JSON response
             (contractId-pair (assoc 'id response))) ;; Find the contractId pair directly in the response
        (kill-buffer)
        (when (consp contractId-pair) ;; Check if contractId-pair is a cons cell
          (cdr contractId-pair))))))


(message (get-contract-id-from-slug
          "kalshi-will-the-government-be-shut"))
(let ((endpoint "mirrorbot/kalshi-will-the-government-be-shut"))
  (manifold-markets-get-request endpoint))

;; (defun manifold-markets-post-managram (user-id amount &optional message)
;;   "Send a managram to someone"
;;   )




(defun fatebook--api-key-fn ()
  "Retrieve the Fatebook API key from `fatebook-auth-source-backend'."
  (let ((credentials (let ((auth-source-creation-prompts '((secret . "Enter API key for %h: "))))
                       (auth-source-search :host "fatebook.io"
                                           :user "defaultUser"
                                           :type fatebook-auth-source-backend
                                           ;;:create ;NOTE: auth-source does not support deletion. A
                                           ;;convoluted idea that could work is that we
                                           ;;just add more and more api keys, and we version them
                                           ;;with the :user handle and then just try all of them if
                                           ;;none worka. Could lead to complaints from fatebook
                                           ;;though.
                                           :max 1))))

    (let ((secret (plist-get (car credentials) :secret)))
      (unless secret
        (error "You need to configure an API key for fatebook. See https://github.com/sonofhypnos/fatebook.el#user-content-storing-your-api-keys"))
      secret)))




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
