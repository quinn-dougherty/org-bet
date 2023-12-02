;;; manifold.el --- Bet on anything -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Quinn Dougherty
;;
;; Author: Quinn Dougherty <quinnd@riseup.net>, Tassilo <sonofhypnos>
;; Maintainer: Quinn Dougherty <quinnd@riseup.net>, Tassilo <sonofhypnos>
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

(defvar manifold-hostname "dev.manifold.markets")
(defvar manifold-base-url (concat manifold-hostname "/api/v0/"))

(defun manifold-parse-json (json-string)
  "Parse the JSON response from Manifold Markets API."
  (json-read-from-string json-string))

;; Auth and network calls
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
                                        :host manifold-hostname
                                        :user "apikey"
                                        :type 'netrc
                                        :require '(:password))))
    (if credential
        (let ((key (plist-get (car credential) :password)))
          (if key
              key
            (error "No API key found for dev.manifold.markets/api/v0 in authinfo.gpg")))
      (error "No credentials found for dev.manifold.markets/api/v0 in authinfo.gpg"))))


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


(defun manifold-get-contract-id-from-slug (marketSlug)
  "Get contractId from marketSlug."
  (let* ((endpoint (format "slug/%s" marketSlug))
         (response (manifold-get-request endpoint))
         (json-response (json-read-from-string response))
         (contractId-pair (assoc 'id json-response)))
    (when (consp contractId-pair) ;; Check if contractId-pair is a cons cell
      (cdr contractId-pair))))

(defun manifold-place-bet-from-slug (slug outcome amount &optional limitProb expiresAt)
  "Place a bet on the Manifold Markets API."
  (let* ((contractId (manifold-get-contract-id-from-slug slug)) ; Retrieve contract ID first
         ;; Now create the data with the retrieved contract ID
         (data `(("amount" . ,amount)
                 ("contractId" . ,contractId)
                 ("outcome" . ,outcome)
                 ,@(when limitProb `(("limitProb" . ,limitProb)))
                 ,@(when expiresAt `(("expiresAt" . ,expiresAt)))))
         (response (manifold-post-request "bet" data)))
    response))

(defun manifold-place-bet (contractId outcome amount &optional limitProb expiresAt)
  "place bet from contractId"
  (let* ((data `(("amount" . ,amount)
                ("contractId" . ,contractId)
                ("outcome" . ,outcome)
                ,@(when limitProb `(("limitProb" . ,limitProb)))
                ,@(when expiresAt `(("expiresAt" . ,expiresAt)))))
         (response (manifold-post-request "bet" data)))
  response))

;; test
(manifold-get-contract-id-from-slug "kalshi-will-the-government-be-shut")
(manifold-place-bet-from-slug "kalshi-will-the-government-be-shut" "NO" 1)



;; search markets

(defun manifold-construct-search-url (term &optional sort filter contractType topicSlug creatorId limit offset)
  "Construct the search URL for Manifold Markets API."
  (concat "search-markets?"
          "term=" (url-encode-url term)
          "&sort=" (or sort "liquidity")
          "&filter=" (or filter "open")
          "&contractType=" (or contractType "BINARY")
          (when topicSlug (concat "&" "topicSlug=" (url-encode-url topicSlug)))
          (when creatorId (concat "&" "creatorId=" creatorId))
          "&limit=" (or (number-to-string (or limit 5)) "5")
          "&offset=" (or (number-to-string (or offset 0)) "0")))

(defun manifold-search-markets (term &optional sort filter contractType topicSlug creatorId limit offset)
  "Search markets on Manifold Markets API."
  (let ((url (manifold-construct-search-url term sort filter contractType topicSlug creatorId limit offset)))
    (let ((response (manifold-get-request url)))
      (json-read-from-string response))))

;; test
(manifold-search-markets "biden")

(defun manifold-interactive-bet (marketId)
  "Function to bet on a market. MARKET-ID is the ID of the market."
  (interactive "sBetting on market: ")
  (let* ((outcome (completing-read "Choose outcome (YES/NO): " '("YES" "NO")))
         (amount (read-number "How much mana? (default 10): " 10))
         )
  ;; Here, add the code to bring up the betting interface
  ;; For example, you might open a new buffer with betting options for the given market-id
  (message "Betting on market: %s toward %s for %s$M" marketId outcome amount)
  (manifold-place-bet marketId outcome amount)
  (message "Bet went through (error handling TODO)")
  ))


(defun manifold-display-search-results (term &optional sort filter contractType topicSlug creatorId limit offset)
  "Display search results from Manifold Markets API."
  (interactive "sSearch term: ")
  (let ((results (manifold-search-markets term sort filter contractType topicSlug creatorId limit offset)))
    (with-output-to-temp-buffer "*Manifold Search Results*"
      (with-current-buffer "*Manifold Search Results*"
        (erase-buffer)
        (insert "Search Results for '" term "':\n\n")
        (dolist (market (append results nil)) ;; Assuming 'results' is a list of markets
          ;; Define market-id and market-name from the market alist
          (let ((market-id (alist-get 'id market))
                (market-name (alist-get 'question market)))  ; Adjust 'name to the actual key for the market name
            ;; Insert market name with interactive properties
            (insert (propertize (format "%s\n" (or market-name "Unknown Market"))
                                'keymap (let ((keymap (make-sparse-keymap)))
                                          (define-key keymap (kbd "<RET>")
                                            (lambda () (interactive) (manifold-interactive-bet market-id)))
                                          keymap)
                                'mouse-face 'highlight
                                'help-echo "Press Enter to bet on this market."))
            ;; Insert other market details
            (insert (format "URL: %s\n" (or (alist-get 'url market) "unknown url")))
            (insert (format "Creator: %s\n" (or (alist-get 'creatorUsername market) "unknown username")))
            (insert "------\n")))))))


;test
(manifold-display-search-results "biden" "liquidity" "resolved" "BINARY" nil nil 2 0)
; test interactive
(call-interactively #'manifold-display-search-results)




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
