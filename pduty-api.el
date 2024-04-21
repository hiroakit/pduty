;;; pduty-api.el --- Access PagerDuty API  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hiroaki ENDOH

;; Author: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; Maintainer: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; URL: https://github.com/hiroakit/pduty
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Access PagerDuty API
;;

;;; Code:

(require 'request)

(defcustom pduty-api-user-token-key nil
  "Using PagerDuty REST API with user token rest api key."
  :type 'string
  :group 'pduty)

(defun pduty-api--request-headers ()
  "Create based http headers for PagerDuty API."
  (unless pduty-api-user-token-key (error "Required API key"))
  (let ((headers '(("Accept" . "application/json")
                   ("Content-Type" . "application/json")))
        (token (cons "Authorization" (format "Token token=%s" pduty-api-user-token-key))))
    (add-to-list 'headers token)
    headers))

(defun pduty-api--request-get (uri)
  "Send HTTP Get request.  URI is PagerDuty API endpoint."
  (unless uri (error "Required URI"))
  (let* ((headers (pduty-api--request-headers))
         (response (request uri
                     :type "GET"
                     :headers headers
                     :parser (lambda ()
                               (let* ((json-object-type 'hash-table)
                                      (json-array-type 'list)
                                      (json-key-type 'keyword))
                                 (json-read)))
                     :sync t)))
    response))

(defun pduty-api-schedules-get (&optional schedule-id)
  "Request Schedules API.  Get a schedule, using SCHEDULE-ID."
  (let* ((base-url "https://api.pagerduty.com/schedules")
         (hexified-schedule-id (when schedule-id
                                 (url-hexify-string schedule-id)))
         (url (concat base-url "/" hexified-schedule-id))
         (response (pduty-api--request-get url)))
    (when (= (request-response-status-code response) 401)
      (error "Response: HTTP 401"))
    (when (= (request-response-status-code response) 200)
      (request-response-data response))))

(defun pduty-api-oncalls-get (user-id)
  "Request Oncalls API.  Required USER-ID of PagerDuty."
  (unless user-id
    (error "Required User ID"))
  (let* ((base-url "https://api.pagerduty.com/oncalls")
         (hexified-user-id (when user-id
                             (url-hexify-string user-id)))
         (url (concat base-url "?user_ids%5B%5D=" hexified-user-id))
         (response (pduty-api--request-get url)))
    (when (= (request-response-status-code response) 401)
      (error "Response: HTTP 401"))
    (when (= (request-response-status-code response) 200)
      (request-response-data response))))
  
(provide 'pduty-api)
;;; pduty-api.el ends here
