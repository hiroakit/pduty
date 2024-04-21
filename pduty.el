;;; pduty.el --- Access PagerDuty  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hiroaki ENDOH

;; Author: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; Maintainer: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; URL: https://github.com/hiroakit/pduty
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (request "0.3.3"))
;; Keywords: tools data convenience

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
;; Access PagerDuty
;;

;;; Code:

(require 'pduty-api)
(require 'org)

(defgroup pduty nil
  "Access PagerDuty."
  :prefix "pduty-"
  :group 'tools)

(defcustom pduty-user-id nil
  "User ID of Pagerduty."
  :type 'string
  :group 'pduty)

(defun pduty-insert-latest-oncall-as-org-entry ()
  "Insert on-call schedule as org entry at cursor."
  (interactive)
  (let* ((oncalls (pduty--fetch-oncalls))
         (latest-oncall (car oncalls)))
    (pduty--insert-org-entry-by-oncall latest-oncall)))

(defun pduty--insert-org-entry-by-oncall (oncall)
  "Create org entry by ONCALL."
  (unless oncall (error "Required oncall"))
  (let* ((schedule (gethash :schedule oncall))
         (start-date-string (pduty--create-org-time-stamp (gethash :start oncall)))
         (end-date-string (pduty--create-org-time-stamp (gethash :end oncall))))
    (cond ((and schedule start-date-string end-date-string)
           (org-insert-heading)
           (insert "PagerDuty on-call\n")
           (insert (format "SCHEDULED: %s--%s" start-date-string end-date-string))
           (org-set-property "CREATED" (with-temp-buffer (org-insert-time-stamp (current-time) t t)))
           (org-set-property "CUSTOM_ID" (org-id-new))
           (org-set-property "PAGERDUTY_SCHEDULE_URL" (gethash :html_url schedule)))
          (t
           (message "[pduty] Not Found On-call Schedule")))))

(defun pduty--create-org-time-stamp (datetime-string)
  "Create \='org-mode\=' time stamp.  DATETIME-STRING is ISO 8601 style string."
  (when datetime-string
    (let* (datetime-parts)
      (setq datetime-parts (parse-time-string datetime-string))
      (format "<%04d-%02d-%02d>"
              (nth 5 datetime-parts)
              (nth 4 datetime-parts)
              (nth 3 datetime-parts)))))

(defun pduty--fetch-oncalls ()
  "Get on-call schedules from PagerDuty API."
  (unless pduty-user-id (error "Required User ID"))
  (let* ((result (list))
         (json (pduty-api-oncalls-get pduty-user-id))
         (oncalls (gethash :oncalls json)))
    (dolist (oncall oncalls)
      (let* ((schedule (gethash :schedule oncall)))
        (when schedule
          (push 'result oncall))))
      result))

(provide 'pduty)
;;; pduty.el ends here
