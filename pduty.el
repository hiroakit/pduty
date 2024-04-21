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

(defgroup pduty nil
  "Access PagerDuty."
  :prefix "pduty-"
  :group 'tools)

(defcustom pduty-user-id nil
  "User ID of Pagerduty."
  :type 'string
  :group 'pduty)

(defun pduty--create-org-time-stamp (datetime-string)
  "Create 'org-mode' time stamp.  DATETIME-STRING is ISO 8601 style string."
  (when datetime-string
    (let* (datetime-parts)
      (setq datetime-parts (parse-time-string datetime-string))
      (format "<%04d-%02d-%02d>"
              (nth 5 datetime-parts)
              (nth 4 datetime-parts)
              (nth 3 datetime-parts)))))

(defun pduty--list-oncalls ()
  "Get Oncall list (date)."
  (unless pduty-user-id
    (error "Required User ID"))
  (let* ((result (list))
         (json (pduty-api-oncalls-get pduty-user-id))
         (oncalls (gethash :oncalls json)))
    (dolist (oncall oncalls)
      (let* ((start-date-string (pduty--create-org-time-stamp
                                 (gethash :start oncall)))
             (end-date-string (pduty--create-org-time-stamp
                               (gethash :end oncall))))
        (when (and start-date-string end-date-string)
          (add-to-list 'result (format "%s--%s" start-date-string end-date-string)))))
      result))

(provide 'pduty)
;;; pduty.el ends here
