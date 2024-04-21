;;; pduty-api-test.el --- Access PagerDuty API  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Hiroaki ENDOH

;; Author: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; Maintainer: Hiroaki ENDOH <hiroakiendoh@gmail.com>
;; URL: https://github.com/hiroakit/pduty

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
;; Test for PagerDuty API
;;

;;; Code:

(require 'ert)
(require 'pduty-api)

(setq pduty-test-dummy-user-token-key "sample-token-key")
(setq pduty-test-user-id (getenv "PDUTY_USER_ID"))

(ert-deftest request-header-authorization-token-nothing ()
  (setq pduty-api-user-token-key nil)
  (should-error (pduty-api--request-headers) :type 'error))

(ert-deftest request-header-authorization ()
  (setq pduty-api-user-token-key pduty-test-dummy-user-token-key)
  (let ((headers (list (cons "Authorization" (format "Token token=%s" pduty-test-dummy-user-token-key))
                       '("Accept" . "application/json")
                       '("Content-Type" . "application/json"))))
    (should (equal (pduty-api--request-headers) headers))))

(ert-deftest pduty-api-oncalls-get-200 ()
  (setq pduty-api-user-token-key (getenv "PDUTY_USER_TOKEN_KEY"))
  (let* ((response (pduty-api-oncalls-get pduty-test-user-id)))
    (should (equal (hash-table-p response) t))))

(ert-deftest pduty-api-oncalls-get-401 ()
  (setq pduty-api-user-token-key pduty-test-dummy-user-token-key)
  (should-error (pduty-api-oncalls-get pduty-test-user-id) :type 'error))

(ert-deftest pduty-api-schedules-get-200 ()
  (setq pduty-api-user-token-key (getenv "PDUTY_USER_TOKEN_KEY"))
  (let* ((response (pduty-api-schedules-get)))
    (should (equal (hash-table-p response) t))))

(ert-deftest pduty-api-schedules-get-401 ()
  (setq pduty-api-user-token-key pduty-test-dummy-user-token-key)
  (should-error (pduty-api-schedules-get) :type 'error))

(provide 'pduty-api-test)
;;; pduty-api-test.el ends here
