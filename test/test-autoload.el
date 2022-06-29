;;; test-autoload.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Hugo Thunnissen <devel@hugot.nl>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'ert)
(require 'phpinspect-fs)
(require 'phpinspect-autoload)

(ert-deftest phpinspect-psr0-fill-typehash ()
  (let* ((fs (phpinspect-make-virtual-fs))
         (typehash (make-hash-table :size 10 :test 'eq))
         (autoload
           (phpinspect-make-psr0-generated :prefix "App\\")))

    (puthash "/home/user/projects/app/src/App/Services/SuperService.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (puthash "/home/user/projects/app/src/Kernel.php"
             ""
             (phpinspect-virtual-fs-files fs))
    (puthash "/home/user/projects/app/src/App/Controller/Banana.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (puthash "/home/user/projects/app/lib/Mailer_Lib.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (setf (phpinspect-psr0-directories autoload) (list "/home/user/projects/app/src/"
                                                       "/home/user/projects/app/lib/"))

    (phpinspect-al-strategy-fill-typehash autoload fs typehash)

    (should-not (hash-table-empty-p typehash))

    (should (string= "/home/user/projects/app/src/App/Services/SuperService.php"
                     (gethash (phpinspect-intern-name "\\App\\Services\\SuperService")
                              typehash)))
    (should (string= "/home/user/projects/app/src/Kernel.php"
                     (gethash (phpinspect-intern-name "\\Kernel")
                              typehash)))
    (should (string= "/home/user/projects/app/src/App/Controller/Banana.php"
                     (gethash (phpinspect-intern-name "\\App\\Controller\\Banana")
                              typehash)))

    (should (string= "/home/user/projects/app/lib/Mailer_Lib.php"
                     (gethash (phpinspect-intern-name "\\Mailer_Lib")
                              typehash)))))

(ert-deftest phpinspect-psr4-fill-typehash ()
  (let* ((fs (phpinspect-make-virtual-fs))
         (typehash (make-hash-table :size 10 :test 'eq))
         (autoload
           (phpinspect-make-psr4-generated :prefix "App\\")))

    (puthash "/home/user/projects/app/src/Services/SuperService.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (puthash "/home/user/projects/app/src/Kernel.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (puthash "/home/user/projects/app/src/Controller/Banana.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (puthash "/home/user/projects/app/lib/Mailer_Lib.php"
             ""
             (phpinspect-virtual-fs-files fs))

    (setf (phpinspect-psr4-directories autoload) (list "/home/user/projects/app/src/"
                                                       "/home/user/projects/app/lib/"))

    (phpinspect-al-strategy-fill-typehash autoload fs typehash)

    (should-not (hash-table-empty-p typehash))

    (should (string= "/home/user/projects/app/src/Services/SuperService.php"
                     (gethash (phpinspect-intern-name "\\App\\Services\\SuperService")
                              typehash)))
    (should (string= "/home/user/projects/app/src/Kernel.php"
                     (gethash (phpinspect-intern-name "\\App\\Kernel")
                              typehash)))
    (should (string= "/home/user/projects/app/src/Controller/Banana.php"
                     (gethash (phpinspect-intern-name "\\App\\Controller\\Banana")
                              typehash)))

    (should (string= "/home/user/projects/app/lib/Mailer_Lib.php"
                     (gethash (phpinspect-intern-name "\\App\\Mailer_Lib")
                              typehash)))))
