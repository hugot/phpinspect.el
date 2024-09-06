;;; phpinspect-test.el --- Unit tests for phpinspect.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

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
(require 'phpinspect)

(require 'phpinspect-test-env
         (expand-file-name "phpinspect-test-env.el"
                           (file-name-directory (macroexp-file-name))))

(ert-deftest phpinspect-get-variable-type-in-block ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = new \\DateTime(); $bar = $foo; Whatever comes after don't matter.")
         (bmap (phpinspect-parse-string-to-bmap code))
         (tokens (phpinspect-parse-string "class Foo { function a(\\Thing $baz) { $foo = new \\DateTime(); $bar = $foo;"))
         (context (phpinspect--get-resolvecontext (phpinspect-current-project) tokens))
         (bmap-context (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (- (length code) 36)))
         (project-root "could never be a real project root")
         (phpinspect-project-root-function
          (lambda (&rest _ignored) project-root))
         (project (phpinspect--make-project
                              :fs (phpinspect-make-virtual-fs)
                              :root project-root
                              :worker (phpinspect-make-worker))))

    (puthash project-root project (phpinspect--cache-projects phpinspect-cache))

    (let ((result (phpinspect-get-variable-type-in-block
                   context "foo"
                   (phpinspect-function-block
                    (car (phpinspect--resolvecontext-enclosing-tokens context)))
                   (phpinspect--make-type-resolver-for-resolvecontext context)))
          (bmap-result (phpinspect-get-variable-type-in-block
                        bmap-context "foo"
                        (phpinspect-function-block
                         (car (phpinspect--resolvecontext-enclosing-tokens context)))
                        (phpinspect--make-type-resolver-for-resolvecontext context))))
      (should (phpinspect--type= (phpinspect--make-type :name "\\DateTime")
                                 result))

      (should (phpinspect--type= (phpinspect--make-type :name "\\DateTime")
                                 bmap-result)))))

(ert-deftest phpinspect-get-pattern-type-in-block ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = new \\DateTime(); $this->potato = $foo;")
         (bmap (phpinspect-parse-string-to-bmap "class Foo { function a(\\Thing $baz) { $foo = new \\DateTime(); $this->potato = $foo;"))
         (context (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (length code)))
         (project-root "could never be a real project root")
         (phpinspect-project-root-function
          (lambda (&rest _ignored) project-root))
         (project (phpinspect--make-project
                              :fs (phpinspect-make-virtual-fs)
                              :root project-root
                              :worker (phpinspect-make-worker))))

    (puthash project-root project (phpinspect--cache-projects phpinspect-cache))

    (let ((result (phpinspect-get-pattern-type-in-block
                   context (phpinspect--make-pattern :m `(:variable "this")
                                                     :m `(:object-attrib (:word "potato")))
                   (phpinspect-function-block
                    (car (phpinspect--resolvecontext-enclosing-tokens context)))
                   (phpinspect--make-type-resolver-for-resolvecontext context))))
      (should (phpinspect--type= (phpinspect--make-type :name "\\DateTime")
                                 result)))))

(ert-deftest phpinspect-get-resolvecontext-multi-strategy ()
  (let* ((code1 "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; $bar = $foo[0]; $bork = [$foo[0]]; $bark = $bork[0]; $borknest = [$bork]; $barknest = $borknest[0][0]; }}")
         (code2  "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; $bar = $foo[0]; $bork = [$foo[0]]; $bark = $bork[0]; $borknest = [$bork]; $barknest = $borknest[0][0]")
         (bmap (phpinspect-parse-string-to-bmap code1))
         (tokens (phpinspect-parse-string code2))
         (context1 (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (- (length code1) 4)))
         (context2 (phpinspect--get-resolvecontext (phpinspect-current-project) tokens)))

    (should (equal (phpinspect--resolvecontext-subject context1)
                   (phpinspect--resolvecontext-subject context2)))
    (should (= (length (phpinspect--resolvecontext-enclosing-tokens context1))
               (length (phpinspect--resolvecontext-enclosing-tokens context2))))))

(ert-deftest phpinspect-get-variable-type-in-block-array-foreach ()
  (let* ((code "class Foo { function a(\\Thing $baz) { $foo = []; $foo[] = $baz; foreach ($foo as $bar) {$bar->")
         (bmap (phpinspect-parse-string-to-bmap code))
         (context (phpinspect-get-resolvecontext (phpinspect--make-dummy-project) bmap (length code)))
         (project-root "could never be a real project root")
         (phpinspect-project-root-function
          (lambda (&rest _ignored) project-root))
         (project (phpinspect--make-project
                   :fs (phpinspect-make-virtual-fs)
                   :root project-root
                   :worker (phpinspect-make-worker))))

    (puthash project-root project (phpinspect--cache-projects phpinspect-cache))

    (let* ((function-token (seq-find #'phpinspect-function-p
                                     (phpinspect--resolvecontext-enclosing-tokens context)))
           (result (phpinspect-get-variable-type-in-block
                    context "bar"
                    (phpinspect-function-block function-token)
                    (phpinspect--make-type-resolver-for-resolvecontext context)
                    (phpinspect-function-argument-list function-token))))

      (should (phpinspect--type= (phpinspect--make-type :name "\\Thing")
                                 result)))))

(ert-deftest phpinspect--find-assignment-ctxs-in-token ()
  (let* ((tokens (cadr
                  (phpinspect-parse-string "{ $foo = ['nr 1']; $bar = $nr2; if (true === ($foo = $nr3)) { $foo = $nr4; $notfoo = $nr5; if ([] === ($foo = [ $nr6 ])){ $foo = [ $nr7 ];}}}")))
         (expected '(((:variable "foo")
                      (:assignment "=")
                      (:array
                       (:variable "nr7")))
                     ((:variable "foo")
                      (:assignment "=")
                      (:array
                       (:variable "nr6")))
                     ((:variable "notfoo")
                      (:assignment "=")
                      (:variable "nr5"))
                     ((:variable "foo")
                      (:assignment "=")
                      (:variable "nr4"))
                     ((:variable "foo")
                      (:assignment "=")
                      (:variable "nr3"))
                     ((:variable "bar")
                      (:assignment "=")
                      (:variable "nr2"))
                     ((:variable "foo")
                      (:assignment "=")
                      (:array
                       (:string "nr 1")))))
         (assignments (phpinspect--find-assignment-ctxs-in-token tokens)))

    (should (equal expected (mapcar #'phpinspect--actx-tokens assignments)))))


(ert-deftest phpinspect-resolve-type-from-context ()
  (let* ((pctx (phpinspect-make-pctx :incremental t :bmap (phpinspect-make-bmap)))
         (code "
namespace Amazing;

class FluffBall
{
    private $fluffer;

    public function __construct(Fluffer $fluffer)
    {
        $this->fluffer = $fluffer;
    }

    public function beFluffy(): void
    {
        $ball = $this->fluffer;

        if ($ball) {
            if(isset($ball->fluff()->poof->upFluff->)) {
                $this->beFluffy();
            }
        }

        $ball->fluff()->poof->
     }
}")
         (token-tree (phpinspect-with-parse-context pctx
                       (phpinspect-parse-string code)))
         (bmap (phpinspect-pctx-bmap pctx))
         (fluffer  (phpinspect-parse-string "
namespace Amazing;

use Vendor\\FluffLib\\Fluff;

class Fluffer
{
    public function fluff(): Fluff
    {
    }
}"))
        (vendor-fluff (phpinspect-parse-string "
namespace Vendor\\FluffLib;
class Fluff
{
    /**
     * @var FlufferUpper
     */
    public $poof;
}"))
        (vendor-fluffer-upper (phpinspect-parse-string "
namespace Vendor\\FluffLib;
class FlufferUpper
{
    public $upFluff;
    public function __construct(DoubleFluffer $upFluff)
    {
        $this->upFluff = $upFluff;
    }
}"))
        (project (phpinspect--make-dummy-project))
        (context (phpinspect-get-resolvecontext project bmap 310)))

    (phpinspect-purge-cache)
    (dolist (tree (list token-tree fluffer vendor-fluff vendor-fluffer-upper))
      (phpinspect-project-add-index project (phpinspect--index-tokens tree)))

    (should (phpinspect--type=
             (phpinspect--make-type :name "\\Vendor\\FluffLib\\DoubleFluffer")
             (phpinspect-resolve-type-from-context
              context
              (phpinspect--make-type-resolver-for-resolvecontext
               context))))

    (setq context (phpinspect-get-resolvecontext project bmap 405))
    (should (phpinspect--type=
             (phpinspect--make-type :name "\\Vendor\\FluffLib\\FlufferUpper")
             (phpinspect-resolve-type-from-context
              context
              (phpinspect--make-type-resolver-for-resolvecontext
               context))))))


(ert-deftest phpinspect-get-last-statement-in-token-with-static-attribute-context ()
  (let* ((php-code-function "
    function doStuff()
    {
        return self::doThing()")
           (php-code-block "
    {
        return self::doThing()")
           (php-code-preceding-block "
    function doStuff()
    {
        if (true === true) {
            forach ($things as $k => $v) {
            }
        }
        self::doThing()")
           (php-code-bare "Thing::something(); Something::other()")
           (get-last-statement
            (lambda (php-code)
              (phpinspect--get-last-statement-in-token
               (car (cdr (phpinspect-parse-string php-code)))))))

      (should (equal `((:word "return") (:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-function)))
      (should (equal `((:word "return") (:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-block)))
      (should (equal `((:word "self") (:static-attrib (:word "doThing"))
                       (:list))
                     (funcall get-last-statement php-code-preceding-block)))
      (should (equal `((:word "Something") (:static-attrib (:word "other"))
                       (:list))
                     (phpinspect--get-last-statement-in-token
                      (phpinspect-parse-string php-code-bare))))))

(ert-deftest phpinspect--find-assignment-by-predicate ()
  (let* ((token '(:block
                  (:variable "bam") (:object-attrib "boom") (:assignment "=")
                  (:variable "beng") (:terminator)
                  (:variable "notbam") (:word "nonsense") (:assignment "=") (:string) (:terminator)
                  (:variable "bam") (:comment) (:object-attrib "boom") (:assignment "=")
                  (:variable "wat") (:object-attrib "call") (:terminator)))
         (assignments (phpinspect--find-assignment-ctxs-in-token token))
         (result (phpinspect--find-assignment-by-predicate
                  assignments
                  (phpinspect--match-sequence-lambda :m `(:variable "bam") :m `(:object-attrib "boom")))))

    (should result)
    (should (equal '((:variable "bam") (:object-attrib "boom"))
                   (phpinspect--assignment-to result)))

    (should (equal '((:variable "wat") (:object-attrib "call"))
                   (phpinspect--assignment-from result)))

    (setq result (phpinspect--find-assignment-by-predicate
                  (phpinspect--actx-preceding-assignments
                   (phpinspect--assignment-ctx result))
                  (phpinspect--match-sequence-lambda :m `(:variable "bam") :m `(:object-attrib "boom"))))
    (should result)

    (should (equal '((:variable "bam") (:object-attrib "boom"))
                   (phpinspect--assignment-to result)))

    (should (equal '((:variable "beng"))
                   (phpinspect--assignment-from result)))))

(ert-deftest phpinspect-parse-function-missing-open-block ()
  (let ((parsed (phpinspect-parse-string "function bla() echo 'Hello'}")))
    (should (equal '(:root (:function
                            (:declaration (:word "function") (:word "bla") (:list)
                                          (:word "echo") (:word "Hello"))))
                   parsed))))

(ert-deftest phpinspect-parse-string-token ()
  (let ((parsed (phpinspect-parse-string "<?php 'string'")))
    (should (equal '(:root (:string "string")) parsed))))

(load-file (concat phpinspect-test-directory "/test-worker.el"))
(load-file (concat phpinspect-test-directory "/test-autoload.el"))
(load-file (concat phpinspect-test-directory "/test-eldoc.el"))
(load-file (concat phpinspect-test-directory "/test-fs.el"))
(load-file (concat phpinspect-test-directory "/test-project.el"))
(load-file (concat phpinspect-test-directory "/test-buffer.el"))
(load-file (concat phpinspect-test-directory "/test-index.el"))
(load-file (concat phpinspect-test-directory "/test-typedef.el"))
(load-file (concat phpinspect-test-directory "/test-type.el"))
(load-file (concat phpinspect-test-directory "/test-util.el"))
(load-file (concat phpinspect-test-directory "/test-bmap.el"))
(load-file (concat phpinspect-test-directory "/test-edtrack.el"))
(load-file (concat phpinspect-test-directory "/test-resolvecontext.el"))
(load-file (concat phpinspect-test-directory "/test-parser.el"))
(load-file (concat phpinspect-test-directory "/test-parse-context.el"))
(load-file (concat phpinspect-test-directory "/test-splayt.el"))
(load-file (concat phpinspect-test-directory "/test-pipeline.el"))
(load-file (concat phpinspect-test-directory "/test-toc.el"))
(load-file (concat phpinspect-test-directory "/test-meta.el"))
(load-file (concat phpinspect-test-directory "/test-resolve.el"))
(load-file (concat phpinspect-test-directory "/test-imports.el"))
(load-file (concat phpinspect-test-directory "/test-suggest.el"))
(load-file (concat phpinspect-test-directory "/test-completion.el"))
(load-file (concat phpinspect-test-directory "/test-token-predicates.el"))


(provide 'phpinspect-test)
;;; phpinspect-test.el ends here
