[![elpa build badge](https://elpa.gnu.org/packages/phpinspect.svg)](https://elpa.gnu.org/packages/phpinspect.html)

# phpinspect.el
PHPInspect is a minor mode that provides code intelligence for PHP in Emacs. At
its core is a PHP parser implemented in Emacs Lisp. PHPInspect comes with
backends for `completion-at-point`, `company-mode` and `eldoc`. A backend for
`xref` (which provides go-to-definition functionality) is planned to be
implemented at a later date. The main documentation of the mode is in the
docstring of the mode itself (`C-h f phpinspect-mode RET` to view, or read it in
the source code of [phpinspect.el](phpinspect.el)).

## Installation
phpinspect.el is available as a
[package](https://elpa.gnu.org/packages/phpinspect.html) in GNU ELPA. Install it
via package.el or an emacs package manager of your choice.

### Install from git
When installing from git, make sure to checkout the master branch instead of the
devel branch if you prioritize stability. The devel branch will be home to the
latest features, but also the latest bugs 🐛 .

```bash
git clone https://github.com/hugot/phpinspect.el ~/projects/phpinspect.el
cd ~/projects/phpinspect.el
make
```

```elisp
(add-to-list 'load-path "~/projects/phpinspect.el")
(require 'phpinspect)
```


## Projects and Finding Types
By default, phpinspect will recognize composer projects and read their
composer.json files for autoload information which is used to find files in
which the types/classes/functions you use in your code are defined. It is also
possible to add an "include directory" of files that should always be read and
indexed for a certain project. To do this, open a file in a project and run `M-x
phpinspect-project-add-include-dir`. You can also edit the list of include
directories via `M-x customize-goup RET phpinspect RET`.

## Example Configuration
If you already have a completion UI setup that is able to use
`completion-at-point-functions` as completion source, you can basically just
enable phpinspect-mode and you'll be good to go. An example of a basic mode hook
configuration to get the most out of phpinspect is the following:

```elisp
(defun my-php-personal-hook ()
  ;; Shortcut to add use statements for classes you use.
  (define-key php-mode-map (kbd \"C-c u\") 'phpinspect-fix-imports)

  ;; Shortcuts to quickly search/open files of PHP classes.
  ;; You can make these local to php-mode, but making them global
  ;; like this makes them work in other modes/filetypes as well, which
  ;; can be handy when jumping between templates, config files and PHP code.
  (global-set-key (kbd \"C-c a\") 'phpinspect-find-class-file)
  (global-set-key (kbd \"C-c c\") 'phpinspect-find-own-class-file)

  ;; Enable phpinspect-mode
  (phpinspect-mode))

(add-hook 'php-mode-hook #'my-php-personal-hook)
```

## Example config with company mode setup

```elisp
;;;###autoload
(defun my-php-personal-hook ()
  ;; It is important to enable `company-mode' before setting
  ;; the variables below.
  (company-mode)
  (setq-local company-minimum-prefix-length 0)
  (setq-local company-tooltip-align-annotations t)
  (setq-local company-idle-delay 0.1)
  (setq-local company-backends '(phpinspect-company-backend))

  ;; Shortcut to add use statements for classes you use.
  (define-key php-mode-map (kbd "C-c u") 'phpinspect-fix-imports)

  ;; Shortcuts to quickly search/open files of PHP classes.
  (global-set-key (kbd "C-c a") 'phpinspect-find-class-file)
  (global-set-key (kbd "C-c c") 'phpinspect-find-own-class-file)

  (phpinspect-mode))

(add-hook 'php-mode-hook #'my-php-personal-hook)
```

## Compilation
It is highly recommended to byte- or native compile phpinspect. Aside from the
normal performance boost that this brings to most packages, it can reduce
phpinspect's parsing time by up to 90%. It especially makes a difference when
incrementally parsing edited buffers. For example:

### benchmarks/parse-file.el uncompiled on Ryzen 5 3600 (time in seconds):
```
Incremental parse (warmup):
Elapsed time: 0.168390 (0.019751 in 1 GC’s)
Incremental parse:
Elapsed time: 0.143811 (0.000000 in 0 GC’s)
Incremental parse (no edits):
Elapsed time: 0.000284 (0.000000 in 0 GC’s)
Incremental parse repeat (no edits):
Elapsed time: 0.000241 (0.000000 in 0 GC’s)
Incremental parse after buffer edit:
Elapsed time: 0.012449 (0.000000 in 0 GC’s)
Incremental parse after 2 more edits:
Elapsed time: 0.015839 (0.000000 in 0 GC’s)
Bare (no token reuse) parse (warmup):
Elapsed time: 0.048996 (0.000000 in 0 GC’s)
Bare (no token reuse) parse:
Elapsed time: 0.052495 (0.000000 in 0 GC’s)
```

### benchmarks/parse-file.el with native compilation on Ryzen 5 3600 (time in seconds):
```
Incremental parse (warmup):
Elapsed time: 0.023432 (0.000000 in 0 GC’s)
Incremental parse:
Elapsed time: 0.018350 (0.000000 in 0 GC’s)
Incremental parse (no edits):
Elapsed time: 0.000076 (0.000000 in 0 GC’s)
Incremental parse repeat (no edits):
Elapsed time: 0.000058 (0.000000 in 0 GC’s)
Incremental parse after buffer edit:
Elapsed time: 0.001212 (0.000000 in 0 GC’s)
Incremental parse after 2 more edits:
Elapsed time: 0.001381 (0.000000 in 0 GC’s)
Bare (no token reuse) parse (warmup):
Elapsed time: 0.013874 (0.000000 in 0 GC’s)
Bare (no token reuse) parse:
Elapsed time: 0.013878 (0.000000 in 0 GC’s)
```

## Development
The codebase of phpinspect is relatively large. As a new contributor, you'll
probably feel a bit lost in the many files, functions and datatypes of the
package. Let me tell you a bit about phpinspect's architecture.

### Domains
There are roughly four main domains under which code in phpinspect can fall:
parsing, indexation, interpetation and user-facing functions. Below is a short
explanation of each domain.

#### Parsing
The parser provides the underpinnings for almost all of phpinspect's
functionalities. It is implemented with specialized macros and supports two main
parsing modes: bare and incremental. The parser code is located in
`phpinspect-parser.el`.

##### Bare Parsing
The parser produces a parse result in the form of a basic list, where each
parsed token is a list itself. The car of each list contains a keyword,
describing its meaning in the parsed code. Below is a lisp form and its
resulting insertion to demonstrate the structure of a syntax tree produced by
the bare parser.

`(insert (pp-to-string (phpinspect-parse-string "function foo(Bar $bar) { return $bar->baz; }")))`

```elisp
(:root
 (:function
  (:declaration
   (:word "function")
   (:word "foo")
   (:list
    (:word "Bar")
    (:variable "bar")))
  (:block
   (:word "return")
   (:variable "bar")
   (:object-attrib
    (:word "baz"))
   (:terminator ";"))))

```

##### Incremental Parsing
Bare parsing is fast, because it makes use of simple lisp datastructures and
keeps the parser logic simple. A downside of this parsing mode is that the
result lacks any extra metadata about the tokens, like what their start- and
endpoints are in a buffer. It also makes it hard to partially update the tree
after a buffer has been edited. As both are essential for a program that needs
to work efficiently in live-edited buffers, the incremental parser was
implemented. When parsing incrementally, the parser still produces the same
structure of nested lists as a result. But it also stores metadata about each
parsed token in an n-ary tree of `phpinspect-meta` objects (see
`phpinspect-meta.el`). The incremental parser is able to partially update an
existing tree after code has been edited, making it efficient for live buffers.

##### Parsers Summarised
The incremental parsing is efficient for use with live edited buffers. Due to
its higher complexity it is not as fast at parsing entire files as the bare
parser. For this reason, both the bare and the incremental parser are used for
what they are best at.

#### Indexation
Within phpinspect, indexation is referred to as the process of extracting
information from parsed code and project configuration files.

##### Code Indexation
The code in
`phpinspect-index.el` consumes bare syntax trees and extracts information about
PHP functions and types from them. Code in `phpinspect-buffer.el` implements the
same functionality, but for live buffers based on the data produced by the
incremental parser.

##### Project Indexation
Code in `phpinspect-autoload.el` implements logic for `psr-4`, `psr-0` and
`files` autoload strategies. It is able to read a composer.json file and
generate autoload information based on which a project's types and functions can
be found.

##### Caching
Code in `phpinspect-cache.el` and `phpinspect-class.el` allows phpinspect to
store the result of code indexations in memory, so that they can be accessed
instantly when a user requests information about their code.

##### Threading
Indexation takes time. Because emacs is single-threaded, you might think that
indexing a project would lock up your emacs for a while. Do not despair!
`phpinspect-worker.el` contains code for a worker with a job queue that uses
collaborative threads to do work when emacs is idle. When you start interacting
with emacs, it will back off and let you do your thing! `phpinspect-pipeline.el`
contains code for something similar to generators in PHP, combined with
collaborative threading. Pipelines are used for more intense processes that
should be completed with a bit more of a hurry. Emacs should remain responsive
while a pipeline is running, but there may be a slightly noticeable sluggishness
while they run. At the moment of writing, pipelines are mainly used to refresh a
projects autoloader.

#### Interpretation
Having parsing, indexation and caching infrastructure is all well and good, but
how to we determine what information is actually useful to show to a user? This
is where code interpretation plays a big role. The main goal of interpretation
in phpinspect is the determination of the PHP type to display information
about. Determining this type is referred to as "resolving" in phpinspect's
codebase. Resolving a type is built around the concept of a "resolvecontext". A
resolvecontext is an object that contains information about a location in a
buffer and its surroundings. Based on the informatoin in the resolvecontext, the
type that a statement is expected to evaluate to can be determined. The code for
resolving types is mostly contained in `phpinspect-type.el`,
`phpinspect-resolve.el` and `phpinspect-resolvecontext.el`. The main entrypoints
are the functions `phpinspect-get-resolvecontext` and
`phpinspect-resolve-type-from-context`.

#### User-facing Functions
User-facing functions in phpinspect are mostly integrations with existing
infrastructure within emacs. For completion there is
`completion-at-point-functions`, for tooltips there is `eldoc` and for
go-to-definition there is `xref`. Aside from these integrations, phpinspect aims
to provide functionalities for code formatting.

##### Completion
Completion is implemented in `phpinspect-completion.el` as a strategy pattern. A
completion strategy can be added by implementing the methods
`phpinspect-comp-strategy-supports` and
`phpinspect-comp-strategy-execute`. Completion strategies are high level
abstractions that build on top of the type resolving code and the code in
`phpinspect-suggest.el`.

##### Eldoc
Eldoc support is implemented in a strategy pattern similar to that of
completion. An eldoc strategy can be added by implementing the methods
`phpinspect-eld-strategy-supports` and `phpinspect-eld-strategy-execute`.

##### Code Formatting
`phpinspect-fix-imports` adds, removes and sorts use statements. At the time of
writing, this is the only code formatting functionality that phpinspect
provides. See `phpinspect-imports.el` for the implementation.

##### Really Want To But Not Implemented: Xref
A thing that would be possible to implement on top of phpinspect's
infrastructure, but not much time has been spent on yet, is an integration with
`xref`. Xref integration would enable functionalities like go-to-definition.

### Building

```bash
make
```

### Running tests
Tests are implemented using `ert`. You can run them in batch mode with the following
command:

```bash
make test

# or:
emacs -L ./ -batch -l ert -l ./test/phpinspect-test.el -f ert-run-tests-batch-and-exit
```
