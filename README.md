# islisp-compat

`islisp-compat` is an implementation of ISLisp (ISO/IEC 13816:2007(E))
running on top of Common Lisp.

`islisp-compat` began as a collection of macros and functions used to
bootstrap an ISLisp compiler using Common Lisp. It has since grown
into a complete implementation of ISO/IEC 13816:2007(E) running on top
of Common Lisp. It intends to be as minimally compliant as possible:
it implements few-to-none extensions, even when they would be
effectively essential for the language to be useful.

`islisp-compat` should run on any ANSI standard-compliant Common Lisp
implementation on any operating system. It is primarily tested on
SBCL, ClozureCL, and CLisp on MacOS and Linux.

## Running

Start up your favorite Common Lisp implementation and do one of the
following in its REPL:

You can either load everything with the load.lisp file:
`(load "load.lisp")`
If you need to load the files from a specific location, you can`defvar`
`*islisp-compat-root*` to the project's root directory before loading.
There is also another `defvar`ed variable: `*islisp-compat-muffle-warnings*`,
which muffles all warnings when loading if it is true.

Alternately, if ASDF is available, you can run:
```
(require :asdf)
(push "path/to/islisp-compat/" asdf:*central-registry*)
(asdf:load-system :islisp-compat)
```

Once `islisp-compat` is loaded in, go into its user package with `(in-package :islisp-user)` and you should be good to go with the ISLisp functions running in the Common Lisp REPL.

## Package details

`islisp-compat` defines three packages: `islisp`, `islisp-user`, and `islisp-sys`.
* The `islisp` package only exports the symbols from the
language specification, and only imports `t`, `nil`, `quote`, `function`, and
`&rest`from the `common-lisp` package.
(Once upon a time, this package was locked on implementations
that supported it, but it is now unlocked to allow variables to be defined with the same names as the predefined functions (which seems to be allowed per the spec).)
* The `islisp-user` package is the one that users are expected to use
when working with `islisp-compat`. It is simply an empty package with
the symbols from `islisp` and some helper functions from
`islisp-sys`. The reader interns everything into here, by default.
* The `islisp-sys` package has all the functions and objects required to
implement the `islisp` package along with a couple utility functions.

## Deviations from the standard

The biggest "bug" in the system is that lambda forms are not
allowed to be the first part of an S-expression.
The code `((lambda (x) (+ x 1)) 5)`, for example, will
compile/evaluate with errors. The reason is that `lambda` is redefined
as a macro so the lambda parameters can be validated.
Given the choice between allowing a rarely-used form or ensuring the
lambda lists used are correct, we went with verifying the lambda lists.
Just put a `funcall` in front of the `(lambda ...)` and you'll be fine.

The ISLisp specification says `def`* forms are only allowed at the program
toplevel and cannot be redefined. `islisp-compat` allows both redefinition
and allows defining forms in non-standard locations.
(Older versions of `islisp-compat` did extra checking to prevent both,
but the checks did not play well with the platform implementation's REPL
and so have been removed.)

`defmacro` forms are not restricted in what functions they can use.
They can call functions with side-effects and user-defined functions.

Symbols that start with a `&` are allowed and are treated like any other
(non-keyword) symbol.

Symbols with an unescaped colon in the name are treated like the are in Common
Lisp and are scoped/interned in the package that is specified before
the colon. Thus, `foo:bar` denotes the symbol `bar` in the package `foo`
and the package `foo` needs to exist.

## Built-in-classes

The classes in ISLisp evaluate to the equivalent class in Common Lisp with two exceptions:

* There is no class in Common Lisp that's equivlent to ISLisp's `<basic-array*>`. It can be emulated with the type annotation `(and (array * *) (not (vector *)))`, but there is nothing that's able to be passed to `cl:find-class`. Uses of the `assure` special form will work, but any other testing using `(class <basic-array*>)` might fail. Both `(class <basic-array*>)` and `(class <general-array*>)` evaluate to `cl:simple-array`.
* The ISLisp class `<parse-error>` is defined as a new condition. There is a `cl:parse-error`, but since there are no equivalents of the functions `parse-error-string` and `parse-error-expected-class` in Common Lisp, `islisp-compat` provides its own `<parse-error>` class that inherits from `cl:parse-error`. The `(class <parse-error>)` special form returns the ISLisp condition, not the Common Lisp one.

Despite `(class <built-in-class>)` evaluating to the same as `(find-class 'cl:built-in-class)`, tests for `<built-in-class>`es require some extra work on the implementation side. `islisp-compat` considers any class that's a `cl:built-in-class`, a `cl:serious-condition`, or not a `cl:standard-class` as an ISLisp "`<built-in-class>`". The reason for this is that conditions are a special type of class in Common Lisp and the Common Lisp standard does not require things listed as a "System Class" to be a built-in class. (A notable example is SBCL's sequence class is a `sb-pcl:system-class` not a `cl:built-in-class`.)

## Conditions

`with-handler` binds its handler to Common Lisp's `serious-condition`
and will pick up any Common Lisp condition that inherits from it.

The ISLisp standard allows the condition classes to either be `<built-in-class>`es or `<standard-class>`es. `islisp-compat` follows the usage of TISL, OpenLisp, and OKI Lisp in considering the condition classes all as built-in classes and therefore not extendable, nor `create`able.

However, in Common Lisp a `condition` is not a `built-in-class`, so there is special code in `assure` and `instancep` to maintain this fiction on the ISLisp side.

(Almost) all the classes in `islisp-compat` that denote conditions match the equivalent condition in Common Lisp. Any condition handling code in Common Lisp that calls into ISLisp code should pretty much work without modification; the opposite should (mostly) work as well.

The exception is `<parse-error>`. As mentioned above, there is a `cl:parse-error`, but since there are no equivalents of the functions `parse-error-string` and `parse-error-expected-class` in Common Lisp, `islisp-compat` provides its own `<parse-error>` class that inherits from `cl:parse-error`.

Any existing CL code handling `cl:parse-error` should work with `islisp:<parse-error>` but passing a `cl:parse-error` to any ISLisp code that expects the ISLisp one may fail. (The `class-of` function can distinguish between the two, however.)

## Continuable conditions

Because `continue-condition` can pass back a value, the `signal-condition` function creates a restart for `use-value` (instead of the expected `continue`) if the `continuable` parameter is a string.

`condition-continuable` considers a condition continuable if either a `continue` restart is associated with it **or** a `use-value` restart. This allows continuable conditions from the Common Lisp side to mostly work as expected.

`continue-condition` will call either the most recent `continue` or `use-value` restart. Note, that in the case of a `continue` restart, the value parameter to `continue-condition` is ignored. This should not affect pure ISLisp code.

## Numbers

All ISLisp math routines only take integers and floating point values.
Ratios and complex numbers are not allowed. Thus, ISLisp's
`numberp` and `(assure <number> ...)` will fail if it receives a
ratio or complex number. 

As a convienence, the `convert` special form will accept ratio
values when converting to `<float>`.

## Keywords

ISLisp implementations are allowed a lot of leeway with how they
handle keywords. Since it is running on top of Common Lisp, `islisp-compat`
simply does the Common Lisp behavior by default: keywords are symbols that
evaluate to themselves. They cannot be used as variable names.

There is a non-standard function in the `islisp` package named `keywordp` that tests if a value is a keyword. Since there is no standard form of a keyword in ISLisp, this is necessary for testing them (and should really be added to the standard).

The reader can be configured to not use this behavior though (see the documentation for configuring the reader).

## Dynamic and lexical variables

In ISLisp, special/dynamic variables (defined with `defdynamic`) and
lexical global variables (defined with `defglobal`) are considered
variables in different namespaces. Common Lisp doesn't have a notion
of lexical global variables, and the implmentations that do (SBCL,
Clozure, Lispworks) typically implement them in a manner that is
unusable in standard ISLisp (i.e. they don't allow local shadowing,
which is required in ISLisp).

Lexical global variables are implemented as a symbol macro pointing to a
global dynamic variable with a random name. This symbol macro gets shadowed by
local lexical bindings, so this all works. One side effect of this,
however, is that `cl:boundp` will not work for `defglobal`ed variables.

Dynamic global variables are simply implemented using `defparameter`.
However, to allow the variables to peacefully coexist with `defglobal`
and local bindings, all dynamic variables in `islisp-compat` are
prefixed with `%dynamic-`. Thus, `(defdynamic *print-level* 0)` would create a global special variable named `%dynamic-*print-level*`.

This means:
1) for the love of all that's holy, don't name a global variable `%dynamic-something`,
2) if you need to access these variables in Common Lisp code, you really should use the `islisp:dynamic` or `islisp:dynamic-let` special forms.

## Implementation defined/dependent behavior

The neutral alphabetic case for symbols is uppercase (the same as Common Lisp).
This is configurable by the reader, however.

The format of filenames obviously depends on the Common Lisp implementation
and the operating system.

The character names allowed after `#\` (i.e. `#\space`, `#\newline`, etc.)
(partially) depends on the implementation. The reader can be configured to read
more than the names ISLisp allows for by default, but printing using those names
is implementation dependent.

`*most-positive-float*` and `*most-negative-float*` are the same
values as the implementation's `cl:most-positive-double-float` and `cl:most-negative-double-float`.

`eq` returns whatever truth value is returned by the implementation's
`cl:eq` for numbers. Similarly for `eql` with any cons cells.

`close` returns whatever value is returned by the implementation's `cl:close`.

`min` and `max` does whatever the implementation does for things like
`(cl:max 2 2.0)`.

`sin`, `cos`, etc. all return 0.0 for `(sin 0)`.

The character comparison functions (`char=`, etc.) all treat `#\A` and
`#\a` as distinct characters.

The default element initial value for `create-list` and
`create-vector` is `nil` and the default for `create-string` is
`#\Space` (which is what TISL, OpenLisp, and OKI all use).

The result of `string-append` does not share any structure with its arguments.

Expansion of the `~nT` and `~&` format directives rely on the
expansion of those same directives in the implementation's `cl:format`.

The correct parameters for `file-position` and `set-file-position`
depends on the implementation and operating system.

`islisp-compat` does not define a default error handler, relying instead on the
Common Lisp implementation's. Therefore, the output of a condition
is dependent on the implementation and its debugger.

Calling `continue-condition` on a non-continuable condition signals a
`<control-error>`.

Going off the end of a condition handler signals a `<control-error>`.

## Reader

There is a full reader provided for the syntax defined in the ISLisp specification. The syntax is mostly the same as Common Lisp except the following are not allowed:
* `#.`
* `#n#`
* `#n=...`
* `#*`
* `#:`
* `#<`
* `#c(r i)`
* `#p"..."`
* `#nR`
* `#s(...)`
* `#,.`

(The `#+` and `#-` syntax is allowed as a configurable extension.)

Additionally, all floating point literals must have a leading and trailing digit (".5 or "1." will trigger an error), and any symbol starting with a + or - is interpreted as a number.

The reader is very configurable. The main entry point to the reader is `islisp-sys:islisp-read`, which takes the same parameters as the `read` function in the standard. However, there is also a `islisp-sys:read-form` function that takes a stream and an `islisp-reader-options` object that holds the same behavior as the arguments to `read`, as well as a bunch of options to configure different interpretations or extensions of the standard ISLisp reader behavior.

There is a `islisp-sys:*default-reader-options*` global variable that
provides the default behavior of the reader. This can be overwriten by
the function call below, or modified with one of the
`reader-options-`* functions corresponding to any of the initargs below.
```
(islisp-sys:make-islisp-reader-options
  :eof-error-p  ; t or nil
  :eof-value  ; any value
  :neutral-alphabetic-case  ; one of: :upcase, :downcase
  :package  ; a package designator
  :feature-conditionals-p  ; t or nil
  :features  ; a list of keywords
  :quote-symbol  ; a symbol, should only be 'cl:quote or 'islisp:quote
  :keyword-behavior  ; one of: :cl, :symbol
  :symbol-colon-behavior  ; one of: :cl, :symbol, :error
  :allowed-character-names  ; a list of strings
  )
```

The `eos-error-p` and `eos-value` slots are the same as the parameters to `read`: `eos-error-p` specifies whether you want the reader to error on end-of-stream, `eos-value` is the value returned if `eos-error-p` is nil. You don't need to set these direcly if you are calling `islisp-read`, only if calling `read-form`.

`neutral-alphabetic-case` must be one of `:upcase` or `:downcase`. By default it is `:upcase` to match Common Lisp's default behavior.

The `package` is the package the reader will intern all the symbols into. This defaults to the `islisp-user` package.

If `feature-conditionals-p` is true, then the `#+` and `#-` syntax forms are allowed. The conditional expressions that can be tested are the same as in Common Lisp: any mix of symbols and lists beginning with `and`/`or`/`not`. The features tested are the keyword symbols in the `features` slot, not the ones in `cl:*features*`. By default, `features` is `(:islisp :islisp-compat)`, plus the standard symbol for the host Common Lisp implementation, if we know it (i.e. `:sbcl`, `:lispworks`, etc.).

`quote-symbol` is the symbol to use for the `'` reader. This defaults to `cl:quote` and really shouldn't be changed ever. (The `quote` symbol in the `islisp` package is now one of the few imported from the `common-lisp` package because dealing with `cl:quote`/`islisp:quote` mismatches was a constant source of headaches.)

`allowed-character-names` is an alist of `("name" . #\character)` pairs that is searched when reading in a character. The string is the character name, and must be lowercase, and the character is the character that is to be "read in" for that name. The defaults are just the two characters allowed by the ISLisp standard (`#\Space` and `#\Newline`), but the Common Lisp defaults are available in the `islisp-sys:+default-common-lisp-character-names+` constant.

`keyword-behavior` is a keyword specifying the behavior to run when a keyword is read in. The available values are `:cl` and `:symbol`.
* `:cl` specifies that keywords are to be read in as a Common Lisp keyword and evaluate to themselves. This is the default.
* `:symbol` specifies that keywords are just another symbol and they are interned in the package specified by the `package` slot. This means they are no longer self-evaluating (they still can't be used as variable names, though, per the spec).

The `symbol-colon-behavior` option specifies what the reader should do upon encountering a ':' in a symbol name. The available options are `:cl`, `:symbol`, and `:error`.
* `:cl` specifies that the symbol will be in the forms `package-name:symbol-name` and the reader should do a lookup of the symbol name in the package. This is the default.
* `:symbol` tells the reader to treat a colon like any other character. Thus, `cl:list` does not denote the `list` symbol in the `CL` package, but the symbol `cl\:list`.
* `:error` disables colon support entirely and reading in any symbol with a colon will signal an error.

## Extensions from the ISLisp Specification

There is a `<violation>` error class that handles the majority of the
syntax errors in special forms. You should only really see this error at
compile time. This is allowed by the spec. and matches the behavior of TISL.

There is a `<sequence>` class that matches the Common Lisp `cl:sequence` which
`<list>` and `<basic-vector` are both considered to inherit from.
This was primarily added so the `<domain-error>`s signalled from the
sequence functions have a useful class to report.

There is a `<class>` class that matches the Common Lisp `cl:class` which
`built-in-class` and `standard-class` are both considered to inherit from.
This was primarily added so the `<domain-error>`s signalled from the
type checking functions have a useful class to report.

There is an `exit` function in the `islisp-sys` package that will quit
the most common Common Lisp implementations.

The following options to defining forms don't check their parameters to allow some access to the underlying implementation: `:generic-function-class`, `:method-combination`, `:metaclass`

## Omissions in the ISLisp Standard

The standard doesn't say what to do if the number of arguments to
`format` don't match the number of format directives that take an
object. `islisp-compat` does what Common Lisp does and ignores
additional arguments and signals a `<program-error>` if there are too
few.

The `parse-number` function doesn't indicate that it ignores whitespace like the `parse-integer` function from Common Lisp does. We signal a `<parse-error>` on leading or trailing whitespace.

The standard doesn't say what, if any, error should be signalled if
the `read` function fails. (Right now we signal `<simple-error>`s).

The standard doesn't saw what type of error should be signalled for an
improper call to `convert`. We signal a `<domain-error>` like TISL and OpenLisp.

For `open-input-file`, etc. despite the element class being able to be
either `<character>` or an integer, the `<domain-error>` it signals
has `<integer>` as its expected class (this matched TISL).

For `open-output-file`, `with-output-file`, etc. the specification
doesn't say what to do if the file exists. We pass `:if-exists
:supersede` to the implementation's `cl:open`. If you need some other
behavior, you will need to test its existence with `probe-file` yourself.

For the `format-`* functions and `write-byte`, if a `<domain-error>` is
signalled with the expected class set to `<stream>` but you passed a stream to it,
the issue is that the stream is not open for output.

For the `read-`* functions, if a `<domain-error>` is signalled with
the expected class set to `<stream>` but you passed a stream to it,
the issue is that the stream is not open for input.

The spec. doesn't say what to do if a handler function returns
normally. We signal a `<control-error>`. This is what OpenLisp does, as well.

## Version History

* 0.0.0: First bootstrap attempt May, 2011
* 0.0.1: `islisp-compat.lisp` created Aug. 2, 2011
* 0.1.0: Split into multiple files Feb. 27, 2024
* 0.2.0: ASDF version Jul. 3, 2024
* 1.0.0: "Complete", bug-fixed release Nov. 20, 2024

## License

`islisp-compat` is released under the terms of the Common Development and
Distribution License 1.1 (SPDX-License-Identifier: CDDL-1.1). Except
for the file bq.lisp, which is in the public domain.

In short, CDDL is a very weak copyleft license: you can include these files
into any project with a compatible license; the only requirement is you must release any
changes you make to these original files. Any files not part of this
project retain their original license(s) (if compatible).

