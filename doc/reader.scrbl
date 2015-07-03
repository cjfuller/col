#lang scribble/manual

@title[#:tag "reader"]{Reader macros}

Package @code{col.reader}

@section[#:tag "map literal reader"]{Map literal reader}

The map literal reader adds a syntax for literals of an arbitrary mapping type:

@codeblock[#:keep-lang-line? #f]{#lang scribble/text
#M(opts-plist){key value key value ...}}

@code{opts-plist} is a completely (even parentheses) optional preference list
of options that specify the result type of the mapping.

@racketgrammar[#:literals (opts-plist option) (opts-plist) (option ...)]
@racketgrammar[option (code:line :type (member :alist :plist :hash-table :fset-map))
                      (code:line :test test)]

If no option is specified, the default is @code{*default-map-literal-type*}.

@code{:test} is only used when the output type is @code{:hash-table}

@defthing[#:kind "dynamic var" *default-map-literal-type* keyword #:value :alist]{
Default map literal type to use when one is not supplied.
}

@section[#:tag "shell command literals"]{Shell command literals}

The shell command literal reader adds a syntax for running a shell
command (using @link["http://quickdocs.org/inferior-shell"]{inferior-shell})
and returning standard output as a string.

Example:

@codeblock[#:keep-lang-line? #f]{#lang scribble/text
#!"ls -al /"}

Within the string, interpolation using
@link["http://weitz.de/cl-interpol/"]{cl-interpol} syntax is available.

@section[#:tag "enabling reader syntax"]{Enabling reader syntax}

@defproc[#:kind "macro" (enable-reader-exts [#:do-push do-push (member t nil) t])
         (void)]{
Enable the map literal and shell command literal readers.  Also enables
cl-interpol @code[#:lang "scribble/text"]{#?""} syntax.}

If @code{do-push} is nil, modify the readtable in place, rather than pushing on
a copy that can be popped using @code{pop-reader-exts}.

@defproc[#:kind "macro" (enable-map-literals [#:do-push do-push (member t nil) t])
         (void)]{
Enable only the map literal syntax.}

@defproc[#:kind "macro" (enable-shell-command-literals [#:do-push do-push (member t nil) t])
         (void)]{
Enable only the shell command literal syntax.}

@defproc[#:kind "macro" (pop-reader-exts) (void)]{
Disable the most recently added reader syntax from this package.  (Calls to
this macro should be matched one-to-one with calls to any of the
@code{enable-} macros.)}
