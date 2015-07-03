#lang scribble/manual

@title[#:tag "misc"]{Miscellaneous}

Package @code{col.misc}

@defproc[#:kind "macro" (nthval [values-returning t] [n integer]) t]{
Get the nth value (0-indexed) of a form returning multiple values.

Note that the arguments are in reverse order from @code{nth} for lists, so that
this can be used in the threading macro.
}

@defproc[#:kind "macro" (nthvals [values-returning t] [nlist (listof integer)]) list]{
Get the values at the supplied positions of a form returning multiple values.

Note that the arguments are in reverse order from @code{nth} for lists, so that
this can be used in the threading macro.
}


@defproc[#:kind "macro" (:= [var symbol] [val t] [body t] ...) t]{
Extremely gratuitous shorthand for let with a single binding:
@codeblock{
(let ((var val))
  body ...)
}

This occasionally improves readability.  Will be deprecated or renamed in a
future release to avoid confusion because of the colon.
}
