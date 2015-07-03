#lang scribble/manual

@title[#:tag "strings"]{Strings}

Package @code{col.strings}

@defproc[(<> [strings string] ...) string]{
Shorthand for concatenating strings.
}

Same as:
@codeblock{
(concatenate 'string strings ...)
}


@defproc[#:kind "generic" (->string [obj t]) (or (eql obj) string)]{
Generic for converting an object to string.

The default implementation is a no-op, returning obj.  This is used when
calling @code{println} from the @code{col.io} package and can be used to make a
custom printed representation.
}
