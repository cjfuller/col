#lang scribble/manual

@title[#:tag "io"]{IO}

Package @code{col.io}

@defproc[(println [s t]) nil]{
Call @code{->string} (from @code{col.io}) on the input, then @code{princ} it,
followed by a newline.
}

@defproc[(slurp [fn (or string pathname)] [#:lines lines (member t nil) nil])
         (or string (listof string))]{
Slurp a file into a string, given a filename string or pathname.

If lines is non-nil, instead of a string, return a list with one string per
line (newlines stripped).
}
