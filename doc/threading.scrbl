#lang scribble/manual

@title[#:tag "threading"]{Threading macro}

Package @code{col.threading}

@defproc[#:kind "macro" (-> [first t] [body t] ...) t]{
Clojure-style threading macro.

Insert the first form into the first argument position of the second form.
Evaluate and insert the result into the first argument position of the next
form.  Repeat until no more forms remain.}

Example:
@codeblock{
(-> '(1 2 3)
    (rmapcar #'1+)
    (car))
}
would return 2.

If you want to call a function with the inserted argument as the only argument,
you can just use the bare function name (not sharp-quoted).

Example:
@codeblock{
(-> '(1 2 3)
    (rmapcar #'1+)
    car)
}


@defproc[#:kind "macro" (rmapcar [lst list] [fn (-> t t)]) list]{
@code{mapcar}, but with the argument order reversed.
This allows passing the list argument using the threading macro.}

@defproc[#:kind "macro" (tee [first t] [body t] ...) list]{
Tee-like macro for use with the threading macro.

Insert first into the first argument position in each of the body forms using
the threading macro.  Collect the results of the body forms into a list.}

@defproc[#:kind "macro" (tap [first t] [form t]) t]{
Tap into the threading macro, evaluating a form for side-effects.

@code{first} is inserted into the first argument position of @code{form} using
the threading macro, the form is evaluated (discarding the result), and
@code{first} is returned.}
