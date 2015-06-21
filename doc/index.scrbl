#lang scribble/manual

@title{Col: a modular collection of utilities for common lisp}

@link["//github.com/cjfuller/col"]{Col} is a collection of common lisp
utilities that I've found useful in my own projects.  I've provided them in a
series of mostly independent packages in hopes that they can be used together
or in a take-only-what-you-like manner.

It currently consists of four major things:

@itemlist[@item{A @seclink["mappings"]{mapping interface} that can be used with all the major mapping
                  types (preference lists, association lists, hash tables,
                  @link["https://common-lisp.net/project/fset/Site/index.html"]{FSet maps})}
          @item{A @seclink["reader"]{reader extension for mapping literals}}
          @item{A @seclink["threading"]{clojure-style threading macro}, with associated utilities}
          @item{Miscellaneous convenience functions/macros/reader macros.}]

See the individual sections in the table of contents for detailed documentation
on each package.

There's also a meta-package (col) that re-exports the contents of all the other packages.

@include-section["mappings.scrbl"]
@include-section["threading.scrbl"]
@include-section["reader.scrbl"]
@include-section["strings.scrbl"]
@include-section["io.scrbl"]
@include-section["misc.scrbl"]
