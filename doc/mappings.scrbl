#lang scribble/manual

@title[#:tag "mappings"]{Mappings}

Package @code{col.mapping}

@section[#:tag "map-interface"]{Common mapping interface}

To implement the common mapping interface, three generic functions must be
implemented for a type: @code{mget}, @code{mset}, and @code{keys}.  Optionally,
if the mapping type is mutable, implement @code{mset!}.

@defproc[(mget [mapping mapping/c] [key any/c]) any/c]{
Get the value associated with key in the provided mapping.  (Generic function.)

If the value is not present in the mapping, return @code{nil}.

Implementations are provide for association lists, preference lists,
hash-tables, and FSet maps.  For association lists and preference lists, keys
are compared using @code{equal}.}


@defproc[(mget* [mapping mapping/c] [key any/c] ...) any/c]{
Get the value associated with the supplied keys in a nested mapping.  Keys
should be supplied in outermost to innermost order.  If any key is absent,
return nil.  Implemented in terms of @code{mget}.

Example:

@codeblock[#:keep-lang-line? #f]{
#lang racket
(mget* my-map 'a 'b)
}

is equivalent to:
@codeblock[#:keep-lang-line? #f]{#lang racket
(mget (mget my-map 'a) 'b)
}}


@defproc[(mset [mapping mapping/c] [key any/c] [value any/c]) mapping/c]{
Set the value associated with the supplied key in a mapping.  Return a new
mapping object.  The original is unchanged.

Note that for alists and plists, this will just cons onto the front of the
list, even if other instances of the key already exist in the map.}


@defproc[(mset* [mapping mapping/c] [keylist (listof any/c)] [value any/c]) mapping/c]{
Set the value associated with the supplied keys in a nested mapping.  Return a new
mapping object.  The original is unchanged at any depth.

Example:
@codeblock[#:keep-lang-line? #f]{
#lang racket
(mset* my-map '(a b) 1)
}

is equivalent to:
@codeblock[#:keep-lang-line? #f]{
#lang racket
(mset my-map 'a (mset (mget my-map 'a) 'b 1))}}

@defproc[(mset& [mapping mapping/c] [key any/c] [value any/c]) mapping/c]{
@code{mset} and rebind.  Set the value associated with key in the mapping
without modification.  @code{mapping} should be a @code{setf}able place.
@code{setf} the place to the new mapping and return that new mapping.

Example:
@codeblock[#:keep-lang-line? #f]{
#lang scribble/text
(defvar x #M{:a 1 :b 2})
(defvar y x)

(mset& x :a 5)
}
After running this, @code{x} is now @code{{:a 5 :b 2}}, but @code{y} is still
the original @code{{:a 1 :b 2}}}

@defproc[(mset! [mapping mapping/c] [key any/c] [value any/c]) any/c]{
Set the value associated with the supplied key in a mutable mapping.  Return
the value.  The mapping is modified in place.

This will cause an error for FSet maps and plists.}

@defproc[(keys [mapping mapping/c]) list/c]{
Get all the keys in the provided mapping as a list.}

@section{Utility}

@defproc[(alist-p [mapping mapping/c]) bool/c]{
Is the provided mapping an association list?

Assumes that if the argument is a list, it's either an alist or a plist.}

@defproc[(plist-p [mapping mapping/c]) bool/c]{
Is the provided mapping a preference list?

Assumes that if the argument is a list, it's either an alist or a plist.}

@defproc[(remove-from-plist [plist list/c] [k any/c]) list/c]{
Remove the provided key and its associated value from the plist.

Don't modify the original.  Return a new plist with all copies of the key and
their values removed.}

@defproc[(hset [hash hash-table/c] [key any/c] [value any/c]) any/c]{
An argument-order-consistent shorthand for setting a value in a hash-table.

Return the value being set.}

@defproc[(htcopy [hash hash-table/c]) hash-table/c]{
Shallow copy a hash table.  The parameters used to construct the original table
are the same for the new table.}

@defproc[(plist->alist [plist list/c]) list/c]{
Convert a plist to an alist.}

@defproc[(alist->ht [alist list/c] [hash-table-initargs any/c] ...) hash-table/c]{
Convert an alist to a hash table.}
