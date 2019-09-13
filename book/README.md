# Around Ordered-Key Value Stores

> "Plans are only good intentions unless they immediately denerate into hard work"
>
> Peter Drucker

## Introduction

While there is no shortage of database systems, there is none that
meets all my requirements:

#. free software, open source or source available
#. can represent and query relational data
#. can represent and query recursive data
#. can represent and query geospatial data
#. can represent and query time data
#. can represent and query text data
#. support efficient pagination
#. support efficient versioning
#. support horizontal scaling
#. support ACID transactions across “objects”
#. embeddable

We could argue indefinitly that one or more of those requirements are
unnecessary, overkill and YAGNI. We could argue that by relaxing a few
of the requirements, a particular software or set of softwares can
come close. We could argue endlessly that building
yet-another-database is NIH, wheel re-invention that curse the
software industry with fragmentation and fatigue. We could invoke UNIX
philosophy, entreprise software architectures, experiences, know-how,
failed patterns, decades of good services and big communities.

We could agree that a lone inexperienced idealist web developer can
not disrupt software industry with an unwiedling project, click-bait
titles and wanna-be Sillicon-Valley friendly landing pages even if
they have the best intentions in the world. We could and hopefully we
will.

Standing on the shoulders of giants, this document is merely trying to
bring the attention of the community to a growing trend in the
industry by demonstrating what one can achieve with an ordered
key-value store. Most of what is described in this document already
exists in the wild hidden in closed-source softwares but also hidden
in freely available projects written in C++, Java, C#, Go, Rust,
Python, Clojure and Scheme.

Anyway, the ideas and algorithms described in this document should
useful. It can help on the path to build your own database
abstractions on top ordered key-value stores and get the most of what
modern data storage systems have to offer.

It seems to me that ordered key-value stores are useful, useable and used.

A question remains: Will it remain a tool for the experts? Maybe it
will become more widely accepted in the toolbelt of backend software
developers? One way to find out the response to that question is to
answer another question: Are most of any domain needs covered by the
abstractions provided in this document or there is still some specific
needs that arise in some domain that require chirurgical
intervention. In the latter case, ordered key-value stores are what
will be the more useful to know.

## Ordered Key-Value Store

> An ordered key-value store is to storage systems what Scheme is to
> programming languages: a programmable programming system.

In this section, we will see the main principles that allow to build
upon higher level abstraction and answer the following questions:

- What is an ordered key-value store?
- How to store data?
- How to query data?
- How to create a schema?

### Getting started

An ordered key-value store is an abstraction used in various storage
systems that allow on-disk persistence. There is many implementations
with various features and limitations. Some of them support some kind
transactional guarantees. Some of them are distributed
ie. horizontally scalable. We will focus on what is common and what is
applicable to most ordered key-value stores.

An ordered key-value store is a mapping of bytes to bytes where
key-value pairs are ordered lexicographically according to the key.

Lexicographical order is natural language dictionary order. That is
the ordering where “care” comes before “careful” and “care” comes
after “car”.

There is an algorithm that allows to represent objects and composition
of objects of the following types as bytes that preserve their
respective natural ordering using a fixed unspecified ordering between
objects that are from different types:

- boolean
- big integer
- float
- double
- symbol
- string
- list
- vector
- bytevector

The consequence is that an ordered key-value store can be seen as a
mapping of key-value pairs ordered by key where keys and values are
objects and composition of objects of the following types ordered
using their natural order and a unspecified fixed order between
objects of different types:

- boolean
- big integer
- float
- double
- symbol
- string
- list
- vector
- bytevector

The ordered key-value store can be seen as a 2×n table where n is the
total number of key-value pairs in the database, such as:

TODO: add table

In real world scenario, most of the time the keys are always
tuples. And as far as the key is concerned, tuples and non-tuples are
not mixed.

The actual okvs manipulates bytes and keeps the keys in dictionary
order. It is possible to represent various base types of the host
language as bytes that preserve their natural ordering.

We can (almost) freely compose (almost) any base type to possibly
achieve a fractal design.

### Forward: Programming Interface

#### Mapping primitives

The okvs, as a mapping, has primitives to get and set a particular
key-value pair. The following will retrieve the value associated with
a key relying on the procedure of library called `(pack)`:

```scheme
(import (cffi wiredtiger okvs))
(import (pack))


(define database (okvs "/tmp/wt" '((create? . #t))))

(okvs-in-transaction database
                     (lambda (txn)
                       (okvs-ref txn (pack 42))))
;; => #f

(okvs-in-transaction database
                     (lambda (txn)
                       (okvs-set! txn (pack 42) (pack "scheme"))))

(okvs-in-transaction database
                     (lambda (txn)
                       (unpack (okvs-ref txn (pack 42)))))
;; => '("scheme")
```

#### Collocation of keys

The particularity of an ordered key-value store is its primitive
procedure that allows to efficently get all key-value pairs that are
inside a given range (or slice) of keys, namely okvs-range:

```scheme
(import (scheme generator))
(import (cffi wiredtiger okvs))
(import (pack))


(define database (okvs "/tmp/wt" '((create . #t))))

(okvs-in-transaction database
                     (lambda (txn)
                       (okvs-set! txn
                                  (pack 1 'title)
                                  "DIY a multi-model database")
                       (okvs-set! txn
                                  (pack 1 'body)
                                  "That is getting started!")))

(define out
  (okvs-in-transaction database
                       (lambda (txn)
                         (generator-map->list
                          (lambda (key value) (cons (unpack key)
                                                    (unpack value)))
                          (okvs-range txn (pack 1) #f (pack 2) #f))))

(equal? out '(((1 body) . "That is getting started!")
              ((1 title) . "DIY a multi-model database")))
;; => #t
```

In the above example, `'(1 body)` comes before `'(1 title)` because
`'body` comes before `'title` because the character `b` appears before
the character `t` in a dictionary.

Another way to look at it is using the table reprensetation:

TODO: add table

An astute reader will recognize that in that particular case using a
prefix-range allows to retrieve the same set of key-value pairs. We
will see later how it can be done.

This works because keys are always in increasing order. A more obvious
use of the range procedure is to retrieve everything between two
points in time. Given the following database:

TODO: add table

We can define the experienced persons as everything that is between
“adult” and “dead” with “dead” persons excluded:

```scheme
(define out
  (okvs-in-transaction database
                       (lambda (txn)
                         (generator-map->list
                          (lambda (key value) (cons (unpack key)
                                                    (unpack value)))
                          (okvs-range txn #vu8(5) #t #vu8(9) #f))))
```

Mind the fact that the procedure `pack` is not used because keys are
already bytevectors.

Let's do another example, let's store some news items by date and try
to query everything from 2011/11 (included) to 2012/02
(included). News items are represented as UUID bytevectors and dates
are lists that follow the following format '(year month):

TODO: add table

#### Keys sharing a common prefix

We previsouly mentioned that prefix range queries “naturally” appear
in the context of ordered key-value stores databases. Consider the
following table:

TODO: add table

The corresponding code will look like the following:

```scheme
(define out
  (okvs-in-transaction database
                       (lambda (txn)
                         (generator-map->list
                          (lambda (key value) (cons (unpack key)
                                                    (unpack value)))
                          (okvs-prefix-range txn (pack 2011))))))
```

It rely on the procedure `(okvs-prefix-range transaction prefix)` to
retrieve all key-value pairs that have a given `PREFIX`.

The procedure okvs-prefix-range is tiny wrapper around okvs-range.

### Beyond: Key Composition

Key composition is the pratice of storing list of objects as key that
take advantage of the ordered nature of the key space with the
procedure okvs-range. It allows to ground schemas and build higher
level abstractions.

#### Unique Keys

The unique keys pattern rely on the fact that what is a “value” from
the application perspective is stored in the key and to possibly leave
the value column empty and make the key unique.

Let's re-imagine the previous news items example and make it more
realistic. In a real world scenario, multiple news can be published
the same year. To be able to store and query by date multiple news
items from the same year, one must make the key unique.

Here is an application view of the news items:

TODO: add table

If we want to query the news items by year, the year must be part the
key since ordered key-value store can only query using the key. Since
the database is a mapping they can not be multiple values associated
with the same key except if you use a list to group every identifiers
in the same key-value pair.

There is occasions, where grouping multiple values for a given key is
a good thing, for example in the case of categorical data and more
generaly in situations where the list will not grow too much. Another
inconvenient of grouping values, is that you can not easily do
pagination. We will explain how pagination happens using
key-composition, it will be obvious that grouping values is not
helpful in that case.

The recommended way to represent the news items in a way that makes it
easy to query and paginate the identifiers is to append the identifier
to the key and leave the value empty:

TODO: add table

That way every key is unique, and one can use okvs-range to query by
date. To do pagination, one must keep around the last seen key, and do
another okvs-range starting from that last seen key.

#### Subspace

Subspaces are another key-composition pattern that allows to split the
key space into multiple subspaces, that when used with care, allow to
reproduce a similar concept to collections and tables.

Again, let's consider news items and more generally somekind of news
application where news items are associated with authors. There is at
least two entities in that application. In other databases, it would
be translated into two collections or tables.

In an ordered key-value store, we can use subspaces. A subspace is a
collocated set of keys that share a common prefix.

The following table demonstrate the use of prefixes to separate
different entities:

TODO: add table

### Final Frontier: Multiple Representations

somekind of denormalization...

### Summary

#. The ordered key-value store is a mapping of bytes-to-bytes where key-value pairs are ordered lexicographically by the key.

#. The pack procedure allows to represent most base data types as bytes while preserving their natural order.

#. Therefore in most cases, we can consider that the okvs stores composition of base data types.

#. The okvs support classic mapping procedures `okvs-set!`, `okvs-ref` and `okvs-delete!`.

#. The most important primitive is `okvs-range` which behavior stems from the property that keys are stored in increasing order.

#. Key composition allows to build schemas.

#. Multiplying representation will slow down writes, but speed up queries

### Exercice

Consider the following database:

TODO: add table

It is not possible to query by tag without doing a full scan of the
news subspace. Rework the database schema to support querying news by
tag.

## Row Store

The row store is inspired from Relational DataBase Management Systems
(RDBMS) in sense that it rely on fixed-length vectors to represent
rows in a primary source of truth subspace. So called indices, a
secondary source of truth, are updated in the same transaction as the
row and allow to efficently query and do pagination. More advanced
indexing scheme are considered in other sections of this document.

In this, section we will:

- Represent tabular data
- Introduce the idea of “introspection” subspace
- Explain how to create secondary representation and how they are
  useful.

### What is the data?

Continuing with the idea of a news application we will represent news
items, authors and tags. Here is the high-level RDBMS representation
of the data we are interested in:

TODO: add tables

### How we want to query the data?

- We want to be able to retrieve news items with its author
  information and join the related tags
- We want to be able to retrieve news items by year
- We want to be able to retrieve articles related to a given tag

### Implementation

#### Introspection

First thing we can do is describe the content of the database in
introspection subspace:

TODO: add table

This representation allows to query the whole schema in a single
`okvs-prefix-range`. Another approach exists where introspection comes
just after the name of the entity, for example instead of
`(introspection author columns)` we would have `(author introspection
columns)`.

#### Primary Representation

TODO: add table

Every entities is in a subspace that takes the name of the entity table.

Given the current schema, we can query rows by the attribute dubbed
primary key (pk) that is present in the key and do pagination but only
in the primary key order. It is not possible given an news item
primary key to join its related tags. It is possible to join the
author using its primary key.

#### Secondary Representation

Like explained previously the primary representation allows to
retrieve rows using their primary keys. To be able to do queries, we
need to copy the original data in some other subspace and take
advantage of the key-composition technics described in the first
chapter.

##### How to join tags with news items?

The news-tag subspace associate a primary key with a tuple of two
integers that are respectivly the article's primary key and the tag
primary key.

To allow to do “news item to tag joins” we must create a subspace
where the news item primary key is part of the key, while still having
every key-value pair unique. We can use the following schema:

TODO: add table

The key has the following format:

```
(news-tag,  index, news-to-tag, tag-pk, news-item-pk) → news-tag-pk
```

##### How to join news items with tags?

Again, thew `news-tag` subspace doesn't allow to retrieve articles
given a tag's primary key. To make it possible to query for articles
that have a particular tag one must create an index in another
subspace:

TODO: add table

The suspace can be described as follow:

```
(news-tag,  index, news-to-tag, news-item-pk-pk, tag-pk) → news-tag-pk
```

##### How to paginate news item by year?

Here we need to make the year attribute part of the key and again make
sure the key is unique:

TODO: add table

### Optimisations

- To save space, one can replace entity name with an integer
- To speed up article by year queries, one can copy the rest of the
  row in the value

### Summary

In this chapter we have seen:

- How to represent tabular data
- How to use subspaces to keep together similar and related data
- How the use of secondary indices can speed-up queries

The row store is good for dense tabular data. If you draw the whole
key space you will recognize that there is lot of duplication. This is
expected, the more general a solution is the more space it will
use. Some optimisation allow to save space and others allow to
speed-up queries. There is always some kind of duality between space
and time.

### Exercices

#. It is not possible to query a tag row by tag name without a full subspace scan, provide schema that allows to retrieve tags informations using its name.

#. Represent the whole database with all optimisations applied.

### Document Store

### Text search

### Space and Time

### Tuple Stores

#### Triple Store

#### Generic Tuple Store

#### Versioned Generic Tuple Store

## Sandbox

<table> <thead> <tr> <th colspan="2">The table header</th> </tr>
    </thead> <tbody> <tr> <td>The table body</td> <td>with two
    columns</td> </tr> </tbody> </table>

```scheme
(define some "thing")
```
