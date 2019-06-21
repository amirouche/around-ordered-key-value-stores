# SRFI 167 and 168 tutorial

## Setup

With good connection, clone this repository with the following command:

```
$ git clone --recursive --depth=1 https://github.com/scheme-live/srfi-167-and-168-tutorial
```

Then run `make init`:

```
$ make init
```

Run `make check` to make sure everything is alright:

```
$ make check
```

Install `rlwrap`, for instance with the following command:

```
$ sudo apt install rlwrap
```

Then run `make repl` to start REPL:

```
$ make repl
```

This will also remove any previous database you created in `/tmp/wt`.

## Getting started with SRFI 167

[SRFI-167](https://srfi.schemers.org/srfi-167/) is an ordered
key-value store (OKVS). That is hashmap that is sorted using
lexicographic (aka. dictionary ordering) of bytes. One rarely interact
with the database using raw bytes instead they are packing procedure
define in `(arew data pack)` that allow to interact with the OKVS as
if it was storing scheme objects.

```scheme
> (import (cffi wiredtiger okvs))
> (import (arew data pack))
```

Open the database with:

```scheme
> (define db (okvs '((home . "/tmp/wt") (create? . #t))))
```

All database related procedure must be wrapped using `(okvs-in-transaction db proc)`.

Given a key, to fetch a value you can do:

```scheme
> (okvs-in-transaction db (lambda (txn) (okvs-ref txn (pack "hello"))))
```

It will return `#f`.

Let's set the key for `"hello"` to `"world"`:

```scheme
> (okvs-in-transaction db (lambda (txn) (okvs-set! txn (pack "hello") (pack "world"))))
```

And retry to fetch the key `"hello"`:

```scheme
> (okvs-in-transaction db (lambda (txn) (okvs-ref txn (pack "hello"))))
```

Don't forget to `unpack`:

```scheme
> (okvs-in-transaction db (lambda (txn) (unpack (okvs-ref txn (pack "hello")))))
```

Let's delete the key:

```scheme
> (okvs-in-transaction db (lambda (txn) (okvs-delete! txn (pack "hello"))))
```

## Getting started with SRFI 168

[SRFI 168](https://srfi.schemers.org/srfi-167/) stores a set of tuples
that have n items. Every tuple is unique. Using that interface you do
not need to manually pack / unpack scheme objects it is done for
you. You need to pass the tuple store specification to every nstore
procedure.

Let's create a triple store:

```scheme
> (import (arew data base nstore))
> (define engine (nstore-engine okvs-ref okvs-set! okvs-delete! okvs-prefix))
> (define triple-store (nstore engine (list 42 1337) '(uid key value)))
```

**Warning:** The argument `(list 42)` is called the subspace
prefix. It allows to hook multiple abstraction in the same
OKVS. Otherwise said, it allows to work as if they were different
database or tables but relying on the same transaction. Mind the fact
that if you hook an abstraction at `(list 42)` and another at `(list
42 1337)` it will not crash but it will not work as expected. Like its
name imply, subspace **prefix** works by prefixing every key of that
space with that scheme object.

Simply, said if several nstore share the same prefix it will not work.

Let's add a two tuples:

```scheme
> (okvs-in-transaction db (lambda (txn) (nstore-add! txn triple-store '(foo bar baz))))
> (okvs-in-transaction db (lambda (txn) (nstore-add! txn triple-store '(foo bar qux))))
```

Querying rely on generator, so you need R7RS `(scheme generator)`:

```scheme
> (import (scheme generator))
```

And bindings are `(scheme mapping hash)`:

```scheme
> (import (scheme mapping hash))
```

Now, let's do a simple query that match the tuples that have `'foo` as `uid`
and `'bar` as `key`:

```scheme
> (okvs-in-transaction db (lambda (txn) (generator-map->list hashmap->alist (nstore-from txn triple-store (list 'foo 'bar (nstore-var 'value))))))
```

That is (almost) all!

## Beyond

Read the following files:

- https://git.sr.ht/~amz3/chez-scheme-arew/tree/master/src/cffi/wiredtiger/okvs-test.scm
- https://git.sr.ht/~amz3/chez-scheme-arew/tree/master/src/arew/data/base/nstore-test.scm
- https://git.sr.ht/~amz3/chez-scheme-arew/tree/master/src/arew/data/base/fstore-test.scm
