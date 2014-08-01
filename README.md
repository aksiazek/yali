yali
====

Yet Another Lisp Interpreter - In Haskell!

YALI is an basically an attempt to evolve the bare-bones interpreter taken from 2006 available at https://github.com/haskell-lisp/blaise and add all features that make lisp the fun language it is.

The first milestone, to to be able to write an eval function in itself is complete.
Read the details in Paul Graham's Roots of Lisp, avaiable currently at http://languagelog.ldc.upenn.edu/myl/ldc/llog/jmc.pdf

==
#### Dependencies

You will need Haskell installed on your system. Download:

    http://www.haskell.org/platform/

or use your package manager like yum, apt or http://chocolatey.org/

You will also benefit a lot from the rlwrap package, a GNU readline command line wrapper, which offers command line history with the familiar up-arrow.

==
#### Running

 ```
 ghc Main.hs
 rlwrap ./Main
 rlwrap ./Main < test.lisp > test.out
 ```

==
#### Planned improvments and enchancements:

- throwing errors relating to bad &rest param while defining rather then evaluation
- necessary operators for integers, which were basically tagalongs for testing so far
- ' quotation instead of tedious (quote sth)
- let & lexical closures
- defmacro, backtick and comma
- multiline code and comments, because writing everything in one line gets boring


#### Comments about code quality welcome!
