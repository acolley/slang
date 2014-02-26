slang
=====

Simple s-expressions language implemented in Haskell

Contains a couple of command-line programs to run a slang-based program:

* **slerp** - Pass it the name of a file with slang code and it will run the program contained and print the result to standard out.
* **sli** - The slang REPL.

Compile these programs using ghc --make and passing in either 'slerp.hs' or 'sli.hs' depending on which program you wish to compile.

Current Features
----------------

* Interpreted
* Function closures
* Dynamic typing
* REPL

Syntax
------

Uses a standard lisp syntax with all the fun of parentheses.

**Adding two numbers**

```
>> (+ 1 2)
3
```

**Defining a function**

```
>> (defn add (x y) (+ x y))
```
