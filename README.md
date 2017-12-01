# coco
A simple Scheme interpreter, created as a Haskell exercise project. It only implements a subset of [R5RS](http://schemers.org/Documents/Standards/R5RS/HTML/) for now. Needs proper testing still as well.

[![asciicast](https://asciinema.org/a/8Lu8xWXFBUOKVR2NszrlLnWhC.png)](https://asciinema.org/a/8Lu8xWXFBUOKVR2NszrlLnWhC)

## Install
1. Prerequisites: [Stack](https://docs.haskellstack.org/en/stable/README/).
2. Build it:
```bash
$ stack build
```
3. Run it:
    - As an interactive interpreter:
    ```bash
    $ make run
    ```
    - Use it to run a Scheme (.scm) file:
    ```bash
    $ stack exec coco-exe -- FILENAME ARG1 ARG2 ARG3
    ```
    Args are passed into the program in the file as a list bound to a variable called `args`.

## What can it do?
### Types
- Lists (:information_desk_person:)
- Dotted Lists
- Numbers (Integers, signed)
- Floats (signed) (Note that type isn't strictly enforced in some operations, like addition, etc)
- Strings
- Bools
- Characters
- Functions

All supported types (and errors) are in [the ValueLib module](src/ValueLib.hs#L10).
Numbers can be written as decimals, hexadecimals, octals, or binary, given the proper Scheme notation.

### Patterns
- Primitive patterns (patterns that are the types themselves).
- Quoted types (e.g. `'(1 2 3)`)
- If conditions.
- Set and define variables (e.g. `(set! x 3)` or `(define x 3)`)
- Define functions.
- Lambda functions.
- Load file (e.g. `(load "test.scm")`).
- Evaluate function (e.g. `(func 1 2)`).

### Built-in Functions
- `+`
- `-`
- `*`
- `/`
- `mod`
- `quotient`
- `remainder`
- `number?`
- `string?`
- `boolean?`
- `list?`
- `zero?`
- `char?`
- `symbol?`
- `not`
- `length`
- `symbol->string`
- `string->symbol`
- `list->string`
- `string->list`
- `=`
- `/=`
- `<`
- `>`
- `<=`
- `>=`
- `&&`
- `||`
- `string=?`
- `string>?`
- `string<?`
- `string<=?`
- `string>=?`
- `char=?`
- `char>?`
- `char<?`
- `char>=?`
- `char<=?`
- `cons`
- `car`
- `cdr`
- `eq?`
- `eqv?`
- `make-string`
- `string`
- `string-length`
- `string-ref`
- `substring`
- `string-append`
- `string-copy`
- `apply`
- `open-input-file`
- `open-output-file`
- `close-input-port`
- `close-output-port`
- `read`
- `write`
- `read-contents`
- `read-all`

The complete list of built-in functions can be found in `primitives` and `ioPrimitives` in [the Eval module](src/Eval.hs).

## References
- [R5RS](http://schemers.org/Documents/Standards/R5RS/HTML/) (Scheme standard doc)
- [Write Yourself a Scheme In 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) (Book)
- [Haskell Wiki](https://wiki.haskell.org/) and official docs in general, to adapt depracated methods/patterns and as a general reference.
