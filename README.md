[![Circle CI](https://circleci.com/gh/loganbraga/hunch.svg?style=svg)](https://circleci.com/gh/loganbraga/hunch)
# Hunch

Hunch is a command-line, system-agnostic application that allows quick creation of file system entries with a custom DSL resembling CSS selectors.

Features:

+ CSS selectors to navigate the file system
+ Custom numbering of duplicate entries
+ External names placeholders to draw file names from external sources
+ Template utilities: create files from a base template instead of empty files.
+ Simulation mode: preview the file system tree before creating entries

## Usage

Basic usage, demonstrating automatic numbering of duplicate entries with the `$` character:


![basic usage](https://i.imgur.com/Y0TtKKH.png)


Drawing names from external sources:


![external sources](https://i.imgur.com/URN1Ymb.png)

Options:


![options](http://i.imgur.com/GpaSVsV.png)


## Syntax

Hunch syntax is a (really) small subset of CSS selectors and emmet-inspired operators:

+ `>` stands for "go down" or "child". It means that the directory on the left side is the parent of
the expression on the right side.
+ `+` stands for "stay on the same level" or "sibling". It means that the expression on the left side
is on the same file system location than the expression on the right side.
+ `*` stands for "repeat" or "times". It means that the expression at one side of the operator is
repeated as many times as the number on the other side.
+ `=` stands for "copy from" or "template". It means that the file on the left side should be copied from
the file on the right side, instead of being created empty.
+ `_?NAME` stands for "take from" or "external source". It means that the name of the entry is not currently determined,
but will be drawn from the external source "NAME" passed as argument.

Expressions can be grouped between parentheses. Logical constraints apply : `hunch '(foobar * 3) > file'` will raise an error, as
a file system entry can't have more than one parent.

### File/Directory disambiguation

Hunch treats every entry as a file by default. If it should be a directory:

+ It should have at least one child (`>` operator) or...
+ ...its name should end with a '/' character.

This allows you to write most expressions without consideration for the "file or directory?" question.
If an entry is a directory without any children, just add a '/' at the end of its name.

### Numbering syntax

Numbering of duplicate entries uses a printf-like syntax. A given token (default `$`) will be replaced in group in entry names.

The number of consecutive token characters indicates optional padding.
If the length of the consecutive token characters is smaller than the length of the current number, the whole number will be displayed anyway.

For example, `file-$$$ * 150` will be replaced by "file-000", "file-001" to "file-150", while
`file-$ * 150` will be replaced by "file-0", "file-1" to "file-150".

Replacement occurs at every occurrence, so `$$$-file-$$$` will be replaced by "000-file-000".


## Input and options

Hunch takes two arguments: the input string to parse and a variable number of "key value" pairs
representing external sources in the form "source_id source_value", where 'source_value' is a string of names separated by a given delimiter (default ',').

Hunch can be configured from command-line options:

+ `-p`, `--templates-path`: A directory containing template files. Setting this option will allow you to reference
template files without specifying the same base path over and over again.
+ `-d`, `--delimiter`: A delimiter character or string, used to split external names. Default: ",".
+ `-t`, `--number-token`: A token character used as a placeholder for numbering replacement. Default "$".
+ `-n`, `--start-at`: A number to start the numbering. Default: "0".
+ `-o`, `--override`: Switch. If a file in input expression already exists, override it, otherwise do nothing.
+ `-v`, `--verbose`: Switch. Print debugging information.
+ `-s`, `--simulate`: Switch. Print the preview of the resulting file system tree after parsing the given input, and exit.

As well as the traditional `-h`/`--help` and `--version` switches.


## Installation

Hunch is written in Haskell 2010. You will need a Haskell distribution (such as Haskell Platform) and a
build/package management tool like `cabal` or `stack`.

Easy installation: `cabal install hunch`.

Building from sources (Stack):

1. `stack setup`
2. `stack build`
3. (`stack install`)

Building from sources (cabal):

1. `cabal install --upgrade-dependencies --dependencies-only`
2. `cabal configure`
3. `cabal build`
4. (`cabal install`)


## TODO

+ Test suite
+ Fix absolute path display in simulation mode.


## License

MIT License. See file LICENSE.
