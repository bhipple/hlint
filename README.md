# HLint [![Hackage version](https://img.shields.io/hackage/v/hlint.svg?label=Hackage)](https://hackage.haskell.org/package/hlint) [![Stackage version](https://www.stackage.org/package/hlint/badge/lts?label=Stackage)](https://www.stackage.org/package/hlint) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/hlint.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/hlint) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/hlint.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/hlint)

HLint is a tool for suggesting possible improvements to Haskell code. These suggestions include ideas such as using alternative functions, simplifying code and spotting redundancies. You can try HLint online at [lpaste.net](http://lpaste.net/) - suggestions are shown at the bottom. This document is structured as follows:

* [Installing and running HLint](#installing-and-running-hlint)
* [FAQ](#faq)
* [Customizing the hints](#customizing-the-hints)
* [Hacking HLint](#hacking-hlint)

### Acknowledgements

This program has only been made possible by the presence of the [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts) package, and many improvements have been made by [Niklas Broberg](http://www.nbroberg.se) in response to feature requests. Additionally, many people have provided help and patches, including Lennart Augustsson, Malcolm Wallace, Henk-Jan van Tuyl, Gwern Branwen, Alex Ott, Andy Stewart, Roman Leshchinskiy, Johannes Lippmann, Iustin Pop, Steve Purcell and others.

### Bugs and limitations

Bugs can be reported [on the bug tracker](https://github.com/ndmitchell/hlint/issues). There are some issues that I do not intend to fix:

* HLint operates on each module at a time in isolation, as a result HLint does not know about types or which names are in scope.
* The presence of `seq` may cause some hints (i.e. eta-reduction) to change the semantics of a program.
* Some transformed programs may require additional type signatures, particularly if the transformations trigger the monomorphism restriction or involve rank-2 types.
* The `RebindableSyntax` extension can cause HLint to suggest incorrect changes.
* HLint turns on many language extensions so it can parse more documents, occasionally some break otherwise legal syntax - e.g. `{-#INLINE foo#-}` doesn't work with `MagicHash`. These extensions can be disabled with `-XNoMagicHash`.

## Installing and running HLint

Installation follows the standard pattern of any Haskell library or program, type `cabal update` to update your local hackage database, then `cabal install hlint` to install HLint.

Once HLint is installed, run hlint source where source is either a Haskell file, or a directory containing Haskell files. A directory will be searched recursively for any files ending with .hs or .lhs. For example, running HLint over darcs would give:


    $ hlint darcs-2.1.2

    darcs-2.1.2\src\CommandLine.lhs:94:1: Warning: Use concatMap
    Found:
      concat $ map escapeC s
    Why not:
      concatMap escapeC s

    darcs-2.1.2\src\CommandLine.lhs:103:1: Suggestion: Use fewer brackets
    Found:
      ftable ++ (map (\ (c, x) -> (toUpper c, urlEncode x)) ftable)
    Why not:
      ftable ++ map (\ (c, x) -> (toUpper c, urlEncode x)) ftable

    darcs-2.1.2\src\Darcs\Patch\Test.lhs:306:1: Warning: Use a more efficient monadic variant
    Found:
      mapM (delete_line (fn2fp f) line) old
    Why not:
      mapM_ (delete_line (fn2fp f) line) old

    ... lots more hints ...

Each hint says which file/line the hint relates to, how serious an issue it is, a description of the hint, what it found, and what you might want to replace it with. In the case of the first hint, it has suggested that instead of applying `concat` and `map` separately, it would be better to use the combination function `concatMap`.

The first hint is marked as an warning, because using `concatMap` in preference to the two separate functions is always desirable. In contrast, the removal of brackets is probably a good idea, but not always. Reasons that a hint might be a suggestion include requiring an additional import, something not everyone agrees on, and functions only available in more recent versions of the base library.

**Bug reports:** The suggested replacement should be equivalent - please report all incorrect suggestions not mentioned as known limitations.

### Automatically Applying Hints

By supplying the `--refactor` flag hlint can automatically apply most
suggestions. Instead of a list of hints, hlint will instead output the
refactored file on stdout. In order to do this, it is necessary to have the
`refactor` executable on you path. `refactor` is provided by the
[`apply-refact`](https://github.com/mpickering/apply-refact) package,
it uses the GHC API in order to transform source files given a list of
refactorings to apply. Hlint directly calls the executable to apply the
suggestions.

Additional configuration can be passed to `refactor` with the
`--refactor-options` flag. Some useful flags include `-i` which replaces the
original file and `-s` which asks for confirmation before performing a hint.

An alternative location for `refactor` can be specified with the
`--with-refactor` flag.

Simple bindings for [vim](https://github.com/mpickering/hlint-refactor-vim),
[emacs](https://github.com/mpickering/hlint-refactor-mode) and [atom](https://github.com/mpickering/hlint-refactor-atom) are provided.

There are no plans to support the duplication nor the renaming hints.

### Reports

HLint can generate a lot of information, making it difficult to search for particular types of errors. The `--report` flag will cause HLint to generate a report file in HTML, which can be viewed interactively. Reports are recommended when there are more than a handful of hints.

### Language Extensions

HLint enables most Haskell extensions, disabling only those which steal too much syntax (currently Arrows, TransformListComp, XmlSyntax and RegularPatterns). Individual extensions can be enabled or disabled with, for instance, `-XArrows`, or `-XNoMagicHash`. The flag `-XHaskell98` selects Haskell 98 compatibility.

### Emacs Integration

Emacs integration has been provided by [Alex Ott](http://xtalk.msk.su/~ott/). The integration is similar to compilation-mode, allowing navigation between errors. The script is at [hs-lint.el](https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el), and a copy is installed locally in the data directory. To use, add the following code to the Emacs init file:

    (require 'hs-lint)
    (defun my-haskell-mode-hook ()
       (local-set-key "\C-cl" 'hs-lint))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

### GHCi Integration

GHCi integration has been provided by Gwern Branwen. The integration allows running `:hlint` from the GHCi prompt. The script is at [hlint.ghci](https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hlint.ghci), and a copy is installed locally in the data directory. To use, add the contents to your [GHCi startup file](https://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#the-ghci-and-haskeline-files).

### Parallel Operation

To run HLint on 4 processors append the flags `-j4`. HLint will usually perform fastest if n is equal to the number of physical processors, which can be done with `-j` alone.

If your version of GHC does not support the GHC threaded runtime then install with the command: `cabal install --flags="-threaded"`

### C preprocessor support

HLint runs the [cpphs C preprocessor](http://hackage.haskell.org/package/cpphs) over all input files, by default using the current directory as the include path with no defined macros. These settings can be modified using the flags `--cpp-include` and `--cpp-define`. To disable the C preprocessor use the flag `-XNoCPP`. There are a number of limitations to the C preprocessor support:

* HLint will only check one branch of an `#if`, based on which macros have been defined.
* Any missing `#include` files will produce a warning on the console, but no information in the reports.

### Unicode support

By default, HLint uses the current locale encoding. The encoding can be overridden with either `--utf8` or `--encoding=value`. For descriptions of some valid [encodings see the mkTextEncoding documentation](http://haskell.org/ghc/docs/latest/html/libraries/base/System-IO.html#v%3AmkTextEncoding).

## FAQ

### Why are hints not applied recursively?

Consider:

    foo xs = concat (map op xs)

This will suggest eta reduction to `concat . map op`, and then after making that change and running HLint again, will suggest use of `concatMap`. Many people wonder why HLint doesn't directly suggest `concatMap op`. There are a number of reasons:

* HLint aims to both improve code, and to teach the author better style. Doing modifications individually helps this process.
* Sometimes the steps are reasonably complex, by automatically composing them the user may become confused.
* Sometimes HLint gets transformations wrong. If suggestions are applied recursively, one error will cascade.
* Some people only make use of some of the suggestions. In the above example using concatMap is a good idea, but sometimes eta reduction isn't. By suggesting them separately, people can pick and choose.
* Sometimes a transformed expression will be large, and a further hint will apply to some small part of the result, which appears confusing.
* Consider `f $ (a b)`. There are two valid hints, either remove the $ or remove the brackets, but only one can be applied.

### Why doesn't the compiler automatically apply the optimisations?

HLint doesn't suggest optimisations, it suggests code improvements - the intention is to make the code simpler, rather than making the code perform faster. The [GHC compiler](http://haskell.org/ghc/) automatically applies many of the rules suggested by HLint, so HLint suggestions will rarely improve performance.

### Why doesn't HLint know the fixity for my custom !@%$ operator?

HLint knows the fixities for all the operators in the base library, but no others. HLint works on a single file at a time, and does not resolve imports, so cannot see fixity declarations from imported modules. You can tell HLint about fixities by putting them in a hint file, or passing them on the command line. For example, pass `--with=infixr 5 !@%$`, or put all the fixity declarations in a file and pass `--hint=fixities.hs`. You can also use [--find](https://rawgithub.com/ndmitchell/hlint/master/hlint.htm#find) to automatically produce a list of fixity declarations in a file.

### How can I use `--with` or `--hint` with the default hints?

HLint does not use the default set of hints if custom hints are specified on the command line using `--with` or `--hint`. To include the default hints pass `--hint=HLint` on the command line.

### Why do I sometimes get a "Note" with my hint?

Most hints are perfect substitutions, and these are displayed without any notes. However, some hints change the semantics of your program - typically in irrelevant ways - but HLint shows a warning note. HLint does not warn when assuming typeclass laws (such as `==` being symmetric). Some notes you may see include:

* __Increases laziness__ - for example `foldl (&&) True` suggests `and` including this note. The new code will work on infinite lists, while the old code would not. Increasing laziness is usually a good idea.
* __Decreases laziness__ - for example `(fst a, snd a)` suggests a including this note. On evaluation the new code will raise an error if a is an error, while the old code would produce a pair containing two error values. Only a small number of hints decrease laziness, and anyone relying on the laziness of the original code would be advised to include a comment.
* __Removes error__ - for example `foldr1 (&&)` suggests and including the note `Removes error on []`. The new code will produce `True` on the empty list, while the old code would raise an error. Unless you are relying on the exception thrown by the empty list, this hint is safe - and if you do rely on the exception, you would be advised to add a comment. 

### What is the difference between error/warning/suggestion?

Every hint has a severity level:

* __Error__ - by default only used for parse errors. 
* __Warning__ - for example `concat (map f x)` suggests `concatMap f x` as a "warning" severity hint. From a style point of view, you should always replace a combination of `concat` and `map` with `concatMap`.
* __Suggestion__ - for example `x !! 0` suggests `head x` as a "suggestion" severity hint. Typically `head` is a simpler way of expressing the first element of a list, especially if you are treating the list inductively. However, in the expression `f (x !! 4) (x !! 0) (x !! 7)`, replacing the middle argument with `head` makes it harder to follow the pattern, and is probably a bad idea. Suggestion hints are often worthwhile, but should not be applied blindly.

The difference between warning and suggestion is one of personal taste, typically my personal taste. If you already have a well developed sense of Haskell style, you should ignore the difference. If you are a beginner Haskell programmer you may wish to focus on warning hints before suggestion hints.

## Customizing the hints

Many of the hints that are applied by HLint are contained in Haskell source files which are installed in the data directory by Cabal. These files may be edited, to add library specific knowledge, to include hints that may have been missed, or to ignore unwanted hints.

### Choosing a package of hints

By default, HLint will use the `HLint.hs` file either from the current working directory, or from the data directory. Alternatively, hint files can be specified with the `--hint` flag. HLint comes with a number of hint packages:

* __Default__ - these are the hints that are used by default, covering most of the base libraries.
* __Dollar__ - suggests the replacement `a $ b $ c` with `a . b $ c`. This hint is especially popular on the [\#haskell IRC channel](http://www.haskell.org/haskellwiki/IRC_channel).
* __Generalise__ - suggests replacing specific variants of functions (i.e. `map`) with more generic functions (i.e. `fmap`).

As an example, to check the file `Example.hs` with both the default hints and the dollar hint, I could type: `hlint Example.hs --hint=Default --hint=Dollar`.

### Ignoring hints

Some of the hints are subjective, and some users believe they should be ignored. Some hints are applicable usually, but occasionally don't always make sense. The ignoring mechanism provides features for suppressing certain hints. Ignore directives can either be written as pragmas in the file being analysed, or in the hint files. Examples of pragmas are:

* `{-# ANN module "HLint: ignore Eta reduce" #-}` - ignore all eta reduction suggestions in this module (use `module` literally, not the name of the module).
* `{-# ANN myFunction "HLint: ignore" #-}` - don't give any hints in the function `myFunction`.
* `{-# ANN myFunction "HLint: error" #-}` - any hint in the function `myFunction` is an error.
* `{-# ANN module "HLint: error Use concatMap" #-}` - the hint to use `concatMap` is an error.
* `{-# ANN module "HLint: warn Use concatMap" #-}` - the hint to use `concatMap` is a warning.
* `{-# ANN module "HLint: suggest Use concatMap" #-}` - the hint to use `concatMap` is a suggestion.

Ignore directives can also be written in the hint files:

* `ignore "Eta reduce"` - suppress all eta reduction suggestions.
* `ignore "Eta reduce" = MyModule1 MyModule2` - suppress eta reduction hints in the `MyModule1` and `MyModule2` modules.
* `ignore = MyModule.myFunction` - don't give any hints in the function `MyModule.myFunction`.
* `error = MyModule.myFunction` - any hint in the function `MyModule.myFunction` is an error.
* `error "Use concatMap"` - the hint to use `concatMap` is an error.
* `warn "Use concatMap"` - the hint to use `concatMap` is a warning.
* `suggest "Use concatMap"` - the hint to use `concatMap` is a suggestion.

These directives are applied in the order they are given, with later hints overriding earlier ones.

### Adding hints

The hint suggesting `concatMap` is defined as:

    warn = concat (map f x) ==> concatMap f x

This line can be read as replace `concat (map f x)` with `concatMap f x`. All single-letter variables are treated as substitution parameters. For examples of more complex hints see the supplied hints file. This hint will automatically match `concat . map f` and `concat $ map f x`, so there is no need to give eta-reduced variants of the hints. Hints may start with `error`, `warn` or `suggest` to denote how severe they are by default. In addition, `hint` is a synonym for `suggest`. If you come up with interesting hints, please submit them for inclusion.

You can search for possible hints to add from a source file with the `--find` flag, for example:

    $ hlint --find=src/Utils.hs
    -- hints found in src/Util.hs
    warn = null (intersect a b) ==> disjoint a b
    warn = dropWhile isSpace ==> trimStart
    infixr 5 !:

These hints are suitable for inclusion in a custom hint file. You can also include Haskell fixity declarations in a hint file, and these will also be extracted. If you pass only `--find` flags then the hints will be written out, if you also pass files/folders to check, then the found hints will be automatically used when checking.

## Hacking HLint

### Tests

Tests can be run either from within a `ghci` session by typing `:test` or by running the standalone binary's tests via `stack exec hlint test`.

New tests for individual hints can be added directly to source and hint files by adding annotations bracketed in `<TEST></TEST>` code comment blocks. As some examples:

```haskell
{-
    Tests to check the zipFrom hint works

<TEST>
zip [1..length x] x -- zipFrom 1 x
zip [1..length y] x
zip [1..length x] x -- ??? @Warning
</TEST>
-}
```

The general syntax is `lhs -- rhs` with `lhs` being the expression you expect to be rewritten as `rhs`. The absence of `rhs` means you expect no hints to fire. In addition `???` lets you assert a warning without a particular suggestion, while `@` tags require a specific severity -- both these features are used less commonly.
