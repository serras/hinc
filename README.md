# `hinc`: Haskell In New Clothes

Braces-and-parens syntax for your favorite language!

### [Try it!](https://serras.github.io/hinc/)

Check the fully-undocumented `hinc`-to-Haskell transpiler.

### [Why?](https://github.com/serras/hinc/blob/master/why.md)

Discussions as [issues](https://github.com/serras/hinc/issues) are also welcome.

### How have you developed it?

That part is actually quite cool. The whole transpiler is developed using a usual stack of `megaparsec` to parse `hinc` code and `haskell-src-exts` to pretty print Haskell code. All of this, in addition to the front-end developed with Miso, is compiled to a single JavaScript file. Really, completely serverless!