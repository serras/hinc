# Dressing Up Haskell

Haskell is, in my opinion, one of the best languages to learn how to code in functional style. Purity by default, powerful pattern matching, and handling of effects, are key ideas which appear in full clarity in Haskell. Alas, many people find Haskell's syntax difficult at the beginning. I always feel I need to devote more time than I should when introducing Haskell.

Hence this experiment: Haskell In New Clothes (`hinc` for short); reimagining Haskell with a syntax inspired in modern JavaScript (or should I say [ECMAScript](https://en.wikipedia.org/wiki/ECMAScript)?). As an appetizer, here is how I envision one would write `mapM` in `hinc`:

```javascript
let mapM(f: (a) => m<b>, lst: List<a>): m<List<b>> where m : Effect
  = case (lst) {
      when Nil -> Nil.pure
      when Cons(x, xs) -> effect {
        let y  = await f(x)
        let ys = await xs.mapM(f)
        Cons(y, ys).pure
      }
    }
```

Are you horrified? Maybe you are wondering whether I've cross the blurry line between sanity and madness? Keep reading! [Try it!](https://serras.github.io/hinc/)

Of course, I am not the first one to try this. [BuckleScript](https://bucklescript.github.io/) and [Reason](https://reasonml.github.io/) provide a JavaScript-like syntax for OCaml, another important functional language. Note however that their goal is also to compile to JavaScript, whereas in Haskell world we are well-served by GHCJS.

## No top-level matching on definitions

Haskellers would usually not use `case` when writing `mapM`, relying instead on the implicit top-level matching in definitions:

```haskell
mapM _ []     = []
mapM f (x:xs) = ...
```

This is one feature which people usually find weird at the beginning (why duplicating the name of the function? why do arguments have _no names_?). In fact, this feature is not shared by other languages outside of Haskell derivatives (such as Agda or Idris) and OCaml. In Scala many functions start with a [explicit `match`](https://docs.scala-lang.org/tour/pattern-matching.html).

## Applications

Most functional programs are simple sequences of nested applications. We dress up those to look a bit more like our parenthesized-application friends.

### Currying everywhere (tuples are not special)

Most programmers are used to call a function with two arguments as `function(arg1, arg2)`. However, in Haskell one writes `function arg1 arg2` instead. The usual reasoning is that `function(arg1, arg2)` is actually calling a _one_-argument `function` with a _tuple_ as the single argument. But why make tuples so special?

`hinc` adopts the philosophy that `(...)` is just a way to provide arguments, not any kind of tuple constructor. Everything is "translated" into a curried version. So one writes:

```javascript
function(arg1, arg2)
(x, y) => body
(Int, Bool) => List<Int>
```

and this is taken exactly as Haskell's:

```haskell
function arg1 arg2
\x -> \y -> body
Int -> Bool -> List Int
```

### Postfix application with `.`

In the `hinc` version of `mapM`, we use `.` in a way that resembles object-oriented notation. However, this is only a syntactic trick. Any use of `.` is simply application where the last argument appears at the front:

```
x.f(a, b, ..., z) ==> f(a, b, ..., z, x)
```

Libraries such as [Ramda](https://ramdajs.com/) explicitly mention _data-last_ functions as a good pattern. Most Haskell functions already follow this pattern, and that's where the choice of `.` comes from. For example, where Haskellers would write:

```haskell
f = average . filter (> 0) . map normalize
```

in `hinc` the idiomatic translation would be:

```haskell
let f(lst) = lst.map(normalize)
                .filter((x) => x > 0)
                .average
```

Note how the order in which operations are written is reversed. This is not by coincidence: people learning Haskell usually have problems with point-free style not because of the composition operator _per se_, but because code is suddenly "reversed". Other communities such as F# have adopted the "pipe forward" operator instead of composition as the default style.

## Java/Swift/TypeScript-syntax for types

This is just a syntactic change: `hinc` adopts the convention of writing type arguments using angle brackets. So `Maybe Int` becomes `Maybe<Int>`. As mentioned above, functions are written with their arguments in parentheses, even though they are "translated" into its curried form.

Note that `hinc` still keeps one interesting feature of Haskell: implicit variable quantification. In the definition of `mapM` above the programmer doesn't have to implicitly say that `a`, `b`, and `m` are type variables. If desired, that could be done as:

```javascript
let mapM<a, b, m>(f: (a) => b, lst: List<a>): List<b>
```

In addition, `hinc` drops some of the built-in types in Haskell, leaving only `=>` as special syntax. So one writes `List<Int>` for `[Int]`, `Tuple<Int, Bool>` for `(Int, Bool)`, and `Equals<Int, Bool>` for `Int ~ Bool`.

### Intertwined signatures and definitions

One of the outstanding characteristics of Haskell code is that type signatures are written _separately_ from their definitions. For example, `mapM` would be written as follows:

```haskell
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
```

In `hinc` the types are written next to the arguments they relate to.

### Constraints go at the end

The definition of `mapM` requires a `Monad` constraint on `m`. In `hinc`, those are written _at the end_ of the typing:

```javascript
let mapM(f: (a) => m<b>, lst: List<a>): m<List<b>> where m : Effect
```

The inspiration comes from [Rust traits](https://doc.rust-lang.org/book/ch10-02-traits.html#clearer-trait-bounds-with-where-clauses).

From that same source we also introduce the syntax `t : c` to what would be written simply `c t` in Haskell. So for example the `elem` function would be written as:

```javascript
let elem(xs: List<a>, e: a) where a : Eq
```

Yes, the resemblance to subtyping constraints in other languages is completely intentional ;)

### Data type definition

Following similar ideas, the definition of data types _always_ uses record and GADT syntax. The type of lists would be then defined as:

```haskell
data List<a> {
  Nil,
  Cons(head: a, tail: List<a>)
}
```

This syntax extends in an easy way to support GADTs by adding a result type to each constructor:

```haskell
data Vec<n, a> {
  VNil : Vec<Zero, a>,
  VCons(vhead: a, vtail: Vec<n, a>) : Vec<Succ(n), a>
}
```

## Blocks with and without effects

Haskell's syntax for pure and effectful functions are quite different. In the former case, we have one single expression which is decorated by `let` bindings in front and `where` bindings at the end:

```haskell
f x = let t = g x in t * t
```

When writing effectful function one usually goes to `do` notation, where bindings can actually take two forms depending on whether they are pure or not:

```haskell
f x = do
  t <- g x
  let r = t * t
  pure r
```

`hinc` tries to narrow this gap by providing just a single kind of block, introduced by curly braces (hey! I said I was inspired by JavaScript!), and where you can have a bunch of `let`s and a final expression.

```javascript
let f(x) = {
  let t = g(x)
  t * t
}
```

If the block has effects, that same keyword introduces the block. Inside of it, one uses `await` where Haskell would use the backarrow `<-`:

```javascript
let f(x) = effect {
  let t = await g(x)
  let r = t * t
  r.pure
}
```

The idea here is to focus on the _similarities_ between both blocks, instead of making the syntax completely apart from each other. `async` is a concept which has been incorporated by [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function), [Rust](https://rust-lang.github.io/async-book/01_getting_started/04_async_await_primer.html), [Kotlin](https://kotlinlang.org/docs/reference/coroutines/coroutines-guide.html), and there are discussions in several other languages.

