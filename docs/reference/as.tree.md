# Display the Tree Structure of Nested Dichotomies

Display the nested structure of a `"dichotomies"` or
`"continuationDichotomies"` object as a 2-D ASCII tree diagram showing
how the response categories are split at each level of the nesting.

## Usage

``` r
as.tree(x, ...)

# S3 method for class 'dichotomies'
as.tree(x, response = NULL, lobstr = FALSE, ...)

# S3 method for class 'continuationDichotomies'
as.tree(x, response = NULL, lobstr = FALSE, ...)
```

## Arguments

- x:

  A `"dichotomies"` or `"continuationDichotomies"` object.

- ...:

  additional arguments (currently unused).

- response:

  Optional character string giving the name of the response variable,
  used as the root label of the tree. If `NULL` (default), the root is
  labeled `"(response)"`.

- lobstr:

  Logical. If `FALSE` (default), renders a 2-D ASCII tree with `/` and
  `\` branch connectors, with each node centered above its two children.
  If `TRUE`, builds a nested list representation of the tree and renders
  it via [`tree`](https://lobstr.r-lib.org/reference/tree.html), which
  must be installed.

## Value

Invisibly returns `x`; called for its side effect of printing.

## Details

The flat list of dichotomies is reconstructed into a binary tree by
matching each dichotomy's domain (the union of its two sides) to the
multi-level groups produced by earlier splits. Branch labels are taken
from the named arguments to
[`dichotomy`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md)
when present, and are otherwise generated automatically as
`{level1, level2, ...}`.

## See also

[`logits`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`continuationLogits`](https://friendly.github.io/nestedLogit/reference/nestedLogit.md),
[`print.dichotomies`](https://friendly.github.io/nestedLogit/reference/nestedMethods.md)

## Examples

``` r
## Womenlf: named group on one branch
comparisons <- logits(work = dichotomy("not.work",
                                       working = c("parttime", "fulltime")),
                      full = dichotomy("parttime", "fulltime"))
as.tree(comparisons, response = "partic")
#>        partic
#>      /          \
#> not.work      working
#>               /      \
#>          parttime fulltime 

## GSS: continuation logits for ordered education levels
cont <- continuationLogits(c("l.t.highschool", "highschool",
                             "college", "graduate"))
as.tree(cont, response = "degree")
#>                degree
#>         /                    \
#> l.t.highschool {highschool, college, graduate}
#>                      /                  \
#>                highschool       {college, graduate}
#>                                     /          \
#>                                 college     graduate 

## Chile: named groups on both branches
chile.dichots <- logits(
  engage    = dichotomy(engaged    = c("Y", "N"),
                        disengaged = c("A", "U")),
  direction = dichotomy("Y", "N"),
  disengage = dichotomy("A", "U"))
as.tree(chile.dichots, response = "vote")
#>       vote
#>     /        \
#> engaged  disengaged
#>  /    \   /       \
#> Y      N A         U 
```
