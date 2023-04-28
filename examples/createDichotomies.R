createDichtomies <- function(x){
  UseMethod("createDichtomies")
}

createDichtomies.dichotomies <- function(x) x

createDichtomies.default <- function(x)
  stop("dichotomies argument must be of class 'dichotomies' or mode 'list'")

createDichtomies.list <- function(x){
  dichotomies <- list()
  helper <- function(x){
    if (length(x) != 2 || !is.list(x))
      stop("ill-formed nested list of dichotomies:",
           "\neach dichotomy must be a list with exactly 2 sets of levels")
    a <- x[[1]]
    b <- x[[2]]
    aa <- unlist(a, recursive=TRUE)
    bb <- unlist(b, recursive=TRUE)
    name <- make.names(
      paste0(paste(aa, collapse="."),
             "_v_",
             paste(bb, collapse="."))
    )
    dichotomies[[name]] <<- dichotomy(aa, bb)
    if (length(a) > 1) helper(a)
    if (length(b) > 1) helper(b)
  }
  helper(x)
  do.call(logits, dichotomies)
}

createDichtomies.list <- function(x){
  helper <- function(dichotomies, x){
    if (length(x) != 2 || !is.list(x))
      stop("ill-formed nested list of dichotomies:",
           "\neach dichotomy must be a list with exactly 2 sets of levels")
    a <- x[[1]]
    b <- x[[2]]
    aa <- unlist(a, recursive=TRUE)
    bb <- unlist(b, recursive=TRUE)
    name <- make.names(
      paste0(paste(aa, collapse="."),
             "_v_",
             paste(bb, collapse="."))
    )
    dichotomies[[name]] <- dichotomy(aa, bb)
    if (length(a) > 1) dichotomies <- helper(dichotomies, a)
    if (length(b) > 1) dichotomies <- helper(dichotomies, b)
    dichotomies
  }
  dichotomies <- helper(list(), x)
  do.call(logits, dichotomies)
}


if (FALSE){
library(nestedLogit)

d1 <- list("a", list("b", list("c", "d")))
d2 <- list(list(list("a", "c"), "d"), "b")
d3 <- list(list("a", "d"), list("b", "c"))

createDichtomies(d1)
createDichtomies(d2)
createDichtomies(d3)

# not OK
x1 <- list("a", list("b", list("c", "e"))) # actually OK (w/o giving levels)
x2 <- list("a", list("b", list("c", list("d", "d"))))
x3 <- list("a", list("b", c("c", "d")))
x4 <- list("a", list("b", "c", "d"))

createDichtomies(x1)
createDichtomies(x2)
createDichtomies(x3)
createDichtomies(x4)

lev <- c("plane", "train", "bus", "car")
travel <- list(
  air = "plane",
  ground = list(
    public = list("train", "bus"),
    private = "car"
  )
)

createDichtomies(travel)

}
