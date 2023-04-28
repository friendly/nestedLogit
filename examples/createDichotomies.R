createDichtomies <- function(x){
  UseMethod("createDichtomies")
}

createDichtomies.dichotomies <- function(x) x

createDichtomies.default <- function(x)
  stop("dichotomies argument must be of class 'dichotomies' or mode 'list'")

# createDichtomies.list <- function(x){
#   dichotomies <- list()
#   helper <- function(x){
#     if (length(x) != 2 || !is.list(x))
#       stop("ill-formed nested list of dichotomies:",
#            "\neach dichotomy must be a list with exactly 2 sets of levels")
#     a <- x[[1]]
#     b <- x[[2]]
#     aa <- unlist(a, recursive=TRUE)
#     bb <- unlist(b, recursive=TRUE)
#     name <- make.names(
#       paste0(paste(aa, collapse="."),
#              "_v_",
#              paste(bb, collapse="."))
#     )
#     dichotomies[[name]] <<- dichotomy(aa, bb)
#     if (length(a) > 1) helper(a)
#     if (length(b) > 1) helper(b)
#   }
#   helper(x)
#   do.call(logits, dichotomies)
# }

createDichtomies.list <- function(x){
  helper <- function(dichotomies, x){
    if (length(x) != 2L || !is.list(x)) {
      stop("ill-formed nested list of dichotomies: ",
           paste("(length = ", length(x), "; is.list = ", is.list(x), ")"),
           "\nEach dichotomy must be a list with exactly 2 sets of levels")
    }
    a <- x[[1L]]
    b <- x[[2L]]
    nm.a <- names(x[1L])
    nm.b <- names(x[2L])
    aa <- unlist(a, recursive=TRUE)
    bb <- unlist(b, recursive=TRUE)
    name <- make.names(
      paste0(if (!is.null(nm.a) && nm.a != "") nm.a else paste(aa, collapse="."),
             "_v_",
             if (!is.null(nm.b) && nm.b != "") nm.b else paste(bb, collapse="."))
    )
    aa.bb <- list(aa, bb)
    names(aa.bb) <- c(nm.a, nm.b)
    dichotomies[[name]] <- do.call(dichotomy, aa.bb)
    if (length(a) > 1L) dichotomies <- helper(dichotomies, a)
    if (length(b) > 1L) dichotomies <- helper(dichotomies, b)
    dichotomies
  }
  dichotomies <- helper(list(), x)
  do.call(logits, dichotomies)
}

print.dichotomies <- function(x, ...) {
  nms <- names(x)
  for (i in seq_along(x)) {
    cat(paste0(
      nms[i],
      ": ",
      names(x[[i]][1L]),
      "{",
      paste(x[[i]][[1L]], collapse = ", "),
      "} vs. ",
      names(x[[i]][2L]),
      "{",
      paste(x[[i]][[2L]], collapse = ", "),
      "}\n"
    ))
  }
  invisible(x)
}



if (FALSE){
library(nestedLogit)

d1 <- list("a", list("b", list("c", "d")))
d2 <- list(list(list("a", "c"), "d"), "b")
d3 <- list(list("a", "d"), list("b", "c"))

createDichtomies(d1)
createDichtomies(d2)
createDichtomies(d3)

x1 <- list("a", list("b", list("c", "e"))) # actually OK (w/o giving levels)
x2 <- list("a", list("b", list("c", list("d", "d"))))
x3 <- list("a", list("b", c("c", "d")))
x4 <- list("a", list("b", "c", "d"))

createDichtomies(x1)
createDichtomies(x2)
createDichtomies(x3)
createDichtomies(x4)

(di1 <- createDichtomies(d1))
names(di1)
names(di1) <- c("d1", "d2", "d3")
di1

dd1 <- list("a", BCD=list("b", CD=list("c", "d")))
createDichtomies(dd1)

dd3 <- list(private=list("a", "d"), public=list("b", "c"))
createDichtomies(dd3)


createDichtomies(travel <- list(
  air = "plane",
  ground = list(
    public = list("train", "bus"),
    private = "car"
  )))

labor <- list(
  work = "not.work",
  full = list("parttime", "fulltime"))

createDichtomies(labor)

}
