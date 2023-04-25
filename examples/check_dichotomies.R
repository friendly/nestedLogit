#' Check validity of dichotomies as nested lists
#'
#'
#' @param x
#' @param levels
#'
#' @author Achim Zeileis
#' @return
#' @export
#'
#' @examples
check_dichotomies <- function(x, levels = NULL) {
  if(!is.null(levels)) {
    xvec <- unlist(x)
    if(anyDuplicated(xvec) > 0L || length(setdiff(xvec, levels)) > 0L || length(setdiff(levels, xvec)) > 0L) {
      stop("dichotomies must contain all levels exactly once")
    }
  }
  if(!is.list(x) || length(x) != 2L) stop("dichotomies must be (recursive) lists of two elements")
  xlist <- sapply(x, is.list)
  if(any(!xlist)) {
    ok <- sapply(which(!xlist), function(i) is.character(x[[i]]) && length(x[[i]]) == 1L)
    if(any(!ok)) stop("terminal elements of dichotomies must be character strings of length 1")
  }
  ok <- if(any(xlist)) {
    all(sapply(x[xlist], check_dichotomies))
  } else {
    TRUE
  }
  return(ok)
}

if(FALSE) {

## levels
lev <- c("a", "b", "c", "d")

## ok
d1 <- list("a", list("b", list("c", "d")))
d2 <- list(list(list("a", "c"), "d"), "b")
d3 <- list(list("a", "d"), list("b", "c"))
check_dichotomies(d1, levels = lev)
check_dichotomies(d2, levels = lev)
check_dichotomies(d3, levels = lev)

## not ok
x1 <- list("a", list("b", list("c", "e")))
x2 <- list("a", list("b", list("c", list("d", "d"))))
x3 <- list("a", list("b", c("c", "d")))
x4 <- list("a", list("b", "c", "d"))
check_dichotomies(x1, levels = lev)
check_dichotomies(x2, levels = lev)
check_dichotomies(x3, levels = lev)
check_dichotomies(x4, levels = lev)

lev <- c("plane", "train", "bus", "car")
travel <- list(
  air = "plane",
  ground = list(
    public = list("train", "bus"),
    private = "car"
  )
)

check_dichotomies(travel, lev)

unlist(travel, names)

}
