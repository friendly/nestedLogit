# check createDichotomies

library(nestedLogit)

d1 <- list("a", list("b", list("c", "d")))
d2 <- list(list(list("a", "c"), "d"), "b")
d3 <- list(list("a", "d"), list("b", "c"))

nestedLogit:::createDichotomies(d1)
nestedLogit:::createDichotomies(d2)
nestedLogit:::createDichotomies(d3)

x1 <- list("a", list("b", list("c", "e"))) # actually OK (w/o giving levels)
x2 <- list("a", list("b", list("c", list("d", "d"))))
x3 <- list("a", list("b", c("c", "d")))
x4 <- list("a", list("b", "c", "d"))

nestedLogit:::createDichotomies(x1)
nestedLogit:::createDichotomies(x2)
nestedLogit:::createDichotomies(x3)
nestedLogit:::createDichotomies(x4)

(di1 <- nestedLogit:::createDichotomies(d1))
names(di1)
names(di1) <- c("d1", "d2", "d3")
di1

dd1 <- list("a", BCD=list("b", CD=list("c", "d")))
nestedLogit:::createDichotomies(dd1)

dd3 <- list(private=list("a", "d"), public=list("b", "c"))
nestedLogit:::createDichotomies(dd3)


nestedLogit:::createDichotomies(travel <- list(
  air = "plane",
  ground = list(
    public = list("train", "bus"),
    private = "car"
  )))

labor <- list(
  work = "not.work",
  full = list("parttime", "fulltime"))

nestedLogit:::createDichotomies(labor)

