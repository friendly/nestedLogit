# test ggeffects

library(ggeffects)
library(nestedLogit)

data("Womenlf", package = "carData")
m <- nestedLogit(partic ~ hincome + children,
                 logits(
                   work = dichotomy("not.work", c("parttime", "fulltime")),
                   full = dichotomy("parttime", "fulltime")
                 ),
                 data = Womenlf
)

ggpredict(m, c("hincome", "children")) |> plot()
