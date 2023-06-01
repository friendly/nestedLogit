# test with ggeffects and other easystats pkgs

library(nestedLogit)

# get dev version
#install.packages("ggeffects", repos = "https://strengejacke.r-universe.dev")
#remotes::install_github("strengejacke/ggeffects")
library(ggeffects)

data("Womenlf", package = "carData")
m <- nestedLogit(partic ~ hincome + children,
                 logits(
                   work = dichotomy("not.work", c("parttime", "fulltime")),
                   full = dichotomy("parttime", "fulltime")
                 ),
                 data = Womenlf
)
new <- expand.grid(
  hincome = c(20, 30, 40),
  children = c("absent", "present")
)

# this doesn't work
#cbind(new, predict(m, newdata = new))

pred <- predict(m, newdata = new)
as.data.frame(pred, newdata=new)


ggpredict(m, c("hincome [30, 40]", "children"))
