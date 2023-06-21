# test ggeffects

install.packages("ggeffects", repos = "https://strengejacke.r-universe.dev")
install.packages("insight", repos = "https://easystats.r-universe.dev")
library(ggeffects)
library(insight)
packageVersion("ggeffects")
# tested using latgest version, 1.2.2.12

library(nestedLogit)

data("Womenlf", package = "carData")
m <- nestedLogit(partic ~ hincome + children,
                 logits(
                   work = dichotomy("not.work", c("parttime", "fulltime")),
                   full = dichotomy("parttime", "fulltime")
                 ),
                 data = Womenlf
)

ggpredict(m, c("hincome[all]", "children")) |> plot()


# Data were 'prettified'. Consider using `terms="hincome [all]"` to get smooth plots.
# Error: At least one term specified in `terms` is no valid model term.



wlf.ggeff <- ggeffect(m, terms = c("hincome[5:40, by=5]", "children"))
plot(wlf.ggeff, facet=TRUE)

# why does this produce only one plot??
