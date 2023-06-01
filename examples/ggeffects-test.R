# test ggeffects

library(ggeffects)
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

ggpredict(m, c("hincome", "children")) |> plot()

# Data were 'prettified'. Consider using `terms="hincome [all]"` to get smooth plots.
# Error: At least one term specified in `terms` is no valid model term.

ggeffect(m, terms = "hincome[5:40, by=5]")

# Can't compute marginal effects, 'effects::Effect()' returned an error.
#
# Reason: object 'prior.weights' not found
# You may try 'ggpredict()' or 'ggemmeans()'.
