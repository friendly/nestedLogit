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

ggpredict(m, c("hincome", "children")) |> plot()

# Using ggeffects 1.2.2.14
# Error in if ((is.null(mf) || nrow(mf) == 0) && source != "environment") { :
#     missing value where TRUE/FALSE needed
#   In addition: Warning message:
#     Could not access model information.
#   > traceback()
#   5: get_data.default(model, source = "frame")
#   4: insight::get_data(model, source = "frame")
#   3: ggpredict_helper(model = model, terms = terms, ci.lvl = ci.lvl,
#                       type = type, typical = typical, ppd = ppd, condition = condition,
#                       back.transform = back.transform, vcov.fun = vcov.fun, vcov.type = vcov.type,
#                       vcov.args = vcov.args, interval = interval, verbose = verbose,
#                       ...)
#   2: ggpredict(m, c("hincome", "children"))
#   1: plot(ggpredict(m, c("hincome", "children")))

# Data were 'prettified'. Consider using `terms="hincome [all]"` to get smooth plots.
# Error: At least one term specified in `terms` is no valid model term.

ggeffect(m, terms = "hincome[5:40, by=5]")

# Can't compute marginal effects, 'effects::Effect()' returned an error.
#
# Reason: object 'prior.weights' not found
# You may try 'ggpredict()' or 'ggemmeans()'.
