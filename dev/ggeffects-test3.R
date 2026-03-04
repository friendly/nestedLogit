# insight::get_predicted() and modelbased::estimate_relation() support dichotomies in their latest (GitHub) versions:
# see: https://github.com/strengejacke/ggeffects/pull/672
#      https://github.com/strengejacke/ggeffects/issues/671#issuecomment-3995544639
#
#

# install latest version
remotes::install_github("strengejacke/ggeffects", ref = "strengejacke/issue671")

library(nestedLogit)
library(modelbased)
library(ggeffects)

packageVersion("modelbased")
# [1] ‘0.14.0.2’
packageVersion("ggeffects")
# [1] ‘2.3.2.1’


data(Womenlf, package = "carData")
comparisons <- logits(
  work = dichotomy("not.work", c("parttime", "fulltime")),
  full = dichotomy("parttime", "fulltime")
)

m <- nestedLogit(
  partic ~ hincome + children,
  dichotomies = comparisons,
  data = Womenlf
)

estimate_relation(m, by = "children")

estimate_relation(m, by = "children", submodel = "dichotomies")
# Error in link_inv(predictions) :
#   Argument eta must be a nonempty numeric vector
# In addition: Warning messages:
# 1: Logistic regression model has a categorical response variable. You may need to set `include_response=TRUE` to make it
#   work for predictions.
# 2: Something went wrong with computing standard errors and confidence intervals for predictions.
# 3: Could not apply Delta method to transform standard errors.
#   You may be able to obtain standard errors by using the
#   `predict="link"` argument value.

estimate_relation(m, by = "children", predict = "link")
# Error in `<current-expression>` : node stack overflow
# In addition: Warning messages:
# 1: Logistic regression model has a categorical response variable. You may need to set `include_response=TRUE` to make it
#   work for predictions.
# 2: Something went wrong with computing standard errors and confidence intervals for predictions.

# test with ggeffects: works!

predict_response(m, "children")

predict_response(m, "children", submodel = "dichotomies")

wlf.pred <- predict_response(wlf.nested, terms = c("hincome", "children"))
plot(wlf.pred)

wlf.pred.dichot <- predict_response(wlf.nested, terms = c("hincome", "children"),
                             submodel = "dichotomies")

plot(wlf.pred.dichot) +
  geom_point() +
  theme(legend.position = "inside",
        legend.position.inside = c(.40, .85))

