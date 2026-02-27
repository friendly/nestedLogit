# Using ggeffects,

library(nestedLogit)
library(ggeffects)
library(ggplot2)

# 1. Fit a nestedLogit model (using the 'Womenlf' dataset from the 'carData' package)
data(Womenlf, package = "carData")
wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                                               full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)

# 2. Calculate adjusted predictions for a specific predictor (e.g., 'hincome')
# Predictions are returned on the response scale (probabilities) by default
wlf.pred <- predict_response(wlf.nested,
                       terms = ~ hincome + children)

# 3. Plot the results
# This automatically handles facets or colors for the different response levels
# TODO: How to plot the logits or probabilities for the dichotomies? This gives plots of the probabilities
#       of the separate response categories.

plot(wlf.pred,
     line_size = 2) +
  labs(title = "Predicted Probabilities of Work by Husband's Income",
       y = "Probability",
       x = "Husband's Income") +
  theme_ggeffects(base_size = 16) +
  theme(legend.position = "top")


# Use nestedLogit predict methods
wlf.pred.nested <- predict(wlf.nested, model = "nested")
str(wlf.pred.nested)

wlf.pred.dichot <- predict(wlf.nested, model = "dichotomies")
str(wlf.pred.dichot)

# Can we use this with `ggeffects`?
