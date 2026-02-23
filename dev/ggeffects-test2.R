# Using ggeffects,
#    easystats:: insight, parameters, performance

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
theme_ggeffects(base_size = 14)
plot(wlf.pred,
     line_size = 2) +
  labs(title = "Predicted Probabilities by Husband's Income",
       y = "Probability",
       x = "Husband's Income")

