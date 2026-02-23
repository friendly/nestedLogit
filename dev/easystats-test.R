# using easystats functions

library(nestedLogit)
library(modelbased) # For generating predictions with CI
library(see)        # For the automated plotting method
library(ggplot2)

# 1. Fit the nestedLogit model
data(Womenlf, package = "carData")

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                                               full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)

# 2. Estimate expected values (probabilities) with 95% confidence intervals
# 'by = "hincome"' specifies the x-axis predictor
predictions <- estimate_expectation(wlf.nested, by = "hincome")

# ERROR:
# Error in link_inv(predictions) :
#   Argument eta must be a nonempty numeric vector

# 3. Plot with confidence bands
# The plot() method from the 'see' package automatically adds the ribbon (confidence band)
plot(predictions) +
  labs(title = "Predicted Probabilities of Work Status",
       subtitle = "With 95% Confidence Bands",
       x = "Husband's Income",
       y = "Probability") +
  theme_modern()
