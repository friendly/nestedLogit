#' ---
#' title: Food choice of alligators
#' ---

# from: https://data.library.virginia.edu/getting-started-with-multinomial-logit-models/


gators <- read.csv('https://static.lib.virginia.edu/statlab/materials/data/table_8-1.csv')
gators$food <- factor(gators$food,
                      levels = c("O", "F", "I"),
                      labels = c("Other", "Fish", "Invertebrates"))

save(gators, file = "data/gators.RData")


# Title: Data on Alligator Food Choice
# Description:
# Agresti (1996, p. 207) gives this data on  59 alligators sampled from a lake in Florida. It has the length of the alligator in meters and the primary food type found in the alligator’s stomach. The food type was classified into three categories: "Fish", "Invertebrate", and "Other".
#  Of interest is whether or not the length of an alligator is associated with the primary food type. Does knowing the length of an alligator give us some indication about its primary food type? If so, how is length associated with the choice of food type?
#  References: Agresti, A. (1996). An introduction to categorical data analysis. Wiley.
