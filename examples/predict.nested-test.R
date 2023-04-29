#' ---
#' title: nested predict methods
#' ---

data(Womenlf, package = "carData")

#' ## Fit the model
#' Use `logits()` and `dichotomy()` to specify the comparisons of interest
comparisons <- logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                      full=dichotomy("parttime", "fulltime"))

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          dichotomies = comparisons,
                          data=Womenlf)


#' ## Get predicted probabilities
new <- expand.grid(hincome=seq(0, 45, length=10),
                   children=c("absent", "present"))

pred.nested <- predict(wlf.nested, newdata=new, model = "nested")

head(pred.nested)

pred.logits <- predict(wlf.nested, newdata=new, model = "dichotomies")

# print method not useful
pred.logits

# show predicted values
pred.logits$work
pred.logits$full

# what predict should give:
sapply(wlf.nested$models, predict, newdata=new)


