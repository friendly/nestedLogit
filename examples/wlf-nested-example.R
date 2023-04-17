#' ---
#' title: Womenlf nested examples, largely for checking that things work
#' ---

#' This file generates errors, warnings, but errors are allowed here
#' 


library(car)

logits(work=dichotomy("not.work", c("parttime", "fulltime")),
       full=dichotomy("parttime", "fulltime"))

as.matrix(logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                 full=dichotomy("parttime", "fulltime")))

#' ## not properly nested:
#' 
#+ error=TRUE
logits(work=dichotomy("not.work", c("parttime", "fulltime")),
       full=dichotomy("not.work", "fulltime"))

# OK
logits(one=dichotomy(c("A", "B"), c("C", "D")),
       two=dichotomy("A", "B"),
       three=dichotomy("C", "D"))

#' ## duplicate dichotomy
#+ error=TRUE
logits(one=dichotomy(c("A", "B"), c("C", "D")),
       two=dichotomy("A", "B"),
       three=dichotomy("A", "B"))

#' ## incomplete
#+ error=TRUE
logits(one=dichotomy(c("A", "B"), c("C", "D")),
       two=dichotomy("A", "B"))

# Arthritis treatment
logits(better = dichotomy("None", c("Some", "Marked")),
       v.better = dichotomy("Some", "Marked"))


m <- nestedLogit(partic ~ hincome + children, 
                 logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime")),
                 data=Womenlf)
m
summary(m)
Anova(m)

m1 <- nestedLogit(partic ~ hincome + children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  contrasts=list(children=contr.sum),
                  subset = 'region == "Ontario"',
                  data=Womenlf)
m1

m2 <- nestedLogit(partic ~ log(hincome)*children, 
                  logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                         full=dichotomy("parttime", "fulltime")),
                  data=Womenlf)
m2
summary(m2)
Anova(m2)

(new <- expand.grid(hincome=seq(1, 45, length=10), children=c("absent", "present")))

(p <- predict(m2, newdata=new))
rowSums(p)
p2 <- predict(m2)
all(abs(rowSums(p2) - 1) < .Machine$double.eps)

plotdata <- cbind(new, p)

par(mfrow=c(1, 2))
plot(c(1, 45), 0:1, type="n", xlab="Husband's Income", ylab="Probability",
     main="Children Absent")
with(plotdata, lines(spline(hincome[1:10], fulltime[1:10]), col="blue", lwd=2))
with(plotdata, lines(spline(hincome[1:10], parttime[1:10]), col="magenta", lwd=2))
with(plotdata, lines(spline(hincome[1:10], not.work[1:10]), col="darkcyan", lwd=2))
legend("topright", inset=.02, lwd=2, col=c("blue", "magenta", "darkcyan"),
       legend=c("Full Time", "Part Time", "Not Working"))

plot(c(1, 45), 0:1, type="n", xlab="Husband's Income", ylab="Probability",
     main="Children Present")
with(plotdata, lines(spline(hincome[11:20], fulltime[11:20]), col="blue", lwd=2))
with(plotdata, lines(spline(hincome[11:20], parttime[11:20]), col="magenta", lwd=2))
with(plotdata, lines(spline(hincome[11:20], not.work[11:20]), col="darkcyan", lwd=2))

continuationLogits(3)
continuationLogits(c("low", "medium", "high", "very.high"))
continuationLogits(c("gradeschool", "some highschool", "highschool grad", 
                     "some univ", "univ grad" ),
                   names=c("some.highschool.or.more", "highschool.grad.or.more",
                           "some.univ.or.more", "univ.grad"))


#' ## check fitted probabilities

p.m2.1 <- predict(m2$models[[1]], newdata=new, type="response")
p.m2.2 <- predict(m2$models[[2]], newdata=new, type="response")
pred.not.work <- 1 - p.m2.1
all.equal(pred.not.work, p[, "not.work"])
pred.parttime <- p.m2.1*(1 - p.m2.2 )
all.equal(pred.parttime, p[, "parttime"])
pred.fulltime <- p.m2.1*p.m2.2
all.equal(pred.fulltime, p[, "fulltime"])
