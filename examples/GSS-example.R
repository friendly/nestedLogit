#' ---
#' title: GSS example-- parent-child education
#' ---

library(nestedLogit)

#' Check crosstab of degree by parentdeg
round(100*with(GSS, prop.table(table(degree, parentdeg), 2)))

m1 <- nestedLogit(degree ~ parentdeg*year, 
                 continuationLogits(c("l.t.highschool",  "highschool", 
                                      "college", "graduate")),
                 data=GSS)
car::Anova(m1)

m2 <- nestedLogit(degree ~ parentdeg + year, 
                  continuationLogits(c("l.t.highschool",  "highschool", 
                                       "college", "graduate")),
                 data=GSS)

summary(m2)

new <- expand.grid(parentdeg=c("l.t.highschool",  "highschool", 
                               "college", "graduate"),
                   year=1972:2016)
pred.GSS <- predict(m2, new)
plotdata <- cbind(new, pred.GSS)

cols <- car::carPalette()
degs <- c("l.t.highschool",  "highschool", 
               "college", "graduate")
deg <- c("< high school", "high school", "college", "graduate")
op <- par(mfrow=c(2, 2), mar=c(4,4,3,1)+.1)
for (i in 1:4) {
  data <- subset(plotdata, parentdeg == degs[i])
  with(data, {
    plot( c(1972, 2016), c(0, 1), 
          type="n",
          xlab="Year", 
          ylab='Fitted Probability',
          main = paste("Parents' Education:", deg[i]),
          cex.lab = 1.1)
    for (j in 1:4){
      lines(year, data[, degs[j]], lwd=3, lty=j, col=cols[j])
    }
  })
  if (i == 1) {
    legend(1972, 1, lty=1:5, lwd=3, col=cols, bty = "n",
           legend=deg,
           xpd=TRUE)
  }
}
par(op)

# check that predicted probabilities are correct

p.m2.1 <- predict(m2$models[[1]], newdata=new, type="response")
p.m2.2 <- predict(m2$models[[2]], newdata=new, type="response")
p.m2.3 <- predict(m2$models[[3]], newdata=new, type="response")

all.equal(1 - p.m2.1, pred.GSS[, "l.t.highschool"])
all.equal(p.m2.1*(1 - p.m2.2), pred.GSS[, "highschool"])
all.equal(p.m2.1*p.m2.2*(1 - p.m2.3), pred.GSS[, "college"])
all.equal(p.m2.1*p.m2.2*p.m2.3, pred.GSS[, "graduate"])

