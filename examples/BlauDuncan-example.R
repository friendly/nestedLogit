#' ---
#' title: Blau-Duncan- Education of fathers and sons
#' ---

BlauDuncan <- read.table("https://www.john-fox.ca/AppliedRegression/BlauDuncan.txt", 
                         header=TRUE, stringsAsFactors=TRUE)


library(nestedLogit)

#' ## Recode education and education.father
BlauDuncan$educ <- factor(car::recode(BlauDuncan$education, 
                                " 0:3='gradeschool.or.less'; 4='some.highschool'; 5='highschool'; 
                                6='some.college'; 7:8='college.or.more' "),
                          levels=c('gradeschool.or.less', 'some.highschool', 
                                   'highschool', 'some.college', 'college.or.more'))

BlauDuncan$educ.father <- factor(car::recode(BlauDuncan$education.father, 
                                        " 0:3='gradeschool.or.less'; 4='some.highschool'; 5='highschool'; 
                                        6='some.college'; 7:8='college.or.more' "),
                                 levels=c('gradeschool.or.less', 'some.highschool', 
                                          'highschool', 'some.college', 'college.or.more'))

#' ## Fit the nested logit model
m.BD <- nestedLogit(educ ~ educ.father + ses.father + race,  
                    continuationLogits(c('gradeschool.or.less', 'some.highschool', 
                                         'highschool', 'some.college', 'college.or.more')),
                    data=BlauDuncan)

m.BD
summary(m.BD)
car::Anova(m.BD)

coef(m.BD)

new <- expand.grid(educ.father=c('gradeschool.or.less', 'some.highschool', 
                                 'highschool', 'some.college', 'college.or.more'), 
                   ses.father=mean(BlauDuncan$ses.father, na.rm=TRUE),
                   race=c("white", "negro", "other"))

pred.BD <- predict(m.BD, new)

plotdata <- cbind(new, pred.BD)

op <- par(mfrow=c(3, 1), mar=c(4, 4, 5, 1)+.1)
cols <- car::carPalette()
for ( rc in c("white", "negro", "other") ) {
  data <- subset(plotdata, race==rc)
  with(data, {
    plot( c(1, 5), c(0, 0.5), 
          type="n",
          axes=FALSE,
          xlab="Father's Education", 
          ylab='Fitted Probability',
          main = paste("Race:", rc),
          cex.lab = 1.1,
          cex.main = 1.25)
    axis(2)
    axis(1, at=1:5, labels=c('gradeschool.or.less', 'some.highschool', 
                             'highschool', 'some.college', 'college.or.more'))
    lines(1:5, gradeschool.or.less,  lwd=3, lty=1, col=cols[1], 
          pch=1, cex=1.5, type="b")
    lines(1:5, some.highschool,  lwd=3, lty=2, col=cols[2], 
          pch=2, cex=1.5, type="b")
    lines(1:5, highschool,  lwd=3, lty=3, col=cols[3], 
          pch=3, cex=1.5, type="b")
    lines(1:5, some.college,  lwd=3, lty=4, col=cols[4], 
          pch=4, cex=1.5, type="b")
    lines(1:5, college.or.more,  lwd=3, lty=5, col=cols[5], 
          pch=5, cex=1.5, type="b")
  })
  
  if (rc == "white") {
    legend(4, 0.7, lty=1:5, pch=1:5, lwd=3, pt.cex=1.5, col=cols, bty = "n",
           legend=c('gradeschool.or.less', 'some.highschool', 
                    'highschool', 'some.college', 'college.or.more'),
           title="Education",
           xpd=TRUE)
  }
}
par(op)

