# R^2 measures for nested logit models
#

See: https://statisticalhorizons.com/r2logistic/ for an overview

Goal: 

* A nice function that can calculate and display nicely one or more R^2 measures for a nested logit model
* Really useful if it could do this for each of the dichotomies, as well as calculate these for the combined model.
* For display select just a few of the measures by default: "McFadden" (or: "McFaddenAdj"), "CoxSnell", "Nagelkerke"
* Ideal would be something (from a print method) that gives:

      Pseudo R^2 and other measures for the nested logit model "wlf.nested"
      
      Model        McFadden    CoxSnell    Nagelkerke    ....        AIC      BIC
      
      work            0.xxx      0.xxx          0.xxx            xxxx.xx  xxxx.xx 
      full            0.xxx      0.xxx          0.xxx
      combined        0.xxx      0.xxx          0.xxx

* A bonus would be a generic function, `RSQ()` with where the main method is `RSQ.nestedLogit()`, but which also
  has methods for glm() logit models, MASS::polr(), multinom()

## What's available

* `DescTools::PseudoR2()` calculates a wide variety of R^2 measures for logistic-type models: `glm()`, `polr`,  `multinom()`, `vglm()` These include:
  * "McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "AldrichNelson", "VeallZimmermann", "Efron", "McKelveyZavoina", "Tjur"
  * McFadden, Cox & Snell, Nagelkerke most commonly used
  * The code for this seems easy to extend
  * There is no print method. Just returns a named list of the measures calculated.
  
  

* `performance::r2()`: calculated R^2 for a variety of models. Has separate functions:
  r2_bayes(), r2_coxsnell(), r2_kullback(), r2_loo(), r2_mcfadden(), r2_nagelkerke(), r2_nakagawa(), r2_tjur(), r2_xu(), r2_zeroinflated(), and r2_mlm().
  
* `rsq::rsq()`: extends traditional lm() R^2 to glm() and mixed models. Not worth considering
  

## Tests

```
library(nestedLogit)
library(DescTools)
library(performance)

data(Womenlf, package = "carData")
comparisons <- logits(
  work = dichotomy("not.work", c("parttime", "fulltime")),
  full = dichotomy("parttime", "fulltime")
)

wlf.nested <- nestedLogit(
  partic ~ hincome + children,
  dichotomies = comparisons,
  data = Womenlf
)

# try performance

performance::r2(wlf.nested)

  # R2 for Logistic Regression
  # work: 0.138
  #  full: 0.333
# what measure was computed???  This doesn't seem that useful

# try DescTools

DescTools::PseudoR2(wlf.nested, which = "all")

# Result:
# [1] NA

models(wlf.nested, "work") |> PseudoR2(which = "all")

#        McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson VeallZimmermann           Efron McKelveyZavoina 
#      0.10225540      0.08540861      0.12931308      0.17431337      0.12163033      0.21144837      0.13943768      0.16070445 
#            Tjur             AIC             BIC          logLik         logLik0              G2 
#      0.13775945    325.73253780    336.44899990   -159.86626890   -178.07544481     36.41835182 
# > ```
# Error: attempt to use zero-length variable name

# maybe that doesn't like pipes? OK!
PseudoR2(models(wlf.nested, "work"), which = "all")

PseudoR2(models(wlf.nested, "full"), which = "all")
 #   McFadden     McFaddenAdj        CoxSnell      Nagelkerke   AldrichNelson VeallZimmermann           Efron McKelveyZavoina 
 # 0.10225540      0.08540861      0.12931308      0.17431337      0.12163033      0.21144837      0.13943768      0.16070445 
 #       Tjur             AIC             BIC          logLik         logLik0              G2 
 # 0.13775945    325.73253780    336.44899990   -159.86626890   -178.07544481     36.41835182 
```

