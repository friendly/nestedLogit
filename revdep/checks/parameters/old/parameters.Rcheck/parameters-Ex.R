pkgname <- "parameters"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('parameters')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bootstrap_model")
### * bootstrap_model

flush(stderr()); flush(stdout())

### Name: bootstrap_model
### Title: Model bootstrapping
### Aliases: bootstrap_model bootstrap_model.default bootstrap_model.merMod

### ** Examples

## Not run: 
##D if (require("boot", quietly = TRUE)) {
##D   model <- lm(mpg ~ wt + factor(cyl), data = mtcars)
##D   b <- bootstrap_model(model)
##D   print(head(b))
##D 
##D   if (require("emmeans", quietly = TRUE)) {
##D     est <- emmeans(b, consec ~ cyl)
##D     print(model_parameters(est))
##D   }
##D }
## End(Not run)



cleanEx()
nameEx("bootstrap_parameters")
### * bootstrap_parameters

flush(stderr()); flush(stdout())

### Name: bootstrap_parameters
### Title: Parameters bootstrapping
### Aliases: bootstrap_parameters

### ** Examples

## Not run: 
##D if (require("boot", quietly = TRUE)) {
##D   set.seed(2)
##D   model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
##D   b <- bootstrap_parameters(model)
##D   print(b)
##D 
##D   if (require("emmeans")) {
##D     est <- emmeans(b, trt.vs.ctrl ~ Species)
##D     print(model_parameters(est))
##D   }
##D }
## End(Not run)



cleanEx()
nameEx("ci.default")
### * ci.default

flush(stderr()); flush(stdout())

### Name: ci.default
### Title: Confidence Intervals (CI)
### Aliases: ci.default ci.glmmTMB ci.merMod

### ** Examples




cleanEx()
nameEx("cluster_analysis")
### * cluster_analysis

flush(stderr()); flush(stdout())

### Name: cluster_analysis
### Title: Cluster Analysis
### Aliases: cluster_analysis

### ** Examples

set.seed(33)
# K-Means ====================================================
rez <- cluster_analysis(iris[1:4], n = 3, method = "kmeans")
rez # Show results
predict(rez) # Get clusters
summary(rez) # Extract the centers values (can use 'plot()' on that)
if (requireNamespace("MASS", quietly = TRUE)) {
  cluster_discrimination(rez) # Perform LDA
}

# Hierarchical k-means (more robust k-means)
if (require("factoextra", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans")
  rez # Show results
  predict(rez) # Get clusters
}

# Hierarchical Clustering (hclust) ===========================
rez <- cluster_analysis(iris[1:4], n = 3, method = "hclust")
rez # Show results
predict(rez) # Get clusters

# K-Medoids (pam) ============================================
if (require("cluster", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], n = 3, method = "pam")
  rez # Show results
  predict(rez) # Get clusters
}

# PAM with automated number of clusters
if (require("fpc", quietly = TRUE)) {
  rez <- cluster_analysis(iris[1:4], method = "pamk")
  rez # Show results
  predict(rez) # Get clusters
}

# DBSCAN ====================================================
if (require("dbscan", quietly = TRUE)) {
  # Note that you can assimilate more outliers (cluster 0) to neighbouring
  # clusters by setting borderPoints = TRUE.
  rez <- cluster_analysis(iris[1:4], method = "dbscan", dbscan_eps = 1.45)
  rez # Show results
  predict(rez) # Get clusters
}

# Mixture ====================================================
if (require("mclust", quietly = TRUE)) {
  library(mclust) # Needs the package to be loaded
  rez <- cluster_analysis(iris[1:4], method = "mixture")
  rez # Show results
  predict(rez) # Get clusters
}



cleanEx()
nameEx("cluster_centers")
### * cluster_centers

flush(stderr()); flush(stdout())

### Name: cluster_centers
### Title: Find the cluster centers in your data
### Aliases: cluster_centers

### ** Examples

k <- kmeans(iris[1:4], 3)
cluster_centers(iris[1:4], clusters = k$cluster)
cluster_centers(iris[1:4], clusters = k$cluster, fun = median)



cleanEx()
nameEx("cluster_discrimination")
### * cluster_discrimination

flush(stderr()); flush(stdout())

### Name: cluster_discrimination
### Title: Compute a linear discriminant analysis on classified cluster
###   groups
### Aliases: cluster_discrimination

### ** Examples

if (requireNamespace("MASS", quietly = TRUE)) {
  # Retrieve group classification from hierarchical cluster analysis
  clustering <- cluster_analysis(iris[, 1:4], n = 3)

  # Goodness of group classification
  cluster_discrimination(clustering)
}



cleanEx()
nameEx("cluster_meta")
### * cluster_meta

flush(stderr()); flush(stdout())

### Name: cluster_meta
### Title: Metaclustering
### Aliases: cluster_meta

### ** Examples

## Not run: 
##D data <- iris[1:4]
##D 
##D rez1 <- cluster_analysis(data, n = 2, method = "kmeans")
##D rez2 <- cluster_analysis(data, n = 3, method = "kmeans")
##D rez3 <- cluster_analysis(data, n = 6, method = "kmeans")
##D 
##D list_of_clusters <- list(rez1, rez2, rez3)
##D 
##D m <- cluster_meta(list_of_clusters)
##D 
##D # Visualize matrix without reordering
##D heatmap(m, Rowv = NA, Colv = NA, scale = "none") # Without reordering
##D # Reordered heatmap
##D heatmap(m, scale = "none")
##D 
##D # Extract 3 clusters
##D predict(m, n = 3)
##D 
##D # Convert to dissimilarity
##D d <- as.dist(abs(m - 1))
##D model <- hclust(d)
##D plot(model, hang = -1)
## End(Not run)



cleanEx()
nameEx("cluster_performance")
### * cluster_performance

flush(stderr()); flush(stdout())

### Name: cluster_performance
### Title: Performance of clustering models
### Aliases: cluster_performance cluster_performance.kmeans
###   cluster_performance.hclust cluster_performance.dbscan
###   cluster_performance.parameters_clusters

### ** Examples

# kmeans
model <- kmeans(iris[1:4], 3)
cluster_performance(model)
# hclust
data <- iris[1:4]
model <- hclust(dist(data))
clusters <- cutree(model, 3)

rez <- cluster_performance(model, data, clusters)
rez
# DBSCAN
if (require("dbscan", quietly = TRUE)) {
  model <- dbscan::dbscan(iris[1:4], eps = 1.45, minPts = 10)

  rez <- cluster_performance(model, iris[1:4])
  rez
}
# Retrieve performance from parameters
params <- model_parameters(kmeans(iris[1:4], 3))
cluster_performance(params)



cleanEx()
nameEx("compare_parameters")
### * compare_parameters

flush(stderr()); flush(stdout())

### Name: compare_parameters
### Title: Compare model parameters of multiple models
### Aliases: compare_parameters compare_models

### ** Examples

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
compare_parameters(lm1, lm2)

# custom style
compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")

## Not run: 
##D # custom style, in HTML
##D result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
##D print_html(result)
## End(Not run)

data(mtcars)
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
compare_parameters(m1, m2)
## Not run: 
##D # exponentiate coefficients, but not for lm
##D compare_parameters(m1, m2, exponentiate = "nongaussian")
##D 
##D # change column names
##D compare_parameters("linear model" = m1, "logistic reg." = m2)
##D compare_parameters(m1, m2, column_names = c("linear model", "logistic reg."))
##D 
##D # or as list
##D compare_parameters(list(m1, m2))
##D compare_parameters(list("linear model" = m1, "logistic reg." = m2))
## End(Not run)



cleanEx()
nameEx("convert_efa_to_cfa")
### * convert_efa_to_cfa

flush(stderr()); flush(stdout())

### Name: convert_efa_to_cfa
### Title: Conversion between EFA results and CFA structure
### Aliases: convert_efa_to_cfa convert_efa_to_cfa.fa efa_to_cfa

### ** Examples




cleanEx()
nameEx("degrees_of_freedom")
### * degrees_of_freedom

flush(stderr()); flush(stdout())

### Name: degrees_of_freedom
### Title: Degrees of Freedom (DoF)
### Aliases: degrees_of_freedom degrees_of_freedom.default dof

### ** Examples

model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
dof(model)

model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
dof(model)
## Not run: 
##D if (require("lme4", quietly = TRUE)) {
##D   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
##D   dof(model)
##D }
##D 
##D if (require("rstanarm", quietly = TRUE)) {
##D   model <- stan_glm(
##D     Sepal.Length ~ Petal.Length * Species,
##D     data = iris,
##D     chains = 2,
##D     refresh = 0
##D   )
##D   dof(model)
##D }
## End(Not run)



cleanEx()
nameEx("display.parameters_model")
### * display.parameters_model

flush(stderr()); flush(stdout())

### Name: display.parameters_model
### Title: Print tables in different output formats
### Aliases: display.parameters_model display.parameters_sem
###   display.parameters_efa_summary display.parameters_efa
###   display.equivalence_test_lm format.parameters_model
###   print_html.parameters_model print_md.parameters_model

### ** Examples

model <- lm(mpg ~ wt + cyl, data = mtcars)
mp <- model_parameters(model)
display(mp)

## Not run: 
##D data(iris)
##D lm1 <- lm(Sepal.Length ~ Species, data = iris)
##D lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
##D lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
##D out <- compare_parameters(lm1, lm2, lm3)
##D 
##D print_html(
##D   out,
##D   select = "{coef}{stars}|({ci})",
##D   column_labels = c("Estimate", "95% CI")
##D )
##D 
##D # line break, unicode minus-sign
##D print_html(
##D   out,
##D   select = "{estimate}{stars}<br>({ci_low} \u2212 {ci_high})",
##D   column_labels = c("Est. (95% CI)")
##D )
## End(Not run)



cleanEx()
nameEx("dominance_analysis")
### * dominance_analysis

flush(stderr()); flush(stdout())

### Name: dominance_analysis
### Title: Dominance Analysis
### Aliases: dominance_analysis

### ** Examples

if (getRversion() >= "3.5.0" && require("domir") &&
  require("performance")) {
  data(mtcars)

  # Dominance Analysis with Logit Regression
  model <- glm(vs ~ cyl + carb + mpg, data = mtcars, family = binomial())

  performance::r2(model)
  dominance_analysis(model)

  # Dominance Analysis with Weighted Logit Regression
  model_wt <- glm(vs ~ cyl + carb + mpg,
    data = mtcars,
    weights = wt, family = quasibinomial()
  )

  dominance_analysis(model_wt, quote_args = "weights")
}



cleanEx()
nameEx("equivalence_test.lm")
### * equivalence_test.lm

flush(stderr()); flush(stdout())

### Name: equivalence_test.lm
### Title: Equivalence test
### Aliases: equivalence_test.lm equivalence_test.merMod
###   equivalence_test.ggeffects

### ** Examples

data(qol_cancer)
model <- lm(QoL ~ time + age + education, data = qol_cancer)

# default rule
equivalence_test(model)

# conditional equivalence test
equivalence_test(model, rule = "cet")

# plot method
if (require("see", quietly = TRUE)) {
  result <- equivalence_test(model)
  plot(result)
}



cleanEx()
nameEx("format_df_adjust")
### * format_df_adjust

flush(stderr()); flush(stdout())

### Name: format_df_adjust
### Title: Format the name of the degrees-of-freedom adjustment methods
### Aliases: format_df_adjust

### ** Examples

library(parameters)

format_df_adjust("kenward")
format_df_adjust("kenward", approx_string = "", dof_string = " DoF")



cleanEx()
nameEx("format_order")
### * format_order

flush(stderr()); flush(stdout())

### Name: format_order
### Title: Order (first, second, ...) formatting
### Aliases: format_order

### ** Examples

format_order(2)
format_order(8)
format_order(25, textual = FALSE)



cleanEx()
nameEx("format_p_adjust")
### * format_p_adjust

flush(stderr()); flush(stdout())

### Name: format_p_adjust
### Title: Format the name of the p-value adjustment methods
### Aliases: format_p_adjust

### ** Examples

library(parameters)

format_p_adjust("holm")
format_p_adjust("bonferroni")



cleanEx()
nameEx("format_parameters")
### * format_parameters

flush(stderr()); flush(stdout())

### Name: format_parameters
### Title: Parameter names formatting
### Aliases: format_parameters format_parameters.default

### ** Examples

model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
format_parameters(model)

model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
format_parameters(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
format_parameters(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
format_parameters(model)



cleanEx()
nameEx("get_scores")
### * get_scores

flush(stderr()); flush(stdout())

### Name: get_scores
### Title: Get Scores from Principal Component Analysis (PCA)
### Aliases: get_scores

### ** Examples

if (require("psych")) {
  pca <- principal_components(mtcars[, 1:7], n = 2, rotation = "varimax")

  # PCA extracted two components
  pca

  # assignment of items to each component
  closest_component(pca)

  # now we want to have sum scores for each component
  get_scores(pca)

  # compare to manually computed sum score for 2nd component, which
  # consists of items "hp" and "qsec"
  (mtcars$hp + mtcars$qsec) / 2
}



cleanEx()
nameEx("model_parameters.BFBayesFactor")
### * model_parameters.BFBayesFactor

flush(stderr()); flush(stdout())

### Name: model_parameters.BFBayesFactor
### Title: Parameters from BayesFactor objects
### Aliases: model_parameters.BFBayesFactor

### ** Examples




cleanEx()
nameEx("model_parameters.aov")
### * model_parameters.aov

flush(stderr()); flush(stdout())

### Name: model_parameters.aov
### Title: Parameters from ANOVAs
### Aliases: model_parameters.aov model_parameters.anova
###   model_parameters.aovlist model_parameters.afex_aov
###   model_parameters.anova.rms model_parameters.Anova.mlm
###   model_parameters.maov

### ** Examples

## Don't show: 
if (requireNamespace("effectsize", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
df <- iris
df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

model <- aov(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model)

model_parameters(model, effectsize_type = c("omega", "eta"), ci = 0.9)

model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
model_parameters(model)
model_parameters(
  model,
  effectsize_type = c("omega", "eta", "epsilon"),
  alternative = "greater"
)

model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
model_parameters(model)
## Don't show: 
}) # examplesIf
## End(Don't show)
## Don't show: 
if (requireNamespace("lme4", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Not run: 
##D mm <- lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species), data = df)
##D model <- anova(mm)
##D 
##D # simple parameters table
##D model_parameters(model)
##D 
##D # parameters table including effect sizes
##D model_parameters(
##D   model,
##D   effectsize_type = "eta",
##D   ci = 0.9,
##D   df_error = dof_satterthwaite(mm)[2:3]
##D )
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("model_parameters.averaging")
### * model_parameters.averaging

flush(stderr()); flush(stdout())

### Name: model_parameters.PMCMR
### Title: Parameters from special models
### Aliases: model_parameters.PMCMR model_parameters.glimML
###   model_parameters.averaging model_parameters.mle2
###   model_parameters.betareg model_parameters.bfsl
###   model_parameters.deltaMethod model_parameters.emmGrid
###   model_parameters.emm_list model_parameters.epi.2by2
###   model_parameters.fitdistr model_parameters.ggeffects
###   model_parameters.SemiParBIV model_parameters.glmm
###   model_parameters.glmx model_parameters.ivFixed
###   model_parameters.ivprobit model_parameters.lmodel2
###   model_parameters.logistf model_parameters.lqmm
###   model_parameters.marginaleffects model_parameters.comparisons
###   model_parameters.marginalmeans model_parameters.hypotheses
###   model_parameters.slopes model_parameters.predictions
###   model_parameters.margins model_parameters.maxLik
###   model_parameters.maxim model_parameters.mediate
###   model_parameters.metaplus model_parameters.meta_random
###   model_parameters.meta_fixed model_parameters.meta_bma
###   model_parameters.logitor model_parameters.poissonirr
###   model_parameters.negbinirr model_parameters.poissonmfx
###   model_parameters.logitmfx model_parameters.probitmfx
###   model_parameters.negbinmfx model_parameters.betaor
###   model_parameters.betamfx model_parameters.mjoint
###   model_parameters.model_fit model_parameters.glht
###   model_parameters.mvord model_parameters.pgmm model_parameters.rqss
###   model_parameters.rqs model_parameters.selection model_parameters.mle
###   model_parameters.systemfit model_parameters.varest
###   model_parameters.t1way model_parameters.med1way
###   model_parameters.dep.effect model_parameters.yuen

### ** Examples

library(parameters)
if (require("brglm2", quietly = TRUE)) {
  data("stemcell")
  model <- bracl(
    research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell,
    type = "ML"
  )
  model_parameters(model)
}
if (require("WRS2") && packageVersion("WRS2") >= "1.1.3") {
  model <- t1way(libido ~ dose, data = viagra)
  model_parameters(model)
}



cleanEx()
nameEx("model_parameters.befa")
### * model_parameters.befa

flush(stderr()); flush(stdout())

### Name: model_parameters.befa
### Title: Parameters from Bayesian Exploratory Factor Analysis
### Aliases: model_parameters.befa

### ** Examples

library(parameters)



cleanEx()
nameEx("model_parameters.cgam")
### * model_parameters.cgam

flush(stderr()); flush(stdout())

### Name: model_parameters.cgam
### Title: Parameters from Generalized Additive (Mixed) Models
### Aliases: model_parameters.cgam model_parameters.gam
###   model_parameters.gamlss model_parameters.gamm model_parameters.Gam
###   model_parameters.scam model_parameters.vgam

### ** Examples

library(parameters)
if (require("mgcv")) {
  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
  model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
  model_parameters(model)
}



cleanEx()
nameEx("model_parameters.default")
### * model_parameters.default

flush(stderr()); flush(stdout())

### Name: model_parameters.default
### Title: Parameters from (General) Linear Models
### Aliases: model_parameters.default model_parameters.glm
###   model_parameters.censReg model_parameters.ridgelm
###   model_parameters.polr model_parameters.negbin

### ** Examples

library(parameters)
model <- lm(mpg ~ wt + cyl, data = mtcars)

model_parameters(model)

# bootstrapped parameters
if (require("boot", quietly = TRUE)) {
  model_parameters(model, bootstrap = TRUE)
}

# standardized parameters
model_parameters(model, standardize = "refit")

# robust, heteroskedasticity-consistent standard errors
if (require("sandwich") && require("clubSandwich")) {
  model_parameters(model, vcov = "HC3")

  model_parameters(model,
    vcov = "vcovCL",
    vcov_args = list(cluster = mtcars$cyl)
  )
}

# different p-value style in output
model_parameters(model, p_digits = 5)
model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")



cleanEx()
nameEx("model_parameters.htest")
### * model_parameters.htest

flush(stderr()); flush(stdout())

### Name: model_parameters.htest
### Title: Parameters from hypothesis tests
### Aliases: model_parameters.htest model_parameters.pairwise.htest
###   model_parameters.coeftest

### ** Examples


model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
model_parameters(model)

model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
model_parameters(model, effectsize_type = "hedges_g")

model <- t.test(mtcars$mpg ~ mtcars$vs)
model_parameters(model, effectsize_type = "hedges_g")

model <- t.test(iris$Sepal.Width, mu = 1)
model_parameters(model, effectsize_type = "cohens_d")

data(airquality)
airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
model <- pairwise.t.test(airquality$Ozone, airquality$Month)
model_parameters(model)

smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)
model <- suppressWarnings(pairwise.prop.test(smokers, patients))
model_parameters(model)

model <- suppressWarnings(chisq.test(table(mtcars$am, mtcars$cyl)))
model_parameters(model, effectsize_type = "cramers_v")




cleanEx()
nameEx("model_parameters.kmeans")
### * model_parameters.kmeans

flush(stderr()); flush(stdout())

### Name: model_parameters.dbscan
### Title: Parameters from Cluster Models (k-means, ...)
### Aliases: model_parameters.dbscan model_parameters.hclust
###   model_parameters.pvclust model_parameters.kmeans
###   model_parameters.hkmeans model_parameters.Mclust model_parameters.pam

### ** Examples

#
# Hierarchical clustering (hclust) ---------------------------
data <- iris[1:4]
model <- hclust(dist(data))
clusters <- cutree(model, 3)

rez <- model_parameters(model, data, clusters)
rez

# Get clusters
predict(rez)

# Clusters centers in long form
attributes(rez)$means

# Between and Total Sum of Squares
attributes(rez)$Total_Sum_Squares
attributes(rez)$Between_Sum_Squares
## Not run: 
##D #
##D # K-means -------------------------------
##D model <- kmeans(iris[1:4], centers = 3)
##D rez <- model_parameters(model)
##D rez
##D 
##D # Get clusters
##D predict(rez)
##D 
##D # Clusters centers in long form
##D attributes(rez)$means
##D 
##D # Between and Total Sum of Squares
##D attributes(rez)$Sum_Squares_Total
##D attributes(rez)$Sum_Squares_Between
## End(Not run)
## Not run: 
##D #
##D # Hierarchical K-means (factoextra::hkclust) ----------------------
##D if (require("factoextra", quietly = TRUE)) {
##D   data <- iris[1:4]
##D   model <- factoextra::hkmeans(data, k = 3)
##D 
##D   rez <- model_parameters(model)
##D   rez
##D 
##D   # Get clusters
##D   predict(rez)
##D 
##D   # Clusters centers in long form
##D   attributes(rez)$means
##D 
##D   # Between and Total Sum of Squares
##D   attributes(rez)$Sum_Squares_Total
##D   attributes(rez)$Sum_Squares_Between
##D }
## End(Not run)
if (require("mclust", quietly = TRUE)) {
  model <- mclust::Mclust(iris[1:4], verbose = FALSE)
  model_parameters(model)
}
## Not run: 
##D #
##D # K-Medoids (PAM and HPAM) ==============
##D if (require("cluster", quietly = TRUE)) {
##D   model <- cluster::pam(iris[1:4], k = 3)
##D   model_parameters(model)
##D }
##D if (require("fpc", quietly = TRUE)) {
##D   model <- fpc::pamk(iris[1:4], criterion = "ch")
##D   model_parameters(model)
##D }
## End(Not run)



cleanEx()
nameEx("model_parameters.merMod")
### * model_parameters.merMod

flush(stderr()); flush(stdout())

### Name: model_parameters.cpglmm
### Title: Parameters from Mixed Models
### Aliases: model_parameters.cpglmm model_parameters.glmmTMB
###   model_parameters.merMod model_parameters.merModList
###   model_parameters.mixed model_parameters.MixMod model_parameters.mixor
###   model_parameters.lme model_parameters.clmm2 model_parameters.clmm
###   model_parameters.rlmerMod model_parameters.HLfit

### ** Examples

library(parameters)
if (require("lme4")) {
  data(mtcars)
  model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
  model_parameters(model)
}



cleanEx()
nameEx("model_parameters.mira")
### * model_parameters.mira

flush(stderr()); flush(stdout())

### Name: model_parameters.mipo
### Title: Parameters from multiply imputed repeated analyses
### Aliases: model_parameters.mipo model_parameters.mira

### ** Examples

library(parameters)
if (require("mice", quietly = TRUE)) {
  data(nhanes2)
  imp <- mice(nhanes2)
  fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
  model_parameters(fit)
}
## Not run: 
##D # model_parameters() also works for models that have no "tidy"-method in mice
##D if (require("mice", quietly = TRUE) && require("gee", quietly = TRUE)) {
##D   data(warpbreaks)
##D   set.seed(1234)
##D   warpbreaks$tension[sample(1:nrow(warpbreaks), size = 10)] <- NA
##D   imp <- mice(warpbreaks)
##D   fit <- with(data = imp, expr = gee(breaks ~ tension, id = wool))
##D 
##D   # does not work:
##D   # summary(pool(fit))
##D 
##D   model_parameters(fit)
##D }
## End(Not run)



# and it works with pooled results
if (require("mice")) {
  data("nhanes2")
  imp <- mice(nhanes2)
  fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
  pooled <- pool(fit)

  model_parameters(pooled)
}



cleanEx()
nameEx("model_parameters.mlm")
### * model_parameters.mlm

flush(stderr()); flush(stdout())

### Name: model_parameters.DirichletRegModel
### Title: Parameters from multinomial or cumulative link models
### Aliases: model_parameters.DirichletRegModel model_parameters.bifeAPEs
###   model_parameters.bracl model_parameters.mlm model_parameters.clm2

### ** Examples

library(parameters)
if (require("brglm2", quietly = TRUE)) {
  data("stemcell")
  model <- bracl(
    research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell,
    type = "ML"
  )
  model_parameters(model)
}



cleanEx()
nameEx("model_parameters.principal")
### * model_parameters.principal

flush(stderr()); flush(stdout())

### Name: model_parameters.PCA
### Title: Parameters from PCA, FA, CFA, SEM
### Aliases: model_parameters.PCA model_parameters.FAMD
###   model_parameters.lavaan model_parameters.principal
###   model_parameters.omega model_parameters.sem

### ** Examples


# lavaan

library(parameters)

# lavaan -------------------------------------
if (require("lavaan", quietly = TRUE)) {
  # Confirmatory Factor Analysis (CFA) ---------

  structure <- " visual  =~ x1 + x2 + x3
                 textual =~ x4 + x5 + x6
                 speed   =~ x7 + x8 + x9 "
  model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
  model_parameters(model)
  model_parameters(model, standardize = TRUE)

  # filter parameters
  model_parameters(
    model,
    parameters = list(
      To = "^(?!visual)",
      From = "^(?!(x7|x8))"
    )
  )

  # Structural Equation Model (SEM) ------------

  structure <- "
    # latent variable definitions
      ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + a*y2 + b*y3 + c*y4
      dem65 =~ y5 + a*y6 + b*y7 + c*y8
    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60
    # residual correlations
      y1 ~~ y5
      y2 ~~ y4 + y6
      y3 ~~ y7
      y4 ~~ y8
      y6 ~~ y8
  "
  model <- lavaan::sem(structure, data = PoliticalDemocracy)
  model_parameters(model)
  model_parameters(model, standardize = TRUE)
}




cleanEx()
nameEx("model_parameters.rma")
### * model_parameters.rma

flush(stderr()); flush(stdout())

### Name: model_parameters.rma
### Title: Parameters from Meta-Analysis
### Aliases: model_parameters.rma

### ** Examples

library(parameters)
mydat <<- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  stderr = c(0.317, 0.317, 0.13, 0.36)
)
if (require("metafor", quietly = TRUE)) {
  model <- rma(yi = effectsize, sei = stderr, method = "REML", data = mydat)
  model_parameters(model)
}
## Not run: 
##D # with subgroups
##D if (require("metafor", quietly = TRUE)) {
##D   data(dat.bcg)
##D   dat <- escalc(
##D     measure = "RR",
##D     ai = tpos,
##D     bi = tneg,
##D     ci = cpos,
##D     di = cneg,
##D     data = dat.bcg
##D   )
##D   dat$alloc <- ifelse(dat$alloc == "random", "random", "other")
##D   d <<- dat
##D   model <- rma(yi, vi, mods = ~alloc, data = d, digits = 3, slab = author)
##D   model_parameters(model)
##D }
##D 
##D if (require("metaBMA", quietly = TRUE)) {
##D   data(towels)
##D   m <- suppressWarnings(meta_random(logOR, SE, study, data = towels))
##D   model_parameters(m)
##D }
## End(Not run)




cleanEx()
nameEx("model_parameters.stanreg")
### * model_parameters.stanreg

flush(stderr()); flush(stdout())

### Name: model_parameters.MCMCglmm
### Title: Parameters from Bayesian Models
### Aliases: model_parameters.MCMCglmm model_parameters.bamlss
###   model_parameters.data.frame model_parameters.bayesQR
###   model_parameters.brmsfit model_parameters.mcmc.list
###   model_parameters.bcplm model_parameters.blrm model_parameters.draws
###   model_parameters.stanfit model_parameters.stanreg

### ** Examples

## Not run: 
##D library(parameters)
##D if (require("rstanarm")) {
##D   model <- suppressWarnings(stan_glm(
##D     Sepal.Length ~ Petal.Length * Species,
##D     data = iris, iter = 500, refresh = 0
##D   ))
##D   model_parameters(model)
##D }
## End(Not run)



cleanEx()
nameEx("model_parameters.zcpglm")
### * model_parameters.zcpglm

flush(stderr()); flush(stdout())

### Name: model_parameters.zcpglm
### Title: Parameters from Zero-Inflated Models
### Aliases: model_parameters.zcpglm model_parameters.mhurdle
###   model_parameters.zeroinfl model_parameters.hurdle
###   model_parameters.zerocount

### ** Examples

library(parameters)
if (require("pscl")) {
  data("bioChemists")
  model <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
  model_parameters(model)
}



cleanEx()
nameEx("n_clusters")
### * n_clusters

flush(stderr()); flush(stdout())

### Name: n_clusters
### Title: Find number of clusters in your data
### Aliases: n_clusters n_clusters_elbow n_clusters_gap
###   n_clusters_silhouette n_clusters_dbscan n_clusters_hclust

### ** Examples

## Not run: 
##D library(parameters)
##D 
##D # The main 'n_clusters' function ===============================
##D if (require("mclust", quietly = TRUE) && require("NbClust", quietly = TRUE) &&
##D   require("cluster", quietly = TRUE) && require("see", quietly = TRUE)) {
##D   n <- n_clusters(iris[, 1:4], package = c("NbClust", "mclust")) # package can be "all"
##D   n
##D   summary(n)
##D   as.data.frame(n) # Duration is the time elapsed for each method in seconds
##D   plot(n)
##D 
##D   # The following runs all the method but it significantly slower
##D   # n_clusters(iris[1:4], standardize = FALSE, package = "all", fast = FALSE)
##D }
## End(Not run)
## Don't show: 
if (require("see", quietly = TRUE) && require("factoextra", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("n_factors")
### * n_factors

flush(stderr()); flush(stdout())

### Name: n_factors
### Title: Number of components/factors to retain in PCA/FA
### Aliases: n_factors n_components

### ** Examples

## Don't show: 
if (require("PCDimension", quietly = TRUE) && require("nFactors", quietly = TRUE) && require("EGAnet", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(parameters)
n_factors(mtcars, type = "PCA")

result <- n_factors(mtcars[1:5], type = "FA")
as.data.frame(result)
summary(result)
## Not run: 
##D # Setting package = 'all' will increase the number of methods (but is slow)
##D n_factors(mtcars, type = "PCA", package = "all")
##D n_factors(mtcars, type = "FA", algorithm = "mle", package = "all")
## End(Not run)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("p_calibrate")
### * p_calibrate

flush(stderr()); flush(stdout())

### Name: p_calibrate
### Title: Calculate calibrated p-values.
### Aliases: p_calibrate p_calibrate.default

### ** Examples

model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
p_calibrate(model, verbose = FALSE)



cleanEx()
nameEx("p_function")
### * p_function

flush(stderr()); flush(stdout())

### Name: p_function
### Title: p-value or consonance function
### Aliases: p_function consonance_function confidence_curve

### ** Examples

model <- lm(Sepal.Length ~ Species, data = iris)
p_function(model)

if (requireNamespace("see") && packageVersion("see") > "0.7.3") {
  model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
  result <- p_function(model)

  # single panels
  plot(result, n_columns = 2)

  # integrated plot, the default
  plot(result)
}



cleanEx()
nameEx("p_value.BFBayesFactor")
### * p_value.BFBayesFactor

flush(stderr()); flush(stdout())

### Name: p_value.BFBayesFactor
### Title: p-values for Bayesian Models
### Aliases: p_value.BFBayesFactor

### ** Examples

data(iris)
model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
p_value(model)



cleanEx()
nameEx("p_value")
### * p_value

flush(stderr()); flush(stdout())

### Name: p_value
### Title: p-values
### Aliases: p_value p_value.default p_value.emmGrid

### ** Examples

data(iris)
model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
p_value(model)



cleanEx()
nameEx("p_value.poissonmfx")
### * p_value.poissonmfx

flush(stderr()); flush(stdout())

### Name: p_value.poissonmfx
### Title: p-values for Marginal Effects Models
### Aliases: p_value.poissonmfx p_value.betaor p_value.betamfx

### ** Examples

if (require("mfx", quietly = TRUE)) {
  set.seed(12345)
  n <- 1000
  x <- rnorm(n)
  y <- rnegbin(n, mu = exp(1 + 0.5 * x), theta = 0.5)
  d <- data.frame(y, x)
  model <- poissonmfx(y ~ x, data = d)

  p_value(model)
  p_value(model, component = "marginal")
}



cleanEx()
nameEx("p_value.zcpglm")
### * p_value.zcpglm

flush(stderr()); flush(stdout())

### Name: p_value.zcpglm
### Title: p-values for Models with Zero-Inflation
### Aliases: p_value.zcpglm p_value.zeroinfl

### ** Examples

if (require("pscl", quietly = TRUE)) {
  data("bioChemists")
  model <- zeroinfl(art ~ fem + mar + kid5 | kid5 + phd, data = bioChemists)
  p_value(model)
  p_value(model, component = "zi")
}



cleanEx()
nameEx("p_value_betwithin")
### * p_value_betwithin

flush(stderr()); flush(stdout())

### Name: ci_betwithin
### Title: Between-within approximation for SEs, CIs and p-values
### Aliases: ci_betwithin dof_betwithin p_value_betwithin

### ** Examples




cleanEx()
nameEx("p_value_kenward")
### * p_value_kenward

flush(stderr()); flush(stdout())

### Name: ci_kenward
### Title: Kenward-Roger approximation for SEs, CIs and p-values
### Aliases: ci_kenward dof_kenward p_value_kenward se_kenward

### ** Examples




cleanEx()
nameEx("p_value_ml1")
### * p_value_ml1

flush(stderr()); flush(stdout())

### Name: ci_ml1
### Title: "m-l-1" approximation for SEs, CIs and p-values
### Aliases: ci_ml1 dof_ml1 p_value_ml1

### ** Examples




cleanEx()
nameEx("p_value_satterthwaite")
### * p_value_satterthwaite

flush(stderr()); flush(stdout())

### Name: ci_satterthwaite
### Title: Satterthwaite approximation for SEs, CIs and p-values
### Aliases: ci_satterthwaite dof_satterthwaite p_value_satterthwaite
###   se_satterthwaite

### ** Examples




cleanEx()
nameEx("parameters_type")
### * parameters_type

flush(stderr()); flush(stdout())

### Name: parameters_type
### Title: Type of model parameters
### Aliases: parameters_type

### ** Examples

library(parameters)

model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
parameters_type(model)

# Interactions
model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Sepal.Width * Species * Petal.Length, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
parameters_type(model)

model <- lm(Sepal.Length ~ Species / Sepal.Width, data = iris)
parameters_type(model)


# Complex interactions
data <- iris
data$fac2 <- ifelse(data$Sepal.Width > mean(data$Sepal.Width), "A", "B")
model <- lm(Sepal.Length ~ Species / fac2 / Petal.Length, data = data)
parameters_type(model)

model <- lm(Sepal.Length ~ Species / fac2 * Petal.Length, data = data)
parameters_type(model)



cleanEx()
nameEx("pool_parameters")
### * pool_parameters

flush(stderr()); flush(stdout())

### Name: pool_parameters
### Title: Pool Model Parameters
### Aliases: pool_parameters

### ** Examples

# example for multiple imputed datasets
if (require("mice")) {
  data("nhanes2")
  imp <- mice(nhanes2, printFlag = FALSE)
  models <- lapply(1:5, function(i) {
    lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
  })
  pool_parameters(models)

  # should be identical to:
  m <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
  summary(pool(m))
}



cleanEx()
nameEx("principal_components")
### * principal_components

flush(stderr()); flush(stdout())

### Name: factor_analysis
### Title: Principal Component Analysis (PCA) and Factor Analysis (FA)
### Aliases: factor_analysis principal_components rotated_data
###   predict.parameters_efa print.parameters_efa sort.parameters_efa
###   closest_component

### ** Examples

## Don't show: 
if (require("nFactors", quietly = TRUE) && require("sparsepca", quietly = TRUE) && require("psych", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
library(parameters)


# Factor Analysis (FA) ------------------------

factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2)
factor_analysis(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
factor_analysis(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)

efa <- factor_analysis(mtcars[, 1:5], n = 2)
summary(efa)
predict(efa, verbose = FALSE)

## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("print.parameters_model")
### * print.parameters_model

flush(stderr()); flush(stdout())

### Name: print.parameters_model
### Title: Print model parameters
### Aliases: print.parameters_model summary.parameters_model

### ** Examples



# custom column layouts ------

data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)

# custom style
result <- compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
print(result)

## Not run: 
##D # custom style, in HTML
##D result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
##D print_html(result)
## End(Not run)



cleanEx()
nameEx("random_parameters")
### * random_parameters

flush(stderr()); flush(stdout())

### Name: random_parameters
### Title: Summary information from random effects
### Aliases: random_parameters

### ** Examples

if (require("lme4")) {
  data(sleepstudy)
  model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  random_parameters(model)
}



cleanEx()
nameEx("reduce_parameters")
### * reduce_parameters

flush(stderr()); flush(stdout())

### Name: reduce_parameters
### Title: Dimensionality reduction (DR) / Features Reduction
### Aliases: reduce_parameters reduce_data

### ** Examples

data(iris)
model <- lm(Sepal.Width ~ Species * Sepal.Length + Petal.Width, data = iris)
model
reduce_parameters(model)

out <- reduce_data(iris, method = "PCA", n = "max")
head(out)



cleanEx()
nameEx("reshape_loadings")
### * reshape_loadings

flush(stderr()); flush(stdout())

### Name: reshape_loadings
### Title: Reshape loadings between wide/long formats
### Aliases: reshape_loadings reshape_loadings.parameters_efa
###   reshape_loadings.data.frame

### ** Examples

if (require("psych")) {
  pca <- model_parameters(psych::fa(attitude, nfactors = 3))
  loadings <- reshape_loadings(pca)

  loadings
  reshape_loadings(loadings)
}



cleanEx()
nameEx("select_parameters")
### * select_parameters

flush(stderr()); flush(stdout())

### Name: select_parameters
### Title: Automated selection of model parameters
### Aliases: select_parameters select_parameters.lm
###   select_parameters.merMod

### ** Examples

model <- lm(mpg ~ ., data = mtcars)
select_parameters(model)

model <- lm(mpg ~ cyl * disp * hp * wt, data = mtcars)
select_parameters(model)




cleanEx()
nameEx("simulate_model")
### * simulate_model

flush(stderr()); flush(stdout())

### Name: simulate_model
### Title: Simulated draws from model coefficients
### Aliases: simulate_model simulate_model.glmmTMB

### ** Examples

model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
head(simulate_model(model))



cleanEx()
nameEx("simulate_parameters")
### * simulate_parameters

flush(stderr()); flush(stdout())

### Name: simulate_parameters.glmmTMB
### Title: Simulate Model Parameters
### Aliases: simulate_parameters.glmmTMB simulate_parameters
###   simulate_parameters.default

### ** Examples

model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
simulate_parameters(model)

## Not run: 
##D if (require("glmmTMB", quietly = TRUE)) {
##D   model <- glmmTMB(
##D     count ~ spp + mined + (1 | site),
##D     ziformula = ~mined,
##D     family = poisson(),
##D     data = Salamanders
##D   )
##D   simulate_parameters(model, centrality = "mean")
##D   simulate_parameters(model, ci = c(.8, .95), component = "zero_inflated")
##D }
## End(Not run)



cleanEx()
nameEx("sort_parameters")
### * sort_parameters

flush(stderr()); flush(stdout())

### Name: sort_parameters
### Title: Sort parameters by coefficient values
### Aliases: sort_parameters sort_parameters.default

### ** Examples

# creating object to sort (can also be a regular data frame)
mod <- model_parameters(stats::lm(wt ~ am * cyl, data = mtcars))

# original output
mod

# sorted outputs
sort_parameters(mod, sort = "ascending")
sort_parameters(mod, sort = "descending")




cleanEx()
nameEx("standard_error")
### * standard_error

flush(stderr()); flush(stdout())

### Name: standard_error
### Title: Standard Errors
### Aliases: standard_error standard_error.default standard_error.factor
###   standard_error.glmmTMB standard_error.merMod

### ** Examples

model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
standard_error(model)

if (require("sandwich") && require("clubSandwich")) {
  standard_error(model, vcov = "HC3")

  standard_error(model,
    vcov = "vcovCL",
    vcov_args = list(cluster = iris$Species)
  )
}



cleanEx()
nameEx("standardize_info")
### * standardize_info

flush(stderr()); flush(stdout())

### Name: standardize_info
### Title: Get Standardization Information
### Aliases: standardize_info standardise_info standardize_info.default

### ** Examples

model <- lm(mpg ~ ., data = mtcars)
standardize_info(model)
standardize_info(model, robust = TRUE)
standardize_info(model, two_sd = TRUE)



cleanEx()
nameEx("standardize_parameters")
### * standardize_parameters

flush(stderr()); flush(stdout())

### Name: standardize_parameters
### Title: Parameters standardization
### Aliases: standardize_parameters standardise_parameters
###   standardize_posteriors standardise_posteriors

### ** Examples

model <- lm(len ~ supp * dose, data = ToothGrowth)
standardize_parameters(model, method = "refit")

## Don't show: 
if (require("lme4", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)
## Don't show: 
if (require("rstanarm", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
## Don't show: 
}) # examplesIf
## End(Don't show)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
