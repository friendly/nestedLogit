# Vignettes illustrating use of ggeffects and easystats packages for nestedLogit models
#

Several other packages offer support for nestedLogit models. It would be nice to illustrate them here, because
their documentation doesn't give any examples.


**ggeffects**: supports the calculation and plotting of marginal effects and predicted probabilities for nested dichotomy models.
I tried several tests: `dev/ggeffects-tests*.R`, but for the main example of `Womenlf`, I can't make the kind of effect plots I like, as in the vignette `vignettes/plotting-ggplot.Rmd`. In particular, I'd like to make plots of predicted probabilities
or log odds for the two dichotomies, `work`, `full` against husband's income, with confidence bands and facets for children: present / absent. 

**easystats** packages: 

* **insight**: Provides a unified interface for accessing model information (like formula and data) from nestedLogit objects.
* **parameters**: Uses it to process and format model parameters and coefficients.
* **performance**: Uses it for calculating various model performance indices and quality 

See `dev/easystats-test.R`
