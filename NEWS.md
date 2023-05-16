# nestedLogit 0.2.2

* add example of `lobstr::tree()` to print nested lists
* use fig.show="hold" to keep `par(op)` with the code

# nestedLogit 0.2.1

* Reset all `par()` and `options()` calls so as to not alter user's workspace. 
* now document all return values. 
* added a reference to DESCRIPTION. It is a book, so no doi:, url, etc.
* fixed one URL that win-builder (spuriously) complains about.


# nestedLogit 0.2.0

* Now allow dichotomies to be specified by a nested (recursive) of binary splits of the categories [suggestion of Achim Zeileis]
* The model object is now of class "nestedLogit" for uniformity.
* A basic `plot()` method now operational
* Added a `linearHypothesis()` method to give Wald tests for hypotheses about coefficients or their linear combinations.
* Expanded vignette to illustrate some other methods.
* Added a `models()` generic and method to extract separate models from the `"nestedLogit"` object
* Added a `logLike()` method, and through it, gets `AIC()` and `BIC()`
* Reorganized documentation to separate nested hypothesis methods.

# nestedLogit 0.1.0

* Initial version
* Added a `NEWS.md` file to track changes to the package.

