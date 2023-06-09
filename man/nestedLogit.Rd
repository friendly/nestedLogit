% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested.R
\name{nestedLogit}
\alias{nestedLogit}
\alias{logits}
\alias{dichotomy}
\alias{continuationLogits}
\title{Binary Logit Models for Nested Dichotomies}
\usage{
nestedLogit(formula, dichotomies, data, subset = NULL, contrasts = NULL, ...)

logits(...)

dichotomy(...)

continuationLogits(levels, names, prefix = "above_")
}
\arguments{
\item{formula}{a model formula with the polytomous response on the left-hand side
and the usual linear-model-like specification on the right-hand side.}

\item{dichotomies}{specification of the logits for the nested dichotomies,
constructed by the \code{logits} and \code{dichotomy} functions,
or \code{continuationLogits}. Alternatively, the \code{dichotomies}
can be specified as a nested (i.e., recursive) list, the elements of
which can be given optional names. See Details.}

\item{data}{a data frame with the data for the model; unlike in most statistical
modeling functions, the \code{data} argument is required. Cases with \code{NA}s
in any of the variables appearing in the model formula will be removed
with a Note message.}

\item{subset}{a character string specifying an expression to fit the model
to a subset of the data; the default, \code{NULL}, uses the full data set.}

\item{contrasts}{an optional list of contrast specification for specific factors in the
model; see \code{\link{lm}} for details.}

\item{\dots}{for \code{nestedLogit}, optional named arguments to be passed to \code{\link{glm}};
for \code{logits}, definitions of the nested logits---with each named argument specifying
a dichotomy; for \code{dichotomy}, two character vectors giving the levels
defining the dichotomy; the vectors can optionally be named.}

\item{levels}{A character vector of set of levels of the variables or a number
specifying the numbers of levels (in which case, uppercase letters will be
use for the levels).}

\item{names}{Names to be assigned to the dichotomies; if absent, names
will be generated from the levels.}

\item{prefix}{a character string (default: \code{"above_"}) used as a prefix to the names of the continuation dichotomies.}
}
\value{
\code{nestedLogit} returns an object of class \code{"nestedLogit"} containing
the following elements:
\itemize{
\item \code{models}, a named list of (normally) \eqn{m - 1} \code{"glm"} objects,
each a binary logit model for one of the \eqn{m - 1} nested dichotomies representing
the \eqn{m}-level response.
\item \code{formula}, the model formula for the nested logit models.
\item \code{dichotomies}, the \code{"dichotomies"} object defining the nested dichotomies
for the model.
\item \code{data.name}, the name of the data set to which the model is fit, of class \code{"name"}.
\item \code{data}, the data set to which the model is fit.
\item \code{subset}, a character representation of the \code{subset} argument or
\code{"NULL"} if the argument isn't specified.
\item \code{contrasts}, the \code{contrasts} argument or \code{NULL} if the argument
isn't specified.
\item \code{contrasts.print} a character representation of the \code{contrasts} argument or
\code{"NULL"} if the argument isn't specified.
}
\code{logits} and \code{continuationLogits} return objects of class \code{"dichotomies"}
and \code{c("continuationDichotomies" "dichotomies")}, respectively, which are two-elements lists,
each element containing a list of two character vectors representing a dichotomy.
\code{dichotomy} returns a list of two character vectors representing a dichotomy.
}
\description{
Fit a related set of binary logit models via the \code{\link{glm}}
function to nested dichotomies, comprising a model for the polytomy.
A polytomous response with \eqn{m} categories can be analyzed using
\eqn{m-1} binary logit comparisons.  When these comparisons are nested,
the \eqn{m-1} sub-models are statistically independent. Therefore,
the likelihood chi-square statistics for the sub-models are additive
and give overall tests for a model for the polytomy.
This method was introduced by Fienberg (1980),and subsequently illustrated by
Fox(2016) and Friendly & Meyer (2016).

\code{dichotomy} and \code{logits} are helper functions to construct the dichotomies.

\code{continuationLogits} constructs a set of \eqn{m-1} logit comparisons, called
continuation logits,
for an ordered response. With \eqn{m=4} levels, say, \code{A, B, C, D},
considered low to high:
The first contrasts \code{B, C, D} against \code{A}.
The second ignores \code{A} and contrasts \code{C, D} against \code{B}.
The second ignores \code{A, B} and contrasts \code{D} against \code{C}.
}
\details{
A \emph{dichotomy} for a categorical variable is a comparison of one subset
of levels against another subset. A set of dichotomies is \emph{nested}, if after
an initial dichotomy, all subsequent ones are \emph{within} the groups of levels
lumped together in earlier ones. Nested dichotomies correspond to a binary tree
of the successive divisions.

For example, for a 3-level response, a first
dichotomy could be \code{ {A}, {B, C}} and then the second one would be
just \code{{B}, {C}}. Note that in the second dichotomy, observations
with response \code{A} are treated as \code{NA}.

The function \code{dichotomy} constructs a \emph{single} dichotomy in the required form,
which is a list of length 2 containing two character vectors giving the levels
defining the dichotomy. The function \code{logits} is used to create the
set of dichotomies for a response factor. Alternatively, the nested dichotomies can be
specified more compactly as a nested (i.e., recursive) list with optionally named
elements; for example,
\code{list(air="plane", ground=list(public=list("train", "bus"), private="car"))}.

The function \code{continuationLogits} provides a
convenient way to generate all dichotomies for an ordered response.
For an ordered response with \eqn{m=4} levels, say, \code{A, B, C, D},
considered low to high:
The dichotomy first contrasts \code{B, C, D} against \code{A}.
The second ignores \code{A} and contrasts \code{C, D} against \code{B}.
The second ignores \code{A, B} and contrasts \code{D} against \code{C}.
}
\examples{
data("Womenlf", package = "carData")

  #' Use `logits()` and `dichotomy()` to specify the comparisons of interest
  comparisons <- logits(work=dichotomy("not.work",
                                       working=c("parttime", "fulltime")),
                        full=dichotomy("parttime", "fulltime"))
  print(comparisons)

  m <- nestedLogit(partic ~ hincome + children,
                   dichotomies = comparisons,
                   data=Womenlf)
  print(summary(m))
  print(car::Anova(m))
  coef(m)

  # equivalent;
  nestedLogit(partic ~ hincome + children,
              dichotomies = list("not.work",
                                 working=list("parttime", "fulltime")),
              data=Womenlf)

  # get predicted values
  new <- expand.grid(hincome=seq(0, 45, length=10),
                     children=c("absent", "present"))
  pred.nested <- predict(m, new)

  # plot
  op <- par(mfcol=c(1, 2), mar=c(4, 4, 3, 1) + 0.1)
  plot(m, "hincome", list(children="absent"),
       xlab="Husband's Income", legend=FALSE)
  plot(m, "hincome", list(children="present"),
       xlab="Husband's Income")
  par(op)


  continuationLogits(c("none", "gradeschool", "highschool", "college"))
  continuationLogits(4)


}
\references{
S. Fienberg (1980). \emph{The Analysis of Cross-Classified Categorical Data},
2nd Edition, MIT Press, Section 6.6.

J. Fox (2016), \emph{Applied Linear Regression and Generalized Linear Models}, 3rd Edition, Sage,
Section 14.2.2.

J. Fox and S. Weisberg (2011), \emph{An R Companion to Applied Regression}, 2nd Edition, Sage, Section 5.8.

M. Friendly and D. Meyers (2016), \emph{Discrete Data Analysis with R}, CRC Press,
Section 8.2.
}
\seealso{
\code{\link{nestedMethods}}
}
\author{
John Fox
}
\keyword{regression}
