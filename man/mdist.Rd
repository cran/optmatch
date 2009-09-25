\name{mdist}
\alias{mdist}
\title{Creates matrices of mdists (distances) between observations for matching}
\description{
  A generic function, with several supplied methods, for creating distance
  matrices between observations to be used in the match process.
  Using these matrices, 
  \code{pairmatch()} or \code{fullmatch()} can determine optimal matches.
}
\usage{
  mdist(x, structure.fmla = NULL, ...)
}
\arguments{
  \item{x}{The object to use as the basis for forming the mdist matrix.
  Methods exist
  for formulas, functions, and generalized linear models.}
  \item{structure.fmla}{A formula denoting the treatment variable on the 
  left hand side and an optional grouping expression on the right hand side. For
  example, \code{z ~ 1} indicates no grouping. \code{z ~ s} subsets the data only
  computing distances within the subsets formed by \code{s}. See method notes for
  for any additional formula options.}
  \item{...}{Additional method arguments. Most methods require a 'data' argument.}
}
\details{
  The \code{mdist} method provides three ways to construct a mdist
  matrix (or list of mdist matrices): function, glm, and formula.

  The \code{mdist.function} method takes a function of two arguments. When
  called, this function will recieve the treatment observations as the first argument
  and the control observations as the second argument. As an example, the following
  computes the raw differences between values of \code{t1} for treatment
  units (here, nuclear plants with \code{pr==1}) and controls (here,
  plants with \code{pr==0}), returning the result as
  a distance matrix:
  \code{sdiffs <- function(treatments, controls) {
      abs(outer(treatments$t1, controls$t1, `-`))
    }
  }

  The \code{mdist.function} method does similar things as the
  earlier optmatch function \code{\link{makedist}}, although the
  interface is a bit different.
  
  The \code{mdist.formula} computes the squared Mahalanobis distance between observations
  using the supplied formula. In addition to the distance formula (the first argument),
  this method can also take a structure formula to denote strata in the observations, e.g.
  \code{~ s} would group the observations by the factor \code{s}.

  The \code{mdist.glm} takes an argument of class \code{glm} as the first argument.
  It computes the deviations between observations using the \code{mad} function.
  See \code{pscore.dist} for more information. 
}
\value{
  Object of class \code{optmatch.dlist}, which is suitable to be given
  as \code{distance} argument to \code{\link{fullmatch}} or
  \code{\link{pairmatch}}. For more information, see \code{\link{pscore.dist}}

}
\author{ Mark M. Fredrickson} %Ben B. Hansen

\references{
  P.~R. Rosenbaum and D.~B. Rubin (1985),
\sQuote{Constructing a control group using multivariate matched sampling
  methods that incorporate the propensity score},
 \emph{The American Statistician}, \bold{39} 33--38.
}

\seealso{ \code{\link{makedist}}, \code{\link{mahal.dist}}, \code{\link{fullmatch}}, \code{\link{pairmatch}},
          \code{\link{pscore.dist}}}
\examples{
data(nuclearplants)

### A propensity score distance:
aGlm <- glm(pr~.-(pr+cost), family=binomial(), data=nuclearplants)
mdist(aGlm)

### A Mahalanobis distance:
mdist(pr ~ t1 + t2, data = nuclearplants)

### Absolute difference on a scalar-distance:

sdiffs <- function(treatments, controls) {
  abs(outer(treatments$t1, controls$t1, `-`))
}

(absdist <- mdist(sdiffs, structure.fmla = pr ~ 1, data = nuclearplants))

### Using pairmatch on the scalar example:
pairmatch(absdist)
}

\keyword{nonparametric }% at least one, from doc/KEYWORDS
