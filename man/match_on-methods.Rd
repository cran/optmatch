\name{match_on}
\alias{InfinitySparseMatrix-class}
\alias{match_on}
\alias{match_on-methods}
\alias{match_on,bigglm-method}
\alias{match_on,formula-method}
\alias{match_on,function-method}
\alias{match_on,glm-method}
\alias{match_on,InfinitySparseMatrix-method}
\alias{match_on,matrix-method}
\alias{match_on,numeric-method}
\title{Create treated to control distances for matching problems}
\usage{
  \S4method{match_on}{function}(x, within = NULL, z = NULL,
    data = NULL, ...)

  \S4method{match_on}{formula}(x, within = NULL, data =
    NULL, subset = NULL, method = "mahalanobis", ...)

  \S4method{match_on}{glm}(x, within = NULL,
    standardization.scale = mad, ...)

  \S4method{match_on}{bigglm}(x, within = NULL, data =
    NULL, standardization.scale = mad, ...)

  \S4method{match_on}{numeric}(x, within = NULL, z, caliper
    = NULL, ...)

  \S4method{match_on}{InfinitySparseMatrix}(x,
    within = NULL, ...)

  \S4method{match_on}{matrix}(x, within = NULL, ...)
}
\arguments{
  \item{x}{An object defining how to create the distances}

  \item{within}{A valid distance specification, such as the
  result of \code{\link{exactMatch}} or
  \code{\link{caliper}}. Finite entries indicate which
  distances to create. Including this argument can
  significantly speed up computation for sparse matching
  problems.}

  \item{...}{Other arguments for methods.}

  \item{z}{A factor, logical, or binary vector indicating
  treatment (the higher level) and control (the lower
  level) for each unit in the study.}

  \item{data}{A \code{data.frame} or \code{matrix}
  containing variables used by the method to construct the
  distance matrix.}

  \item{subset}{A subset of the data to use in creating the
  distance specification.}

  \item{method}{A string indicating which method to use in
  computing the distances from the data. The current
  possibilities are \code{"mahalanobis", "euclidean"}.}

  \item{standardization.scale}{Standardizes the data based
  on the median absolute deviation (by default).}

  \item{caliper}{The width of a caliper to fit on the
  difference of scores.  This can improve efficiency versus
  first creating all the differences and then filtering out
  those entries that are larger than the caliper.}
}
\value{
  A distance specification (a matrix or similar object)
  which is suitable to be given as the \code{distance}
  argument to \code{\link{fullmatch}} or
  \code{\link{pairmatch}}.
}
\description{
  A function with which to produce matching distances, for
  instance Mahalanobis distances, propensity score
  discrepancies or calipers, or combinations thereof, for
  \code{\link{pairmatch}} or \code{\link{fullmatch}} to
  subsequently \dQuote{match on}. Conceptually, the result
  of a call \code{match_on} is a treatment-by-control
  matrix of distances. Because these matrices can grow
  quite large, in practice \code{match_on} produces either
  an ordinary dense matrix or a special sparse matrix
  structure (that can make use of caliper and exact
  matching constraints to reduce storage requirements).
  Methods are supplied for these sparse structures,
  \code{InfinitySparseMatrix}es, so that they can be
  manipulated and modified in much the same way as dense
  matrices.
}
\details{
  \code{match_on} is generic. There are several supplied
  methods, all providing the same basic output: a matrix
  (or similar) object with treated units on the rows and
  control units on the columns. Each cell [i,j] then
  indicates the distance from a treated unit i to control
  unit j. Entries that are \code{Inf} are said to be
  unmatchable. Such units are guaranteed to never be in a
  matched set. For problems with many \code{Inf} entries,
  so called sparse matching problems, \code{match_on} uses
  a special data type that is more space efficient than a
  standard R \code{matrix}. When problems are not sparse
  (i.e. dense), \code{match_on} uses the standard
  \code{matrix} type.

  \code{match_on} methods differ on the types of arguments
  they take, making the function a one-stop location of
  many different ways of specifying matches: using
  functions, formulas, models, and even simple scores. Many
  of the methods require additional arguments, detailed
  below. All methods take a \code{within} argument, a
  distance specification made using
  \code{\link{exactMatch}} or \code{\link{caliper}} (or
  some additive combination of these or other distance
  creating functions). All \code{match_on} methods will use
  the finite entries in the \code{within} argument as a
  guide for producing the new distance. Any entry that is
  \code{Inf} in \code{within} will be \code{Inf} in the
  distance matrix returned by \code{match_on}. This
  argument can reduce the processing time needed to compute
  sparse distance matrices.

  The \code{match_on} function is similar to the older, but
  still supplied, \code{\link{mdist}} function. Future
  development will concentrate on \code{match_on}, but
  \code{mdist} is still supplied for users familiar with
  the interface. For the most part, the two functions can
  be used interchangeably by users.

  The \code{function} method takes as its \code{x} argument
  a function of three arguments: \code{index}, \code{data},
  and \code{z}. The \code{data} and \code{z} arguments will
  be the same as those passed directly to \code{match_on}.
  The \code{index} argument is a matrix of two columns,
  representing the pairs of treated and control units that
  are valid comparisons (given any \code{within}
  arguments). The first column is the row name or id of the
  treated unit in the \code{data} object. The second column
  is the id for the control unit, again in the \code{data}
  object. For each of these pairs, the function should
  return the distance between the treated unit and control
  unit. This may sound complicated, but is simple to use.
  For example, a function that returned the absolute
  difference between to units using a vector of data would
  be \code{f <- function(index, data, z) { abs(apply(index,
  1, function(pair) { data[pair[1]] - data[pair[2]] })) }}.
  (Note: This simple case is precisely handled by the
  \code{numeric} method.)

  The formula method produces, by default, a Mahalanobis
  distance specification based on the formula \code{Z ~ X1
  + X2 + ... }, where \code{Z} the treatment indicator. A
  Mahalanobis distance scales the Euclidean distance by the
  inverse of the covariance matrix. Other options can be
  selected by the \code{method} argument.

  The \code{glm} method accepts a fitted propensity model,
  extracts distances on the linear propensity score (logits
  of the estimated conditional probabilities), and rescales
  the distances by the reciprocal of the pooled s.d. of
  treatment- and control-group propensity scores. (The
  scaling uses \code{mad}, for resistance to outliers, by
  default; this can be changed to the actual s.d., or
  rescaling can be skipped entirely, by setting argument
  \code{standardization.scale} to \code{sd} or \code{NULL},
  respectively.) The resulting distance matrix is the
  absolute difference between treated and control units on
  the rescaled propensity scores. This method relies on the
  \code{numeric} method, so you may pass a \code{caliper}
  argument.

  The \code{bigglm} method works analogously to the
  \code{glm} method, but with \code{bigglm} objects,
  created by the \code{bigglm} function from package
  \sQuote{biglm}, which can handle bigger data sets than
  the ordinary glm function can.

  The \code{numeric} method returns the absolute difference
  for treated and control units computed using the vector
  of scores \code{x}. Either \code{x} or \code{z} must have
  names.

  The \code{matrix} and \code{InfinitySparseMatrix} just
  return their arguments as these objects are already valid
  distance specifications.
}
\examples{
data(nuclearplants)
match_on.examples <- list()
### Propensity score distances.
### Recommended approach:
(aGlm <- glm(pr~.-(pr+cost), family=binomial(), data=nuclearplants))
match_on.examples$ps1 <- match_on(aGlm)
### A second approach: first extract propensity scores, then separately
### create a distance from them.  (Useful when importing propensity
### scores from an external program.)
plantsPS <- predict(aGlm)
match_on.examples$ps2 <- match_on(pr~plantsPS, data=nuclearplants)
### Full matching on the propensity score.
fullmatch(match_on.examples$ps1, data = nuclearplants)
fullmatch(match_on.examples$ps2, data = nuclearplants)
### Because match_on.glm uses robust estimates of spread, 
### the results differ in detail -- but they are close enough
### to yield similar optimal matches.
all(fullmatch(match_on.examples$ps1)==fullmatch(match_on.examples$ps2, data = nuclearplants)) # The same

### Mahalanobis distance:
match_on.examples$mh1 <- match_on(pr ~ t1 + t2, data = nuclearplants)

### Absolute differences on a scalar:
tmp <- nuclearplants$t1
names(tmp) <- rownames(nuclearplants)

(absdist <- match_on(tmp, z = nuclearplants$pr, 
                  exclusions = exactMatch(pr ~ pt, nuclearplants)))

### Pair matching on the variable `t1`:
pairmatch(absdist)


### Propensity score matching within subgroups:
match_on.examples$ps3 <- match_on(aGlm, exactMatch(pr ~ pt, nuclearplants))
fullmatch(match_on.examples$ps3, data = nuclearplants)

### Propensity score matching with a propensity score caliper:
match_on.examples$pscal <- match_on.examples$ps1 + caliper(match_on.examples$ps1, 1)
fullmatch(match_on.examples$pscal, data = nuclearplants) # Note that the caliper excludes some units

### A Mahalanobis distance for matching within subgroups:
match_on.examples$mh2 <- match_on(pr ~ t1 + t2 , data = nuclearplants,
                            excludes = exactMatch(pr ~ pt, nuclearplants))

### Mahalanobis matching within subgroups, with a propensity score
### caliper:
fullmatch(match_on.examples$mh2 + caliper(match_on.examples$ps3, 1), data = nuclearplants)
}
\references{
  P.~R. Rosenbaum and D.~B. Rubin (1985),
  \sQuote{Constructing a control group using multivariate
  matched sampling methods that incorporate the propensity
  score}, \emph{The American Statistician}, \bold{39}
  33--38.
}
\seealso{
  \code{\link{fullmatch}}, \code{\link{pairmatch}},
  \code{\link{exactMatch}}, \code{\link{caliper}}
}

