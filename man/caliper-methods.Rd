\name{caliper}
\alias{caliper}
\alias{caliper-methods}
\alias{caliper,InfinitySparseMatrix-method}
\alias{caliper,matrix-method}
\alias{caliper,optmatch.dlist-method}
\title{Prepare matching distances suitable for matching within calipers.}
\usage{
  \S4method{caliper}{InfinitySparseMatrix}(x, width = 1,
    exclude = c(), compare = `<=`)

  \S4method{caliper}{matrix}(x, width = 1, exclude = c(),
    compare = `<=`)

  \S4method{caliper}{optmatch.dlist}(x, width = 1,
    exclude = c(), compare = `<=`)
}
\arguments{
  \item{x}{A distance specification created with
  \code{\link{match_on}} or similar.}

  \item{width}{The width of the caliper: how wide of a
  margin to allow in matches. Must be a scalar value.}

  \item{exclude}{(Optional) A character vector of
  observations (corresponding to row and column names) to
  exclude from the caliper.}

  \item{compare}{A function that decides that whether two
  observations are with the caliper. The default is
  \code{`<=`}. \code{`<`} is a common alternative.}
}
\value{
  A matrix like object that is suitable to be given as
  \code{distance} argument to \code{\link{fullmatch}} or
  \code{\link{pairmatch}}. The caliper will be only zeros
  and \code{Inf} values, indicating a possible match or no
  possible match, respectively.

  You can combine the results of \code{caliper} with other
  distances using the \code{`+`} operator. See the examples
  for usage.
}
\description{
  Encodes calipers, or maximum allowable distances within
  which to match. The result of a call to \code{caliper} is
  itself a distance specification between treated and
  control units that can be used with \code{pairmatch()} or
  \code{fullmatch()}. Calipers can also be combined with
  other distance specifications for richer matching
  problems.
}
\details{
  \code{caliper} is a generic function with methods for any
  of the allowed distance specifications: user created
  matrices, the results of \code{\link{match_on}}, the
  results of \code{\link{exactMatch}}, or combinations
  (using \code{`+`}) of these objects.

  \code{width} provides the size of the caliper, the
  allowable distance for matching. If the distance between
  a treated and control pair is less than or equal to this
  distance, it is allowed kept; otherwise, the pair is
  discarded from future matching. The default comparison of
  ``equal or less than can'' be changed to any other
  comparison function using the \code{comparison} argument.

  If you wish to exclude specific units from the caliper
  requirements, pass the names of these units in the
  \code{exclude} argument. These units will be allowed to
  match any other unit.
}
\examples{
data(nuclearplants)


### Caliper of 100 MWe on plant capacity
caliper(match_on(pr~cap, data=nuclearplants, method="euclidean"), width=100)

### Caliper of 1/2 a pooled SD of plant capacity
caliper(match_on(pr~cap, data=nuclearplants), width=.5)

### Caliper  of .2 pooled SDs in the propensity score
ppty <- glm(pr ~ . - (pr + cost), family = binomial(), data = nuclearplants)
ppty.dist <- match_on(ppty)

pptycaliper <- caliper(ppty.dist, width = .2)

### caliper on the Mahalanobis distance
caliper(match_on(pr ~ t1 + t2, data = nuclearplants), width = 3)

### Combining a Mahalanobis distance matching with a caliper
### of 1 pooled SD in the propensity score:
mhd.pptyc <- caliper(ppty.dist, width = 1) +
          match_on(pr ~ t1 + t2, data = nuclearplants)
pairmatch(mhd.pptyc)

### Excluding observations from caliper requirements:
caliper(match_on(pr ~ t1 + t2, data = nuclearplants), width = 3, exclude = c("A", "f"))

}
\author{
  Mark M. Fredrickson and Ben B. Hansen
}
\references{
  P.~R. Rosenbaum and D.~B. Rubin (1985),
  \sQuote{Constructing a control group using multivariate
  matched sampling methods that incorporate the propensity
  score}, \emph{The American Statistician}, \bold{39}
  33--38.
}
\seealso{
  \code{\link{exactMatch}}, \code{\link{match_on}},
  \code{\link{fullmatch}}, \code{\link{pairmatch}}
}
\keyword{nonparametric}

