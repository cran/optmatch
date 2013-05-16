\name{pairmatch}
\alias{pair}
\alias{pairmatch}
\title{Optimal 1:1 and 1:k matching}
\usage{
  pairmatch(distance, controls = 1,
    remove.unmatchables = FALSE, ...)

  pair(distance, controls = 1, remove.unmatchables = FALSE,
    ...)
}
\arguments{
  \item{distance}{A matrix of non-negative discrepancies,
  each indicating the permissibility and desirability of
  matching the unit corresponding to its row (a
  'treatment') to the unit corresponding to its column (a
  'control'); or a list of such matrices made using
  \code{\link{match_on}}.  Finite discrepancies indicate
  permissible matches, with smaller discrepancies
  indicating more desirable matches. Matrix
  \code{distance}, or the matrix elements of
  \code{distance}, must have row and column names.}

  \item{controls}{The number of controls to be matched to
  each treatment}

  \item{remove.unmatchables}{Should treatment group members
  for which there are no eligible controls be removed prior
  to matching?}

  \item{...}{Additional arguments to pass to
  \code{\link{fullmatch}}. It is strongly suggested that
  you pass a \code{data} argument. It is an error to pass
  \code{min.controls}, \code{max.controls}, or
  \code{omit.fraction} as \code{pairmatch} must set these
  values.}
}
\value{
  A \code{\link{optmatch}} object (\code{factor})
  indicating matched groups.
}
\description{
  Given a treatment group, a larger control reservoir, and
  discrepancies between each treatment and control unit,
  finds a pairing of treatment units to controls that
  minimizes the sum of discrepancies.
}
\details{
  This is a wrapper to \code{\link{fullmatch}}; see its
  documentation for more information, especially on
  additional arguments to pass.

  If \code{remove.unmatchables} is \code{FALSE}, then if
  there are unmatchable treated units then the matching as
  a whole will fail and no units will be matched.  If
  \code{TRUE}, then this unit will be removed and the
  function will attempt to match each of the other
  treatment units.  (In this case matching can still fail,
  if there is too much competition for certain controls; if
  you find yourself in that situation you should consider
  full matching, which necessarily finds a match for
  everyone with an eligible match somewhere.) The units of
  the \code{optmatch} object returned correspond to members
  of the treatment and control groups in reference to which
  the matching problem was posed, and are named
  accordingly; the names are taken from the row and column
  names of \code{distance} (with possible additions from
  the optional \code{data} argument).  Each element of the
  vector is the concatenation of: (i) a character
  abbreviation of \code{subclass.indices}, if that argument
  was given, or the string '\code{m}' if it was not; (ii)
  the string \code{.}; and (iii) a non-negative integer.
  Unmatched units have \code{NA} entries. Secondarily,
  \code{fullmatch} returns various data about the matching
  process and its result, stored as attributes of the named
  vector which is its primary output.  In particular, the
  \code{exceedances} attribute gives upper bounds, not
  necessarily sharp, for the amount by which the sum of
  distances between matched units in the result of
  \code{fullmatch} exceeds the least possible sum of
  distances between matched units in a feasible solution to
  the matching problem given to \code{fullmatch}.  (Such a
  bound is also printed by \code{print.optmatch} and by
  \code{summary.optmatch}.)
}
\examples{
data(nuclearplants) 

### Pair matching on a Mahalanobis distance 
mhd <- match_on(pr ~ t1 + t2, data = nuclearplants) 
( pm1 <- pairmatch(mhd) ) 
summary(pm1) 

### Pair matching within a propensity score caliper.  
ppty <- glm(pr~.-(pr+cost), family=binomial(), data=nuclearplants) 
( pm2 <- pairmatch(mhd + caliper(match_on(ppty), 2)) ) 
summary(pm2)

### Propensity balance assessment. Requires RItools package.
if(require(RItools)) summary(pm2, ppty)

### 1:2 matched triples
tm <- pairmatch(mhd, controls = 2)
summary(tm)

### Creating a data frame with the matched sets attached.
### match_on(), caliper() and the like cooperate with pairmatch()
### to make sure observations are in the proper order:
all.equal(names(tm), row.names(nuclearplants))
### So our data frame including the matched sets is just
cbind(nuclearplants, matches=tm)

### In contrast, if your matching distance is an ordinary matrix
### (as earlier versions of optmatch required), you'll
### have to align it by observation name with your data set. 
cbind(nuclearplants, matches = tm[row.names(nuclearplants)])

}
\references{
  Hansen, B.B. and Klopfer, S.O. (2006), \sQuote{Optimal
  full matching and related designs via network flows},
  \emph{Journal of Computational and Graphical Statistics},
  \bold{15}, 609--627.
}
\seealso{
  \code{\link{matched}}, \code{\link{caliper}},
  \code{\link{fullmatch}}
}
\keyword{nonparametric}
\keyword{optimize}

