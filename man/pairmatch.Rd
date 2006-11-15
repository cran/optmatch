\name{pairmatch}
\alias{pairmatch}
\title{Optimal 1:1 and 1:k matching}
\description{
Given a treatment group, a larger control reservoir, and discrepancies
between each treatment and control unit, finds a pairing of treatment
units to controls that minimizes the sum of discrepancies.
}
\usage{
pairmatch(distance, controls = 1, tol = 0.001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distance}{A matrix of nonnegative discrepancies, 
each indicating the permissibility and desirability of matching the unit 
corresponding to its row (a 'treatment') to the unit
corresponding to its column (a 'control'); or a list of such matrices
made using \code{\link{makedist}}.  Finite
discrepancies indicate permissible matches, with smaller
discrepancies indicating more desirable matches. 
Matrix \code{distance}, or the matrix elements of \code{distance},
must have row and column names. }
  \item{controls}{ The number of controls to be matched to each treatment.}
  \item{tol}{ Tolerance -- see \code{\link{fullmatch}} for details. }
}
\details{
This is a wrapper to \code{\link{fullmatch}}; see its documentation for
more information.

 \code{fullmatch} tries to guess the
  order in which units would have been given in a data frame, and to
  order the factor that it returns accordingly.  If the dimnames of
  \code{distance}, or the matrices it lists, are not simply row numbers
  of the data frame you're working with, then you should compare the
  names of fullmatch's output to your row names in order to be sure
  things are in the proper order.  You can relieve yourself of these
  worries by using \code{\link{makedist}} to produce the distances, as
  it passes the ordering of units to \code{fullmatch}, which then uses
  it to order its outputs.

   The value of \code{tol} can have a substantial effect on
  computation time; with smaller values, computation takes longer.
  Not every tolerance can be met, and how small a tolerance is too small
  varies with the machine and with the details of the problem.  If
  \code{fullmatch} can't guarantee that the tolerance is as small as the
  given value of argument \code{tol}, then matching proceeds but a
  warning is issued. 
}
\value{Primarily, a named vector of class \code{c('optmatch',
  'factor')}.  Elements of this vector correspond to members of the
  treatment and control groups in reference to which the matching
  problem was posed, and are named accordingly; the names are taken from
  the row and column names of \code{distance}.  Each element of
  the vector is the concatenation of: (i) a character abbreviation of
  \code{subclass.indices}, if that argument was given, or the string
  '\code{m}' if it was not; (ii) the string \code{.}; and (iii) a
  nonnegative integer or the string \code{NA}.  In this last place,
  positive whole numbers indicate placement of the unit into a matched
  set, a number beginning with zero indicates a unit that was not
matched, and \code{NA}
  indicates that all or part of the matching problem given to
  \code{fullmatch} was found to be infeasible.
 Secondarily, \code{fullmatch} returns various data about the matching
  process and its result, stored as attributes of the named vector
  which is its primary output.  In particular, the \code{exceedances}
  attribute gives upper bounds, not necessarily sharp, for the amount by
  which the sum of distances between matched units in the result of
  \code{fullmatch} exceeds the least possible sum of distances between
  matched units in a feasible solution to the matching problem given to
  \code{fullmatch}.  Such a bound is also printed by
  \code{print.optmatch}.  
}
\references{
  Hansen, B.B. and Klopfer, S.O. (2006), \sQuote{Optimal full matching and related designs via network flows}, \emph{Journal of Computational and Graphical Statistics}, \bold{15}, 609--627.
  }
\seealso{ \code{\link{matched}}, \code{\link{makedist}}, \code{\link{fullmatch}} }
\examples{
data(plantdist)
pr <- logical(26)
pr[match(dimnames(plantdist)[[1]], LETTERS)] <- TRUE

plantspm <- pairmatch(plantdist)

}
\keyword{ nonparametric }% at least one, from doc/KEYWORDS
\keyword{ optimize }% __ONLY ONE__ keyword per line
