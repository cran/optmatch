\name{fmatch}
\alias{fmatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Full matching }
\description{
  A low level function used by \code{fullmatch} to do full matching.
}
\usage{
fmatch(distance.matrix, max.row.units, max.col.units, min.col.units = 1, f = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distance.matrix}{ Matrix of distances }
  \item{max.row.units}{ Max no of row units per matched set }
  \item{max.col.units}{ Max no of col units per matched set }
  \item{min.col.units}{ Min no of col units per matched set }
  \item{f}{ Fraction of the control reservoir to be omitted from
  the match }
}
%\details{}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
\author{Ben Hansen}
\seealso{\code{\link{fullmatch}}}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
\keyword{internal}% at least one, from doc/KEYWORDS

