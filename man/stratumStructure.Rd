\name{stratumStructure}
\alias{stratumStructure}
\title{Return structure of matched sets}
\description{
   Tabulate treatment:control ratios occurring in matched sets, and the
   frequency of their occurrence.
}
\usage{
stratumStructure(stratum,trtgrp=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{stratum}{Matched strata, as returned by \code{\link{fullmatch}} or
\code{\link{pairmatch}}}
\item{trtgrp}{Dummy variable for treatment group membership.  (Not
required if \code{stratum} is an optmatch object, as returned by
\code{\link{fullmatch}} or \code{\link{pairmatch}}.) }
 }
\value{
  A table showing frequency of occurrence of those treatment:control
  ratios that occur.
}
\author{Ben Hansen}
\note{
}
\seealso{\code{\link{matched}}, \code{\link{fullmatch}} }
\examples{
data(plantdist)

plantsfm <- fullmatch(plantdist) # A full match with unrestricted
                                 # treatment-control balance
plantsfm1 <- fullmatch(plantdist,min.controls=2, max.controls=3)

stratumStructure(plantsfm)
stratumStructure(plantsfm1)
}

\keyword{ nonparametric }% at least one, from doc/KEYWORDS
\keyword{ optimize }% __ONLY ONE__ keyword per line
