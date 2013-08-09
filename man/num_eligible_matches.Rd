\name{num_eligible_matches}
\alias{num_eligible_matches}
\title{Returns the number of eligible matches for the distance.}
\usage{
  num_eligible_matches(x)
}
\arguments{
  \item{x}{Any distance object.}
}
\value{
  A list counting the number of eligible matches in the
  distance.
}
\description{
  This will return a list of the number of finite entries
  in a distance matrix. If the distance has no subgroups,
  it will be a list of length 1. If the distance has
  subgroups (i.e. \code{x} is an
  \code{BlockedInfinitySparseMatrix}, it will be a named
  list.)
}

