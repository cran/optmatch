\name{subdim}
\alias{subdim}
\title{Returns the dimension of each valid subproblem}
\usage{
  subdim(x)
}
\arguments{
  \item{x}{A distance specification to get the
  sub-dimensions of.}
}
\value{
  A list of the dimensions of each valid subproblem. Any
  subproblems with 0 controls or 0 treatments will be
  ignored. The names of the entries in the list will be the
  names of the subproblems, if they exist.
}
\description{
  Returns a list containing the dimensions of all valid
  subproblems.
}

