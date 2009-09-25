pairmatch <- function(distance, controls=1, tol=0.001)
  {
stopifnot(is.matrix(distance)||class(distance)[1]=="optmatch.dlist",
          all(floor(controls)==controls), controls>0)

if (class(distance)[1]=="optmatch.dlist")
  {
nt <- unlist(lapply(distance, function(x){dim(x)[1]}))
nc <- unlist(lapply(distance, function(x){dim(x)[2]}))
omf <- (nc-controls*nt)/nc
if (any(omf<0)) stop('not enough controls in some subclasses')
} else {
  nt <- dim(distance)[1]
  nc <- dim(distance)[2]
  omf <- (nc-controls*nt)/nc
  if (any(omf<0)) stop('not enough controls')
}
fullmatch(distance=distance, min.controls=controls,
          max.controls=controls, omit.fraction=omf,
          tol=tol)
  }

