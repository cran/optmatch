"[.optmatch" <-
  function(x, ..., drop=FALSE)
{
  y <- NextMethod("[")
  attr(y,"contrast.group") <- "["(attr(x, "contrast.group"),...)
### The following is something of a kluge.  It would make more sense
### to remove matched distances that have been removed from the optmatch
### vector, but doing that is not straightforward, since the distances don't
### straighforwardly line up with the observations.  At present (version 0.6),
### the matched.distances attribute is only used in summary.optmatch;
### I have inserted code there to compensate for non-subsetting of the
### matched distances attribute in the case where matching has failed in some
### subclasses.
  if (!is.null(attr(x, "matched.distances"))) 
    attr(y, "matched.distances") <- attr(x, "matched.distances")
  class(y) <- c("optmatch", "factor")
  y
}

