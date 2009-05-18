"[.optmatch" <-
  function(x, ..., drop=FALSE)
{
  y <- NextMethod("[")
  attr(y,"contrast.group") <- "["(attr(x, "contrast.group"),...)
  class(y) <- c("optmatch", "factor")
  y
}

