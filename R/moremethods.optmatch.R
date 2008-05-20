"[.optmatch" <-
  function(x, i, drop=FALSE)
{
  y <- NextMethod("[")
  attr(y,"contrast.group") <- "["(attr(x, "contrast.group"),i)
  class(y) <- c("optmatch", "factor")
  y
}

