summary.optmatch <- function(object, maxsum=100, ...)
{
cat("Tolerance within", sum(attr(object, "exceedances")), "\n")
summary.factor(object, maxsum=maxsum, ...)
}

