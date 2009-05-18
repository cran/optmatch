summary.optmatch <- function(object, 
                             propensity.model = NULL, ...,
                             digits= max(3, getOption("digits")-4))
{
# Things to display in the summary method:
## effective sample size -- stratumStructure
## outlying propensity-score distances -- matched.distances()
## overall balance -- xBalance()

  stratstr <- stratumStructure(object,min.controls=.2,max.controls=5)
  cnmp <- attr(stratstr, "comparable.num.matched.pairs")
  attr(stratstr, "comparable.num.matched.pairs") <- NULL
  cat("Structure of matched sets:\n")
  print(stratstr)
  cat("Effective Sample Size: ", signif(cnmp, digits), "\n")
  cat("(equivalent number of matched pairs).\n\n")

  matchdists <- unlist(attr(object, "matched.distances"))
  cat("sum(matched.distances)=",
      signif(sum(matchdists),digits),"\n",sep="")
  cat("(within",
      signif(sum(attr(object, "exceedances")),digits),
      "of optimum).\n")
  cat("Percentiles of matched distances:\n")
  print(signif(quantile(matchdists, prob=c(0,.5, .95, 1)), digits))

  ## optional call to xbalance if it is loaded
  if(exists("xBalance") && !is.null(propensity.model) && 'glm'%in%class(propensity.model)) {
    bres <- xBalance(fmla = formula(propensity.model),
                   strata=object, data = model.frame(propensity.model),
                   report=c('z.scores', 'chisquare.test'))$overall
    row.names(bres) <- " "
    cat("Balance test overall result:\n")
    print(bres, digits=3)
  } else {
    if (!is.null(propensity.model))
      {
    cat("For covariate balance information, load the RItools package and\n")
    cat("pass a (glm) propensity model to summary() as a second argument.\n")
  }
  }
}


