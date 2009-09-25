
summary.optmatch <- function(object, 
                             propensity.model = NULL, ...,
                             min.controls=.2, max.controls=5,
                             quantiles=c(0,.5, .95, 1),
                             digits= max(3, getOption("digits")-4))
{
# Things to display in the summary method:
## effective sample size -- stratumStructure
## outlying propensity-score distances -- matched.distances()
## overall balance -- xBalance()
  if (all(mfd <- matchfailed(object))) return("Matching failed.  (Restrictions impossible to meet?) Enter ?matchfailed for more info.")
  
    if (any(mfd))  {
      cat(paste("Matching failed in subclasses containing",sum(mfd),
                "of",length(mfd),"observations.\n"))
      cat("Reporting on subclasses where matching worked. (Enter ?matchfailed for more info.)\n")
    } 
  
  stratstr <- stratumStructure(object[!mfd, drop=TRUE],min.controls=min.controls,max.controls=max.controls)
  cnmp <- attr(stratstr, "comparable.num.matched.pairs")
  attr(stratstr, "comparable.num.matched.pairs") <- NULL
  cat("Structure of matched sets:\n")
  print(stratstr)
  cat("Effective Sample Size: ", signif(cnmp, digits), "\n")
  cat("(equivalent number of matched pairs).\n\n")

  matchdists <- attr(object, "matched.distances")[levels(object[!mfd, drop=TRUE])]
  matchdists <- unlist(matchdists)
  cat("sum(matched.distances)=",
      signif(sum(matchdists),digits),"\n",sep="")
  cat("(within",
      signif(sum(attr(object, "exceedances")),digits),
      "of optimum).\n")
  cat("Percentiles of matched distances:\n")
  print(signif(quantile(matchdists, prob=quantiles), digits))

  ## optional call to xbalance if it is loaded
  if(exists("xBalance") && 
     !is.null(propensity.model) &&
     inherits(propensity.model, "glm")) {

    # users must save the model for reliable behavior.
    # we warn, instead of an error, but the user may get an error
    # from model.frame or later
    if(is.null(propensity.model$model)) {
      warning("This propensity seems to have been created with 'model = FALSE'.\nFor better behavior, re-run glm() (or similar) with 'model = TRUE'.")  
    }

    bres <- xBalance(fmla = formula(propensity.model),
                   strata=object[!mfd, drop=TRUE],
                     data = expand.model.frame(propensity.model,
                       all.vars(formula(propensity.model)),
                       na.expand=TRUE)[!mfd,],
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


