## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, prompt=TRUE)

## -----------------------------------------------------------------------------
2 + 2

## -----------------------------------------------------------------------------
my.variable <- 2 + 2
my.variable * 3

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("optmatch")

## ----echo=FALSE,message=FALSE-------------------------------------------------
library(optmatch)

## ----eval=FALSE---------------------------------------------------------------
#  library(optmatch)

## ----echo=FALSE---------------------------------------------------------------
data(nuclearplants)

## ----eval=FALSE---------------------------------------------------------------
#  data(nuclearplants)

## -----------------------------------------------------------------------------
head(nuclearplants)

## ----eval=FALSE---------------------------------------------------------------
#  help("nuclearplants")

## ----eval=FALSE---------------------------------------------------------------
#  nuclearplants$pt
#  table(nuclearplants$pt)
#  with(nuclearplants, table(pt))

## -----------------------------------------------------------------------------
nuke.nopt <- subset(nuclearplants, pt == 0)

## ----eval=FALSE---------------------------------------------------------------
#  head(nuke.nopt)
#  tail(nuke.nopt)

## -----------------------------------------------------------------------------
table(nuke.nopt$pr)

## -----------------------------------------------------------------------------
pairmatch(pr ~ cap, data = nuke.nopt)

## -----------------------------------------------------------------------------
print(pairmatch(pr ~ cap, data = nuke.nopt), grouped = TRUE)

## ----results="asis", echo=FALSE, warning=FALSE--------------------------------
a <- with(nuke.nopt, data.frame(
                         Plant=row.names(nuke.nopt),
                         Date=round(date-65, 1),
                         Capacity=round(x=(cap-400),digits=-1))[as.logical(pr),])

b <- with(nuke.nopt, data.frame(
                         Plant=row.names(nuke.nopt),
                         Date=round(date-65, 1),
                         Capacity=round(x=(cap-400),digits=-1))[!as.logical(pr),])

rownames(a) <- NULL
rownames(b) <- NULL

c <- cbind(data.frame(rbind(as.matrix(a), matrix(nrow=nrow(b)-nrow(a), ncol=3))), b)
if (requireNamespace("pander", quietly = TRUE)) {
  pander::pandoc.table(c, style="multiline", missing="",
                       caption='New-site (left columns) versus existing-site (right columns) plants. "date" is `date-65`; "capacity" is `cap-400`.')
} else {
  show(c)
}

## ----eval=FALSE---------------------------------------------------------------
#  summary(pairmatch(pr ~ cap, data = nuke.nopt))

## -----------------------------------------------------------------------------
pm <- pairmatch(pr ~ cap, data = nuke.nopt)

## ----eval=FALSE---------------------------------------------------------------
#  summary(lm(cost ~ pr + pm, data = nuke.nopt))

## -----------------------------------------------------------------------------
tm <- pairmatch(pr ~ cap, controls = 2, data = nuke.nopt)

## ----eval=FALSE---------------------------------------------------------------
#  pairmatch(pr ~ cap, controls = 3, data=nuke.nopt)

## ----error=TRUE---------------------------------------------------------------
pairmatch(pr ~ cap + cost, caliper=.001, data = nuke.nopt)

## ----eval=FALSE---------------------------------------------------------------
#  cap.noadj <- lm(cap ~ pr, data = nuke.nopt)
#  summary(cap.noadj)

## -----------------------------------------------------------------------------
summary(lm(cap ~ pr, data = nuke.nopt))$coeff["pr",]

## -----------------------------------------------------------------------------
summary(lm(cap ~ pr + pm, data = nuke.nopt))$coeff["pr",]

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("RItools")
#  library(RItools)
#  balanceTest(pr ~ cap + t2, data = nuke.nopt)

## ----echo = FALSE, message = FALSE--------------------------------------------
if (requireNamespace("RItools", quietly = TRUE)) {
  library(RItools)
} else {
  cat("RItools package not installed properly")
}

## ----echo = FALSE-------------------------------------------------------------
if (requireNamespace("RItools", quietly = TRUE)) {
  RItools::balanceTest(pr ~ cap + t2, data = nuke.nopt)
}

## ----eval = FALSE-------------------------------------------------------------
#  balanceTest(pr ~ cap + t2 + strata(pm) - 1, data = nuke.nopt)
#  # The `- 1` suppresses the unmatched output to make the output cleaner

## ----echo = FALSE-------------------------------------------------------------
if (requireNamespace("RItools", quietly = TRUE)) {
  RItools::balanceTest(pr ~ cap + t2 + strata(pm) - 1, data = nuke.nopt)
}

## -----------------------------------------------------------------------------
psm <- glm(pr ~ date + t1 + t2 + cap + ne + ct + bw + cum.n + pt,
           family = binomial, data = nuclearplants)

## ----fig.width=5, fig.height=5, fig.alt="Box plot showing distribution of propensity scores"----
boxplot(psm)

## -----------------------------------------------------------------------------
ps.pm <- pairmatch(psm, data = nuclearplants)
summary(ps.pm)

## -----------------------------------------------------------------------------
psm.dist <- match_on(psm, data=nuclearplants)

## -----------------------------------------------------------------------------
caliper(psm.dist, 2)

## -----------------------------------------------------------------------------
ps.pm2 <- pairmatch(psm.dist, data = nuclearplants)
ps.pm3 <- pairmatch(psm.dist + caliper(psm.dist, 2), data = nuclearplants)
all.equal(ps.pm, ps.pm2, check.attributes=FALSE)
all.equal(ps.pm, ps.pm3, check.attributes=FALSE)
summary(ps.pm3)

## -----------------------------------------------------------------------------
mhd1 <- match_on(pr ~ date + cap + scores(psm), data=nuclearplants)
mhpc.pm <- pairmatch(mhd1, caliper=1, data=nuclearplants)
summary(mhpc.pm) # oops
mhpc.pm <- pairmatch(mhd1, caliper=2, data=nuclearplants)
summary(mhpc.pm) # better!

## ----eval=FALSE---------------------------------------------------------------
#  balanceTest(pr ~ date + t1 + t2 + cap + ne + ct + bw + cum.n,
#              data = nuclearplants)
#  balanceTest(pr ~ date + t1 + t2 + cap + ne + ct + bw + cum.n + pt +
#                strata(ps.pm2) - 1,
#              data = nuclearplants)

## ----eval = FALSE-------------------------------------------------------------
#  myb <- balanceTest(pr ~ date + t1 + t2 + cap + ne + ct + bw + cum.n +
#                       strata(ps.pm2),
#                     data = nuclearplants)
#  plot(myb)
#  print(myb, digits=1)

## ----fig.width=5, fig.height=5, echo = FALSE, fig.alt="Love plot showing the change in balance with and without matching"----
if (requireNamespace("RItools", quietly = TRUE)) {
  tryCatch({
    myb <- RItools::balanceTest(pr ~ date + t1 + t2 + cap + ne + ct + bw + cum.n +
                                  strata(ps.pm2), data = nuclearplants)
    print(myb, digits=1)
    p <- plot(myb)
    print(p)
  }, error = function(e) {
    cat(paste("RItools is producing an unexpected error. Please report",
              "this to https://github.com/markmfredrickson/optmatch/issues"))
  })

} else {
  cat("RItools package not installed properly")
}

## -----------------------------------------------------------------------------
summary(ps.pm2, psm)

## ----eval=FALSE---------------------------------------------------------------
#  summary(fullmatch(pr ~ date + cap, data = nuke.nopt))
#  summary(fullmatch(pr ~ date + cap, data = nuke.nopt, min = 1))
#  summary(fullmatch(pr ~ date + cap, data = nuke.nopt, min = 2, max = 3))

## -----------------------------------------------------------------------------
pairmatch(pr ~ date + cap + scores(psm), data=nuclearplants)
pairmatch(pr ~ date + cap + scores(psm) + strata(pt), data=nuclearplants)

## -----------------------------------------------------------------------------
cap.dist <- match_on(pr ~ cap, data = nuke.nopt)
pm1 <- pairmatch(pr ~ cap, data=nuke.nopt)
pm2 <- pairmatch(cap.dist, data=nuke.nopt)
all.equal(pm1, pm2, check.attributes = FALSE)
summary(pm2)

## -----------------------------------------------------------------------------
round(cap.dist[1:3, 1:3], 1)

## -----------------------------------------------------------------------------
round(cap.dist + caliper(cap.dist, 2), 1)

## ----eval=FALSE---------------------------------------------------------------
#  pairmatch(cap.dist + caliper(cap.dist, 2), data = nuke.nopt)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("xtable") # if not already installed
#  data(tli, package = "xtable")

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("DOS") # if not already installed
#  install.packages("DOS2") # if not already installed
#  data(package = "DOS")
#  data(package = "DOS2")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("arm") # if not already installed
#  data(lalonde, package = "arm")
#  help("lalonde", package = "arm")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("Hmisc") # if not already installed
#  Hmisc:::getHdata(rhc, what = "all")

