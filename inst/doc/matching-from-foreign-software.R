## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, prompt=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  externaldata <- read.csv("externaldata.csv", header = TRUE)
#  externaldata$match <- fullmatch(..., data = externaldata)
#  write.csv(externaldata, file = "externaldata.matched.csv")

## ----eval=FALSE---------------------------------------------------------------
#  sasdata <- read.csv("/path/to/save/sasout.csv", header = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  library(optmatch)
#  f <- fullmatch(gender ~ age + ppty, data = sasdata)
#  sasdata$match <- f

## ----eval=FALSE---------------------------------------------------------------
#  write.csv(sasdata, "/path/to/saverout.sas.csv", row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  library(haven)
#  statadata <- read_dta("/path/to/save/toR.dta")

## ----eval=FALSE---------------------------------------------------------------
#  library(optmatch)
#  f <- fullmatch(gender ~ age + ppty, data = sttadata)
#  statadata$match <- f

## ----eval=FALSE---------------------------------------------------------------
#  write_dta(statadata, "/path/to/save/rout.stata.dta")

