makedist <- function(structure.fmla, data,
                     fn=function(trtvar, dat, ...){
                       matrix(0, sum(trtvar), sum(!trtvar),
                              dimnames=list(names(trtvar)[trtvar],
                                names(trtvar)[!trtvar]))},
                     ...)
  {
  if (!attr(terms(structure.fmla), "response")>0) 
    stop("structure.fmla must specify a treatment group variable")

  zpos <- attr(terms(structure.fmla), "response")
  vars <- eval(attr(terms(structure.fmla), "variables"), data, 
             parent.frame())
  zzz <- vars[[zpos]]
  stopifnot(is.numeric(zzz) | is.logical(zzz),
            all(!is.na(zzz)), any(zzz<=0), any(zzz>0))
# more informative error message here...
  zz <- (zzz>0)
  vars <- vars[-zpos]
  names(zz) <- row.names(model.frame(structure.fmla,data))
if (length(vars)>0)
  {
  ss <- interaction(vars, drop=TRUE)
} else ss <- factor(zz>=0, labels="m")
  ans <- tapply(zz, ss, FUN=fn,
                dat=data, ..., simplify=FALSE)
  FUNchk <- unlist(lapply(ans,
                          function(x){!is.matrix(x) & !is.vector(x)}))
  if (any(FUNchk)) { stop("FUN should always return matrices")}

  mdms <- split(zz,ss)
  NMFLG <- FALSE

  for (ii in (1:length(ans)))
  {
    dn1 <- names(mdms[[ii]])[mdms[[ii]]]
    dn2 <- names(mdms[[ii]])[!mdms[[ii]]]
    if (is.null(dim(ans[[ii]])))
      {
      if (length(dn1)>1 & length(dn2)>1)
        { stop("FUN should always return matrices")}
      if (length(ans[[ii]])!=max(length(dn1), length(dn2)))
        { stop(paste("unuseable FUN value for stratum", names(ans)[ii]))}
      
      if (is.null(names(ans[[ii]])))
          {
           ans[[ii]] <-  matrix(ans[[ii]], length(dn1), length(dn2),
                         dimnames=list(dn1,dn2))
         } else
        { if (length(dn1)>1)
            {
              ans[[ii]] <- matrix(ans[[ii]],length(dn1), 1,
                                 dimnames=list(names(ans[[ii]]), dn2))
            } else {
              ans[[ii]] <- matrix(ans[[ii]], 1, length(dn2),
                                 dimnames=list(dn1, names(ans[[ii]])))
            }
        }
      } else {
      if (!all(dim(ans[[ii]])==c(length(dn1), length(dn2))))
        { stop(paste("FUN value has incorrect dimension at stratum",
                     names(ans)[ii])) }
      if (is.null(dimnames(ans[[ii]])))
        {
          dimnames(ans[[ii]]) <- list(dn1, dn2)
          NMFLG <- TRUE
        } else {
          if (!all(dn1%in%dimnames(ans[[ii]])[[1]]) |
              !all(dn2%in%dimnames(ans[[ii]])[[2]]) )
            { stop(paste(
                    "dimnames of FUN value don't match unit names in stratum",
                         names(ans)[ii])) }
        }
      
      }
  }

  if (NMFLG){
warning("FUN value not given dimnames; assuming they are list(names(trtvar)[trtvar], names(trtvar)[!trtvar])")}

  
  attr(ans, 'row.names') <- names(zz)
  class(ans) <- c('optmatch.dlist', 'list')
  ans
  }
