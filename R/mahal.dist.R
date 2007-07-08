mahal.dist <- function(distance.fmla, data, structure.fmla=NULL, inverse.cov=NULL)
  {
    
if (is.null(structure.fmla))
  {
  if (!attr(terms(distance.fmla,data=data), "response")>0) 
    stop("either distance.fmla or structure.fmla must specify a treatment group variable")
  structure.fmla <- update.formula(distance.fmla, .~1,data=data)
}
distance.fmla <- terms(distance.fmla, data=data)
distance.fmla <- update.formula(distance.fmla, ~-1+.,data=data)
dfr <- model.matrix(distance.fmla, model.frame(distance.fmla,data))

if (is.null(inverse.cov))
  {
  zpos <- attr(terms(structure.fmla,data=data), "response")
  vars <- eval(attr(terms(structure.fmla,data=data), "variables"), data, 
             parent.frame())
  zz <- vars[[zpos]]>0
  cv <- cov(dfr[zz, ,drop=FALSE])*(sum(zz)-1)/(length(zz)-2)
  cv <- cv + cov(dfr[!zz,,drop=FALSE])*(sum(!zz)-1)/(length(zz)-2)
  icv <- try( solve(cv), silent=TRUE)
  if (inherits(icv,"try-error"))
    {
       dnx <- dimnames(cv)
       s <- svd(cv)
       nz <- (s$d > sqrt(.Machine$double.eps) * s$d[1])
       if (!any(nz))
         stop("covariance has rank zero")

       icv <- s$v[, nz] %*% (t(s$u[, nz])/s$d[nz])
       dimnames(icv) <- dnx[2:1]
    }
} else
{
stopifnot(is.matrix(inverse.cov),
          dim(inverse.cov)[1]==dim(inverse.cov)[2],
          all.equal(dimnames(inverse.cov)[[1]],dimnames(inverse.cov)[[2]]),
          all(dimnames(inverse.cov)[[1]]%in%dimnames(dfr)[[2]]))
}

makedist(structure.fmla, data.frame(data, dfr),
         fn=optmatch.mahalanobis, inverse.cov=icv)
  }


optmatch.mahalanobis <- function(trtvar, dat, inverse.cov)
  {
    
  myMH <- function(Tnms, Cnms, inv.cov, data) {
   stopifnot(!is.null(dimnames(inv.cov)[[1]]), 
             all.equal(dimnames(inv.cov)[[1]], dimnames(inv.cov)[[2]]),
             all(dimnames(inv.cov)[[1]] %in% names(data)))
   covars <- dimnames(inv.cov)[[1]]
   xdiffs <- as.matrix(data[Tnms,covars])
   xdiffs <- xdiffs - as.matrix(data[Cnms,covars])
   rowSums((xdiffs %*% inv.cov) * xdiffs)
 }

  
  ans <- outer(names(trtvar)[trtvar], names(trtvar)[!trtvar],
               FUN=myMH, inv.cov=inverse.cov, data=dat)
  dim(ans) <- c(sum(trtvar), sum(!trtvar))
  dimnames(ans) <- list(names(trtvar)[trtvar], names(trtvar)[!trtvar])
  ans
  
  }
