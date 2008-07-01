stratumStructure <- function(stratum,trtgrp=NULL)
{
if (class(stratum)[1]!="optmatch" & is.null(trtgrp))
  stop("stratum not of class \'optmatch\'; trtgrp must be specified")
if (class(stratum)[1]!="optmatch")
  warning("stratum not of class optmatch; was this intended?")
if (class(stratum)[1]=="optmatch" & is.null(attr(stratum, "contrast.group")) & is.null(trtgrp))
  stop("Argument 1 is of class optmatch but it has lost its contrast.group attribute; must specify trtgrp")
if (class(stratum)[1]=="optmatch" & !is.null(attr(stratum, "contrast.group")) & !is.null(trtgrp))
  warning("ignoring second argument to stratumStructure")
if (class(stratum)[1]=="optmatch")
   {
     tgp <- attr(stratum, "contrast.group")
   } else {
     tgp <- trtgrp
   }
if (!any(tgp<=0) | !any(tgp>0))
   warning("No variation in (trtgrp>0); was this intended?")

stratum <- as.integer(as.factor(stratum))
if (any(is.na(stratum)))
  stratum[is.na(stratum)] <- max(stratum, na.rm=TRUE) + 1:sum(is.na(stratum))

ttab <- table(stratum,as.logical(tgp))
ans <- table(paste(ttab[,2], ttab[,1], sep=":"),
             dnn="stratum treatment:control ratios")
tnn <- unlist(strsplit(names(ans), ":", fixed=FALSE))
tnn <- as.numeric(tnn)
onez <- tnn[2*(1:length(ans))-1]==1 & tnn[2*(1:length(ans))]==0
if (any(onez))
  {
tnn[2*(1:length(ans))-1][onez] <- Inf
tnn[2*(1:length(ans))][onez] <- 1
}
ans <- ans[order(-tnn[2*(1:length(ans))-1],tnn[2*(1:length(ans))])]
attr(ans, "comparable.num.matched.pairs") <- sum(2/(1/ttab[,1] + 1/ttab[,2]))
ans
}  
