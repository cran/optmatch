pscore.dist <- function(glmobject, structure.fmla=NULL)
  {
stopifnot(all(c('y', 'linear.predictors','data')%in%names(glmobject)))

if (is.null(structure.fmla))
  {
    structure.fmla <- as.formula("ZzZz~1")
} else
{
structure.fmla <- update.formula(structure.fmla, ZzZz~.)
stopifnot(all(all.vars(structure.fmla)%in%names(glmobject$data)))
}
ZzZz <- glmobject$y>0
pooled.sd <- sqrt((var(glmobject$linear.predictors[ZzZz])*(sum(ZzZz)-1) +
                   var(glmobject$linear.predictors[!ZzZz])*(sum(!ZzZz)-1))/
                  (length(ZzZz)-2))
Ppty <- glmobject$linear.predictors/pooled.sd

makedist(structure.fmla,
         data.frame(ZzZz, Ppty),
         fn=function(trtvar,data)
         {
           sclr <- data[names(trtvar), 'Ppty']
           names(sclr) <- names(trtvar)
           abs(outer(sclr[trtvar], sclr[!trtvar], '-'))
         }
           )

}
