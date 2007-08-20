pscore.dist <- function(glmobject, structure.fmla=NULL)
  {
stopifnot(all(c('y', 'linear.predictors','data')%in%names(glmobject)))

if (is.null(structure.fmla))
  {
    structure.fmla <- as.formula("ZzZz~1")
} else
{
structure.fmla <- update.formula(structure.fmla, ZzZz~.)
if (!all(all.vars(structure.fmla)%in%c('ZzZz',names(model.frame(glmobject)))))
  warning('stratifying variables (in structure.fmla) not in propensity specification')
}
ZzZz <- glmobject$y>0
pooled.sd <- sqrt((var(glmobject$linear.predictors[ZzZz])*(sum(ZzZz)-1) +
                   var(glmobject$linear.predictors[!ZzZz])*(sum(!ZzZz)-1))/
                  (length(ZzZz)-2))
PpTy <- glmobject$linear.predictors/pooled.sd

attr(structure.fmla, 'generation.increment') <- 1

makedist(structure.fmla,
         data.frame(ZzZz, PpTy,model.frame(glmobject)),
         fn=function(trtvar,data)
         {
           sclr <- data[names(trtvar), 'PpTy']
           names(sclr) <- names(trtvar)
           abs(outer(sclr[trtvar], sclr[!trtvar], '-'))
         }
           )

}
