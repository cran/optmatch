mdist <- function(x, structure.fmla = NULL, ...) {
  UseMethod("mdist", x)
}

# mdist method: optmatch.dlist
mdist.optmatch.dlist <- function(x, structure.fmla = NULL, ...) {
  return(x)  
} # just return the argument

# mdist method: function
# for the function method, both data and structure fmla are required
# api change from makedist: I think it would make more sense to have
# the function take two data.frames: the treatments in this stratum
# and the controls in the stratum. It could then return the matrix of
# mdists, which the rest of the function would markup with rownames
# etc.
mdist.function <- function(x, structure.fmla = NULL, data = NULL, ...) {
  
  if (is.null(data) || is.null(structure.fmla)) {
    stop("Both data and the structure formula are required for
    computing mdists from functions.")  
  }
  
  theFun <- match.fun(x)
  parsedFmla <- parseFmla(structure.fmla)
  
  if(is.null(parsedFmla[[1]])) {
    stop("Treatment variable required")  
  }
  
  if((identical(parsedFmla[[2]], 1) && is.null(parsedFmla[3])) ||
     (length(parsedFmla[[2]]) > 1)) {
    stop("Please specify the grouping as either: z ~ grp or z ~ 1 | grp")  
  }

  treatmentvar <- parsedFmla[[1]]

  if(is.null(parsedFmla[3][[1]])) { # I swear subscripting is incomprehensible!
    strata <- parsedFmla[[2]]
  } else {
    strata <- parsedFmla[[3]]
  }


  # split up the dataset by parts
  # call optmatch.dlist maker function on parts, names
  
    # create a function to produce one distance matrix
  doit <- function(data) {
     # indicies are created per chunk to split out treatment and controls
     indices <- data[as.character(treatmentvar)] == 1
     treatments <- data[indices,]
     controls <- data[!indices,]
     distances <- theFun(treatments, controls, ...)

     colnames(distances) <- rownames(controls)
     rownames(distances) <- rownames(treatments)
     
     return(distances)
  }

  if (!(identical(strata, 1))) {
    ss <- factor(eval(strata, data), labels = 'm')
    return(lapply(split(data, ss), doit))  
  } else {
    return(doit(data))
  }
}


# mdist method: formula
mdist.formula <- function(x, structure.fmla = NULL, data = NULL, ...) {
  
  if (is.null(data)) { 
    stop("data argument is required for computing mdists from formulas")
  }

  if (is.null(structure.fmla)) {
    # we might need to parse x to get the structure
    parsed <- parseFmla(x)

    # this block occurs if the grouping factor is present
    # e.g. z ~ x1 + x2 | grp
    if (!is.null(unlist(parsed[3]))) {
      x <- as.formula(paste(as.character(parsed[1:2]), collapse = "~"))
      structure.fmla <- as.formula(paste("~", parsed[[3]]))
    }
  }
  mahal.dist(x, data = data, structure.fmla = structure.fmla, ...)
}

# mdist method: glm
mdist.glm <- function(x, structure.fmla = NULL, ...) {
  pscore.dist(x,  structure.fmla = structure.fmla, standardization.scale=mad, ...)
}

# parsing formulas for creating mdists
parseFmla <- function(fmla) {

  treatment <- fmla[[2]]
  rhs <- fmla[[3]]
  if (length(rhs) == 3 && rhs[[1]] == as.name("|")) {
    covar <- rhs[[2]]
    group <- rhs[[3]]

  } else {
    covar <- rhs
    group <- NULL 
  }
  
  return(c(treatment, covar, group))

}

# mdist method: bigglm
mdist.bigglm <- function(x, structure.fmla = NULL, data = NULL, ...)
{
  if (is.null(data)) 
    stop("data argument is required for computing mdists from bigglms")
  
  if (!is.data.frame(data))
    stop("mdist doesn't understand data arguments that aren't data frames.")
  
  if (is.null(structure.fmla) | !inherits(structure.fmla, 'formula'))
    stop("structure.fmla argument required with bigglms.
(Use form 'structure.fmla=<treatment.variable> ~ 1'
 for no stratification before matching)")

theps <- predict(x, data, type='link', se.fit=FALSE)
if (length(theps)!=dim(data)[1])
stop("predict.bigglm() returns a vector of the wrong length;
are there missing values in data?")


Data <-  model.frame(structure.fmla, data=data)
treatmentvar <- as.character(structure.fmla[[2]])
pooled.sd <- szn.scale(theps, Data[[treatmentvar]],...)

Data$tHePs <- theps/pooled.sd

psdiffs <- function(treatments, controls) {
abs(outer(as.vector(treatments$tHePs),
as.vector(controls$tHePs), `-`))
}

mdist(psdiffs, structure.fmla=structure.fmla,
      data=Data)
}

### mdist method: numeric.
### (mdist can't work with numeric vectors at present,
### but it can return an informative error message).

mdist.numeric <- function(x, structure.fmla = NULL, trtgrp=NULL, ...)
{

  stop("No mdist method for numerics. 
  Consider using mdist(z ~ ps | strata, data = your.data) 
  where ps is your numeric vector, z is your treatment assignment, 
  and strata (optional) indicates a stratification variable, all 
  columns in your.data")

###  if (is.null(trtgrp))
###    stop("Can't turn a numeric vector alone into a dist; give a trtgrp= argument also.")
}
