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
    stop("Data is required for computing mdists from formulas")
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
