# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

mahalanobisHelper <- function(data, index, invScaleMat) {
    .Call(`_optmatch_mahalanobisHelper`, data, index, invScaleMat)
}

ismOps <- function(o, a, b) {
    .Call(`_optmatch_ismOps`, o, a, b)
}

r_smahal <- function(index, data, z) {
    .Call(`_optmatch_r_smahal`, index, data, z)
}

subsetInfSparseMatrix <- function(whichRows, whichCols, ismX) {
    .Call(`_optmatch_subsetInfSparseMatrix`, whichRows, whichCols, ismX)
}

