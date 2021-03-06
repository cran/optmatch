################################################################################
# Tests for makedist function
################################################################################

context("compute_mahalanobis tests")

mahal.cov <- function(x, z) {
    mt <- cov(x[z, ,drop=FALSE]) * (sum(z) - 1) / (length(z) - 2)
    mc <- cov(x[!z, ,drop=FALSE]) * (sum(!z) - 1) / (length(!z) - 2)
    return(mt + mc)
}

trad.mahal <- function(index, x, z) {
    x.cov <- mahal.cov(x, z)
    ans <- apply(index, 1, function(pair)
        return(mahalanobis(x[pair[1], ], x[pair[2], ], x.cov)))
    return(sqrt(ans))
}

make.data <- function(k) {
    # generate some interesting but random vectors
    set.seed(20130811)
    mvrnorm_simple <- function(n, mu, Sigma) # based on MASS's `mvrnorm`
    {
    tol <- 1e-06
    p <- length(mu)
    if (!all(dim(Sigma) == c(p, p))) 
        stop("incompatible arguments")
    eS <- eigen(Sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -tol * abs(ev[1L]))) 
        stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n)
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
        t(X)
    nm <- names(mu)
    if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
        nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if (n == 1) 
        drop(X)
    else t(X)
        }

    x <- mvrnorm_simple(k, mu = c(-1, 0, 1),
                  Sigma = matrix(c(1, 0.25, 0.25,
                      0.25, 1, 0.25,
                      0.25, 1, 0.25), nrow = 3))
    rownames(x) <- paste('v', seq_len(nrow(x)), sep='')

    # top 10% assigned to treatment
    tmp <- rowSums(x)
    z <- vector("integer", k)
    z[order(tmp, decreasing = TRUE)[1:(0.1 * k)]] <- 1L
    z <- as.logical(z)

    # get treatment X control pairs into an index
    tns <- rownames(x)[z]
    nt <- length(tns)

    cns <- rownames(x)[!z]
    nc <- length(cns)

    t.coord <- rep(tns, nc)
    c.coord <- rep(cns, each = nt)

    index <- cbind(t.coord, c.coord)

    # return index, data, and z
    return(list(index = index, data = x, z = z))
}

test_that("Checking mahalanobis distance", {
    # random data set of size btw 100 and 500
    args <- make.data(sample(100:500, 1))

    expect_equal(
        compute_mahalanobis(args$index, args$data, args$z),
        trad.mahal(args$index, args$data, args$z))
})

test_that("#168, singleton treatment/control units", {

  index <- matrix(c("1", "1", "1", "2", "3", "4"), ncol = 2, byrow = FALSE)
  data <- matrix(as.numeric(1:4), ncol = 1, dimnames = list(as.character(1:4), "scores"))
  z <- c(TRUE, FALSE, FALSE, FALSE)
  names(z) <- as.character(1:4)

  t <- compute_mahalanobis(index, data, z)
  expect_length(t, 3)
  expect_false(any(is.na(t)))

  index <- matrix(c("2", "3", "4", "1", "1", "1"), ncol = 2, byrow = FALSE)
  data <- matrix(as.numeric(1:4), ncol = 1, dimnames = list(as.character(1:4), "scores"))
  z <- c(FALSE, TRUE, TRUE, TRUE)
  names(z) <- as.character(1:4)

  c <- compute_mahalanobis(index, data, z)
  expect_length(c, 3)
  expect_false(any(is.na(c)))

})
