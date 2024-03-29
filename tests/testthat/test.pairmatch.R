################################################################################
# Pairmatch tests
################################################################################

context("pairmatch function")
source("utilities.R")

test_that("No cross strata matches", {
  # test data
  Z <- rep(c(0,1), 4)
  B <- rep(c(0,1), each = 4)
  distances <- 1 + exactMatch(Z ~ B)

  expect_warning(res <- pairmatch(distances))
  expect_false(any(is.na(res)))
  expect_false(any(res[1:4] %in% res[5:8]))

})

test_that("Remove unmatchables", {
  A <- matrix(c(1,1,Inf,1,1,Inf,1,1,Inf,1,1,Inf), nrow = 3)
  dimnames(A) <- list(1:3, 4:7)

  expect_warning({expect_true(all(is.na(pairmatch(A, remove.unmatchables = FALSE))))
    expect_true(!all(is.na(pairmatch(A, remove.unmatchables = TRUE))))
  })

  Ai <- as.InfinitySparseMatrix(A)
  expect_warning({expect_true(all(is.na(pairmatch(Ai, remove.unmatchables = FALSE))))
    expect_true(!all(is.na(pairmatch(Ai, remove.unmatchables = TRUE))))
  })

})

test_that("Omit fraction computed per subproblem", {

  # this is easiest to show using the nuclearplants data
  data(nuclearplants)

  psm <- glm(pr~.-(pr+cost), family=binomial(), data=nuclearplants)
  em <- exactMatch(pr ~ pt, data = nuclearplants)

  res.pm <- pairmatch(match_on(psm) + em, data=nuclearplants)

  expect_true(!all(is.na(res.pm)))

})

test_that("Compute omit fraction based on reachable treated units", {
  m <- matrix(c(1,Inf,3,Inf, Inf,
                1,2,Inf,Inf, Inf,
                1,1,Inf,3, Inf),
                byrow = TRUE,
                nrow = 3,
                dimnames = list(letters[1:3], LETTERS[22:26]))

  # Control unit Z is completely unreachable. Therefore it should not be a problem
  # to drop it.

  expect_warning(expect_true(!all(is.na(pairmatch(m[,1:4])))))

  ## 12/3/13: The below is no longer correct. Per issue #74, 0.4 is the correct omit.fraction,
  ## since subDivStrat now handles adjusting it for unmatchable controls automatically.
  # when the wrong omit.fraction value is computed both of these tests should fail
  # note: the correct omit.fraction to pass to fullmatch is 0.25
  # it is wrong to pass 0.4
  # and remove.unmatchables does not solve the problem
  # expect_true(!all(is.na(pairmatch(m))))
  # expect_true(!all(is.na(pairmatch(m, remove.unmatchables = TRUE))))

  ## 12/3/13: With the update, 0.4 is correct. Remove.unmatchables = TRUE should operate the same here
  expect_warning({p1 <- pairmatch(m)
    p2 <- pairmatch(m, remove.unmatchables=TRUE)
  })

  expect_true(sum(is.na(p1)) == 2)
  expect_true(all(p1==p2, na.rm=TRUE))
  attr(p1, "call") <- attr(p2, "call") <- NULL
  expect_true(identical(p1, p2))
})

test_that("Pass additional arguments to fullmatch", {
  df <- data.frame(z = rep(c(0,1), 5),
                   x = 1:10,
                   y = rnorm(10))
  df$w <- df$y + rnorm(10)
  rownames(df) <- letters[1:10][sample(1:10)]

  # mahal based ISM object
  m <- match_on(z ~ x + y + w, data = df)

  expect_warning(pairmatch(m), "data") # no 'data' argument

  pairmatch(m, data = df)

  # it is an error to pass any of the following: max.controls, min.controls.
  # omit.fraction
  expect_error(pairmatch(m, data = df, max.controls = 2),
               "Invalid argument\\(s\\) to pairmatch: max\\.controls")
  expect_error(pairmatch(m, data = df, min.controls = 2),
               "Invalid argument\\(s\\) to pairmatch: min\\.controls")
  expect_error(pairmatch(m, data = df, mean.controls = 2),
               "Invalid argument\\(s\\) to pairmatch: mean\\.controls")
  expect_error(pairmatch(m, data = df, omit.fraction = 2),
               "Invalid argument\\(s\\) to pairmatch: omit\\.fraction")

})

test_that("pairmatch UI cleanup", {
  n <- 14
  set.seed(124202)
  test.data <- data.frame(Z = rep(0:1, each = n/2),
                          X1 = rnorm(n, mean = 5),
                          X2 = rnorm(n, mean = -2, sd = 2),
                          B = rep(0:1, times = n/2))

  m <- match_on(Z ~ X1 + X2, data=test.data)

  pm.dist <- pairmatch(m, data=test.data)

  pm.form <- pairmatch(Z ~ X1 + X2, data=test.data)

  match_equal(pm.dist, pm.form)

  # with "with()"

  pm.with <- with(data=test.data, pairmatch(Z~X1 + X2))

  match_equal(pm.dist, pm.with)

  # passing a glm
  ps <- glm(Z~X1+X2, data=test.data, family=binomial)

  m <- match_on(ps, data=test.data, caliper=2.5)
  # one unmatchable treatment

  pm.ps <- pairmatch(ps, data=test.data, caliper=2.5, remove.unmatchables=TRUE)

  pm.match <- pairmatch(m, remove.unmatchables=TRUE, data=test.data)

  pm.glm <- pairmatch(glm(Z~X1+X2, data=test.data, family=binomial), data=test.data, caliper=2.5, remove.unmatchables=TRUE)

  match_equal(pm.ps, pm.glm)
  match_equal(pm.ps, pm.match)
  match_equal(pm.glm, pm.match)

  # passing inherited from glm

  class(ps) <- c("foo", class(ps))

  pm.foo <- pairmatch(ps, data=test.data, caliper=2.5, remove.unmatchables=TRUE)

  match_equal(pm.ps, pm.foo)

  # with scores

  ps <- glm(Z~X1, data=test.data, family=binomial)

  m <- match_on(Z ~ X2 + scores(ps), data=test.data)

  pm.dist <- pairmatch(m, data=test.data)

  pm.form <- pairmatch(Z~ X2 + scores(ps), data=test.data)

  match_equal(pm.dist, pm.form)

  # passing numeric

  X1 <- test.data$X1
  Z <- test.data$Z

  names(X1) <- row.names(test.data)
  names(Z) <- row.names(test.data)
  pm.vector <- pairmatch(X1,z=Z, data=test.data, caliper=2)
  expect_warning(pm.vector2 <- pairmatch(X1,z=Z, caliper=2))

  m <- match_on(X1, z=Z, caliper=2)
  pm.mi <- pairmatch(m, data=test.data)

  match_equal(pm.vector, pm.mi)
  rm(X1, Z)

  # function

  n <- 16
  test.data <- data.frame(Z = rep(0:1, each = n/2),
                          X1 = rep(1:4, each = n/4),
                          B = rep(0:1, times = n/2))

  sdiffs <- function(index, data, z) {
    abs(data[index[,1], "X1"] - data[index[,2], "X1"])
  }

  result.function <- match_on(sdiffs, z = test.data$Z, data = test.data)

  pm.funcres <- pairmatch(result.function, data=test.data)

  pm.func <- pairmatch(sdiffs, z = test.data$Z, data=test.data)
  expect_error(pairmatch(sdiffs, z = Z), "A data argument must be given when passing a function")

  match_equal(pm.funcres, pm.func)

  # passing bad arguments

  expect_error(pairmatch(test.data), "Invalid input, must be a potential argument to match_on")
  expect_error(pairmatch(TRUE), "Invalid input, must be a potential argument to match_on")


})

test_that("pairmatch warns when given a 'within' arg that it's going to ignore", {
    m <- matrix(1, nrow = 2, ncol = 3,
                dimnames = list(c("a", "b"), c('d', 'e', 'f')))
    B <- rep(1:3, each = 2)
    names(B) <- letters[1:6]
    em <- exactMatch(B, rep(c(0,1), 3))
    expect_warning(pairmatch(m, within=em), "gnor")
    expect_warning(pairmatch(as.InfinitySparseMatrix(m), within=em), "gnor")
})

test_that("NAs in irrelevant data slots don't trip us up", {
  n <- 16
  test.data <- data.frame(Z = rep(0:1, each = n/2),
                          X1 = rep(1:4, each = n/4),
                          B = rep(0:1, times = n/2))
  test.data$B[1] <- NA

  expect_equal(length(pairmatch(Z~X1, data=test.data)), n)

})

test_that("matched.distances attr removed per #57", {
  data(nuclearplants)

  p1 <- pairmatch(glm(pr ~ t1 + ne, data=nuclearplants, family=binomial),
                  within=exactMatch(pr ~ ne, data=nuclearplants),
                  data=nuclearplants)

  expect_true(is.null(attr(p1, "matched.distances")))

})

test_that("sane data arguments", {
  Z <- rep(c(0,1), 4)
  B <- rep(c(0,1), each = 4)
  distances <- 1 + exactMatch(Z ~ B)

  expect_warning(pairmatch(distances), "not guaranteed")
  # Issue #56
  expect_error(pairmatch(distances, data=distances), "are not found")
})

# Helper function for issue #123 - making sure NA's in
# treatment vector properly apply to match and propogate
# down to attributes
NA_checker <- function(match, NAvals) {
  expect_true(all(is.na(match[NAvals])))
  expect_true(all(!is.na(match[-NAvals])))
  for (attr in c("contrast.group", "subproblem")) {
    vals <- attr(match, attr)
    expect_true(all(is.na(vals[NAvals])))
    expect_true(all(!is.na(vals[-NAvals])))
  }
}

test_that("#123: Supporting NA's in treatment, pairmatch.formula", {
  data <- data.frame(z = rep(0:1, each = 5),
                     x = rnorm(10), fac=rep(c(rep("a",2), rep("b",3)),2) )
  p <- pairmatch(z ~ x, data = data)
  expect_true(all(!is.na(p)))
  p <- pairmatch(z ~ x + strata(fac), data = data)
  expect_true(all(!is.na(p)))

  # Now add an NA

  data$z[c(1, 6)] <- NA
  p <- pairmatch(z ~ x, data = data)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))

  p <- pairmatch(z ~ x + strata(fac), data = data)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))

})

test_that("#123: Supporting NA's in treatment, pairmatch.numeric", {
  z <- rep(0:1, each = 5)
  x <- rnorm(10)
  fac <- rep(c(rep("a",2), rep("b",3)),2)
  names(z) <- names(x) <- names(fac) <- 1:10
  expect_warning(p <- pairmatch(x, z = z))
  expect_true(all(!is.na(p)))
  expect_equal(length(p), length(z))

  data <- data.frame(z, x, fac)
  p2 <- pairmatch(x, z = z, data = data)
  expect_equivalent(p[sort(names(p))], p2[sort(names(p2))])

  em <- exactMatch(z~fac, data = data)
  p3 <- pairmatch(x, z = z, within=em, data=data)
  expect_true(all(!is.na(p3)))

  # Now add an NA

  z[c(1, 6)] <- NA
  expect_warning(p <- pairmatch(x, z = z))
  expect_true(all(!is.na(p)))
  expect_equal(length(p), length(z) - 2)
  expect_false("1" %in% names(p))
  expect_false("6" %in% names(p))
  cg <- attr(p, "contrast.group")
  expect_equal(length(cg), length(z) - 2)
  expect_false("1" %in% names(cg))
  expect_false("6" %in% names(cg))
  sp <- attr(p, "subproblem")
  expect_equal(length(sp), length(z) - 2)
  expect_false("1" %in% names(sp))
  expect_false("6" %in% names(sp))

  data <- data.frame(z, x, fac)
  p <- pairmatch(x, z = z, data = data)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))

  em <- exactMatch(z~fac, data = data)
  p <- pairmatch(x, z = z, within=em, data=data)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))

})

test_that("#123: Supporting NA's in treatment, pairmatch.function", {

  data <- data.frame(z = rep(0:1, each = 5),
                     x = rnorm(10))

  sdiffs <- function(index, data, z) {
    abs(data[index[,1], "x"] - data[index[,2], "x"])
  }

  p <- pairmatch(sdiffs, z = data$z, data = data)
  expect_equal(length(p), nrow(data))

  data$z[c(1,6)] <- NA

  p <- pairmatch(sdiffs, z = data$z, data = data)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))
})

test_that("#123: Supporting NA's in treatment, pairmatch.glm/bigglm", {

  data <- data.frame(z = rep(0:1, each = 5),
                     x = rnorm(10))

  mod <- glm(z ~ x, data = data, family = binomial)

  p <- pairmatch(mod)
  expect_equal(length(p), nrow(data))

  p2 <- pairmatch(mod, data = data)
  expect_equivalent(p, p2)

  data$z[c(1,6)] <- NA

  mod <- glm(z ~ x, data = data, family = binomial)

  p <- pairmatch(mod)
  expect_equal(length(p), nrow(data))
  NA_checker(p, c(1, 6))

  p2 <- pairmatch(mod, data = data)
  expect_equivalent(p, p2)

})

test_that("#116: If nt>nc, try creating nc pairs" , {
  data <- data.frame(z = c(rep(0, 2),rep(1,3)),
                     x = rnorm(5))
  p <- pairmatch(z ~ x, data = data)
  expect_false(all(is.na(p)))

  data <- data.frame(z = c(rep(0, 2),rep(1,2)),
                     x = rnorm(4))
  expect_error(p <- pairmatch(z~x, controls = 2, data = data))
})

test_that('Hints accepted',{
  set.seed(201905)
  data <- data.frame(z = rep(0:1, each = 5),
                     x = rnorm(10), fac=rep(c(rep("a",2), rep("b",3)),2) )
  mo  <- match_on(z ~ x, data=data)
  p1a <- pairmatch(mo, data = data, tol=0.1)
  expect_is(attr(p1a, "MCFSolutions"), "FullmatchMCFSolutions")
  expect_silent(pairmatch(mo, data = data, tol=0.0001, hint=p1a))
  mos <- match_on(z ~ x + strata(fac), data=data)
  p1b <- pairmatch(mos, data = data, tol=0.1)
  expect_is(attr(p1b, "MCFSolutions"), "FullmatchMCFSolutions")
  expect_warning(pairmatch(mos, data = data, tol=0.0001, hint=p1a) , "ignoring")
  expect_silent(pairmatch(mos, data = data, tol=0.0001, hint=p1b))
})
test_that("If matching fails, we should give a warning", {
  m <- match_on(pr ~ cost, data = nuclearplants)
  # One subproblem, matching fails
  expect_warning(pairmatch(m + caliper(m, .1), data = nuclearplants),
                 "Matching failed")
  # Multiple subproblems, some fail
  expect_warning(pairmatch(m + caliper(m, .1) + exactMatch(pr ~ pt, data = nuclearplants),
                           data = nuclearplants),
                 "subproblem matching failed")
  # Multiple subproblems, all fails
  expect_warning(pairmatch(m + caliper(m, .05) + exactMatch(pr ~ pt, data = nuclearplants),
                           data = nuclearplants),
                 "Matching failed")
})


test_that("LEMON solvers", {

  # While all solves give the same solution to this problem, this needn't be
  # true in all cases, so if this starts randomly erroring, as long as all
  # solvers give reasonable results, this test can be dropped.

  data("nuclearplants")
  p1 <- pairmatch(pr ~ cost + t1, data = nuclearplants)
  p2 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                  solver = "LEMON")
  p3 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                  solver = LEMON("CycleCancelling"))
  p4 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                  solver = LEMON("CapacityScaling"))
  p5 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                  solver = LEMON("CostScaling"))
  p6 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                  solver = LEMON("NetworkSimplex"))
  mytol <- .Machine$double.eps^(1/4)
  match_equivalent(p1, p2)
  match_equivalent(p1, p3)
  match_equivalent(p1, p4)
  match_equivalent(p1, p5)
  match_equivalent(p1, p6)

  if (requireNamespace("rrelaxiv", quietly = TRUE)) {
    p7 <- pairmatch(pr ~ cost + t1, data = nuclearplants,
                    solver = "RELAX-IV")
    match_equivalent(p1, p7)
  }


})
