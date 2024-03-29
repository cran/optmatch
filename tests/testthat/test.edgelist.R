context("edgelist functions")

test_that("Basic", {
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    ## ISM method
    ism.el <- edgelist(ism, c(rns, cns))

    expect_is(ism.el, "EdgeList")
    expect_equal(dim(ism.el),
                 c(length(ism.el@.Data[[1]]), length(ism.el@.Data)))
    expect_equal(subset(ism.el, i == "TU" & j == "C3")$dist, 7)

    ## matrix method
    m <- as.matrix(ism)
    m.el <- edgelist(m, c(rns, cns))

    expect_is(m.el, "EdgeList")
    expect_equal(subset(m.el, i == "TU" & j == "C3")$dist, 7)
    ## EdgeList method
    expect_identical(edgelist(m.el, c(rns, cns)), m.el)

    ## data frame method (i.e. an EdgeList that had its S4 class dropped)
    expect_false(is(dplyr::filter(m.el, TRUE), "EdgeList"))
    expect_is(dplyr::filter(m.el, TRUE), "data.frame")
    expect_identical(edgelist(dplyr::filter(m.el, TRUE)), m.el)

    ## error on other classes
    expect_error(edgelist(numeric(1), c(rns, cns)), "Not implemented")
})

test_that("EdgeList type is preserved on replacement of column entries",{
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    ## ISM method
    ism.el <- edgelist(ism, c(rns, cns))

    expect_is(ism.el, "EdgeList")
    expect_equal(ism.el$'dist', 1L:7L)
    ism.el$'dist'  <- ism.el$'dist' + 0.5
    expect_is(ism.el, "EdgeList")
})
test_that("default levels",{
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    ism.el <- edgelist(ism, c(rns, cns))
    expect_identical(edgelist(ism), ism.el)


    m <- as.matrix(ism)
    m.el <- edgelist(m, c(rns, cns))
    expect_identical(edgelist(m), m.el)

    el  <- edgelist(ism.el, c(rns, cns))
    expect_identical(edgelist(ism.el), el)

    df  <- asS3(ism.el)
    df.el  <- edgelist(df, c(rns, cns))
    expect_identical(edgelist(df), df.el)
})
test_that("revise levels set",{
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    levs_crosswalk  <- setNames(tolower(c(cns, rns)), nm=c(cns, rns))
    ism.el.r  <- edgelist(ism, levs_crosswalk)
    expect_is(ism.el.r, "EdgeList")
    expect_setequal(levels(ism.el.r$"i"), tolower(c(cns, rns)))

    m <- as.matrix(ism)
    m.el.r  <- edgelist(m, levs_crosswalk)
    expect_is(m.el.r, "EdgeList")
    expect_setequal(levels(m.el.r$"i"), tolower(c(cns, rns)))

    el  <- edgelist(ism)
    el.el.r  <- edgelist(el, levs_crosswalk)
    expect_is(el.el.r, "EdgeList")
    expect_setequal(levels(el.el.r$"i"), tolower(c(cns, rns)))

    df  <- asS3(el)
    df.el.r  <- edgelist(df, levs_crosswalk)    
    expect_is(df.el.r, "EdgeList")
    expect_setequal(levels(df.el.r$"i"), tolower(c(cns, rns)))
})

test_that("remove edges to or from nodes not appearing in y",{
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    ## ISM method
    ism.el <- edgelist(ism, c(rns[-2], cns))
    expect_equal(nrow(ism.el), length(ism)-2)
    ## verify no scrambling of values:
    expect_equal(dplyr::filter(ism.el, i == "TU" & j == "C3")$dist, 7)

    ## matrix method
    m <- as.matrix(ism)
    m.el <- edgelist(m, c(rns[-2], cns))
    expect_equal(nrow(m.el), length(ism)-2)
    ## verify no scrambling of values:
    expect_equal(dplyr::filter(m.el, i == "TU" & j == "C3")$dist, 7)

    expect_is(m.el, "EdgeList")
    expect_equal(dplyr::filter(m.el, i == "TU" & j == "C3")$dist, 7)
    ## EdgeList method
    expect_identical(edgelist(m.el, c(rns[-2], cns)), m.el)

    ## data frame method (i.e. an EdgeList that had its S4 class dropped)
    expect_false(is(dplyr::filter(m.el, TRUE), "EdgeList"))
    expect_is(dplyr::filter(m.el, TRUE), "data.frame")
    expect_equivalent(edgelist(dplyr::filter(m.el, TRUE), c(rns[-2], cns)), m.el)
})

context("is_matchable()")
test_that("EdgeList method", {
    cns <- c("C1", "C2", "C3", "C4")
    rns <- c("TZ", "TY", "TX", "TW", "TU")
    ism <- makeInfinitySparseMatrix(1:7,
                                    cols = c(1, 2, 1, 2, 3, 1, 3),
                                    rows = c(2, 2, 3, 3, 3, 5, 5),
                                    colnames = cns,
                                    rownames = rns)
    ism.el  <- edgelist(ism)
    expect_silent(matchable_rows  <- is_matchable(rns, ism.el, "rows"))
    expect_is(matchable_rows, "logical")
    unmatchable_rows  <- !matchable_rows
    expect_equal(rns[unmatchable_rows], c("TZ", "TW"))

    expect_silent(unmatchable_rows_e  <- !is_matchable(rns, ism.el, "either"))
    expect_equal(rns[unmatchable_rows_e], c("TZ", "TW"))

    expect_silent(unmatchable_cols  <- !is_matchable(cns, ism.el, "cols"))
    expect_equal(cns[unmatchable_cols], c("C4"))
    expect_silent(unmatchable_cols_e  <- !is_matchable(cns, ism.el, "either"))
    expect_equal(cns[unmatchable_cols_e], c("C4"))

    expect_silent(unmatchable_either  <- !is_matchable(c(rns, cns), ism.el, "either"))
    expect_equal(c(rns,cns)[unmatchable_either], c("TZ", "TW","C4"))
})
