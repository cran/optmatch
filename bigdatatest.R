set.seed(20130424)
k <- 10000 # number of units in each of the treated and conrol groups

distances <- matrix(abs(rnorm(k * k)), nrow = k, ncol = k)

rownames(distances) <- paste("T", 1:k, sep = "")
colnames(distances) <- paste("C", 1:k, sep = "")

fullmatch(distances)
