
plantdist <- matrix(nrow=7, ncol=19,byrow=TRUE,data=c(
28, 0, 3,22,14,30,17,28,26,28,20,22,23,26,21,18,34,40,28,
24, 3, 0,22,10,27,14,26,24,24,16,19,20,23,18,16,31,37,25,
10,18,14,18, 4,12, 6,11, 9,10,14,12, 6,14,22,10,16,22,28,
 7,28,24, 8,14, 2,10, 6,12, 0,24,22, 4,24,32,20,18,16,38,
17,20,16,32,18,26,20,18,12,24, 0, 2,20, 6, 8, 4,14,20,14,
20,31,28,35,20,29,22,20,14,26,12, 9,22, 5,15,12, 9,11,12,
14,32,29,30,18,24,17,16,10,22,12,10,17, 6,16,14, 4, 8,17),
dimnames=list(c("A","B","C","D","E","F","G"),
c("H","I","J","K","L","M","N","O","P","Q","R",
"S","T","U","V","W","X","Y","Z")))

plantsfm <- fullmatch(plantdist) # A full match with unrestricted
                                 # treatment-control balance
pr <- logical(26)
pr[match(dimnames(plantdist)[[1]], names(plantsfm))] <- TRUE

table(plantsfm,                        # treatment-control balance, 
ifelse(pr,'treated', 'control')) # by matched set

tapply(names(plantsfm),                # worst treatment-control 
plantsfm, FUN= function(x, dmat) {     # distances, by matched set
max(
    dmat[match(x, dimnames(dmat)[[1]]), 
         match(x, dimnames(dmat)[[2]])], 
    na.rm=TRUE )
}, dmat=plantdist)

plantsfm1 <- fullmatch(plantdist, # A full match with 
min.controls=2, max.controls=3)   # restrictions on matched sets' 
                                  # treatment-control balance

table(plantsfm1, ifelse(          # treatment-control balance
      pr,'treated','control'))    # is improved by restrictions

tapply(names(plantsfm1),                # but distances between
plantsfm1, FUN= function(x, dmat) {     # matched units increase
max(                                    # slightly
    dmat[match(x, dimnames(dmat)[[1]]), 
         match(x, dimnames(dmat)[[2]])], 
    na.rm=TRUE )
}, dmat=plantdist)

all(plantsfm1==fullmatch(           # an equivalent call to fullmatch()
t(plantdist),
min.controls=(1/3),
max.controls=(1/2)) )

plantsfm2 <- fullmatch(plantdist, # This use of omit.fraction
min.controls=2, max.controls=2,  # works as desired, but sometimes
omit.fraction=(5/19))            # less than the specified fraction
                                 # of the control group will be omitted.
table(plantsfm2,
ifelse(pr, 'treated', 'control'))

( maxdist <- tapply(names(plantsfm2), # worst treatment-control 
plantsfm2, FUN= function(x, dmat) {   # distances, by matched set
max(
    dmat[match(x, dimnames(dmat)[[1]]), 
         match(x, dimnames(dmat)[[2]])], 
    na.rm=TRUE )
}, dmat=plantdist) )


maxdist[                           # worst treatment-control distances,
suppressWarnings(                  # excluding unmatched units.
matched(names(maxdist))            # matched() is usually applied to values
) ]                                # of fullmatch(); this nonstandard usage
                                   # gives a warning, here suppressed.
