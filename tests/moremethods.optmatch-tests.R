library(optmatch)
data(plantdist)
plantsfm <- fullmatch(plantdist)
plantsfm[1:10]
attributes(plantsfm[1:10])
attributes(plantsfm[5:10,drop=TRUE])
plantsfm[1:26 <11]
attributes(plantsfm[1:26 <6])
attributes(plantsfm[1:26 <6,drop=TRUE])

plantsfm[5] <- "m.4"
plantsfm[1:5]
attributes(plantsfm)

plantsfm <- fullmatch(plantdist)
plantsfm[26:1]
attributes(plantsfm[26:1])

