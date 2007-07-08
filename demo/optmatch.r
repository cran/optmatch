data(plantdist)

cat("**A pair match**\n")
plantspm <- pairmatch(plantdist) 
stratumStructure(plantspm)

(plantspm.d <- matched.distances(plantspm,plantdist))
mean(unlist(plantspm.d))


cat("**A 1:2 match**\n")
plantstm <- pairmatch(plantdist,controls=2) 
stratumStructure(plantstm)
mean(unlist(matched.distances(plantstm,plantdist)))
unlist(lapply(matched.distances(plantstm,plantdist), max))

cat("**A full match**\n")
plantsfm <- fullmatch(plantdist) 
stratumStructure(plantsfm)
mean(unlist(matched.distances(plantsfm,plantdist)))

cat("**Full matching with restrictions**\n")
plantsfm1 <- fullmatch(plantdist, 
min.controls=2, max.controls=3)   
stratumStructure(plantsfm1)
mean(unlist(matched.distances(plantsfm1,plantdist)))

all(plantsfm1==fullmatch(
      t(plantdist),
      min.controls=(1/3),
      max.controls=(1/2)) )

cat("**Mahalanobis matching with propensity calipers**\n")

if (!match('nuclear',ls(),nomatch=0)) data(nuclear, package="boot")

psm <- glm(pr~.-(pr+cost), family=binomial(), data=nuclear)
psd <- pscore.dist(psm)
mhd <- mahal.dist(pr~.-(pr+cost), nuclear)
plantsfm2 <- fullmatch(mhd/(psd<=.25))

matched.distances(plantsfm2,psd)
matched.distances(plantsfm2,mhd)
