library(optmatch)
data(plantdist)

plantsfm <- fullmatch(plantdist) 
plantsfm1 <- fullmatch(plantdist,min.controls=2, max.controls=3)

stratumStructure(plantsfm)
stratumStructure(plantsfm1)

data(nuclearplants)
psd <- pscore.dist(glm(pr~.-(pr+cost), family=binomial(),
                       data=nuclearplants))
stratumStructure(fullmatch(psd))
stratumStructure(fullmatch(psd/(psd<.25)))
