0: very negative

1: negative

2: somewhat negative

3: somewhat positive

4: positive

5: very positive


descrCor <-  cor(concatenated_factors)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9)                     



# group by sentiment and data engineering #

library(dplyr)

sentimentIphone_5 <- dplyr::filter(iphone, iphonesentiment == 5 )
sentimentIphone_4 <- dplyr::filter(iphone, iphonesentiment == 4 )
sentimentIphone_3 <- dplyr::filter(iphone, iphonesentiment == 3 )
sentimentIphone_2 <- dplyr::filter(iphone, iphonesentiment == 2 )
sentimentIphone_1 <- dplyr::filter(iphone, iphonesentiment == 1 )

sentimentgalaxy_5 <- dplyr::filter(galaxy, galaxysentiment == 5)
sentimentgalaxy_4 <- dplyr::filter(galaxy, galaxysentiment == 4)
sentimentgalaxy_3 <- dplyr::filter(galaxy, galaxysentiment == 3)
sentimentgalaxy_2 <- dplyr::filter(galaxy, galaxysentiment == 2)
sentimentgalaxy_1 <- dplyr::filter(galaxy, galaxysentiment == 1)

corrplot(cor(sentimentgalaxy_1), method = “square”)

