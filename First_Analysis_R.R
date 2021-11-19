InsectSprays

with(InsectSprays, as.factor(spray))

par(mfcol=c(1,3))

library(lattice)

with(InsectSprays, densityplot(count))

with(InsectSprays, hist(count, breaks=10, lwd=2,col=rainbow(10), main=("Histogram of Count Against Spray")))

boxplot(count~spray, col=rainbow(7),data=InsectSprays, main="Boxplot of Insect Count by Spraytype")

summary(InsectSprays)

model1<-with(InsectSprays, lm(count~spray))

with(InsectSprays, model1)

summary(model1)

anva<-aov(sqrt(count)~spray, data=InsectSprays)
anva

summary(anva)


plot(anva)

termplot(anva)
