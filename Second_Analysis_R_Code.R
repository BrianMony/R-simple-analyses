#Loading Age and Weight Data into R's Working Memory

alpha<-read.table("C:\\Users\\User\\Desktop\\One.csv", header=TRUE, sep=(","), strip.white =TRUE)

alpha

#Seeking Descriptive and Graphical Statistics of the Data frame Alpha

summary(alpha)

attributes(alpha)

#Drawing up the Scatter plot of Weight against Age 

library(lattice)

xyplot(WEIGHT~AGE, data=alpha, col="blue", main="Scatterplot of Weight against Age",auto.key =(list(x=0.5, y=0.5)))

#Drawing up the Histograms of Weight and Age Side by Side for Comparison Purposes

par(mfrow=c(1,2))

with(alpha, hist(WEIGHT, breaks=21, col=rainbow(7), main="A Histogram Suggestive of WEIGHT's Distribution"))

with(alpha, hist(AGE, breaks=21, col=rainbow(7), main="A Histogram Suggestive of AGE's Distribution"))

#Drawing up the Densityplots of Weight and Age Side by Side for Comparison Purposes

par(mfrow=c(1,2))

with(alpha, densityplot(WEIGHT, col=rainbow(7), lwd=2, main="A Densityplot of Weight"))

with(alpha, densityplot(AGE, col=rainbow(7), lwd=2, main="A Densityplot of Weight"))

#Drawing up the Boxplots of Weight and Age Side by Side for Comparison Purposes

par(mfrow=c(1,2))

with(alpha, boxplot(WEIGHT, col="blue", main="A Boxplot of Weight"))

with(alpha, boxplot(AGE, col="red", main="A Boxplot of Age"))

#linear Modelling Weight Against Age

mod1<-lm(WEIGHT~AGE, data=alpha)

mod1

#Obtaining Information from the Above Linear Model

summary(mod1)

#Results from the above model indicate a slight positive correlation between a person's weight and his/her age

#The Adjusted R=squared suggests a correlation coefficient of 0.561. This figure implies that 56.1% of changes in weight is explained by changes in a person's age

#Thus, the resulting predictive model is: WEIGHT= 65.1504 +2.7055*AGE

