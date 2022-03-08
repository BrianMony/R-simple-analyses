attach (InsectSprays)

#Viewing the data set so as to know how best to proceed

head(InsectSprays)

#Inspecting the data to check its cleanliness and analyzability

str(InsectSprays)

#Obtaining the dataset's visual descriptives

par(mfcol=c(1,3))

library(lattice)

library(tidyverse)

library(ggplot2)


densityplot <- InsectSprays %>% ggplot(aes(x=count)) +
  geom_density(fill='blue', alpha=0.5) +
  labs(title='A Densityplot Showing the Distribution of Sprays', x='Spray Count')

densityplot

hist <- InsectSprays %>% ggplot(aes(x= count)) +
  geom_histogram(aes(y=..density..), fill = 'blue', alpha=0.5, col='black',bins = 10) +
  labs(title = 'A Histogram Ascertaining the Distribution Sprays', x='Spray Count') + 
  geom_density(lwd=1.5, col='red')

hist

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
