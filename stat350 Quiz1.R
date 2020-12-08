
##Stat350 Quiz1

setwd("C:/Users/carol/Desktop/stat350")
mydata <- read.csv("snow.csv",header=TRUE)
mydata


##Q2
fit <- lm(Late ~ Early, data= mydata)
summary(fit)

##Q7
anova(fit)



##Q8 95% CI
confint(fit,"Early")



