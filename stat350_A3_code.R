#Q2.


#b

x1 <- c(4.49,3.04,3.94,2.63, 4.55, 3.88,2.92, 2.82, 3.17, 2.91)
x2 <- c(2.92,4.33,4.27,1.92, 2.47, 2.36, 3.21, 4.22, 1.80, 2.35)
y <- c(-5.32,-9.24, -5.89, 1.15, -1.47, 1.91, -3.99, -6.82, 1.49, -0.89)


fit1 <- lm(y ~ x1+x2)
#anova(fit1)  # which would give an estimate of MSE = 2.073, but because sigmasq =2 is given, use 2 is better. 


X <- cbind(rep(1,length(x1)), x1, x2)
X


sigmasq = 2
sigmasq*solve(t(X) %*% X) 


#sigmasq2 = 2.703
#sigmasq2*solve(t(X) %*% X) 

vcov(fit1)


#c


H = X%*%solve(t(X) %*% X)%*%t(X) 
hii<-diag(H) 
hii

sigmasq1*(1-hii[c(1,3)])

2*(1-hii[1])
2*(1-hii[3])



#d

H[1,3]
-sigmasq1*H[1,3]




# Q3

setwd("C:/Users/carol/Desktop/stat350")
data <- read.csv("data_computer_experiment.csv",header=TRUE)
data
head(data)

#a)

fit2  <- lm(location~. , data = data)

round(vcov(fit2),4)
vcov(fit2)[2,2]

#summary(fit2)$coef[2,2]^2

#b)

vcov(fit2)[2,6]




#c)



# Model Adequacy Checking

summary(fit2)
resid(fit2)
rstandard(fit2)
rstudent(fit2)
plot(fit2,which=1) #i)constant variance non-linear iv)outlier
plot(fit2,which=2) #ii)The distribution is normal in general but some extrem outliers
plot(fit2,which=5) #v)no influential observation cooks distance is small smaller than 0.5
                  #iii)no leverage points
plot(fit2,which=4) #small cooks distance

plot(data$energy,resid(fit2))
plot(data$thickness,resid(fit2))
plot(data$flux,resid(fit2))
plot(data$gamma,resid(fit2))
plot(data$opacity,resid(fit2))

#vi)vi)everyone expect flux are linear, so we need to transform flux


residtable<-cbind(resid(fit2), rstandard(fit2), rstudent(fit2))
summary(residtable)

# very large outliers 



# Partial Plots 
# y, given x2
mod1 <- lm(location ~. -thickness,data=data )
resid.1 <- resid(mod1)
# x1, given x2
mod2 <- lm(thickness ~. -location, data=data)
resid.2 <- resid(mod2)

plot(resid.2, resid.1, main='thickness added plot') 
#linear relation




mod1 <- lm(location ~. -flux,data=data )
resid.1 <- resid(mod1)
# x1, given x2
mod2 <- lm(flux ~. -location, data=data)
resid.2 <- resid(mod2)

plot(resid.2, resid.1, main=' plot with flux', col=3)
#a little nonlinear