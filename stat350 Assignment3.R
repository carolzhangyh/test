#Q2.


#b

x1 <- c(4.49,3.04,3.94,2.63, 4.55, 3.88,2.92, 2.82, 3.17, 2.91)
x2 <- c(2.92,4.33,4.27,1.92, 2.47, 2.36, 3.21, 4.22, 1.80, 2.35)
y <- c(-5.32,-9.24, -5.89, 1.15, -1.47, 1.91, -3.99, -6.82, 1.49, -0.89)


fit1 <- lm(y ~ x1+x2)
 
anova(fit1)

X <- cbind(rep(1,length(x1)), x1, x2)
X


sigma_sq1 = 2  #sigma square =2 is given in the question
sigma_sq1*solve(t(X) %*% X) 


sigmasq_2 = 2.703  #ANOVA shown the estimate of MSE is 2.703
sigmasq_2*solve(t(X) %*% X) 

vcov(fit1)


#c


H = X%*%solve(t(X) %*% X)%*%t(X) 
hii<-diag(H) 
hii

sigma_sq1*(1-hii[c(1,3)])

2*(1-hii[1])
2*(1-hii[3])



#d

H[1,3]
-sigma_sq1*H[1,3]




# Q3

setwd("C:/Users/carol/Desktop/stat350")
mydata <- read.csv("data_computer_experiment.csv",header=TRUE)
mydata

head(mydata)

#a)

fit2  <- lm(location~. , data = mydata)

round(vcov(fit2),5)
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
plot(fit2,which=1)  ##i)constant variance ii)not normal not linear, outliers
plot(fit2,which=2)  ##iv)has outliers normal overall, bur has some outliers
plot(fit2,which=5)  ##v)not influential iii)no large leverage points cook's distance less than 0.5

plot(data$energy,resid(fit2))
plot(data$thickness,resid(fit2))
plot(data$flux,resid(fit2))  #flux is not linear, need to trans gfdvcx cvbfgedvcbn cendvfcbnghmyjform flux
plot(data$gamma,resid(fit2))
plot(data$opacity,resid(fit2))




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




mod1 <- lm(location ~. -flux,data=data )
resid.1 <- resid(mod1)
# x1, given x2
mod2 <- lm(flux ~. -location, data=data)
resid.2 <- resid(mod2)

plot(resid.2, resid.1, main='flux added plot')