# Q4.

###a.
set.seed(789)

x_1 <- rnorm(n=200,mean=0,sd=2)
x_2 <- rnorm(200,0,2)
e <- rnorm(n=200,mean=0,sd=1)
y <- 1+2*x1+5*x2+ e

fit <- lm(y~x_1+x_2)
fit

###b.

summary(fit)
round(vcov(fit),5)


X <- cbind(rep(1,200),x1,x2)
X


sigma2=1 

round(vcov(fit),5)
round(sigma2*solve(t(X) %*% X),5)






SLR: 
  
  n=200
sigma2=1
varx1 = 2^2

Sxx1 = varx1*(n-1)
varbeta1 = sigma2/Sxx1
varbeta1




###c 

summary(fit)

summary(fit)$coef[2,4] < 0.05

anova(fit)

###d 

rejectH0 <- NULL

for (i in 1:1000){
  x_1 <- rnorm(n=200,mean=0,sd=2)
  x_2 <- rnorm(n=200,mean=0,sd=2)
  e <- rnorm(n=200,mean=0,sd=1)
  y <- 1+2*x1+5*x2+ e
  fit <- lm(y~x1+x2)
  rejectH0[i] = summary(fit)$coef[2,4]<0.05
}
  

rejectH0
mean(rejectH0)
sd(rejectH0)


