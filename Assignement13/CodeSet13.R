#Question f
data <- read.csv("C:\\Users\\Ivan\\Desktop\\MA1\\Econometrics\\Insurance.csv")

Y <- data$Y
X <- data$X
n <- nrow(data)

#Log likelihood under H1
log_under_H1 <- function(params)
{
  beta <-params[1]
  rho <- params[2]
  return(-1*sum(-rho*log(beta+X)+(rho-1)*log(Y)-Y*(beta+X)**(-1)-log(gamma(rho))))
}

#Log likelihood under H0
log_under_H0 <- function(beta)
{
  return(-1*(-sum(log(beta+X))-sum(Y*(beta+X)**(-1))))
}


#Estimates of parameters under H0
beta_H0 <- optim(par = c(1),fn =log_under_H0)$par
print(beta_H0)
#Estimates of parameters under H1
beta_H1<- optim(par = c(1,0.5),log_under_H1)$par[1]
rho_H1 <- optim(par = c(1,0.5),log_under_H1)$par[2]
print(beta_H1)
print(rho_H1)

#H0 covariance matrix estimation

# Method 1 covariance under H0
cov_method1_H0 <- 1/sum((beta_H0+X)**(-2))
print(cov_method1_H0)
#Method 2 covariance under H0
cov_method2_H0 <- -1/sum((beta_H0+X)**(-2)-2*Y*(X+beta_H0)**(-3))
print(cov_method2_H0)
#Method 3 covariance under H0
cov_method3_H0<- 1/sum((-(beta_H0+X)**(-1)+Y*(beta_H0+X)**(-2))**2)
print(cov_method3_H0)

#H1 covariance matrix estimation

# Method 1 H1
cov_method1_H1 <- function(rho,beta)
{
  I = matrix(0,nrow=2,ncol=2)
  I[1,1] = rho*sum((beta+X)**(-2))
  I[1,2] = sum((beta+X)**(-1))
  I[2,1] = I[1,2] 
  I[2,2] = n*trigamma(rho)
  solve(I)
}

print(cov_method1_H1(rho_H1,beta_H1))
#Method 2 H1
cov_method2_H1 <- function(rho,beta)
{
  I = matrix(0,nrow=2,ncol=2)
  I[1,1] = rho*sum((beta+X)**(-2))-2*sum(Y*(beta+X)**(-3))
  I[1,2] = -sum((beta+X)**(-1))
  I[2,1] = I[1,2] 
  I[2,2] = -n*trigamma(rho)
  -solve(I)
}
print(cov_method2_H1(rho_H1,beta_H1))
#Method 3 H1
cov_method3_H1 <- function(rho,beta)
{
  I = matrix(0,nrow=2,ncol=2)
  for(i in 1:n)
  {
    I[1,1] = I[1,1] +(- rho/(beta+X[i]) + Y[i]*(beta+X[i])**(-2))**(2)
    I[1,2] = I[1,2] +(-rho/(beta+X[i])+Y[i]*(beta+X[i])**(-2))*(-log(beta+X[i])+log(Y[i])-digamma(rho))
    I[2,1] = I[1,2]
    I[2,2] = I[2,2] + (-log(beta+X[i])+log(Y[i])-digamma(rho))**(2)
  }
  solve(I)
}
print(cov_method3_H1(rho_H1,beta_H1))

#Question g
crit_value<-qchisq(.95, df=1)
print(crit_value)

#Likelihood ratio test 
test<- 2*(-log_under_H1(c(beta_H1,rho_H1))+log_under_H1(c(beta_H0,1)))
print(test)


#Wald test with covariance matrix estimated from each method
W <- (rho_H1-1)**2
#M1
w1 <- W/cov_method1_H1(rho_H1,beta_H1)[2,2]
print(w1)
#M2
w2 <- W/cov_method2_H1(rho_H1,beta_H1)[2,2]
print(w2)
#M3
w3 <- W/cov_method3_H1(rho_H1,beta_H1)[2,2]
print(w3)

#Lagrange multiplier test with covariance matrix estimated from each method
g = matrix(0,nrow = 2, ncol = 1)
g[1] = sum(-(beta_H0+X)**(-1)+Y*(beta_H0+X)**(-2))
g[2] = sum(-log(beta_H0+X)+log(Y))-n*digamma(1)
#M1
m1 <- t(g)%*%cov_method1_H1(1,beta_H0)%*%g
print(m1)
#M2
m2 <- t(g)%*%cov_method2_H1(1,beta_H0)%*%g
print(m2)
#M3
m3 <- t(g)%*%cov_method3_H1(1,beta_H0)%*%g
print(m3)
