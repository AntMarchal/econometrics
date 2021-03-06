## Econometrics -  Exercise Set 2
## Exercise 3
#=================================================================================================#
# Brodard Lionel, Giro Tomas, Marchal Antoine, Schonenberger Ivan                                                                                    #
#=================================================================================================#
install.packages("stargazer")
library(stargazer)
## Question a
## Parameters
alpha <- 0.5
beta_1 <- 0.8
beta_2 <- 1.3
real_beta <-rbind(alpha,beta_1,beta_2)
n <- 20
## Creation of the Matrix of X, the error term and calculation of Y
x_0 <- c(rep_len(1,n))
set.seed(12)
x_1 <- c(runif(n,0,10))
set.seed(1054)
x_2 <- c(runif(n,0,10))
set.seed(12)
X <- cbind(x_0,x_1,x_2)

eps <- c(rnorm(n,0,4))

Y <- c(rep_len(1,n))
Y <- alpha*x_0 + beta_1*x_1 + beta_2*x_2 + eps

print(cbind(Y,x_1,x_2)) # Print the 20 observations of y, x1,x2

## Question b
# Estimation of Beta
est_B <- matrix(0,1,3)
est_B<-solve(t(X)%*%X)%*%t(X)%*%Y
print(est_B)

## Question c
est_Y <- X%*%est_B #estimated Y
est_e <- Y-est_Y #residual error
# creation of equations that need to be tested
equa11<-t(est_e)%*%est_e
equa12 <- t(Y)%*%Y-t(est_B)%*%t(X)%*%Y-t(Y)%*%X%*%est_B+t(est_B)%*%t(X)%*%X%*%est_B
equa21<-t(est_B)%*%t(X)%*%Y
equa22<-t(Y)%*%X%*%est_B
# check if they are equal
all(round(equa11,3)==round(equa12,3))
all(round(equa21,3)==round(equa22,3))

## Question d
# find matrix M -> My=e -> M=(I-Xinv(X'X)X')
I <- diag(20)
M <- I-X%*%solve(t(X)%*%X)%*%t(X)
# round the matrix M and square it to be able to test idempotent
RoundedM=round(M,4)
RoundedM2=round(M%*%M,4)
# find matrix P -> P= Xinv(X'X)-X'
P <- X%*%solve(t(X)%*%X)%*%t(X)
# round the matrix P and square it to be able to test idempotent
RoundedP=round(P,4)
RoundedP2=round(P%*%P,4)
# to test if they are symmetric and idempotent
#symetry testing
all(RoundedM==t(RoundedM))
all(RoundedP==t(RoundedP))
#idempotent testing
all(RoundedM2==RoundedM)
all(RoundedP2==RoundedP)
message("Is the Matrix M symetric? ", all(RoundedM==t(RoundedM)))
message("Is the Matrix P symetric? ", all(RoundedP==t(RoundedP)))
message("Is the Matrix M idempotent? ", all(RoundedM2==RoundedM))
message("Is the Matrix P idempotent? ", all(RoundedP2==RoundedP))

## Question e
# partial model
px_0 <- x_0
px_1 <- x_1
pX <- cbind(px_0,px_1)
pest_B <- matrix(0,1,3)
pest_B <- solve(t(pX)%*%pX)%*%t(pX)%*%Y
pest_Y <- pX%*%pest_B
pest_e <- Y-pest_Y
print(pest_B)
# SSE calculation
fSSE <- t(est_e)%*%est_e
pSSE <- t(pest_e)%*%pest_e
print(fSSE)
print(pSSE)
SSE<- data.frame("Full Model SSE" =fSSE, "Partial Model SSE" =pSSE)
## Point f
# R2 and ajusted R2 calculation
# SSR calculation est_Y-avY
# compute average of y
mean_Y <- c(rep_len(mean(Y),n))
# fSSR <- 
fSSR <- sum((est_Y-mean_Y)^2)
# pSSR <-
pSSR <- sum((pest_Y-mean_Y)^2)
# SST calculation
# fSST <-
fSST <- sum((Y-mean_Y)^2)
# pSST <-
pSST <- sum((Y-mean_Y)^2)

# R squared
fR2 <- 1-(fSSE/fSST)
pR2 <- 1-(pSSE/pSST)
print(fR2)
print(pR2)

# Adjusted-R squared = 1 - ((1-R2)(n-1)/(n-k-1)), where k= number of independant regressor
fk <- 2
pk <- 1
fAR2 <- 1-((1-fR2)*(n-1)/(n-fk-1))
pAR2 <- 1-((1-pR2)*(n-1)/(n-pk-1))
print(fAR2)
print(pAR2)
R2 <- matrix(c(fR2,fAR2,pR2,pAR2),ncol=2,byrow=TRUE)
colnames(R2) <- c("Squared R","Adjusted Squared R")
rownames(R2) <- c("Full Model","Partial Model")
R2 <- as.table(R2)
R2
## Question g
# create a vector n for the 6 subsamples
n1<-10
n2<-20
n3<-50
n4<-100
n5<-250
n6<-500
n=cbind(n1,n2,n3,n4,n5,n6)

# generate a sample of X, errors of n6=500 and calculate the full model
X_500_0 <- c(rep_len(1,n6))
set.seed(12)
X_500_1 <- c(runif(n6,0,10))
set.seed(1054)
X_500_2 <- c(runif(n6,0,10))
X_500 <- cbind(X_500_0,X_500_1,X_500_2)
set.seed(12)
Neps <- c(rnorm(n6,0,4))
Y_new <- c(rep_len(1,n6))
Y_new <- alpha*X_500_0 + beta_1*X_500_1 + beta_2*X_500_2 + Neps

# create a loop to calculate the estimated beta for each subsample
estimated_beta_full <-matrix(1,nrow=3,ncol = 6)
ee <-c(rep(1,6))
for (i in 1:6){
  X<-cbind(rep(1,n[i]),X_500_1[1:n[i]],X_500_2[1:n[i]])
  estimated_beta_full[1:3,i]<-solve(t(X)%*%X)%*%t(X)%*%Y_new[1:n[i]]
  e<-Y_new[1:n[i]]-X%*%estimated_beta_full[1:3,i]
  ee[i]<-t(e)%*%e
}
print(estimated_beta_full)
stargazer(estimated_beta_full)
# generate a NEW sample of X, errors of n6=500 and calculate the full model
sec_X_500_0 <- c(rep_len(1,n6))
set.seed(14)
sec_X_500_1 <- c(runif(n6,0,10))
set.seed(27)
sec_X_500_2 <- c(runif(n6,0,10))
sec_X_500 <- cbind(sec_X_500_0,sec_X_500_1,sec_X_500_2)
set.seed(14)
sec_Neps <- c(rnorm(n6,0,4))
sec_Y_new <- c(rep_len(1,n6))
sec_Y_new <- alpha*sec_X_500_0 + beta_1*sec_X_500_1 + beta_2*sec_X_500_2 + sec_Neps

# create a loop to calculate the estimated beta for each subsample
sec_estimated_beta_full <-matrix(1,nrow=3,ncol = 6)
sec_ee <-c(rep(1,6))
for (i in 1:6){
  X<-cbind(rep(1,n[i]),sec_X_500_1[1:n[i]],sec_X_500_2[1:n[i]])
  sec_estimated_beta_full[1:3,i]<-solve(t(X)%*%X)%*%t(X)%*%sec_Y_new[1:n[i]]
  e<-sec_Y_new[1:n[i]]-X%*%sec_estimated_beta_full[1:3,i]
  ee[i]<-t(e)%*%e
}
print(sec_estimated_beta_full)
stargazer(sec_estimated_beta_full)