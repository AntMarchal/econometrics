#a)
X_T_X <- matrix(c(100,123,96,109,123,252,125,189,96,125,167,146,109,189,146,168), nrow=4,ncol=4)
y_T_y <- c(3924)
X_T_y <- c(460,810,615,712)
n <- X_T_X[1,1]

cor <- matrix(, nrow=3,ncol=3)

for( i in 1:nrow(cor)){
  for(j in 1:ncol(cor)){
    col_i_mean <- X_T_X[i+1,1]/n
    col_j_mean <- X_T_X[j+1,1]/n

    cor[i,j]<-(X_T_X[i+1,j+1]-n*col_i_mean*col_j_mean)/(sqrt(X_T_X[i+1,i+1]-n*col_i_mean*col_i_mean)*sqrt(X_T_X[j+1,j+1]-n*col_j_mean*col_j_mean))
  }
}

print(cor)
#b)
## high condition number
ei <- eigen(X_T_X)$values

compute_cond<- function(X){
  v <-eigen(X)$values
  sqrt(max(v)/min(v))
}
cond <- compute_cond(X_T_X)

print(cond)


#c)
vifs <-c()
betas <- c()
for(i in 1:3){
  design_k <- solve(X_T_X[-(i+1),-(i+1)])
  val_Xt <- c(X_T_X[i+1,1],X_T_X[i+1,2],X_T_X[i+1,3],X_T_X[i+1,4])
  val_Xt <- val_Xt[-(i+1)]
  beta_k <- design_k %*% val_Xt
  betas <- cbind(betas,beta_k)
  
  above <-0
  below <-0
  if(i==3){
    above <- X_T_X[i+1,i+1]+t(beta_k)%*%X_T_X[-(i+1),-(i+1)]%*%beta_k-2*(beta_k[1]*X_T_X[1,i+1]+beta_k[2]*X_T_X[2,i+1]+beta_k[3]*X_T_X[3,i+1])
    below <- X_T_X[i+1,i+1]-X_T_X[1,i+1]*X_T_X[1,i+1]/n
  }
  else if(i==2){
    above <- X_T_X[i+1,i+1]+t(beta_k)%*%X_T_X[-(i+1),-(i+1)]%*%beta_k-2*(beta_k[1]*X_T_X[1,i+1]+beta_k[2]*X_T_X[2,i+1]+beta_k[3]*X_T_X[4,i+1])
    below <- X_T_X[i+1,i+1]-X_T_X[1,i+1]*X_T_X[1,i+1]/n
  }
  else{
    above <- X_T_X[i+1,i+1]+t(beta_k)%*%X_T_X[-(i+1),-(i+1)]%*%beta_k-2*(beta_k[1]*X_T_X[1,i+1]+beta_k[2]*X_T_X[3,i+1]+beta_k[3]*X_T_X[4,i+1])
    below <- X_T_X[i+1,i+1]-X_T_X[1,i+1]*X_T_X[1,i+1]/n
  }
  r_sq <- 1 - above/below
  vifs <- append(vifs, 1/(1-r_sq))
}

print(vifs)

##vif highest for 3, thus we remove it

##d)

beta1 <- solve(X_T_X)%*%X_T_y
##dropped covariate regression
beta_small <- solve(X_T_X[-4,-4])%*%X_T_y[-4]

print(beta1)
print(beta_small)
