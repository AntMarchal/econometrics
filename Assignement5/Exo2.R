#a)
X_T_X <- matrix(c(100,123,96,109,123,252,125,189,96,125,167,146,109,189,146,168), nrow=4,ncol=4)
y_T_y <- c(3924)
X_T_y <- c(460,810,615,712)
n <- X_T_X[1,1]

cov <- matrix(, nrow=3,ncol=3)

for( i in 1:nrow(cov)){
  for(j in 1:ncol(cov)){
    col_i_mean <- X_T_X[i+1,1]/n
    col_j_mean <- X_T_X[j+1,1]/n
    #cov[i,j]<- X_T_X[1+i,1+j]-(col_i_mean)*X_T_X[j+1,1]-(col_j_mean)*X_T_X[i+1,1]+n*col_i_mean*col_j_mean
    cov[i,j]<-(X_T_X[i+1,j+1]-n*col_i_mean*col_j_mean)/(sqrt(X_T_X[i+1,i+1]-n*col_i_mean*col_i_mean)*sqrt(X_T_X[j+1,j+1]-n*col_j_mean*col_j_mean))
  }
}

print(cov)
#b)
## high condition number
ei <- eigen(X_T_X)$values
#cond <- sqrt(max(ei)/min(ei))
compute_cond<- function(X){
  v <-eigen(X)$values
  sqrt(max(v)/min(v))
}
cond <- compute_cond(X_T_X)

print(cond)
drop_one_var <- which.min(c(compute_cond(X_T_X[-1,-1]),compute_cond(X_T_X[-2,-2]),compute_cond(X_T_X[-3,-3])))

#remove second variable or third one
corrected_X_T_X <- X_T_X[-drop_one_var,-drop_one_var]

#c)