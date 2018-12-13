set.seed(123)

##e
##1
n <- 200
mu <- 1.2
sigma <- 0.8
X <- rnorm(n,mu,sqrt(sigma))
Y <- exp(X)

mu_hat  <- 2*log(mean(Y))-log(mean(Y**2))/2
sigma_hat  <- log(mean(Y**2))-2*log(mean(Y))


##2
##Define our function q to be optimized to get parameters
q <- function(Y,W,theta,n){
  ## get each mi
  m1 <- sum(Y)/n-exp(theta[1]+ theta[2]/2)
  m2 <- sum(Y**2)/n-exp(2*theta[1]+2*theta[2])
  m3 <- sum(1/Y)/n-exp(-theta[1] + theta[2]/2)
  m <- c(m1,m2,m3)
  return(t(m)%*%W%*%m)
}

##Define m_bar to get test statistics
m_bar <- function(Y,mu,sigma,n){
  m1 <- sum(Y)/n-exp(mu+ sigma/2)
  m2 <- sum(Y**2)/n-exp(2*mu+2*sigma)
  m3 <- sum(1/Y)/n-exp(-mu + sigma/2)
  m <- c(m1,m2,m3)
  return(m)
}

##Optimization routine which yields parameters
get_theta <- function(Y,W,n){
  return(optim(par = c(0,0),fn = q, n=n,W=W,Y=Y))
}

##Get inverse of phi
inv_phi <- function(Y,mu,sigma,n){
  phi <- matrix(rep(0,9),nrow=3, ncol=3)
  for(i in 1:n){
    m1 <- Y[i]-exp(mu+ sigma/2)
    m2 <- Y[i]**2-exp(2*mu+2*sigma)
    m3 <- 1/Y[i]-exp(-mu + sigma/2)
    m <- c(m1,m2,m3)
    phi <-phi + m %*% t(m)
  }
  phi <- phi/n
  return(solve(phi))
}

##Get first theta to get phi
theta1 <- get_theta(Y,diag(3),n)$par
##Get first inv_phi
inv_phi_1<- inv_phi(Y,theta1[1],theta1[2],n)
##Get new theta with updated phi
theta2 <- get_theta(Y,inv_phi_1,n)$par
##Get final inv_phi for overidentification test
inv_phi_2 <- inv_phi(Y,theta2[1],theta2[2],n)

##f
##We construct our test statistic:
test_stat <- n*m_bar(Y,theta2[1],theta2[2],n)%*%inv_phi_2%*%m_bar(Y,theta2[1],theta2[2],n)
crit_val <- qchisq (.95, df=3-2)
##We fail to reject the hypothesis

##g
##We have only one parameter now, so we change our q function to optimize
q_r <- function(Y,W,theta,n){
  ## get each mi
  m1 <- sum(Y)/n-exp(theta[1]+ theta[1]/2)
  m2 <- sum(Y**2)/n-exp(2*theta[1]+2*theta[1])
  m3 <- sum(1/Y)/n-exp(-theta[1] + theta[1]/2)
  m <- c(m1,m2,m3)
  return(t(m)%*%W%*%m)
}

get_theta2 <- function(Y,W,n){
  return(optim(par = c(0),fn = q_r, n=n,W=W,Y=Y))
}

mu1 <- get_theta2(Y,diag(3),n)$par
inv_phi_g_1 <- inv_phi(Y,mu1,mu1,n)
mu2 <- get_theta2(Y,inv_phi_g_1,n)$par
inv_phi_g_2 <- inv_phi(Y,mu2,mu2,n)

g_test_stat <- n*m_bar(Y,mu2,mu2,n)%*%inv_phi_g_2%*%m_bar(Y,mu2,mu2,n)
crit_val_g <- qchisq(.95,3-(2-1))
##We reject the null hypothesis, which is consistent since mu != sigma (in this simulated example)


##h
##Repeat number of experiment several times to get more stable values
number_exp=1000
##Augment number of samples for each experiment. This should yield better and better estimates
vect_n <- c(20,200,2000,20000)

aggr_mu_i <- c()
aggr_sigma_i <- c()
aggr_mu_opt <- c()
aggr_sigma_opt <- c()
for(i in 1:length(vect_n)){
  mu_i <- c()
  sigma_i <- c()
  mu_opt <- c()
  sigma_opt <- c()
  for(j in 1:number_exp){
    X <- rnorm(vect_n[i],mu,sqrt(sigma))
    Y <- exp(X)
    
    thetas_i <- get_theta(Y,diag(3),vect_n[i])$par
    inv_phi_mat<- inv_phi(Y,thetas_i[1],thetas_i[2],vect_n[i])
    thetas_opt <- get_theta(Y,inv_phi_mat,vect_n[i])$par
    
    mu_i <- append(mu_i, thetas_i[1])
    sigma_i <- append(sigma_i, thetas_i[2])
    mu_opt <- append(mu_opt, thetas_opt[1])
    sigma_opt <- append(sigma_opt, thetas_opt[2])
  }
  aggr_mu_i <- append(aggr_mu_i, mean(mu_i))
  aggr_sigma_i <- append(aggr_sigma_i, mean(sigma_i))
  aggr_mu_opt <- append(aggr_mu_opt, mean(mu_opt))
  aggr_sigma_opt <- append(aggr_sigma_opt, mean(sigma_opt))
}
