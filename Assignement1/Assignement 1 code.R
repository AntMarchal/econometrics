#Exercise 2
install.packages("rstudioapi")
install.packages("moments")
library(moments)
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
# Assuming you opened the script from the folder we gave you
setwd(dirname(current_path ))
set.seed(420)

#2.b
x <- seq(0, 7, 0.01)
exp1 <- dexp(x,1)
exp2 <- dexp(x, 2)
exp3 <- dexp(x, 3)
plot(x, exp3, type="l", col="blue",ylab="y",xlab="x",main="Exponential Density Plots",xlim=c(0,3))
lines(x, exp2, col="red",type="l")
lines(x, exp1, col="dark red",type="l")

#2.c
x <- rexp(1000,1)
h<-hist(x, breaks=50,col="blue", main="Histogram vs Theoretical Exponential",xlab="Values") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dexp(xfit,1) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="black", lwd=3, type="l")

#2.d
result_vect <- c()
for(i in 1:2000){
  x_k <- rexp(1000,2)
  z_k <- 2*sqrt(1000)*((sum(x_k)/1000)-0.5)
  result_vect <- c(result_vect, z_k)
}

h<-hist(result_vect, breaks=50,col="blue", main="Histogram vs Theoretical Normal",xlab="Values") 
xfit<-seq(min(result_vect),max(result_vect),length=40) 
yfit<-dnorm(xfit,0,1) 
yfit <- yfit*diff(h$mids[1:2])*length(result_vect) 
lines(xfit, yfit, col="black", lwd=3)

#Exercise3

#3.a, 3.b
#Assuming the data files are directly in the working directory, otherwise set the path manually
dataSP <- read.csv("^GSPC.csv")
dataAAPL <- read.csv("AAPL.csv")

data <- dataSP[,c(1,6)]
names(data)[2]<-paste("Adj.CloseGSPC")
data$Adj.CloseAAPL <- dataAAPL$Adj.Close

#3.c
data$Adj.CloseGSPC <- data$Adj.CloseGSPC/data[1,2]*100
data$Adj.CloseAAPL <- data$Adj.CloseAAPL/data[1,3]*100
plot(data$Adj.CloseAAPL, type="l", col="black",ylab="Normalized Return",xlab="Open Market Days",main="Adjusted Close Prices Renormalized")
lines(data$Adj.CloseGSPC, col="red",type="l")
##then click to put labels
##text(locator(), labels = c("APPL", "S&P500"))

#3.d
returnAAPL <- c()
returnGSPC <- c()
##check index
for(i in 2:(length(data$Adj.CloseAAPL))){
  returnAAPL <- c(returnAAPL,(data[i,3]-data[i-1,3])/data[i-1,3])
  returnGSPC <- c(returnGSPC,(data[i,2]-data[i-1,2])/data[i-1,2])
}
barplot(returnAAPL,main="Daily Returns of Apple Stock", ylab ="Daily Percentage Change", xlab="Open Market Days")
barplot(returnGSPC,main="Daily Returns of S&P500 Index", ylab ="Daily Percentage Change", xlab="Open Market Days")

#3.e
statsGSPC <- c(min(returnGSPC), max(returnGSPC), mean(returnGSPC), median(returnGSPC), var(returnGSPC), skewness(returnGSPC), kurtosis(returnGSPC))
statsAAPL <- c(min(returnAAPL), max(returnAAPL), mean(returnAAPL), median(returnAAPL), var(returnAAPL), skewness(returnAAPL), kurtosis(returnAAPL))
#3.f
plot(returnAAPL, returnGSPC, main="Scatterplot Stock Returns vs Index Returns", 
     xlab="AAPL Daily Returns ", ylab="S&P500 Daily Returns ", pch=19)
cor(returnAAPL,returnGSPC)