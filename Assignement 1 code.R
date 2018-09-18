#Exercise 2
install.packages("moments")
library(moments)
#2.b
x <- seq(0, 7, length=1000)
exp1 <- dexp(x,1)
exp2 <- dexp(x, 2)
exp3 <- dexp(x, 3)
matplot(x,cbind(exp1,exp2,exp3),type="l")

#2.c
x <- rexp(1000,1)

h<-hist(x, breaks=50,col="blue", main="Historgram vs Theoretical",xlab="Values") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dexp(xfit,1) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="black", lwd=3)

#2.d

result_vect <- c()
##+1
for(i in 1:2001){
  x_k <- rexp(1000,2)
  z_k <- 2*sqrt(1000)*((sum(x_k)/1000)-0.5)
  result_vect <- c(result_vect, z_k)
}

h<-hist(result_vect, breaks=50,col="blue", main="Historgram vs Theoretical",xlab="Values") 
xfit<-seq(min(result_vect),max(result_vect),length=40) 
yfit<-dnorm(xfit,0,1) 
yfit <- yfit*diff(h$mids[1:2])*length(result_vect) 
lines(xfit, yfit, col="black", lwd=3)


#Exercise3

#3.a, 3.b
##relative path files
dataSP <- read.csv("C:\\Users\\Ivan\\Desktop\\MA1\\Econometrics\\^GSPC.csv")
dataAAPL <-read.csv("C:\\Users\\Ivan\\Desktop\\MA1\\Econometrics\\AAPL.csv")
data <- dataSP[,c(1,6)]
names(data)[2]<-paste("Adj.CloseGSPC")
data$Adj.CloseAAPL <- dataAAPL$Adj.Close

#3.c
data$Adj.CloseGSPC <- data$Adj.CloseGSPC/data[1,2]*100
data$Adj.CloseAAPL <- data$Adj.CloseAAPL/data[1,3]*100
##joli plot
matplot(cbind(data$Adj.CloseAAPL,data$Adj.CloseGSPC))

#3.d
returnAAPL <- c()
returnGSPC <- c()
##check index
for(i in 2:(length(data$Adj.CloseAAPL))){
  returnAAPL <- c(returnAAPL,(data[i,3]-data[i-1,3])/data[i-1,3])
  returnGSPC <- c(returnGSPC,(data[i,2]-data[i-1,2])/data[i-1,2])
}
barplot(returnAAPL)
barplot(returnGSPC)
#3.e
statsGSPC <- c(min(returnGSPC), max(returnGSPC), mean(returnGSPC), median(returnGSPC), var(returnGSPC), skewness(returnGSPC), kurtosis(returnGSPC))
statsAAPL <- c(min(returnAAPL), max(returnAAPL), mean(returnAAPL), median(returnAAPL), var(returnAAPL), skewness(returnAAPL), kurtosis(returnAAPL))
#3.f
##JOli
plot(returnAAPL, returnGSPC, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
cor(returnAAPL,returnGSPC)