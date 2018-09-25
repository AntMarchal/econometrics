#EX3
#a)
L=20
X1<-numeric(L)
X2<-numeric(L)
Y<-numeric(L)
e<-numeric(L)
a=0.5
b1=0.8
b2=1.3

for(i in 1:L){
  e[i]<-rnorm(1,0,4)
  X1[i]<-runif(1,0,10)
  X2[i]<-runif(1,0,10)
  Y[i]<-a+b1*X1[i]+b2*X2[i]+e[i]
}
#b)
X<-matrix(c(X1,X2)
