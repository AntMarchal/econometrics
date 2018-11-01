install.packages("ggm")
library(ggm)

df <- read.csv("C:\\Users\\Ivan\\Desktop\\MA1\\Econometrics\\econometrics\\Assignement7\\US_Gasoline_Market.csv")

##Build y
CONS<-1e6*df$GASEXP/(df$GASP*df$POP)
y <- CONS

##add intercept
##Build design matrix X
df <- df[,c("INCOME","GASP","PNC","PUC","PPT","PD","PN","PS","YEAR")]
df <- cbind(rep(1,nrow(df)),df)
n <- nrow(df)

##get cols other than intercept
ps <- powerset(2:ncol(df))
perms<- vector("list", length(ps))

for(i in 1:length(ps)){
  perms[[i]]<-c(unlist(ps[i]))
}

getBeta <-function(y,X){
  solve(t(X)%*%X)%*%t(X)%*%y
}

rmses <- c()
for(p in perms){
  res <- c()
  ##get beta
  for(i in 1:n){
    design_X <- data.matrix(df[-i,c(1,p)])
    design_y <- y[-i]
    beta <- getBeta(design_y,design_X)
    pred_err <- ((y[i]-data.matrix(df[i,c(1,p)])%*%beta)**2)/n
    res <- append(res,pred_err)
  }
  rmses <- append(rmses, sqrt(sum(res)))
  ##append to rmse
}


table <- cbind(1:length(perms),rmses)
##full model,
full_model_rmse <-table[nrow(table),2]
table <- table[order(table[,2], decreasing = F),]

best <- table[1:5,]
worst <- table[(nrow(table)-5):nrow(table),]

##simple to general,

##general to simple,