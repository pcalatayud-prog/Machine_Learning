#
dimensiones=dim(Pulsaciones)

n=dimensiones[1]

plot(Pulsaciones$Altura, Pulsaciones$Peso)
train <- 1:ceiling(n/2)
order.index <- order(Pulsaciones$Peso)
Peso.sort <- Pulsaciones$Peso[order.index]
Altura.sort <- Pulsaciones$Altura[order.index]
points(Altura.sort[train], Peso.sort[train], pch=16, col="red")
mean.peso <- mean(Peso.sort[train])
abline(h=mean.peso)

mse.train <- mse(Peso.sort[train],mean.peso); #mse.train
mse.test <- mse(Peso.sort[-train],mean.peso); #mse.test 
########### kfold


idx.aleatorios <- sample(1:n,n,replace=F)
K <- 10
tam <- ceiling(n/K)

yest4 <- rep(NA, length(train)) # La actualización es ineficiente

for (i in 0:(K-1)){
  idx.test <- idx.aleatorios[(i*tam+1):((i+1)*tam)]
  idx.test <- idx.test[!is.na(idx.test)]
  lm4 <- lm(Pulsaciones$Peso~Pulsaciones$Altura, subset=-idx.test)
  yest4[idx.test] <- predict(lm4, data.frame(Pulsaciones$Altura=Pulsaciones$Altura[idx.test]))
}

mse4 <- mse(Pulsaciones$Peso,yest4); 

#Si hacemos un 10-fold tenemos 10 modelos.

#ROC

library(pROC)
obs<-c(rep(0,50),rep(1,50));
prd<-obs+2*(runif(100)-0.5);
prd[which(prd<0)]<-0; prd[which(prd>1)]<-1;
plot(roc(obs,prd), print.auc=TRUE)
hist(prd)


