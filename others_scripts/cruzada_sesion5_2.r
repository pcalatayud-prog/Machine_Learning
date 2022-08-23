#install.packages('ISLR')
library(ISLR)

mae<-function(obs,est){
  return(mean(abs(obs-est)))
}
mse <-function(obs,est){
  return(mean((obs-est)^2))
}

load('C:\\Users\\pcalatayud\\Desktop\\master\\data-mining\\datasets\\Pulsaciones.rda')
attach(Pulsaciones)
n <- length(Altura)

#--------- Modelo sin test. Se entrena con toda la muestra---------------------%
x11()
# Usamos las funciones comunes de R
plot(Altura,Peso,main='scaterplot de la altura y el peso', pch=19) #plot(x,y)
# Aniadimos la linea de ajuste
abline(lm(Peso~Altura), col="red") # regression line (y~x)

# Salida de la regresion
Reg.1<-lm(Peso~Altura) # regression model (y~x)
summary(Reg.1)
coeficientes.1<-coef(Reg.1)
residuos.1<-residuals(Reg.1)
# Pintamos el residuo
x11()
plot(residuos.1,type='l')

mean(residuos.1) # Residuos: media cero y varianza constante
var(residuos.1)

mean(Peso)
sd(Peso)


# Comparamos el valor real con el estimado
yest.1<-fitted(Reg.1)
mae.Reg.1<- mae(Peso,yest.1); 
mae.Reg.1 # Error de validacion
mse.Reg.1<-mse(Peso,yest.1); 
mse.Reg.1

x11()
plot(Peso,type='l')
lines(yest.1,col='red')

# Validation
#------------- HOLD OUT ------------------------%
# Tomamos primero el modelo y=cte, donde la cte es la media de la variable y
# para los elementos de train. Es decir para cualquier x, la prediccion de y
# es siempre la misma. En este caso entrenamos el modelo con la mitad de los datos
x11()
plot(Altura, Peso,main="hold out example")
train <- 1:ceiling(n/2)
order.index <- order(Peso)
Peso.sort <- Peso[order.index]
Altura.sort <- Altura[order.index]
points(Altura.sort[train], Peso.sort[train], pch=16, col="red")
mean.peso <- mean(Peso.sort[train]) #y.est=cte esa cte es la media de la variable y selecionada en train
abline(h=mean.peso)

mse.train <- mse(Peso.sort[train],mean.peso); mse.train

mse.test <- mse(Peso.sort[-train],mean.peso); mse.test # El error de test es mucho mayor ya que el modelo no generaliza. Mejor si cogemos los datos aleatoriamente.

# Repetimos el proceso con el mismo modelo y=cte pero cogiendo los datos de entrenamiento aleatoriamente
set.seed(1) # to obtain the same results for all the users when a R's random number generator is used (sample function in this case)
train <- sample(n,ceiling(n/2))
x11()
plot(Altura, Peso)
points(Altura[train], Peso[train], pch=16, col="red")
mean.peso <- mean(Peso[train]) #y.est=cte esa cte es la media de la variable y selecionada en train
abline(h=mean.peso)

mse.train <- mse(Peso[train],mean.peso); mse.train

mse.test <- mse(Peso[-train],mean.peso); mse.test
x11()
plot(Altura, Peso)
for (i in c(1:5)){
  train <- sample(n,ceiling(n/2))
  mean.peso <- mean(Peso[train]) #y.est=cte esa cte es la media de la variable y selecionada en train
  abline(h=mean.peso)
  print(mse(Peso[-train],mean.peso))
}

x11()
set.seed(1)
train <- sample(n,ceiling(n/2))
plot(Altura, Peso)
points(Altura[train], Peso[train], pch=16, col="red")


Reg.2<-lm(Peso~Altura, data=Pulsaciones, subset=train)
yest.2 <- predict(Reg.2, data.frame(Altura=Altura[-train]))
mse.Reg.2<-mse(Peso[-train],yest.2); mse.Reg.2 #If we choose different training set, the mse will be dif

yest.2.train <- predict(Reg.2, data.frame(Altura=Altura[train]))
mse.Reg.2.train<-mse(Peso[train],yest.2.train); mse.Reg.2.train

yest.3 <- rep(NA, length(train))
train <- 1:n
for (i in train){
  Reg.i<-lm(Peso~Altura, data=Pulsaciones, subset=train[-i])
  9
  yest.3[i]<-predict(Reg.i,data.frame(Altura=Altura[i]))
}
mse.Reg.3<-mse(Peso,yest.3); mse.Reg.3

#---------- K FOLD ----------------
# In this method, there is some variability in the cross validation estimates as a result of the
# variability in how the observations are divided into the k-folds. But this variability is typically much lower than
# the variability in the test error estimates that results from the validation set approach.
idx.aleatorios <- sample(1:n,n,replace=F)
K <- 10
tam <- ceiling(n/K)
yest4 = c()
for (i in 0:(K-1)){
  idx.test <- idx.aleatorios[(i*tam+1):((i+1)*tam)]
  idx.test <- idx.test[!is.na(idx.test)]
  lm4 <- lm(Peso~Altura, subset=-idx.test)
  yest4[idx.test] <- predict(lm4, data.frame(Altura=Altura[idx.test]))
}

mse4 <- mse(Peso,yest4); mse4

for (K in c(2,5,10)){
  for (r in c(1:5)){
    idx.aleatorios <- sample(1:n,n,replace=F)
    tam <- ceiling(n/K)
    yest4 = c()
    for (i in 0:(K-1)){
      idx.test <- idx.aleatorios[(i*tam+1):((i+1)*tam)]
      idx.test <- idx.test[!is.na(idx.test)]
      lm4 <- lm(Peso~Altura, subset=-idx.test)
      yest4[idx.test] <- predict(lm4, data.frame(Altura=Altura[idx.test]))
    }
    mse4 <- mse(Peso,yest4); print(paste0(K,", ", r,": ",mse4))
  }
}










