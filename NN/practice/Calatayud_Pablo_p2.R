#library(keras)
#library(tensorflow)
#library(reticulate)
#install_tensorflow()
#install_keras()
#install_keras(tensorflow = "gpu")

load(file="C:\\Users\\pcalatayud\\Desktop\\master\\MLl\\practicas\\Madrid_Alumno.rda")


xTest_normalization <- function(train,test) {
  #Input:
  #train: train Set predictors
  #test: test set predictors  
  
  #Inicializamos la matriz de test
  
  xtest <- matrix(0,nrow(test),ncol(test))
  #sd y mean del xTrain para cada predictor
  sd1 <- apply(train,2,sd)
  mean1 <- apply(train,2,mean)
  
  for (i in 1:ncol(test)) {
    # escalamos el xTest
    # (x - mean(x)) / sd(x)
    xtest[,i] <- ((test[,i]-mean1[i])/sd1[i])
  }
  return(xtest)
  
  #Output:
  #xtest: conjunto de test escalado
}

#Escalamos xTest con la funcion anterior
xTest <- xTest_normalization(xTrain,xTest)

#Aplicamos la normalizacion al train set
xTrain <- apply(xTrain, 2, function(x) (scale(x)))

dimensiones_train=dim(xTrain)
dimensiones_test=dim(xTest)
cat("Train. Número de datos:",dimensiones_train[1])
cat("\n")
cat("Train. Número de Dimensiones:",dimensiones_train[2])
cat("\n")
cat("Test. Número de datos:",dimensiones_test[1])
cat("\n")
cat("Test. Número de Dimensiones:",dimensiones_test[2])

#--------------------- MODELO PARA CLASIFICACION ------------------------------#
yTrain_occ <- ifelse(yTrain<=1,0,1) #Convertir lluvia a binario (occurencia)


model_class <- keras_model_sequential() 

#Topologia tipo VAE

inputs = layer_input(shape = dimensiones_train[2])
#Ponemos el numero de columnas de mnist
x = inputs
l1 = layer_dense(x,units = 10, activation = "sigmoid")
l2a = layer_dense(l1,units = 20, activation = "sigmoid")
l2b = layer_dense(l1,units = 20)
l3 = layer_add(list(l2a,l2b))#combinacion lineal
l3_activated = layer_activation(l3,activation = "sigmoid")
outputs = layer_dense(l3_activated,units = 1, activation = "sigmoid")


# model <- keras_model(inputs = inputs, outputs = outputs)
model_class <- keras_model(inputs = inputs, outputs = outputs)


model_class %>% compile(loss = 'binary_crossentropy',
                  optimizer = optimizer_sgd(learning_rate = 0.01), 
                  metrics = 'accuracy')


callbacks = list(callback_early_stopping(patience = 15),
                 callback_model_checkpoint(filepath=paste0('model1_occ.h5'),
                 monitor='accuracy', save_best_only=TRUE))


history <- model_class %>% fit(xTrain, yTrain_occ, epochs = 10000, 
                         batch_size = 128, 
                         verbose = 1, callbacks=callbacks)   

plot(history)


# CONFUSION MATRIX
yTrain_occ_pred <- model_class$predict(xTrain)
yTrain_occ_pred <- ifelse(yTrain_occ_pred<=0.5,0,1) #Convertir sigmoide binario (occurencia)

a=table(as.factor(yTrain_occ_pred),as.factor(yTrain_occ))
cat("Train Confusion Matrix:")
cat("Accuracy Train Occurency: ",(sum(diag(a))) / length(yTrain_occ_pred))

yTest_occ <- model_class$predict(xTest)
yTest_occ <- ifelse(yTest_occ<=0.5,0,1) #Convertir sigmoide a binario (occurencia)


#---------------------- MODELO PARA PREDICCION --------------------------------#
model_pred <- keras_model_sequential() 
#Topologia tipo VAE
inputs = layer_input(shape = dimensiones_train[2])
x = inputs
l1 = layer_dense(x,units = 10, activation = "sigmoid")
l2a = layer_dense(l1,units = 20, activation = "sigmoid")
l2b = layer_dense(l1,units = 20)
l3 = layer_add(list(l2a,l2b))#combinacion lineal
l3_activated = layer_activation(l3,activation = "sigmoid")
outputs = layer_dense(l3_activated,units = 1, activation = "linear")

model_pred <- keras_model(inputs = inputs, outputs = outputs)

model_pred %>% compile(loss = 'mse',
                       optimizer = optimizer_sgd(learning_rate=0.01))

callbacks = list(callback_early_stopping(patience = 15),
                 callback_model_checkpoint(filepath=paste0('model1_reg.h5'),
                                           monitor='loss', save_best_only=TRUE))

history_pred <- model_pred %>% fit(xTrain, yTrain, epochs = 10000, 
                         batch_size = 128, 
                         verbose = 1, callbacks=callbacks)   

plot(history_pred)

yTest_reg_pred   <- model_pred$predict(xTest)
yTrain_reg_pred <- model_pred$predict(xTrain)


yTrain_reg <- as.numeric(yTrain_occ) * as.numeric(yTrain_reg_pred)
yTest_reg  <- as.numeric(yTest_occ) * as.numeric(yTest_reg_pred)

#save(yTest_ocu,yTest_reg,file="yTest.rda")


#--------------------------------- Resultados -------------------------------------#

#Ocurrencia binario
#---------------------------------
cat("Accuracy Train Occurency: ",(sum(diag(a))) / length(yTrain_occ_pred))
cat("tabla")
print(a)
#Lluvia regresion
#---------------------------------
rmse <- function(x, y) {
  stopifnot(length(x) == length(y))
  sqrt(mean((x - y)^2))
}

cat("Train precipitation prediction RMSE",rmse(yTrain_reg,yTrain))




