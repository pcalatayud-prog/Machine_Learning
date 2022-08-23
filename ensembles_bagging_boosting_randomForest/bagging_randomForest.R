install.packages("randomForest")

install.packages("adabag")

library(randomForest)


n=nrow(iris)


# train/test partition
indtrain = sample(1:n, round(0.75*n)) # indices for train
indtest = setdiff(1:n, indtrain) # indices for test

#? randomForest


# RF
rf = randomForest(Species ~., iris , subset = indtrain)
# RF configuration: no. of trees? no. of predictors
#considered at each node?
rf 

# OOB error out-of-bag
x11()
plot(rf$err.rate[, 1], type = "b",
     xlab = "no trees",
     ylab = "OBB error")
grid()


x11()
plot(rf$err.rate[1:50, 1], type = "b",
     xlab = "no trees",
     ylab = "OBB error")
grid()

#Parece claro que con 26  arboles el modelo funciona bien
#y el OBB se estabiliza