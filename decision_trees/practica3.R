# install.packages("tree")
library(tree)

#Selecting two attributes to visualize the tree
ir <- iris[,c(3,4,5)]; # Petal.Length and Petal.Width
#coge las columnas 3 4 5
tree <- tree(Species ~., ir)
plot(tree)
#dibuja las variables

x11()
partition.tree(tree)
plot(ir[, 1],ir[, 2], type="n",
     xlab="petal length", ylab="petal width")
text(ir[, 1], ir[, 2], c("s", "c", "v")[ir[, 3]])
partition.tree(tree, add = TRUE, cex = 1.5)

#la setosa siempre queda por esa zona. Y no hay ninguna versicolor o virginica
#Tal y como esta la muestra no es necesario ahcer mas subdivisiones
#En el caos de setosa es un nodo terminal

#Ahora tendremos que intentar separar versicolor y virginica

#Y eso es lo que se consigue con el arbol de clasificacion

#El indentado en el output indica el camino

pt <- prune.tree(tree);
x11()
plot(pt)

#La primera informacion que nos aporta el arbol es que la temperatura no tiene#
#la suficiente capacidad de discriminacion como para imponerse al resto de 
#variables

#El principal problema de este algoritmo es el OVERFITING

tennis = read.csv("C:\\Users\\pcalatayud\\Desktop\\master\\data-mining\\datasets\\playingTennis.csv")

install.packages("C50")
library("C50")

t = C5.0(formula = "Adecuado"  ~ .,data = tennis)
#plot the tree
plot(t)

summary(t)


