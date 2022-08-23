#install.pacakges("fields")
#install.packages("spam")
#install.packages("dotCall64")
#install.packages("grid")

#install.packages("viridis")
#install.packages("viridisLite")

library(fields)
library(spam)
library(dotCall64)
library(grid)
library(viridis)
library(viridisLite)
###############
## mushrooms ##
###############
data=read.csv("C:\\Users\\pcalatayud\\Desktop\\master\\data-mining\\datasets\\mushrooms.csv")
str(data)

# 1) Crea un gráfico de barras que muestre el porcentaje de setas de cada color que hay en el dataset

colores=as.factor(data$cap.color) #colores

col = levels(colores)  # colores
nobs.col = c()  # inicializo vector vacío
for (icol in col) {
  nobs.col = c(nobs.col, sum(data$cap.color == icol))
}
df = 100*nobs.col/nrow(data); names(df) = col  # creo data.frame con porcentaje
barplot(df)  # barplot

# 2) Crea un p-color que muestre, para cada color, el número de setas que hay para cada tipo de forma
sh = levels(data$cap.shape)  # formas
nobs.col.sh = matrix(NA, nrow = length(col), ncol = length(sh))  # inicializo matriz vacía
for (icol in col) {  # bucle en colores
  indcol = which(data$cap.color == icol)
  data.col = data$cap.shape[indcol]
  for (ish in sh) {  # bucle en formas
    nobs.col.sh[which(col == icol), which(sh == ish)] = sum(data.col == ish)
  }
}
rownames(nobs.col.sh) = col  # nombro filas matriz
colnames(nobs.col.sh) = sh  # nombro columnas matriz

image.plot(1:length(col), 1:length(sh), nobs.col.sh,   # p-color
           xaxt = "n", yaxt = "n",
           xlab = "color", ylab = "shape",
           col = terrain.colors(20))
axis(1, at = 1:length(col), labels = col)  # etiquetas eje x
axis(2, at = 1:length(sh), labels = sh)  # etiquetas eje y

############
## fruits ##
############
data = read.csv("C:\\Users\\pcalatayud\\Desktop\\master\\data-mining\\datasets\\fruits.txt", sep = "")

# 1) Crea dos barplots que muestren la altura y anchura media para cada tipo de fruta
fruit = levels(factor(data$fruit_name))  # tipos de fruta
width.fruit = c()  # inicializo vector vacío
height.fruit = c()  # inicializo vector vacío
for (ifruit in fruit) {
  indfruit = which(data$fruit_name == ifruit)
  width.fruit = c(width.fruit, mean(data$width[indfruit]))
  height.fruit = c(height.fruit, mean(data$height[indfruit]))
}
names(width.fruit) = fruit
names(height.fruit) = fruit
x11()
par(mfrow = c(1, 2))  # para crear una matriz (1 fila y 2 columnas) de gráficos
barplot(width.fruit, main = "Width", ylab = "cm")  # barplot anchura
barplot(height.fruit, main = "Height", ylab = "cm")  #  barplot altura

x11()
plot(data$width,data$height, col=(factor(data$fruit_subtype)))


###########
## meteo ##
###########
data = read.csv("/home/rodrigo/work/DOCENCIA/2021-2022/M1966_mineria_datos_data_science/datasets/meteo.csv")
str(data)

# 1) Dibuja la serie temporal de precipitación diaria en Lisboa
plot(data$y, type = "l", main = "Precipitation in Lisbon",
     xlab = "days", ylab = "mm")
grid()

# 2) Dibuja el valor medio de cada variable predictora
x = data[, -c(1:2)]
plot(apply(x, 2, "mean", na.rm = T),
     pch = 20, xlab = "variable", ylab = "valor")
grid()

# 3) Calcula la correlación entre cada variable predictora y la variable objetivo (precipitación en Lisboa). Señala las correlaciones cuyo valor absoluto sea mayor que 0.4
nvar = ncol(x)
cor.xy = c()  # inicializo vector vacío
for (ivar in 1:nvar) {
  cor.xy = c(cor.xy, 
             cor(x[, ivar], data$y, method = "spearman",  # correlación Spearman
                 use = "pairwise"))
}
plot(cor.xy, pch = 19, xlab = "variable", ylab = "Spearman correlation")  # plot correlaciones
indsig = which(abs(cor.xy) > 0.4)  # identifico correlaciones "fuertes"
points(indsig, cor.xy[indsig], pch = 19, col = "red")  # las señalo en rojo en el gráfico
grid()


###########
## MNIST ##
###########
data = read.csv("/home/rodrigo/work/DOCENCIA/2021-2022/M1966_mineria_datos_data_science/datasets/MNIST_train.csv")
str(data)  # 784 (28x28) pixels por imagen, 3628 imágenes

# 1) Comprueba si el dataset está balanceado. ¿Cúantas observaciones hay para cada dígito?
number = data$label  # dígitos
nobs.number = c()  # inicializo vector vacío
for (i in sort(unique(number))) {
  nobs.number = c(nobs.number, sum(number == i))
}
names(nobs.number) = sort(unique(number))
barplot(100*nobs.number/nrow(data), xlab = "number", ylab = "% of data")  # barplot

# 2) Dibuja (utilizando p-colors) los 20 primeros "8"
npixel = sqrt(784)  # número de píxeles en el eje x y en el y
ind8 = which(number == 8)  # identifico las observaciones que corresponden a un "8"
par(mfrow = c(4, 5))
for (i in ind8[1:20]) {  # recorro los 20 primeros "8"
  image(matrix(as.numeric(data[i, -1]), nrow = npixel, ncol = npixel),  # p-color
        xaxt = "n", yaxt = "n",
        xlab = "color", ylab = "shape",
        col = terrain.colors(20))
}
