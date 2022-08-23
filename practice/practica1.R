#Pablo Calatayud
#Punto 1. Reglas de asociacion

#1.0 Considerar uno de los algoritmos de asociación vistos en clase y obtener 
#las reglas representativas del dataset fijando los parámetros de aprendizaje (soporte, confianza, etc...). 
#Analizar los resultados en términos generales

#Voy a escoger el algoritmo apriori, para un soporte de 0.1 y una confianza de 0.5.
#He seleccionado maxima longitud 5 para el itemset porque si no los calculos me iban muy lento.

library("grid")
library("arules")
library("arulesViz")
library("caret")
library(lattice)
library(ggplot2)
library(tree)
library(rpart)
library(caret)
library(ISLR)

dataset = read.csv("C:\\Users\\pcalatayud\\Desktop\\master\\data-mining\\practicas\\mushrooms.csv")

#1.1 ¿Cuantas reglas se han generado?

rules <- apriori(dataset, parameter=list(support = 0.2, confidence = 0.5,
                                         maxlen=5,target="rules"))

cat("Se han generado un total de", length(rules) ,"reglas")

#1.2 ¿Existe alguna regla redundante?, ¿Cuántas?

indRedundantRules <- which(is.redundant(rules))

cat("Numero de reglas redundantes:",length(indRedundantRules))
cat("\n")
cat("Porcentaje de reglas redundates:",
    (length(indRedundantRules)/length(rules))*100 ,"%")

#1.3 ¿Existe alguna regla que incluya la variable objetivo: Class=edible ó 
#     Class=poisonous?, ¿Cuantas?

rules_e_p=(subset(rules, subset = rhs %in% c("class=p","class=e") 
                  | lhs %in% c("class=p","class=e")))

cat("Numero de reglas que complen la condicion",length(rules_e_p))

#1.4 De cara a ser utilizada como modelo predictivo es adecuado que la variable 
#    objetivo se encuentre 
#    en el consecuente de la regla de asociación, ¿se da esta propiedad en 
#    alguna regla?


#No tiene ningun sentido. Esa regla no aporta ningun tipo de informacion. Se ve 
#muy facil con el ejemplo de clase
#Comprar {naranjas,pan,mantequilla} => comprar {naranjas} 

#Como se dice la slide 21 de S03_AssociationRules.pdf 
#"Definimos  una  regla de asociación  como  una  implicación  X -> Y,  donde  
#X  es  un  conjunto  de 
#items e Y es un conjunto de items no incluidos en X."
#Por tanto por deficion no hay ninguna regla que cumpla esa propiedad. 

#1.5 Considerar los subconjuntos de reglas con ambas clases como consecuente e 
#    ilustrar las variables implicadas 
#    en cada caso. Considerar alguno de los grafos vistos para apoyar las 
#    conclusiones obtenidas.

rules_e_cons=(subset(rules, subset = rhs %in% c("class=e")))

rules_p_cons=(subset(rules, subset = rhs %in% c("class=p")))

inspect(head(sort(rules_e_cons, by ="confidence"),20))

inspect(head(sort(rules_p_cons, by ="confidence"),20))

#Es posible observar por ejemplo que:
#       odor=n, bruises=t, gill.size=b, spore.print.color=n, habitat=d, son 
#       caracteristicas de mushrooms edible
# 
#       odor=f, bruises =f, gill.color=b, son caractetisticas de mushrooms poisonous
#


#Algunos plots

plot(rules_e_cons, engine = "htmlwidget")

plot(rules_e_cons, method = "matrix", measure="lift",engine = "htmlwidget")

plot(rules_e_cons, method="paracoord", engine = "htmlwidget")

plot(rules_e_cons, method = "graph", engine = "htmlwidget")

plot(rules_p_cons, engine = "htmlwidget")

plot(rules_p_cons, method = "matrix", measure="lift",engine = "htmlwidget")

plot(rules_p_cons, method="paracoord", engine = "htmlwidget")

plot(rules_p_cons, method = "graph", engine = "htmlwidget")

#Parte 2.
#Para ello, dividiremos el dataset en dos subconjuntos indpendedientes de 
#train y test (75% y 25% del total, respectivamente). 
dimensions=dim(dataset)
n=dimensions[1]
ind <- sample(1:n, ceiling(0.75*n))  # índices aleatorios para training 
dataset.train <- dataset[ind, ]  # training
dataset.val <- dataset[-ind,]  # validation

t = tree(class ~ ., dataset.train)
t.cv = cv.tree(t, K = 10)

md = 1:30
trctrl = trainControl(method = "repeatedcv", number = 3,repeats = 50)
mod <- train(class ~ .,data = dataset.train, method = "rpart2",
              trControl = trctrl,tuneGrid = expand.grid(maxdepth = md))

plot(mod)
#Es claro ver que no tiene sentido escoger mas de 6 capas




