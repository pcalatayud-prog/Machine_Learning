#Practica 1

#Date: 12/11/2021
#Objetivo: Trabajando con el algortimo apriori
#Autor: Pablo Calatayud

library("grid")
library("arules")
#install.packages("arulesViz")
library("arulesViz")

#Aprendiendo de los datos: Algoritmo A Priori y dataSet Groceries

data(Groceries)
#El dataSet Groceries tiene 169 items y 9835 transactions

rules_groceries <- apriori(Groceries, parameter=list(support = 0.001, confidence = 0.5))
#1. ¿Cuantas reglas de asociación se han generado?

#Se han generado 5668 rules

#2.¿Qué porcentaje de reglas es redundante?

indRedundantRules <- which(is.redundant(rules_groceries))
inspect(rules_groceries[indRedundantRules])
#De las 5668 rules hay 572 que son redundantes
#Por tanto un 10% de las reglas son redundantes

#Filtrar las reglas redundantes
rAprioriFiltered <- rules_groceries[!is.redundant(rules_groceries)]
inspect(rAprioriFiltered)

#3. Teniendo en cuenta el conjunto de datos de partida, ¿resulta útil el 
#conjunto de reglas generado?


#Al tener 169 items y 9835 transacciones de acuerdo con la slide 32
#tendriamos 2^169=7.5e50 itemSets Y (3^169)-(2^170)+1=4.3e80 itemSets

#Y hemos reducido el problema a 5096 reglas

#4. ¿Cual es la regla con mayor confianza? ¿Y con menor?

inspect(head(sort(rAprioriFiltered, by ="confidence"),1))
#lhs                   rhs          support       confidence  coverage      lift         count
#[1] {rice,sugar} => {whole milk} 0.001220132     1           0.001220132   3.913649     12 

inspect(tail(sort(rAprioriFiltered, by ="confidence"),1))
#lhs                   rhs                   support      confidence    coverage      lift       count
#[1] {tropical fruit,                                                                        
#  other vegetables,                                                                      
#  whole milk,                                                                            
#  rolls/buns}       => {root vegetables}     0.002033554  0.5           0.004067107    4.58722    20


#5. ¿Cual es la regla con mayor interés? ¿Y con menor?
head(quality(rAprioriFiltered))


plot(rAprioriFiltered, engine = "htmlwidget")


#6. En el objeto aparece una medida llamada lift, ¿qué mide?

#lift(X => Y ) = supp(XuY)/(supp(X)*supp(Y))=P(X inter Y)/(P(X)*P(Y))
  #lift= supp(X U Y)/supp(x)*supp(y) 
  #lift >> 1 asociacion muy fuerte
  #lift = 1
  #lift << 1 antiasociacion

#7. Escribe el podium asociado a cada una de las tres medidas: 
#support, confidence y lift.

inspect(head(sort(rAprioriFiltered, by ="confidence"),3))

inspect(head(sort(rAprioriFiltered, by ="support"),3))

inspect(head(sort(rAprioriFiltered, by ="lift"),3))

#8.¿Se puede inferir que la regla es persistente respecto a la medida de interés?

#Entre 

#9. Explorar los ejemplos de la función plot.

x11()
m <- plot(rAprioriFiltered, method = "matrix", measure="lift", engine = "htmlwidget")
m
x11()
plot(rAprioriFiltered, engine = "htmlwidget")

plot(rAprioriFiltered, method = "matrix", measure="lift",engine = "htmlwidget")

plot(rAprioriFiltered, method="paracoord", engine = "htmlwidget")
x11()
plot(rAprioriFiltered, method = "graph", engine = "htmlwidget")

#10. ¿Hay alguna agrupación de items que se de con gran frecuencia? 
#Por ejemplo, ¿los productos de limpieza se compran de forma conjunta? 
#  ¿Los productos de cuidado personal?, etc...











