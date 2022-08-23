#Data-Mining.

#Clase 11/11/2021

library("arules")
library("arulesViz")
#asociation rules

# If we have not installed the "arulesViz" library we must do it first:
if (!require(arulesViz)) install.packages("arulesViz", dependencies = TRUE)
#Intersante comando!

itemset <- paste0("item", 1:10)
base::sample(itemset, size = 3, replace = FALSE)
#https://stackoverflow.com/questions/35240971/what-are-the-double-colons-in-r
#double colons meaning
#It means that we are using the function sample from the base pacakge

#------------- Ejemplo clase --------------------------%
# 1. Definicion del 
#Creamos la tabla con las 4 transacciones
table <- list(c("p","l","O","b"), c("p","l"), c("p","O","c"), c("p","l","O","c"))
transactions <- as(table, "transactions")
inspect(transactions)
#Intentamos buscar si hay asociacion entre elementos


itemFrequency(transactions, type = "a")#En terminos absolutos
#La cantidad de veces que aparece cada item individual en cada transaccion
itemFrequency(transactions, type = "r")#En terminos relativos
#La probabilidad de aparecer cada item en cada transaccion

#Un poco de representacion
itemFrequencyPlot(transactions)
itemFrequencyPlot(transactions, support = .5)

# Ejecución del algoritmo 'apriori'
iApriori <- apriori(transactions, parameter = list(supp = 0, conf = 0, target = "frequent itemsets"))
# Inspección del resultado
inspect(iApriori)


#Inspección del conjunto de reglas. Filtrado y redundancia.

# %in% filtra los sets de item que contienen "b" o "l" o ambas
inspect(subset(iApriori, subset = items %in% c("b","l")))

# %ain% filtra los sets de item que contienen "b" y "l" simultáneamente
inspect(subset(iApriori, subset = items %ain% c("b","l")))

# %pin% realiza "partial matching", de acuerdo con la expresión indicada.
# Este ejemplo no es el mejor, porque los códigos de los items son un únic caracter.
# Ver ?arules::match para un ejemplo mejor
inspect(subset(iApriori, subset = items %pin% c("b")))

# %oin% ("only in") es más restrictivo que %in%: 
# Filtra los itemsets que únicamente contienen los elementos indicados:
inspect(subset(iApriori, subset = items %oin% c("b","l")))

# El parámetro target = "rules" indica al algoritmo apriori la generación de reglas:

rApriori <- apriori(transactions, parameter = list(supp = 0, conf = 0, target = "rules"))
inspect(rApriori)

#La inclusión de reglas cuyo antecedente (lhs: left hand side) es el conjunto vacío ({}) pueden fitrarse incluyendo 
#restricciones respecto a la longitud mínima (minlen).
rApriori <- apriori(transactions, parameter = list(supp = 0, conf = 0, target = "rules", minlen = 2))
inspect(rApriori)

#Al inspeccionar el objeto, bien con las reglas bien con los itemsets, se muestran por defecto tres parámetros, support, 
#confidence y lift, si bien se pueden obtener 
#un gran número de medidas de interés alternativas, gran parte de las cuales pueden usarse también en el proceso de aprendizaje:

interestMeasure(rApriori, c("support", "chiSquare", "confidence", "conviction","cosine", "coverage", "leverage", "lift", "oddsRatio"), transactions)


#Si bien no hemos realizado ningún filtro a las reglas obtenidas, más allá de las impuestas por los algoritmos de aprendizaje, 
#es habitual obtener 
#reglas redundantes la cuales debemos filtrar. Para ello, la función is.redundant nos permite localizar estas redundancias:
indRedundantApriori <- which(is.redundant(rApriori))
inspect(rApriori[indRedundantApriori])
#Procedemos a filtrar dichas redes:
rAprioriFiltered <- rApriori[!is.redundant(rApriori)]
inspect(rAprioriFiltered)

#Finalmente, podemos resolver dudas como las siguientes:
#Encontrar todas las reglas relacionando cualquier producto con uno dado (p.e. O - Naranja).

inspect(subset(rAprioriFiltered, subset = rhs %in% c("O")))

inspect(subset(rAprioriFiltered, subset = lhs %in% c("O")))

#Encontrar todas las reglas que cumplan los dos criterios anteriores para ciertos productos dados.
inspect(subset(rAprioriFiltered, subset = rhs %in% c("O") | lhs %in% c("O")))

#Encontrar el conjunto de reglas con mayor confianza cumpliendo alguno de los criterios anteriores.
inspect(subset(rAprioriFiltered, subset = rhs %in% c("O") | lhs %in% c("O") & confidence >= 0.5))

#Consultar el conjunto de reglas con alguno de los parámetros con valores más altos (head) o bajos (tail)

inspect(head(sort(rAprioriFiltered, by ="lift"),3))

inspect(tail(sort(rAprioriFiltered, by ="lift"),3))
x11()
m <- plot(rAprioriFiltered, method = "matrix", measure="lift", engine = "html")
htmlwidgets::saveWidget(m, 'demo.html', selfcontained = FALSE)
IRdisplay::display_html('<iframe src="demo.html"></iframe>')

#Matriz de itemsets
X11()
image(iApriori@items, xlab = "Items", ylab = "Itemsets")

#Diagramas de dispersión (scatterplots) de reglas:
X11()
plot(rAprioriFiltered,engine = "htmlwidget")

#Matrices de asociaciones:
x11()
plot(rAprioriFiltered, method = "matrix", measure="lift",engine = "htmlwidget")


#Matriz de asociaciones por grupos, incluyendo parámetros:

plot(rAprioriFiltered, method="grouped", measure="lift",engine = "htmlwidget")
plot(rAprioriFiltered, method="grouped", measure="support",engine = "htmlwidget")


#Enlaces de las reglas:

plot(rAprioriFiltered, method="paracoord")

oneRule <- sample(rAprioriFiltered, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = transactions)

#Visualización basada en grafos

#inspect(head(sort(rules, by = "confidence"),4))

plot(rAprioriFiltered, method = "graph",engine = "htmlwidget")

plot(rAprioriFiltered, method = NULL, measure = "support", shading = "lift", interactive = FALSE, data = NULL, control = NULL,)






