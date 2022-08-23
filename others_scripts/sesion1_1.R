library(ISLR) #An Introduction to Statistical Learning in R
library(arules) # Association Rules
library(arulesViz) # Association Rules
library(caret) # Model learning and evaluation
library(psych)
library(class)
library(FNN)
library(tree)
## Statistical tools:
library(MASS)
library(stats)
## Reading data:
library(readr)
## Clustering methods:
library(mclust)
library(e1071)
library(sparcl)
library(kohonen)
library(cluster)

library(ggplot2) # Data visualization
library(car)
library(boot)
library(lattice)
library(latticeExtra)
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyverse)
library(devtools)
install_github(ggbiplot,vqv)
library(ggbiplot)
library(fields)
library(rpart)
library(sparcl) # colorea las hojas del dendrograma de forma facil
library(partykit)
library(rpart.plot)
library(gtrends)


google.trends = gtrends(c("big data"), gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date

plot(google.trends, type = "l")
google.trends = gtrends(c("machine learning"), gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
lines(google.trends, col = "blue")

