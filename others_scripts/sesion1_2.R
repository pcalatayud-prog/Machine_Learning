#install.package("gtrendsR")

library(gtrendsR)
library(reshape2)

x11()

trends = gtrends(c("dogecoin"),geo = c("ES"), gprop = "web", time = "today+5-y")[[1]]
trends = dcast(trends, date ~ keyword + geo, value.var = "hits")
rownames(trends) = trends$date
plot(trends, type = "l",col="blue")
trends = gtrends(c("dogecoin"),geo = c("US"), gprop = "web", time = "today+5-y")[[1]]
trends = dcast(trends, date ~ keyword + geo, value.var = "hits")
rownames(trends) = trends$date
lines(trends, type = "l",col="red")



trends1 = gtrends(c("blockchain"), gprop = "web", time = "today+5-y")[[1]]
trends1 = dcast(trends1, date ~ keyword + geo, value.var = "hits")
rownames(trends1) = trends1$date
lines(trends1, col = "blue")

#lines(trends$date,trends$hits,col="blue")
#lines(trends1$date,trends1$hits,col="red")


