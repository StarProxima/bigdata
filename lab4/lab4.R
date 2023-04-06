
mix <-read.csv("mix.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)
men <- read.csv("men.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)
women<-read.csv("women.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

#посчитаем сумму всех медалей
stat_mix <- sapply(mix[,-1], sum)
stat_men <- sapply(men[,-1], sum)
stat_women <- sapply(women[,-1], sum)

par(mfrow=c(1,3))
barplot(stat_mix, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Микст (за все время)")
barplot(stat_men, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Мужчины (за все время)")
barplot(stat_women, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Женщины (за все время)")

#сделаем фреймы только 1 мест
first_mix <- mix[,c(1:2)][mix$`1` > 0, ]
first_men <- men[,c(1:2)][men$`1` > 0, ]
first_women <- women[,c(1:2)][women$`1` > 0, ]

pie(first_mix$`1`, labels=first_mix$`1`, col=rainbow(length(first_mix$`1`)), main = "Количество золотых медалей (мужчины)\nза все время")
legend(-1.1, 1.1, first_mix$`year`, cex = 0.7, fill=rainbow(length(first_mix$`year`)))

#pie(first_men$`1`, labels=first_men$`1`, col=rainbow(length(first_men$`1`)), main = "Количество золотых медалей (мужчины)\nза все время")
#legend(-1.1, 1.1, first_men$`year`, cex = 0.7, fill=rainbow(length(first_men$`year`)))

#pie(first_women$`1`, labels=first_women$`1`, col=rainbow(length(first_women$`1`)), main = "Количество золотых медалей (мужчины)\nза все время")
#legend(-1.1, 1.1, first_women$`year`, cex = 0.7, fill=rainbow(length(first_women$`year`)))
