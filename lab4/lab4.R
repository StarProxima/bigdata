
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

pie(first_mix$`1`, labels=first_mix$`1`, col=rainbow(length(first_mix$`1`)), main = "Количество золотых медалей (микст)\nза все время")
legend(-1.1, 1.1, first_mix$`year`, cex = 0.7, fill=rainbow(length(first_mix$`year`)))

#pie(first_men$`1`, labels=first_men$`1`, col=rainbow(length(first_men$`1`)), main = "Количество золотых медалей (мужчины)\nза все время")
#legend(-1.1, 1.1, first_men$`year`, cex = 0.7, fill=rainbow(length(first_men$`year`)))

#pie(first_women$`1`, labels=first_women$`1`, col=rainbow(length(first_women$`1`)), main = "Количество золотых медалей (мужчины)\nза все время")
#legend(-1.1, 1.1, first_women$`year`, cex = 0.7, fill=rainbow(length(first_women$`year`)))

#выделим призовые места
prize_mix <- data.frame(Год=mix$year, Призовых=rowSums(mix[, 2:4]))
prize_men <- data.frame(Год=men$year, Призовых=rowSums(men[, 2:4]))
prize_women <- data.frame(Год=women$year, Призовых=rowSums(women[, 2:4]))

par(mfrow=c(1,1))
plot(prize_mix, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,2), main="Призовые места Германии по фигурному катанию за 30 лет")
lines(prize_men, type="o", pch=19, col="navyblue")
lines(prize_women, type="o", pch=19, col="hotpink")
legend(min(prize_mix$Год), 2, c("Микст", "Мужчины", "Женщины"), fill=c("#3be8b0", "navyblue", "hotpink"))
axis(side=1, at=prize_mix$year)


events_gold <- read.csv("gold_medals.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_gold$Год, events_gold$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,50), xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних олимпиад")
lines(events_gold$Год, events_gold$Китай, type="o", pch=19, col="#1aafd0")
lines(events_gold$Год, events_gold$Япония, type="o", pch=19, col="#6a67ce")
lines(events_gold$Год, events_gold$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_gold$Год, events_gold$Россия, type="o", pch=19, col="gray70")
lines(events_gold$Год, events_gold$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_gold$Год, events_gold$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_gold$Год)
legend(max(events_gold$Год) - 1.5, 53, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))


events_prizes <- read.csv("priz_places.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_prizes$Год, events_prizes$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,130), xlab="Год", ylab="Медалей", main="Призовые места за последние 6 олимпиад")
lines(events_prizes$Год, events_prizes$КИТАЙ, type="o", pch=19, col="#1aafd0")
lines(events_prizes$Год, events_prizes$Япония, type="o", pch=19, col="#6a67ce")
lines(events_prizes$Год, events_prizes$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_prizes$Год, events_prizes$Россия, type="o", pch=19, col="gray70")
lines(events_prizes$Год, events_prizes$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_prizes$Год, events_prizes$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_prizes$Год)
legend(max(events_prizes$Год) - 1.5, 137, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))

#выделим призовые места за последние 6 лет
prize_mix_6 <- tail(prize_mix, 6)
prize_men_6 <- tail(prize_men, 6)
prize_women_6 <- tail(prize_women, 6)

par(mfrow=c(1,3))
plot(prize_mix_6, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,7), main="Призовые места Германии по фигурному катанию\n за последние 6 ОИ")
lines(prize_men_6, type="o", pch=11, col="navyblue")
lines(prize_women_6, type="o", pch=11, col="hotpink")
legend(min(prize_mix_6$Год), 7.2, cex=0.7 ,c("Микст", "Мужчины", "Женщины"), fill=c("#3be8b0", "navyblue", "hotpink"))
axis(side=1, at=prize_mix_6$Год)

prize_grouped = data.frame(Призовых_Микс=prize_mix_6$Призовых, Призовых_М=prize_men_6$Призовых, Призовых_Ж=prize_women_6$Призовых)
barplot(height=t(as.matrix(prize_grouped)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=prize_mix_6$Год, col=c("#3be8b0","navyblue", "hotpink"), main="Количество призовых мест Германия\n по фигурному катанию за последние 6 ОИ")

prize_6_sum <- sapply(prize_grouped, sum)
pie(prize_6_sum, labels=c(prize_6_sum["Призовых_Микс"], prize_6_sum["Призовых_М"], prize_6_sum["Призовых_Ж"]), col=c("#3be8b0", "navyblue", "hotpink"), main="Всего призовых мест у М и Ж из Германии\n по фигурному катанию за последние 6 ОИ")
