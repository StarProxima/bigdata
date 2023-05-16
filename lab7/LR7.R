# Задание 1.

# импорт данных (все спортсмены по всем видам спорта)
df <- read.table("athlete_events.csv", sep=",", header=TRUE)

figure_skating<-df[which(df[, "Sport"]=="Figure Skating"), c("Name", "Sex", "Weight", "Sport")]

# удаление строчек с пустым значением поля "Вес"
figure_skating<-figure_skating[-which(is.na(figure_skating[, "Weight"])),]

# удаление повторяющихся строчек
figure_skating<-unique(figure_skating)

# значения столбца "Вес" становятся вектором
x<-figure_skating[1:nrow(figure_skating), "Weight"]

# Одномерные статистические тесты.

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x)

# Графический способ.

# гистограмма с линией плотности
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
hist(x, freq=FALSE, col="gray", main="Гистограмма по весу", xlab="Значения веса", ylab="Частота")
lines(x2, fun, col=2, lwd=2)

# квантильно-квантильный график
qqnorm(x)
qqline(x, col=4, lwd=2)
#title(main="Квантильно-квантильный график", xlab="Выборочные квантили", ylab="Теоретические квантили")

# Тест Стьюдента.
t.test(x, mu=60, conf.int=TRUE)

# Тест Уилкоксона.
wilcox.test(x, mu=mean(x), conf.int=TRUE)


#Задание 2.

speed_figure<-df[which(df[, "Sport"]%in%c("Speed Skating", "Figure Skating")), c("Name", "Sex", "Weight", "Sport")]

speed_figure<-speed_figure[-which(is.na(speed_figure[, "Weight"])),]

speed_figure<-speed_figure[which(speed_figure[, "Sex"]=="F"),]

speed_figure$Sport <- factor(speed_figure$Sport)
speed_figure$Sport <- droplevels(speed_figure$Sport)
speed_figure <- unique(speed_figure)

speed_figure_data<-speed_figure[1:nrow(speed_figure), "Weight"]

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(speed_figure_data)

# квантильно-квантильный график
qqnorm(speed_figure_data)
qqline(speed_figure_data, col=4, lwd=2)

# проверка равенство дисперсий
# Тест Флингера-Киллина.
fligner.test(speed_figure$Weight~speed_figure$Sport, speed_figure)

# проверка на отсутствие разницы в среднестатистическом значении 
t.test(speed_figure$Weight~speed_figure$Sport, paired=FALSE, var.equal=TRUE)
