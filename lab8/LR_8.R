paste0('Количество переменных = ', length(longley))
paste0('Объем выборки = ', nrow(longley))
summary(longley)
cor(longley)

plot(longley$Employed, longley$Unemployed, type='l')
plot(longley$Year, longley$Employed, type='l')
plot(longley$Year, longley$Unemployed, type='l')
plot(longley$Year, longley$Armed.Forces, type='l')
plot(longley$Year, longley$GNP, type='l')
plot(longley$Unemployed, longley$Armed.Forces)


library(car)
par(mfrow=c(2, 2))
qqPlot(longley$GNP)
qqPlot(longley$Employed)
qqPlot(longley$Unemployed)
qqPlot(longley$Armed.Forces)

shapiro.test(longley$GNP)
shapiro.test(longley$Employed)
shapiro.test(longley$Unemployed)
shapiro.test(longley$Armed.Forces)


par(mfrow=c(1, 1))
cor(longley$GNP, log(longley$GNP),method="spearman")
plot(longley$GNP, log(longley$GNP), type='l')



df <- read.csv("Izrael2.csv", sep=",", header=T,
               fileEncoding="UTF-8")


df <- df[, -c(1:3)]; df <- t(df)
colnames(df) <- df[1, ]; df <- df[-1, -c(23:27)]
df[df == '..'] <- NA; df <- as.data.frame(df)
df <- data.frame(lapply(df, as.numeric)); rownames(df) <- 1989:2017


library(car)
scatterplotMatrix(df[, c(1:2)], spread=FALSE, lty.smooth=2,
                  main="Матрица диаграмм рассеяния")

plot(rownames(df), df[, 2], type = 'l', xlab = 'Год', ylab = 'Прирост ВВП (в %)')

plot(df$SP.POP.GROW, df[, 2], xlab = 'Прирост населения', ylab = 'Прирост ВВП')
cor.test(df$SP.POP.GROW, df[, 2])

plot(df$SP.POP.GROW, df$SL.UEM.BASC.ZS, xlab = 'Прирост населения', 
     ylab = 'Безработица среди граждан с базовым образованием')
# cor.test(df$SP.POP.GROW, df$SL.UEM.BASC.ZS)


par(mfrow=c(1, 2))
plot(df$SP.POP.GROW, df$SP.DYN.LE00.IN, xlab = 'Прирост населения', 
     ylab = 'Продолжительность жизни')
cor.test(df$SP.POP.GROW, df$SP.DYN.LE00.IN)

plot(df$SP.POP.GROW, df$SP.DYN.CDRT.IN, xlab = 'Прирост населения', 
     ylab = 'Смертность (на 1000 чел.)')
cor.test(df$SP.POP.GROW, df$SP.DYN.CDRT.IN)


plot(df$SE.TER.CUAT.BA.ZS, df$NE.EXP.GNFS.KD.ZG, xlab = 'Люди с высшим образованием', 
     ylab = 'Экспорт товаров')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$NE.EXP.GNFS.KD.ZG)

plot(df$SE.TER.CUAT.BA.ZS, df$NV.MNF.TECH.ZS.UN, xlab = 'Люди с высшим образованием', 
     ylab = 'Высокотехнологичное производство')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$NV.MNF.TECH.ZS.UN)


par(mfrow=c(1, 1))
plot(df$SE.XPD.TOTL.GD.ZS, df$SE.TER.CUAT.BA.FE.ZS, xlab = 'Расходы на высшее образование', 
     ylab = 'Кумулятивный прирост бакалавров среди женщин (в %)')
# cor.test(df$SE.XPD.TOTL.GD.ZS, df$SE.TER.CUAT.BA.FE.ZS)

plot(df$SE.TER.CUAT.BA.ZS, df$IP.JRN.ARTC.SC, xlab = 'Люди с высшим образованием', 
     ylab = 'Статьи в научных и технических журналах')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$IP.JRN.ARTC.SC)



library(ellipse)
plotcorr(cor(df))

image(1:ncol(cor(df)), 1:nrow(cor(df)), cor(df), col = rainbow(22), axes = F,
      xlab = '', ylab = '')
axis(1, at = 1:ncol(cor(df)), labels=colnames(cor(df)), las = 2)
axis(2, at = 1:nrow(cor(df)), labels=rownames(cor(df)), las = 1)



fit <- lm(SP.DYN.LE00.IN ~ SP.POP.GROW, df)
fit
plot(df$SP.POP.GROW, df$SP.DYN.LE00.IN, xlab="Прирост населения", 
     ylab="Продолжительность жизни", col="blue")
abline(fit, col="red")
scatterplotMatrix(df[, c(12, 13)], spread=FALSE, lty.smooth=2)


plot(df$SP.POP.GROW, predict(fit), xlab="Прирост населения", ylab="Продолжительность жизни")
abline(fit, col = "green")