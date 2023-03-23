table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

data <- table[,-c(1,2)]

v_gt7 <- apply(data, 2, function(x) sum(x > 7 & !is.na(x)))
v_lt3 <- apply(data, 2, function(x) sum(x < 3 & !is.na(x)))


v_all <- v_gt7 + v_lt3