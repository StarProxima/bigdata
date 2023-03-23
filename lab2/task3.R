table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

data <- table[,-c(1,2)]

votes <- sort(apply(data, 2, mean), decreasing = TRUE);votes
