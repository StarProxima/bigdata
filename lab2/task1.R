survey_results <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

stat <- summary(survey_results)
