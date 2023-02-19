# Read in the CSV file
survey_results <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

# Calculate summary statistics for each column
summary(survey_results)



