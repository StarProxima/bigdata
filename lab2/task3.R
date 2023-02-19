# Read in the CSV file
table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

data <- table[,-c(1,2)]

# Calculate the mean vote for each column and sort in descending order
votes <- sort(apply(data, 2, function(x) mean(x)), decreasing = TRUE)

print(votes)