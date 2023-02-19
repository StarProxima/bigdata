# Read in the CSV file
table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

data <- table[,-c(1,2)]

# Count the number of people who preferred >7 and <3 for each column
votes_gt7 <- apply(data, 2, function(x) sum(x > 7 & !is.na(x)))
votes_lt3 <- apply(data, 2, function(x) sum(x < 3 & !is.na(x)))

# Print the results
print(votes_gt7)
print(votes_lt3)

