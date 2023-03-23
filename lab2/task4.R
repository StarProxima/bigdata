# Load the gplots package
library(gplots)


table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)


ratings <- table[,3:12]


mean_ratings <- sort(apply(ratings, 2, mean), decreasing = TRUE)

my_palette <- colorRampPalette(c("green", "yellow", "red"))

barplot2(mean_ratings, main = "Average Rating by Enchant", 
         xlab = "Enchant", ylab = "Average Rating",
         col=my_palette(length(mean_ratings)))


