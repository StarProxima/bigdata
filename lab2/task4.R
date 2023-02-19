# Load the gplots package
library(gplots)

# Read in the CSV file
table <- read.csv("survey_results.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

# Subset the data to include only the columns with the ratings
ratings <- table[,3:12]

# Calculate the mean rating and standard deviation for each column
mean_ratings <- sort(apply(ratings, 2, mean), decreasing = TRUE)
sd_ratings <- apply(ratings, 2, sd)

# Define the color scale
my_palette <- colorRampPalette(c("green", "yellow", "red"))

# Create a bar plot with the color scale and error bars
barplot2(mean_ratings, main = "Average Rating by Enchant", 
         xlab = "Enchant", ylab = "Average Rating",
         col=my_palette(length(mean_ratings)))

# Add error bars to the plot
arrows(x0=seq(0.7, 12, by = 1.2), y0=mean_ratings - sd_ratings, 
       x1=seq(0.7, 12, by = 1.2), y1=mean_ratings , 
       code=1, length=0.1, angle=90, lwd=1)
