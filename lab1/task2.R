# Create a data frame of average monthly air temperatures
temp <- data.frame(month = 1:12,
                   temperature = c(-10.8, -13.1, -3.7, 7.2, 13.3, 17.6, 20.6, 19.4, 13.9, 6.4, -4, -7))

# Calculate the quantitative change in temperature from month to month (in degrees)
temp$delta_temperature <- c(NA, diff(temp$temperature))

# Find the numbers and names of winter months (with an average monthly temperature below zero)
winter_months <- temp[temp$temperature < 0, ]
winter_month_numbers <- winter_months$month
winter_month_names <- month.abb[winter_month_numbers]
print(paste("The winter months are:", paste(winter_month_names, collapse = ", ")))
