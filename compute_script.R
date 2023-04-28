# load packages for reading file
library(tidyverse)

# Reads in CSV file
cali_data = read_csv("california_data.csv")
texas_data = read_csv("texas_data.csv")

# For california data, creates two new columns percent_cases and percent_occupied
cali_data_complete = cali_data %>%
  dplyr::mutate(percent_cases = round((Residents.Cases / Population * 100), digits = 1)) %>%
  dplyr::mutate(percent_occupied = round((Population / Capacity * 100), digits = 1))

# Writes result to a file
write.csv(cali_data_complete, file = "cali_data_complete.csv")

# Runs spearmans corr test on cali data
cor.test(cali_data_complete$percent_cases, 
         cali_data_complete$percent_occupied,
         method = "spearman"
         )

# Plots scatterplot for califonia prisons
plot(cali_data_complete$percent_cases, 
     cali_data_complete$percent_occupied, 
     xlab = "Percent Cases", 
     ylab = "Percent Occupied", 
     main = "California Prisons")


# START TEXAS

# For texas data, creates two new columns percent_cases and percent_occupied
texas_data_complete = texas_data %>%
  dplyr::mutate(percent_cases = round((Residents.Cases / Population * 100), digits = 1)) %>%
  dplyr::mutate(percent_occupied = round((Population / Capacity * 100), digits = 1))

# Writes result to a file
write.csv(texas_data_complete, file = "texas_data_complete.csv")

# Runs spearmans corr test on texas data
cor.test(texas_data_complete$percent_cases, 
         texas_data_complete$percent_occupied,
         method = "spearman"
)

# Plots scatterplot for texas prisons
plot(texas_data_complete$percent_cases, 
     texas_data_complete$percent_occupied, 
     xlab = "Percent Cases", 
     ylab = "Percent Occupied", 
     main = "Texas Prisons")
