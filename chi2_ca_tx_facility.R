setwd("C:\\Users\\gilli\\OneDrive\\Desktop\\BNFO APP SP2023")

#these next two statements import the data
californiadata <- read.csv("californiaprison_cases_deaths_population_cleaned.csv")
texasdata <- read.csv("texasprison_cases_deaths_population_cleaned.csv")

# Calculate observed frequencies for California
ca_cases <- sum(californiadata$Residents.Cases)
ca_deaths <- sum(californiadata$Residents.Deaths)
ca_population <- sum(californiadata$Population)
ca_observed <- c(ca_cases, ca_deaths, ca_population)

# Calculate observed frequencies for Texas
tx_cases <- sum(texasdata$Residents.Confirmed)
tx_deaths <- sum(texasdata$Residents.Deaths)
tx_population <- sum(texasdata$Population)
tx_observed <- c(tx_cases, tx_deaths, tx_population)

# Create frequency matrix
frequency_matrix <- rbind(ca_observed, tx_observed)
colnames(frequency_matrix) <- c("Cases", "Deaths", "Population")
rownames(frequency_matrix) <- c("California", "Texas")

# look at frequency matrix
frequency_matrix

# Perform chi-squared test
chi_results <- chisq.test(frequency_matrix)

#creates matrix to run post-hoc test
observed <- matrix(c(sum(californiadata$Residents.Cases), sum(californiadata$Residents.Deaths),
                     sum(texasdata$Residents.Confirmed), sum(texasdata$Residents.Deaths)),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("California", "Texas"),
                                   c("Cases", "Deaths")))

#conducts post-hoc test
posthoc <- pairwise.prop.test(observed, p.adjust.method = "bonferroni")

#prints the results
posthoc