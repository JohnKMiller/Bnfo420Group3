#this sets the work directory to the folder where my files are
setwd("C:\\Users\\gilli\\OneDrive\\Desktop\\BNFO APP SP2023")

#these next two statements import the data
prisondata <- read.csv("prison_state_cleaned.csv")
usdata <- read.csv("total_death_cleaned.csv")

#this congregates the data into a frequency matrix to run the chi-sq
observed <- rbind(
  c(sum(prisondata$Residents.Confirmed)), sum(prisondata$Residents.Deaths))
  c(sum(usdata$Cases), sum(usdata$Deaths))

#runs the chi-sq
result <- chisq.test(observed)

#prints the results
result

#creates matrix to run post-hoc test
observed <- matrix(c(sum(prisondata$Residents.Confirmed), sum(prisondata$Residents.Deaths),
                     sum(usdata$Cases), sum(usdata$Deaths)),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("Prison Residents", "US Population"),
                                   c("Confirmed Cases", "Deaths")))

#conducts post-hoc test
posthoc <- pairwise.prop.test(observed, p.adjust.method = "bonferroni")

#prints the results
posthoc