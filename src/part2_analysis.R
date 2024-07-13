# Set the working directory to the location of your data files
setwd("C:/Users/User/Dropbox/Uni work/Analytics Programing/Report Project")

# Load the data using base R functions
patients <- read.csv("data/patientsUG.csv", stringsAsFactors = FALSE)
encounters <- read.csv("data/encountersUG.csv", stringsAsFactors = FALSE)
conditions <- read.csv("data/conditionsUG.csv", stringsAsFactors = FALSE)

# Display first few rows of each data frame to ensure they are loaded correctly
head(patients)
head(encounters)
head(conditions)

# Filter for COVID-19 related conditions
covid_conditions_1 <- conditions[conditions$DESCRIPTION == "COVID-19", ]
covid_conditions_2 <- conditions[conditions$DESCRIPTION == "Suspected COVID-19", ]

# Combine the results
covid_conditions <- rbind(covid_conditions_1, covid_conditions_2)

#Remove unnecessary
rm(covid_conditions_1)
rm(covid_conditions_2)

# Remove duplicates, keeping confirmed COVID-19 only if it exists
covid_conditions <- covid_conditions[
  order(
    covid_conditions$PATIENT, 
    covid_conditions$DESCRIPTION == "COVID-19", 
    decreasing = TRUE
  ), 
]
covid_conditions <- covid_conditions[!duplicated(covid_conditions$PATIENT), ]

# Display
head(covid_conditions)

# Merge conditions with encounters
covid_patients <- merge(covid_conditions, encounters, by.x = "ENCOUNTER", by.y = "Id")

# Merge the result with patients data
covid_patients <- merge(covid_patients, patients, by = "PATIENT.x", by.y = "Id")

# Display first few rows of merged data to ensure merge is done correctly
head(covid_patients)

#Operation checks if each patient ID in 'conditions' is present in 'covid_patients'
library(dplyr)

# Perform a semi join to filter rows in conditions that have a match in covid_patients$PATIENT.x
covid_patient_conditions <- semi_join(conditions, covid_patients, by = c("PATIENT" = "PATIENT.x"))

# Merge with patient data to get gender
covid_patient_conditions <- merge(covid_patient_conditions, patients, by.x = "PATIENT", by.y = "Id")

#Display test as always!
head(covid_patient_conditions)

#Function to get top 10 conditions for a given gender
get_top_conditions <- function(gender) {
  gender_conditions <- covid_patient_conditions[covid_patient_conditions$GENDER == gender, ]
  condition_counts <- table(gender_conditions$DESCRIPTION)
  top_conditions <- head(sort(condition_counts, decreasing = TRUE), 10)
  return(data.frame(Condition = names(top_conditions), Count = as.vector(top_conditions)))
}


# Get top 10 conditions for males and females separately 
top_conditions_male <- get_top_conditions("M")
top_conditions_female <- get_top_conditions("F")


#DISPLAY IT!!
print("Top 10 conditions for males:")
print(top_conditions_male)

print("Top 10 conditions for females:")
print(top_conditions_female)