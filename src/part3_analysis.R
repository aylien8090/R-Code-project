# Set the working directory to the location of your data files
setwd("C:/Users/User/Dropbox/Uni work/Analytics Programing/Report Project")

# Load the data using base R functions
patients <- read.csv("data/patientsUG.csv", stringsAsFactors = FALSE)
encounters <- read.csv("data/encountersUG.csv", stringsAsFactors = FALSE)
conditions <- read.csv("data/conditionsUG.csv", stringsAsFactors = FALSE)

# Display first few rows of each data frame to ensure they are loaded correctly
head(patients[, 1:3])
head(encounters[, 1:3])
head(conditions[, 1:3])

# Filter for COVID-19 related conditions
covid_conditions_1 <- conditions[conditions$DESCRIPTION == "COVID-19", ]
covid_conditions_2 <- conditions[conditions$DESCRIPTION == "Suspected COVID-19", ]

# Combine the results
covid_conditions <- rbind(covid_conditions_1, covid_conditions_2)

#Remove unnecessary
rm(covid_conditions_1)
rm(covid_conditions_2)

# Separate confirmed COVID-19 cases
confirmed_covid <- covid_conditions[covid_conditions$DESCRIPTION == "COVID-19", ]

# Separate non-confirmed COVID-19 cases
suspected_covid <- covid_conditions[covid_conditions$DESCRIPTION != "COVID-19", ]

# Combine confirmed COVID-19 cases first, then suspected cases
covid_conditions <- rbind(confirmed_covid, suspected_covid)

# Remove duplicates, keeping the first occurrence (which will be confirmed COVID-19 if it exists)
covid_conditions <- covid_conditions[!duplicated(covid_conditions$PATIENT), ]

# Display
head(covid_conditions[, 1:3])

# Merge conditions with encounters
covid_patients <- merge(covid_conditions, encounters, by.x = "ENCOUNTER", by.y = "Id")

# Merge the result with patients data
covid_patients <- merge(covid_patients, patients, by = "PATIENT.x", by.y = "Id")

num2 <- nrow(covid_patients)

# Display first few rows of merged data
head(covid_patients[, 1:3])

# Filter for relevant encounter types
relevant_encounters <- covid_patients[covid_patients$ENCOUNTERCLASS %in% c("ambulatory", "emergency", "inpatient", "urgentcare"), ]

View(relevant_encounters)

#Summaries of race and gender
summary_by_race <- table(relevant_encounters$ENCOUNTERCLASS, relevant_encounters$RACE)
summary_by_gender <- table(relevant_encounters$ENCOUNTERCLASS, relevant_encounters$GENDER)

#Convert to data frame for plotting
summary_by_race_df <- as.data.frame(summary_by_race)
summary_by_gender_df <- as.data.frame(summary_by_gender)
colnames(summary_by_race_df) <- c("EncounterClass", "Race", "Count")
colnames(summary_by_gender_df) <- c("EncounterClass", "Gender", "Count")

View(summary_by_gender_df)
View(summary_by_race_df)


# Plot the summary of encounter types by gender
library(ggplot2)
ggplot(summary_by_gender_df, aes(x = EncounterClass, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Summary of Encounter Types by Gender",
       x = "Encounter Class",
       y = "Count")


# Plot the summary of encounter types by race
ggplot(summary_by_race_df, aes(x = EncounterClass, y = Count, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Summary of Encounter Types by Race",
       x = "Encounter Class",
       y = "Count")
