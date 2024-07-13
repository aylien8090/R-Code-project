#Set the working directory to the location of your data files
setwd("C:/Users/User/Dropbox/Uni work/Analytics Programing/Report Project")

#Load the data using base R functions
patients <- read.csv("data/patientsUG.csv", stringsAsFactors = FALSE)
encounters <- read.csv("data/encountersUG.csv", stringsAsFactors = FALSE)
conditions <- read.csv("data/conditionsUG.csv", stringsAsFactors = FALSE)

#Display first few rows of each data frame to ensure they are loaded correctly
head(patients)
head(encounters)
head(conditions)

# Filter for COVID-19 related conditions
covid_conditions_1 <- conditions[conditions$DESCRIPTION == "COVID-19", ]
covid_conditions_2 <- conditions[conditions$DESCRIPTION == "Suspected COVID-19", ]

# Combine the results
covid_conditions <- rbind(covid_conditions_1, covid_conditions_2)

# Remove unnecessary
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


#Remove unnecessary
rm(confirmed_covid)
rm(suspected_covid)

# Display
head(covid_conditions)

# Merge conditions with encounters
covid_patients <- merge(covid_conditions, encounters, by.x = "ENCOUNTER", by.y = "Id")

# Merge the result with patients data
covid_patients <- merge(covid_patients, patients, by = "PATIENT.x", by.y = "Id")

covid_patients$RECOVERYSTATUS <- NA  # Create RecoveryStatus coloum

# For loop 
for (i in 1:nrow(covid_patients)) {
  if (covid_patients$STOP.x[i] == "") {
    covid_patients$RECOVERYSTATUS[i] <- "Not Recovered"
  } else {
    covid_patients$RECOVERYSTATUS[i] <- "Recovered"
  }
}

# Calculate Age
calculate_age <- function(birthDate) {
  today <- Sys.Date()
  birthDate <- as.Date(birthDate)
  age <- as.numeric(difftime(today, birthDate, units = "weeks")) %/% 52.25
  return(age)
}

# Add age and age group columns
covid_patients$Age <- sapply(covid_patients$BIRTHDATE, calculate_age)

# Define the age groups
age_breaks <- c(-Inf, 18, 35, 50, Inf)
age_labels <- c("0-18", "19-35", "36-50", "51+")

# Assign age groups to the covid_patients data frame
covid_patients$AgeGroup <- cut(
  covid_patients$Age,
  breaks = age_breaks,
  labels = age_labels
)

library(dplyr)

# Function to calculate recovery duration
calculate_recovery_duration <- function(df) {
  df %>%
    mutate(
      START.x = as.Date(START.x),
      STOP.x = as.Date(STOP.x),
      RECOVERYDURATION = as.numeric(difftime(STOP.x, START.x, units = "days"))
    ) %>%
    mutate(
      RECOVERYDURATION = ifelse(RECOVERYSTATUS == "Recovered", RECOVERYDURATION, NA)
    )
}

# Apply the function to your dataframe
covid_patients <- calculate_recovery_duration(covid_patients)


#Summarize data by recovery status
summary_by_recovery <- table(covid_patients$RECOVERYSTATUS)
summary_by_gender <- table(covid_patients$GENDER, covid_patients$RECOVERYSTATUS)
summary_by_age <- table(covid_patients$AgeGroup, covid_patients$RECOVERYSTATUS)
summary_by_zip <- table(covid_patients$ZIP, covid_patients$RECOVERYSTATUS)
summary_by_symptoms <- table(covid_patients$DESCRIPTION.x, covid_patients$RECOVERYSTATUS)
summary_by_recoveryduration <- table(covid_patients$RECOVERYDURATION, covid_patients$RECOVERYSTATUS)

# Convert to data frame
summary_by_recovery_df <- as.data.frame(summary_by_recovery)
colnames(summary_by_recovery_df) <- c("RecoveryStatus", "Count")

summary_by_gender_df <- as.data.frame(summary_by_gender)
colnames(summary_by_gender_df) <- c("Gender", "RecoveryStatus", "Count")

summary_by_age_df <- as.data.frame(summary_by_age)
colnames(summary_by_age_df) <- c("AgeGroup", "RecoveryStatus", "Count")

summary_by_zip_df <- as.data.frame(summary_by_zip)
colnames(summary_by_zip_df) <- c("Zip", "RecoveryStatus", "Count")

# Filter the data for "Recovered" status
recovered_by_zip <- summary_by_zip_df %>% filter(RecoveryStatus == "Recovered")

# Arrange the data in descending order
top_recovered_by_zip <- recovered_by_zip %>% arrange(desc(Count))

# Select the top 5 ZIP codes
top_5_zip <- top_recovered_by_zip %>% head(5)

summary_by_symptoms_df <- as.data.frame(summary_by_symptoms)
colnames(summary_by_symptoms_df) <- c("Symptom", "RecoveryStatus", "Count")

#Analyse how these factors impact the recovery outcome
head(summary_by_recovery_df)
head(summary_by_age_df)
head(summary_by_gender_df)
head(top_5_zip)
head(summary_by_symptoms_df)

library(tidyverse)

# Visualize the impact of RECOVERYDURATION on RECOVERYSTATUS
ggplot(covid_patients, aes(x = RECOVERYSTATUS, y = RECOVERYDURATION, fill = RECOVERYSTATUS)) +
  geom_boxplot() +
  labs(title = "Recovery Duration by Recovery Status", x = "Recovery Status", y = "Recovery Duration (days)") +
  theme_minimal()

# Overall Recovery Status
ggplot(summary_by_recovery_df, aes(x = RecoveryStatus, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall Recovery Status",
       x = "Recovery Status",
       y = "Count")

# Recovery Status by Age Group
ggplot(summary_by_age_df, aes(x = AgeGroup, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Age Group",
       x = "Age Group",
       y = "Count")

# Recovery Status by Gender
ggplot(summary_by_gender_df, aes(x = Gender, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Gender",
       x = "Gender",
       y = "Count")

# Recovery Status by ZIP Code
ggplot(summary_by_zip_df, aes(x = Zip, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by ZIP Code",
       x = "ZIP Code",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Recovery Status by Symptoms
ggplot(summary_by_symptoms_df, aes(x = Symptom, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Symptoms",
       x = "Symptom",
       y = "Count")
