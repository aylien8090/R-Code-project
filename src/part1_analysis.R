# Set the workging directory to the location of your data files
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

# Display first few rows of merged data to ensure merge is done correctly
head(covid_patients)

# Frequency table of the element
county_distribution <- table(covid_patients$COUNTY)
county_distribution <- sort(county_distribution, decreasing = TRUE)

county_distribution_df <- data.frame(County = names(county_distribution), Number_of_Patients = as.vector(county_distribution))

#Remove the "county" part
county_distribution_df$County <- sub(" County", "", county_distribution_df$County)

print(county_distribution_df)

# Load necessary library
library(ggplot2)

# Create the ggplot for county distribution
ggplot(county_distribution_df, aes(x = reorder(County, -Number_of_Patients), y = Number_of_Patients)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of COVID-19 Across Counties", x = "County", y = "Number of Patients")

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



age_group_distribution <- table(covid_patients$AgeGroup)
age_group_distribution <- sort(age_group_distribution, decreasing = TRUE)
age_group_distribution_df <- data.frame(AgeGroup = names(age_group_distribution), Number_of_Patients = as.vector(age_group_distribution))
print(age_group_distribution_df)

# Barplot of age distribution
barplot(height = age_group_distribution_df$Number_of_Patients, 
        names.arg = age_group_distribution_df$AgeGroup, 
        main = " COVID-19 Distribution Across Age Groups",
        xlab = "Age Group", 
        ylab = "Number of Patients")

# Histogram of age distribution
hist(covid_patients$Age, 
     breaks = 10,  # Number of bins
     main = "COVID-19 Distribution Across Age", 
     xlab = "Age", 
     ylab = "Frequency")

