# Install and load necessary packages

#install.packages(c("dplyr"))
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)


## Load the data set into R
accidents <- read.csv("accidents.csv")


#Overview the data from 2014 and 2015
glimpse(accidents)

head(accidents)

summary(accidents)


# Convert Accident.Date to Date type
accidents$Accident.Date <-
  as.Date(accidents$Accident.Date, format = "%d/%m/%Y")


# Convert Time..24hr.
accidents$Time..24hr. <-
  as.POSIXct(strptime(sprintf("%04d", accidents$Time..24hr.), "%H%M"), format = "%H%M")



# Examine data for any missing values. Give an example of Missing at Random and not missing at Random data.

# Check for missing values in the entire data set
missing_values <- is.na(accidents)

# To get a summary of missing values by column
missing_summary <- colSums(missing_values)
print(missing_summary)


##MAR OR NOT?

# Create a contingency table for the "Age of Casualty" and "Sex of Casualty" columns
contingency_table <-
  table(accidents$Age.of.Casualty, accidents$Sex.of.Casualty)

# Perform the chi-square test of independence
chi_square_test <- chisq.test(contingency_table, correct = FALSE)

# Print the results of the chi-square test
print(chi_square_test)


#Since the p-value is greater than the chosen significance level (e.g., 0.05), we fail to reject the null hypothesis.
#This means that there is no significant association between the missingness of the "Age of Casualty" column and all other categorical variables in the dataset.
#Therefore, we can conclude that the missingness of the "Age of Casualty" column is random with respect to all other categorical variables in the dataset.


# Calculate the mean of the non-missing values in the "Age of Casualty" column
#mean_age <- mean(accidents$Age.of.Casualty, na.rm = TRUE)

# Replace the missing values in the "Age of Casualty" column with the mean value
#accidents$Age.of.Casualty <-
#  ifelse(is.na(accidents$Age.of.Casualty),
#         round(mean_age),
#         accidents$Age.of.Casualty)

unique(accidents$Age.of.Casualty)


##Examine and fix anomalies in “Road Surface” and “1st Road Class “columns.

# Check the unique values in the Road.Surface
unique(accidents$Road.Surface)

# Convert the encoding of the 'Road.Surface' column to UTF-8 to handle special characters
accidents$Road.Surface <-
  iconv(accidents$Road.Surface, "latin1", "UTF-8")

# Replace the inconsistent 'Wet \\xa8 Damp' values in the 'Road.Surface' column with 'Wet/Damp'
accidents <- accidents %>%
  mutate(Road.Surface = gsub("Wet \\xa8 Damp", "Wet/Damp", Road.Surface))

# Re-code numeric values in 'Road.Surface' to their corresponding descriptions
accidents <- accidents %>%
  mutate(
    Road.Surface = recode(
      Road.Surface,
      "1" = "Dry",
      "2" = "Wet / Damp",
      "3" = "Snow",
      "4" = "Frost / Ice",
      "5" = "Flood (surface water over 3cm deep)"
    )
  )

# Replace 'Wet / Damp' with 'Wet/Damp' and 'Frost / Ice' with 'Frost/Ice' for consistency
accidents <- accidents %>%
  mutate(
    Road.Surface = gsub("Wet / Damp", "Wet/Damp", Road.Surface),
    Road.Surface = gsub("Frost / Ice", "Frost/Ice", Road.Surface)
  )

# Consolidate 'Ice' under 'Frost/Ice' and 'Wet' under 'Wet/Damp'
accidents <- accidents %>%
  mutate(Road.Surface = recode(Road.Surface,
                               "Ice" = "Frost/Ice",
                               "Wet" = "Wet/Damp"))



# Check the unique values in X1st.Road.Class columns
unique(accidents$X1st.Road.Class)


# Re-code numeric values in 'X1st.Road.Class' to their corresponding descriptions
accidents <- accidents %>%
  mutate(
    X1st.Road.Class = recode(
      X1st.Road.Class,
      "1" = "Motorway",
      "2" = "A(M)",
      "3" = "A",
      "4" = "B",
      "5" = "C",
      "6" = "Unclassified"
    )
  )

#Correct mapping is done based on the guidance.csv
correct_mapping <- c(
  "U" = "Unclassified",
  "A58" = "A",
  "A646" = "A",
  "B6138" = "B",
  "A629" = "A",
  "A641" = "A",
  "A672" = "A",
  "A6033" = "A",
  "A6139" = "A",
  "A644" = "A",
  "A62" = "A",
  "B6114" = "B",
  "A6319" = "A",
  "B6112" = "B",
  "M62" = "Motorway",
  "A681" = "A",
  "B6113" = "B",
  "A629(M)" = "A(M)",
  "A643" = "A",
  "A6036" = "A",
  "A6025" = "A",
  "A647" = "A",
  "A6026(M)" = "A(M)",
  "A649" = "A",
  "A6026" = "A",
  "A" = "A",
  "Unclassified" = "Unclassified",
  "Motorway" = "Motorway",
  "B" = "B",
  "A(M)" = "A(M)"
)

# Correct mapping is mutate to  the X1st.Road.Class column
accidents <- accidents %>%
  mutate(X1st.Road.Class = recode(X1st.Road.Class,!!!correct_mapping))



## Using functions from the dplyr to delete any unnecessarily columns or rows.

# Remove the unnecessary column
accidents <- accidents %>%
  select(-Local.Authority)# As it is same for all



## Examine “Age of Casualty” for any outlines using the three methods discussed in the lecture.Justify your answer


# Create a histogram of the Age of Casualty
ggplot(data = accidents, aes(x = Age.of.Casualty)) +
  geom_histogram(binwidth = 10,
                 fill = "blue",
                 color = "black") +
  labs(title = "Histogram of Age of Casualty",
       x = "Age of Casualty",
       y = "Frequency")


# 1) Boxplot method

# Create a boxplot of the age of casualty
boxplot(accidents$Age.of.Casualty, main = "Boxplot of Age of Casualty")

# Identify the outliers
outliers <- boxplot.stats(accidents$Age.of.Casualty)$out
print(outliers)



# 2) 3σ Rule

# Calculate the mean and standard deviation of the age of casualty
mean_age <- mean(accidents$Age.of.Casualty, na.rm = TRUE)
sd_age <- sd(accidents$Age.of.Casualty, na.rm = TRUE)

# Identify the outliers
outliers <-
  accidents$Age.of.Casualty[abs(accidents$Age.of.Casualty - mean_age) > 3 * sd_age]
print(outliers)

# Remove the outliers from the data
#accidents <-
#  accidents[!(accidents$Age.of.Casualty %in% outliers),]


# 3) Hampel Identifier

# Calculate the median and MAD of the age of casualty
median_age <- median(accidents$Age.of.Casualty, na.rm = TRUE)
mad_age <- mad(accidents$Age.of.Casualty, na.rm = TRUE)

# Identify the outliers
outliers <-
  accidents$Age.of.Casualty[abs(accidents$Age.of.Casualty - median_age) > 3.5 * mad_age]
print(outliers)


# Save the cleaned version of the data as "clean_accident.csv"
write.csv(accidents, file = "clean_accident.csv", row.names = FALSE)


##  EXPLORATION

#Importing the clean dataset
accident <- read.csv("clean_accident.csv")


#QUESTION 1

# Create a new column for weather conditions
accident$Weather.Conditions <-
  factor(
    accident$Weather.Conditions,
    levels = 1:9,
    labels = c(
      "Fine without high winds",
      "Raining without high winds",
      "Snowing without high winds",
      "Fine with high winds",
      "Raining with high winds",
      "Snowing with high winds",
      "Fog or mist – if hazard",
      "Other",
      "Unknown"
    )
  )

# male and female casualties
male_accidents <- accident[accident$Sex.of.Casualty == 1,]
female_accidents <- accident[accident$Sex.of.Casualty == 2,]


# Calculate the number of accidents for each weather condition for male and female casualties
male_accidents_by_weather <-
  table(male_accidents$Weather.Conditions)
female_accidents_by_weather <-
  table(female_accidents$Weather.Conditions)


#Print accidents based on weather
print(male_accidents_by_weather)
print(female_accidents_by_weather)


# Determine if there are any weather conditions where male drivers/riders
#have more accidents than female drivers

for (i in 1:length(male_accidents_by_weather)) {
  if (male_accidents_by_weather[i] > female_accidents_by_weather[i]) {
    print(
      paste(
        "Male drivers have",
        male_accidents_by_weather[i] - female_accidents_by_weather[i],
        "accidents more than  females when weather is",
        names(male_accidents_by_weather)[i]
      )
    )
  } else if (female_accidents_by_weather[i] > male_accidents_by_weather[i]) {
    print(
      paste(
        "Female drivers have",
        female_accidents_by_weather[i] - male_accidents_by_weather[i],
        "accidents more than males when weather is",
        names(female_accidents_by_weather)[i]
      )
    )
  }
}


##QUESTION 2

# Convert the Accident.Date column to a date format
accident$Accident.Date <-
  as.Date(accident$Accident.Date, format = "%Y-%m-%d")

# Group the data by year and count the number of casualties for each year
casualties_by_year <-
  aggregate(accident$Accident.Date,
            by = list(Year = format(accident$Accident.Date, "%Y")),
            FUN = length)
colnames(casualties_by_year) <- c("Year", "Total_Casualties")

# Convert the Year variable to a numeric variable
casualties_by_year$Year <- as.numeric(casualties_by_year$Year)

# Plot the trend of casualties over the years
ggplot(casualties_by_year, aes(x = Year, y = Total_Casualties)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Total Casualties", title = "Trend of Casualties Over Time") +
  theme_minimal()


# Print the result
print("Number of casualties for each year:")
print(casualties_by_year)



### Light conditions and severity

custom_palette <- c("#FF5322", "#33FF57", "#3366FF")

# Create a bar plot of the number of casualties by lighting conditions and casualty severity
ggplot(accident, aes(
  x = factor(Lighting.Conditions),
  fill = factor(Casualty.Severity)
)) +
  geom_bar(position = "dodge") +
  labs(x = "Lighting Conditions",
       y = "Number of Casualties",
       title = "Relationship between Lighting Conditions and Casualty Severity") +
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "Daylight: street lights present",
      "Daylight: no street lighting",
      "Daylight: street lighting unknown",
      "Darkness: street lights present and lit",
      "Darkness: street lights present but unlit",
      "Darkness: no street lighting",
      "Darkness: street lighting unknown"
    )
  ) +
  scale_fill_manual(values = custom_palette,
                    labels = c("Fatal", "Serious", "Slight")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.9, 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )



#Weather condition and number of vehicles involved

# Create a bar plot of the number of vehicles involved by weather condition
ggplot(accident, aes(
  x = factor(Weather.Conditions),
  fill = factor(Weather.Conditions)
)) +
  geom_bar(position = "dodge") +
  labs(x = "Weather Conditions",
       y = "Number of Vehicles",
       title = "Relationship between Weather Conditions and Number of Vehicles Involved") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###REGRESSION

#Train the regression model with the following set of attributes:
#“Casualty Class”,“Casualty Severity” and “Type of Vehicle”, “Weather condition”.

# Filter the data where
missing_age <- accident %>%
  filter(
    is.na(Age.of.Casualty) &
      !is.na(Casualty.Class) &
      !is.na(Casualty.Severity) &
      !is.na(Type.of.Vehicle) &
      !is.na(Weather.Conditions)
  )

# View the filtered data
print(missing_age)

# Train the regression model
model <-
  lm(
    Age.of.Casualty ~ Casualty.Class + Casualty.Severity + Type.of.Vehicle + Weather.Conditions,
    data = cleaned_data
  )

# Print the summary of the model
summary(model)

#Predict missing age value
missing_age$predicted_age <- predict(model, newdata = missing_age)

#Round up-to nearest value as age can't be decimal
missing_age$predicted_age <- round(missing_age$predicted_age)

#Replace the predicted age instead of NA in Age of Casualty
accident$Age.of.Casualty[is.na(accident$Age.of.Casualty)] <-
  missing_age$predicted_age

print(missing_age$predicted_age)

#Saving the data
write.csv(accident, file = "regression.csv", row.names = FALSE)
