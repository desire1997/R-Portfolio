# Libraries
library(tidyverse)
library(readr)
library(dplyr)

# SECTION 1: Data Import and Preparation

# Importing my Data
SwedenHousingPricesOld <- read_csv("C:/Users/Lenovo/Downloads/data/SHP.csv")

# Renaming my Data for Consistency
SwedenHousingPrices <- SwedenHousingPricesOld %>%
  rename(
    ad_id = "ad_id",
    date_published = "date_published",
    typology = "typology",
    asking_price = "asking_price_sek",
    land_area = "land_area_sqm",
    living_area = "living_area_sqm",
    sqm_price = "sqm_price_sek",
    number_of_rooms = "number_rooms",
    address = "address",
    location = "location",
    coordinates = "coordenates"
  )

# View the first few rows of the dataset
head(SwedenHousingPrices)

# Display the names of the variables
names(SwedenHousingPrices)

# Checking the structure of the data
str(SwedenHousingPrices)

# Retrieving specific values
SwedenHousingPrices[4, 3]

# Remove rows with missing values
SwedenHousingPrices <- na.omit(SwedenHousingPrices)

# Convert relevant columns to numeric format
SwedenHousingPrices <- SwedenHousingPrices %>%
  mutate(
    asking_price = as.numeric(asking_price),
    land_area = as.numeric(land_area),
    living_area = as.numeric(living_area),
    sqm_price = as.numeric(sqm_price),
    number_of_rooms = as.numeric(number_of_rooms)
  )


#Display rows where living area is greater than 100 sqm
SwedenHousingPrices[SwedenHousingPrices$living_area > 100, ]

# Select only the relevant columns for analysis
SwedenHousingPrices <- SwedenHousingPrices %>%
  select(ad_id, date_published, typology, asking_price, land_area, 
         living_area, sqm_price, number_of_rooms, address, location, coordinates)



# SECTION 2: Descriptive Statistics


# Summarize key housing metrics
HousingSummary <- SwedenHousingPrices %>%
  summarise(
    # Calculate averages
    Avg_AskingPrice = mean(asking_price, na.rm = TRUE),
    Avg_LivingArea = mean(living_area, na.rm = TRUE),
    Avg_SqmPrice = mean(sqm_price, na.rm = TRUE),
    Avg_NumberOfRooms = mean(number_of_rooms, na.rm = TRUE),
    
    # Calculate median and total for asking price
    Median_AskingPrice = median(asking_price, na.rm = TRUE),
    Total_AskingPrice = sum(asking_price, na.rm = TRUE)

  )

#Distribution of asking prices across property types
price_by_typology <- SwedenHousingPrices %>%
  group_by(typology) %>%
  summarise(
    Avg_AskingPrice = mean(asking_price, na.rm = TRUE),
    Total_AskingPrice = sum(asking_price, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_AskingPrice))
print(price_by_typology)


# Print the summarized statistics
print(HousingSummary)

# Correlation between living_area and asking_price
cor(SwedenHousingPrices$living_area, SwedenHousingPrices$asking_price)


# Correlation between key variables
correlation_matrix <- cor(SwedenHousingPrices %>%
                            select(asking_price, living_area, sqm_price, number_of_rooms), 
                          use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)


# SECTION 3: Regression Analysis

# Simple Linear Regression: Asking Price & Living Area
simple_model <- lm(asking_price ~ living_area, data = SwedenHousingPrices)
print("Coefficients of Simple Model:")
print(summary(simple_model)$coefficients)


# Multiple Linear Regression: Asking Price & Other Variables
multiple_model <- lm(asking_price ~ living_area + sqm_price + number_of_rooms, data = SwedenHousingPrices)
print("Summary of Multiple Regression Model:")
print(summary(multiple_model)



      
# SECTION 4: Visualization     

#Scatterplot: Relationship between living area and asking price
      ggplot(SwedenHousingPrices, aes(x = living_area, y = asking_price, color = typology)) +
        geom_point() +
        labs(
          title = "Scatterplot: Living Area vs. Asking Price",
          x = "Living Area (sqm)",
          y = "Asking Price (SEK)",
          caption = "Source: SHP dataset"
        ) +
        theme_minimal()    
      

 # Bar chart: Average Asking Price by Typology
      SwedenHousingPrices %>%
        group_by(typology) %>%
        summarise(Avg_AskingPrice = mean(asking_price, na.rm = TRUE)) %>%
        ggplot(aes(x = reorder(typology, -Avg_AskingPrice), y = Avg_AskingPrice, fill = typology)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Average Asking Price by Typology", 
             x = "Typology", 
             y = "Average Asking Price") +
        theme_minimal()
      

      
  # Boxplot: Asking Price Distribution Across Typologies
      SwedenHousingPrices %>%
        ggplot(aes(x = typology, y = asking_price, fill = typology)) +
        geom_boxplot() +
        labs(title = "Asking Price Distribution by Typology", 
             x = "Typology", 
             y = "Asking Price (SEK)") +
        theme_minimal()
      
      
  # SECTIION 5: Hypothesis Testing
      
      # Filter for specific typologies ("APARTMENT" and "HOUSE")
      FilteredData <- SwedenHousingPrices %>%
        filter(typology %in% c("APARTMENT", "HOUSE"))
      
      # Perform a T-Test: Asking Price by Typology
      t_test_result <- t.test(asking_price ~ typology, data = FilteredData)
      print("T-Test Result:")
      print(t_test_result)
      
      # Perform a Chi-Square Test
      chi_table <- table(SwedenHousingPrices$typology, SwedenHousingPrices$asking_price > median(SwedenHousingPrices$asking_price, na.rm = TRUE))
      chi_test_result <- chisq.test(chi_table)
      print("Chi-Square Test Result:")
      print(chi_test_result)


      # SECTION 6: Save Cleaned Data

      # Saving the cleaned data
write.table(SwedenHousingPrices, "C:/Users/Lenovo/Downloads/data/real_SwedenHousingPrices.csv", sep = ",", row.names = FALSE)











