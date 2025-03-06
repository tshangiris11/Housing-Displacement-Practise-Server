## ----setup, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("tidycensus")   
install.packages("tidyverse") 
install.packages("sf")        

library(tidycensus)
library(tidyverse)
library(sf)


## ----install tmap,echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
install.packages("tmap") 
install.packages("viridis") 


## ----load tmape, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tmap)
library(viridis)

## ----get data, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# API
census_api_key("4a1ebc849fc0b8c6eb495412532ce5918841de8b", overwrite = TRUE)

# variable
acs_vars <- c(
  "B25070_001E",  
  "B25070_007E",  
  "B25003_002E", 
  "B25003_003E",  
  "B19013_001E",  
  "B03002_003E",  # white
  "B03002_004E",  # African
  "B03002_006E",  # asian
  "B03002_012E"   # latino
)
#  2014-2018data
la_acs_2018 <- get_acs(
  geography = "tract",
  variables = acs_vars,
  year = 2018,  # 2014-2018 rolling average
  state = "CA",
  county = "Los Angeles County",
  survey = "acs5"
)
la_acs_2018_wide <- la_acs_2018 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  group_by(GEOID, NAME) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "clean_{.col}"), .groups = "drop")

# rename
la_acs_2018_wide <- la_acs_2018_wide %>%
  rename(
    total_renters = clean_B25070_001,
    extreme_rent_burden = clean_B25070_007,
    owner_occupied = clean_B25003_002,
    renter_occupied = clean_B25003_003,
    median_income = clean_B19013_001,
    white_pop = clean_B03002_003,
    black_pop = clean_B03002_004,
    asian_pop = clean_B03002_006,
    latino_pop = clean_B03002_012
  ) %>%
  mutate(year = 2018)  
  # 2018-2022
la_acs_2022 <- get_acs(
  geography = "tract",
  variables = acs_vars,
  year = 2022,  # 2018-2022 rolling average
  state = "CA",
  county = "Los Angeles County",
  survey = "acs5"
)

la_acs_2022_wide <- la_acs_2022 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  group_by(GEOID, NAME) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "clean_{.col}"), .groups = "drop")

# rename variable
la_acs_2022_wide <- la_acs_2022_wide %>%
  rename(
    total_renters = clean_B25070_001,
    extreme_rent_burden = clean_B25070_007,
    owner_occupied = clean_B25003_002,
    renter_occupied = clean_B25003_003,
    median_income = clean_B19013_001,
    white_pop = clean_B03002_003,
    black_pop = clean_B03002_004,
    asian_pop = clean_B03002_006,
    latino_pop = clean_B03002_012
  ) %>%
  mutate(year = 2022)  


## ----merge data, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
# merge
la_acs_compare <- bind_rows(la_acs_2018_wide, la_acs_2022_wide)

la_acs_compare <- la_acs_compare %>%
  mutate(
    rent_burden_pct = (extreme_rent_burden / total_renters) * 100,
    homeownership_rate = (owner_occupied / (owner_occupied + renter_occupied)) * 100,
    renter_rate = (renter_occupied / (owner_occupied + renter_occupied)) * 100
  )


## ----population by race data, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Ensure post-COVID period is correctly labeled
la_acs_lai_merged <- la_acs_compare %>%
  mutate(post = ifelse(year == 2022, "Post-COVID (2022)", "Pre-COVID (2018)"))

# 📊 **Summary: Total Population by Race Group (Pre & Post COVID)**
race_population_summary <- la_acs_lai_merged %>%
  group_by(post) %>%
  summarise(
    total_population = sum(white_pop + black_pop + latino_pop + asian_pop, na.rm = TRUE),  # Total population
    white_population = sum(white_pop, na.rm = TRUE),
    black_population = sum(black_pop, na.rm = TRUE),
    latino_population = sum(latino_pop, na.rm = TRUE),
    asian_population = sum(asian_pop, na.rm = TRUE),
    other_population = total_population - (white_population + black_population + latino_population + asian_population),
    .groups = "drop"
  )

# Print results
print(race_population_summary)

## ----covid vs income group data, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Ensure post-COVID period is correctly labeled
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(post = ifelse(year == 2022, "Post-COVID (2022)", "Pre-COVID (2018)"))

# **Define Income Groups** (Low, Middle, High) Based on Median Income Distribution
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(
    income_group = case_when(
      median_income < quantile(median_income, 0.33, na.rm = TRUE) ~ "Low Income",
      median_income >= quantile(median_income, 0.33, na.rm = TRUE) & median_income < quantile(median_income, 0.67, na.rm = TRUE) ~ "Middle Income",
      median_income >= quantile(median_income, 0.67, na.rm = TRUE) ~ "High Income",
      TRUE ~ "Unknown"
    )
  )

# 📊 **Summary: Total Population by Income Group (Pre & Post COVID)**
income_population_summary <- la_acs_lai_merged %>%
  group_by(post, income_group) %>%
  summarise(
    total_population = sum(white_pop + black_pop + latino_pop + asian_pop, na.rm = TRUE),  # Total population within each income group
    .groups = "drop"
  )

# Print results
print(income_population_summary)


## ----Total Population by Income Group (Pre vs. Post COVID-19), echo=FALSE--------------------------------------------------------------------------------------------------------------------
library(scales)  # For formatting numbers with commas

# Ensure post-COVID period is correctly labeled
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(post = ifelse(year == 2022, "Post-COVID (2022)", "Pre-COVID (2018)"))

# Define Income Groups (Low, Middle, High) Based on Median Income Distribution
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(
    income_group = case_when(
      median_income < quantile(median_income, 0.33, na.rm = TRUE) ~ "Low Income",
      median_income >= quantile(median_income, 0.33, na.rm = TRUE) & median_income < quantile(median_income, 0.67, na.rm = TRUE) ~ "Middle Income",
      median_income >= quantile(median_income, 0.67, na.rm = TRUE) ~ "High Income",
      TRUE ~ "Unknown"
    )
  )

# Summary: Total Population by Income Group (Pre & Post COVID)
income_population_summary <- la_acs_lai_merged %>%
  group_by(post, income_group) %>%
  summarise(
    total_population = sum(white_pop + black_pop + latino_pop + asian_pop, na.rm = TRUE),  # Total population within each income group
    .groups = "drop"
  )

# Plot total population by income group before and after COVID-19
ggplot(income_population_summary, aes(x = income_group, y = total_population, fill = post)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +       # Adjust bar width and dodge spacing
  geom_text(aes(label = scales::comma(total_population)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4) +                                                     # Display values on bars
  labs(
    title = "Total Population by Income Group (Pre vs. Post COVID-19)",
    x = "Income Group",
    y = "Total Population",
    fill = "Time Period"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),                                    # Rotate x-axis labels for readability
    plot.title = element_text(face = "bold", hjust = 0.5),                                # Center-align the title and make it bold
    legend.position = "top"                                                              # Move legend to the top for better layout
  ) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +                                   # Use distinct colors for time periods
  scale_y_continuous(labels = scales::comma)   


## ----Summary: Rent Burden Population & Rate by Income Group (Pre & Post COVID)*, echo=FALSE--------------------------------------------------------------------------------------------------
# Ensure post-COVID period is correctly labeled
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(post = ifelse(year == 2022, "Post-COVID (2022)", "Pre-COVID (2018)"))

# **Define Income Groups** (Low, Middle, High) Based on Median Income Distribution
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(
    income_group = case_when(
      median_income < quantile(median_income, 0.33, na.rm = TRUE) ~ "Low Income",
      median_income >= quantile(median_income, 0.33, na.rm = TRUE) & median_income < quantile(median_income, 0.67, na.rm = TRUE) ~ "Middle Income",
      median_income >= quantile(median_income, 0.67, na.rm = TRUE) ~ "High Income",
      TRUE ~ "Unknown"
    )
  )

# 📊 **Summary: Rent Burden Population & Rate by Income Group (Pre & Post COVID)**
rent_burden_summary <- la_acs_lai_merged %>%
  group_by(post, income_group) %>%
  summarise(
    rent_burdened_population = sum(extreme_rent_burden, na.rm = TRUE),  # Total rent-burdened households
    total_renter_population = sum(renter_occupied, na.rm = TRUE),  # Total renters
    rent_burden_rate = (rent_burdened_population / total_renter_population) * 100,  # Compute rent burden rate
    .groups = "drop"
  )

# Print results
print(rent_burden_summary)

## ----plot: Rent Burden Population & Rate by Income Group (Pre & Post COVID)*, echo=FALSE-----------------------------------------------------------------------------------------------------
# Plot rent burden rate by income group with pre vs. post COVID comparison
ggplot(rent_burden_summary, aes(x = income_group, y = rent_burden_rate, fill = post)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = round(rent_burden_rate, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Pre-COVID (2018)" = "steelblue", "Post-COVID (2022)" = "darkred")) +
  labs(title = "Rent Burden Rate by Income Group (Pre vs. Post COVID)",
       x = "Income Group",
       y = "Rent Burden Rate (%)",
       fill = "Time Period") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        axis.text.x = element_text(angle = 45, hjust = 1))


## ----summary:homeownership*, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------

# Assign a dominant race group to each observation
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(
    race_group = case_when(
      white_pop > black_pop & white_pop > latino_pop & white_pop > asian_pop ~ "White",
      black_pop > white_pop & black_pop > latino_pop & black_pop > asian_pop ~ "Black",
      latino_pop > white_pop & latino_pop > black_pop & latino_pop > asian_pop ~ "Latino",
      asian_pop > white_pop & asian_pop > black_pop & asian_pop > latino_pop ~ "Asian",
      TRUE ~ "Other"  # Fallback category if no dominant group
    )
  )

# Ensure pre/post-COVID labeling
la_acs_lai_merged <- la_acs_lai_merged %>%
  mutate(post = ifelse(year == 2022, "Post-COVID (2022)", "Pre-COVID (2018)"))
  # Summary statistics for homeownership by race group
homeownership_summary <- la_acs_lai_merged %>%
  group_by(post, race_group) %>%
  summarise(
    homeowner_population = sum(owner_occupied, na.rm = TRUE),  # Total homeowners
    total_population = sum(owner_occupied + renter_occupied, na.rm = TRUE),  # Total housing units
    homeownership_rate = (homeowner_population / total_population) * 100,  # Compute homeownership rate
    .groups = "drop"
  )

# Print results
print(homeownership_summary)

## ----plot:homeownership*, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
# Ensure the dataset is correctly structured and filter out any NA, NaN, or "Other"
homeownership_summary <- homeownership_summary %>%
  filter(!is.na(homeownership_rate) & !is.nan(homeownership_rate) & race_group != "Other")

# Create a side-by-side bar chart with colors and percentage labels
ggplot(homeownership_summary, aes(x = race_group, y = homeownership_rate, fill = post)) +
  geom_bar(stat = "identity", position = "dodge") +  # Side-by-side bars
  geom_text(aes(label = round(homeownership_rate, 1)),  # Add text labels
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Pre-COVID (2018)" = "steelblue", "Post-COVID (2022)" = "darkred")) +
  labs(
    title = "Homeownership Rate by Race Group (Pre vs. Post COVID)",
    x = "Race Group",
    y = "Homeownership Rate (%)",
    fill = "Time Period"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## ----la map file*, echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load libraries
library(tigris)
library(sf)

# Get LA County census tracts shapefile
la_map_data <- tracts(state = "CA", county = "Los Angeles", year = 2020, class = "sf")

# Convert GEOID to character to match `la_acs_compare`
la_map_data$GEOID <- as.character(la_map_data$GEOID)

# Check if the data loads properly
head(la_map_data)


## ----la map Homeownership in Los Angeles (2013-2018 vs. 2018-2022)*, echo=FALSE--------------------------------------------------------------------------------------------------------------
# Assign periods with explicit ordering
la_acs_compare <- la_acs_compare %>%
  mutate(post = factor(ifelse(year == 2022, "Post-COVID (2018-2022)", "Pre-COVID (2013-2018)"),
                       levels = c("Pre-COVID (2013-2018)", "Post-COVID (2018-2022)")))

# Convert GEOID to character to match with spatial data
la_acs_compare$GEOID <- as.character(la_acs_compare$GEOID)
# Load Census tract geometry for Los Angeles County
la_tracts <- tracts(state = "CA", county = "Los Angeles", year = 2021, class = "sf")

# Merge ACS data with tract boundaries
la_map_data <- la_tracts %>%
  left_join(la_acs_compare, by = "GEOID")
  # 🏡 Homeownership Map: Actual Number of Homeowners (Side-by-Side)
ggplot(la_map_data) +
  geom_sf(aes(fill = owner_occupied), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "Owner-Occupied Homes") +
  facet_wrap(~post, ncol = 2) +  # Left = Pre-COVID, Right = Post-COVID
  labs(
    title = "Homeownership in Los Angeles (2014-2018 vs. 2018-2022)",
    subtitle = "Total number of owner-occupied homes by census tract",
    caption = "Data Source: ACS 2013-2018 & ACS 2018-2022"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


## ----la map Rent Burden in Los Angeles (2013-2018 vs. 2018-2022)*, echo=FALSE----------------------------------------------------------------------------------------------------------------
# Assign periods with explicit ordering
la_acs_compare <- la_acs_compare %>%
  mutate(post = factor(ifelse(year == 2022, "Post-COVID (2018-2022)", "Pre-COVID (2013-2018)"),
                       levels = c("Pre-COVID (2013-2018)", "Post-COVID (2018-2022)")))

# Convert GEOID to character to match with spatial data
la_acs_compare$GEOID <- as.character(la_acs_compare$GEOID)
# 🏠 Rent Burden Map: Actual Number of Rent-Burdened Households (Side-by-Side)
ggplot(la_map_data) +
  geom_sf(aes(fill = extreme_rent_burden), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "Rent-Burdened Households") +
  facet_wrap(~post, ncol = 2) +  # Ensure left = Pre-COVID, right = Post-COVID
  labs(
    title = "Rent Burden in Los Angeles (2014-2018 vs. 2018-2022)",
    subtitle = "Total number of rent-burdened households by census tract",
    caption = "Data Source: ACS 2013-2018 & ACS 2018-2022"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

## ----la map Median Household Income in Los Angeles (2013-2018 vs. 2018-2022)*, echo=FALSE----------------------------------------------------------------------------------------------------
# Assign periods with explicit ordering
la_acs_compare <- la_acs_compare %>%
  mutate(post = factor(ifelse(year == 2022, "Post-COVID (2018-2022)", "Pre-COVID (2014-2018)"),
                       levels = c("Pre-COVID (2013-2018)", "Post-COVID (2018-2022)")))

# Convert GEOID to character to match with spatial data
la_acs_compare$GEOID <- as.character(la_acs_compare$GEOID)
# Load Census tract geometry for Los Angeles County
la_tracts <- tracts(state = "CA", county = "Los Angeles", year = 2021, class = "sf")

# Merge ACS data with tract boundaries
la_map_data <- la_tracts %>%
  left_join(la_acs_compare, by = "GEOID")
  # 💰 Median Income Map: Actual Dollar Amount (Side-by-Side)
ggplot(la_map_data) +
  geom_sf(aes(fill = median_income), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "Median Income ($)") +
  facet_wrap(~post, ncol = 2) +  # Left = Pre-COVID, Right = Post-COVID
  labs(
    title = "Median Household Income in Los Angeles (2013-2018 vs. 2018-2022)",
    subtitle = "Income disparities across census tracts before and after COVID-19",
    caption = "Data Source: ACS 2013-2018 & ACS 2018-2022"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


## ----Box plot Income Variation Across Racial Groups (Pre vs. Post COVID-19)*, echo=FALSE-----------------------------------------------------------------------------------------------------
la_acs_long <- la_acs_compare %>%
  mutate(
    race_group = case_when(
      white_pop > black_pop & white_pop > latino_pop & white_pop > asian_pop ~ "White",
      black_pop > white_pop & black_pop > latino_pop & black_pop > asian_pop ~ "Black",
      latino_pop > white_pop & latino_pop > black_pop & latino_pop > asian_pop ~ "Latino",
      asian_pop > white_pop & asian_pop > black_pop & asian_pop > latino_pop ~ "Asian",
      TRUE ~ "Other"
    ),
    post = ifelse(year == 2022, "Post-COVID (2018-2022)", "Pre-COVID (2014-2018)")
  ) %>%
  filter(!is.na(median_income))  # Remove missing income values

# Plot side-by-side boxplot
ggplot(la_acs_long, aes(x = race_group, y = median_income, fill = post)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Remove extreme outliers for better readability
  scale_y_continuous(labels = scales::comma) +  # Format y-axis for large numbers
  scale_fill_manual(values = c("#1f78b4", "#e31a1c")) +  # Blue for Pre-COVID, Red for Post-COVID
  labs(
    title = "Income Variation Across Racial Groups (Pre vs. Post COVID-19)",
    x = "Race Group",
    y = "Median Household Income ($)",
    fill = "Time Period"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

## ----Racial Composition Shift (Pre vs. Post COVID-19)*, echo=FALSE---------------------------------------------------------------------------------------------------------------------------
# Compute total population explicitly
la_acs_compare <- la_acs_compare %>%
  mutate(total_population = white_pop + black_pop + latino_pop + asian_pop)

# Aggregate racial population pre- and post-COVID
race_summary <- la_acs_compare %>%
  mutate(
    period = ifelse(year == 2022, "Post-COVID (2018-2022)", "Pre-COVID (2013-2018)")
  ) %>%
  group_by(period) %>%
  summarise(
    White = sum(white_pop, na.rm = TRUE),
    Black = sum(black_pop, na.rm = TRUE),
    Latino = sum(latino_pop, na.rm = TRUE),
    Asian = sum(asian_pop, na.rm = TRUE),
    Other = sum(total_population, na.rm = TRUE) - (White + Black + Latino + Asian)  # Compute 'Other'
  ) %>%
  pivot_longer(cols = -period, names_to = "race_group", values_to = "population") %>%
  group_by(period) %>%
  mutate(percentage = (population / sum(population)) * 100) %>%
  filter(population > 0)  # Remove zero-population categories

# 🎨 Create a side-by-side bar chart
ggplot(race_summary, aes(x = race_group, y = percentage, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Side-by-side bars
  scale_fill_manual(values = c("Pre-COVID (2014-2018)" = "#1f78b4", "Post-COVID (2018-2022)" = "#e31a1c")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +  # Add percentage labels
  labs(
    title = "Racial Composition Shift (Pre vs. Post COVID-19)",
    x = "Race Group", y = "Percentage (%)",
    fill = "Time Period"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


## ----OLS Regression: How income group affects rent burden during COVID*, echo=FALSE----------------------------------------------------------------------------------------------------------
install.packages('fixest')
# Ensure racial composition is calculated separately for each year
la_acs_compare <- la_acs_compare %>%
  group_by(year) %>%  
  mutate(
    total_pop = white_pop + black_pop + latino_pop + asian_pop,  # Ensure it's correct per year
    white_pct = (white_pop / total_pop) * 100,
    black_pct = (black_pop / total_pop) * 100,
    latino_pct = (latino_pop / total_pop) * 100,
    asian_pct = (asian_pop / total_pop) * 100
  ) %>%
  ungroup() 
# Create income groups based on quantiles
la_acs_compare <- la_acs_compare %>%
  mutate(
    income_group = case_when(
      median_income <= quantile(median_income, 0.33, na.rm = TRUE) ~ "Low Income",
      median_income > quantile(median_income, 0.33, na.rm = TRUE) & 
      median_income <= quantile(median_income, 0.66, na.rm = TRUE) ~ "Middle Income",
      median_income > quantile(median_income, 0.66, na.rm = TRUE) ~ "High Income",
      TRUE ~ NA_character_  # Handle missing values
    )
  )
  # Convert income_group to factor for regression
la_acs_compare <- la_acs_compare %>%
  mutate(income_group = factor(income_group, levels = c("Low Income", "Middle Income", "High Income")))
  # Load necessary library
library(fixest)

# Run OLS Regression: How income group affects rent burden during COVID
rent_burden_reg <- feols(
  rent_burden_pct ~ post * income_group + 
    homeownership_rate + median_income + total_renters + 
    extreme_rent_burden + white_pct + black_pct + latino_pct | GEOID,
  data = la_acs_compare, 
  cluster = ~GEOID  # Cluster standard errors at GEOID level
)

# Show regression summary
summary(rent_burden_reg)


## ----Run OLS Regression: Homeownership & Income Groups During COVID*, echo=FALSE-------------------------------------------------------------------------------------------------------------
# Ensure income groups are categorized separately for each year
la_acs_compare <- la_acs_compare %>%
  group_by(year) %>%  # Group by year to compute quantiles separately
  mutate(
    income_group = case_when(
      median_income <= quantile(median_income, 0.33, na.rm = TRUE) ~ "Low Income",
      median_income > quantile(median_income, 0.33, na.rm = TRUE) & 
      median_income <= quantile(median_income, 0.66, na.rm = TRUE) ~ "Middle Income",
      median_income > quantile(median_income, 0.66, na.rm = TRUE) ~ "High Income",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%  # Remove grouping to avoid affecting regression
  mutate(income_group = factor(income_group, levels = c("Low Income", "Middle Income", "High Income")))
# Ensure racial composition is calculated separately for each year
la_acs_compare <- la_acs_compare %>%
  group_by(year) %>%  
  mutate(
    total_pop = white_pop + black_pop + latino_pop + asian_pop,  # Ensure it's correct per year
    white_pct = (white_pop / total_pop) * 100,
    black_pct = (black_pop / total_pop) * 100,
    latino_pct = (latino_pop / total_pop) * 100,
    asian_pct = (asian_pop / total_pop) * 100
  ) %>%
  ungroup()  # Remove grouping

# Run OLS Regression: Homeownership & Income Groups During COVID
homeownership_reg <- feols(
  homeownership_rate ~ post * income_group + 
    median_income + total_renters + extreme_rent_burden + 
    white_pct + black_pct + latino_pct | GEOID,
  data = la_acs_compare, 
  cluster = ~GEOID  # Cluster standard errors at GEOID level
)

# Show regression summary
summary(homeownership_reg)


## ----Run OLS Regression: Homeownership Rate*, echo=FALSE-------------------------------------------------------------------------------------------------------------------------------------
# ✅ Run OLS Regression: Homeownership Rate
reg_homeownership <- feols(
  homeownership_rate ~ post * (black_pct + latino_pct + asian_pct) +
    median_income + total_renters + extreme_rent_burden + income_group | GEOID,
  data = la_acs_compare, 
  cluster = ~GEOID
)
summary(reg_homeownership)


## ----Run OLS Regression: rent burden Rate*, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------
# Run OLS Regression: Rent Burden
reg_rent_burden <- feols(
  rent_burden_pct ~ post * (black_pct + latino_pct + asian_pct) +
    median_income + total_renters + extreme_rent_burden + income_group | GEOID,
  data = la_acs_compare, 
  cluster = ~GEOID
)
summary(reg_rent_burden)


## ----Run OLS Regression: median income*, echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------
# ✅ Run OLS Regression: Median Income
reg_median_income <- feols(
  median_income ~ post * (black_pct + latino_pct + asian_pct) +
    homeownership_rate + total_renters + extreme_rent_burden + income_group | GEOID,
  data = la_acs_compare, 
  cluster = ~GEOID
)
summary(reg_median_income)

