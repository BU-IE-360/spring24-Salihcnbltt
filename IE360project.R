# Install the required libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(zoo)) install.packages("zoo")
if (!require(forecast)) install.packages("forecast")
if (!require(astsa)) install.packages("astsa")
if (!require(ggthemes)) install.packages("ggthemes")
if (!require(viridis)) install.packages("viridis")
if (!require(reshape2)) install.packages("reshape2")

# Load tidyverse
library(tidyverse)
library(ggthemes)
library(viridis)

# Load the data
production <- read_csv(file.choose())
weather_info <- read_csv(file.choose())

# Pivot wider the weather_info data based on `lat` and `lon`
weather_info_wide <- weather_info %>% 
  pivot_wider(names_from = c(lat, lon),
              values_from = c("dswrf_surface",
                              "tcdc_low.cloud.layer",
                              "tcdc_middle.cloud.layer", 
                              "tcdc_high.cloud.layer",
                              "tcdc_entire.atmosphere",
                              "uswrf_top_of_atmosphere", 
                              "csnow_surface",
                              "dlwrf_surface",
                              "uswrf_surface",
                              "tmp_surface"),
              id_cols = c(date, hour)) # (the id columns are `date` and `hour`)

# Join the `production` and `weather_info_wide` tibbles based on `date` and `hour`
tbl_merged <- production %>% 
  right_join(weather_info_wide, by = c("date",
                                       "hour"))

# Arrange the data based on `date` and `hour`
tbl_merged <- tbl_merged %>% 
  arrange(date,
          hour)

# Add indices
tbl_merged <- tbl_merged %>% 
  mutate(index = 1:nrow(tbl_merged))

# Function to calculate the max capacity in the past 24 hours starting from 48 hours before
calculate_max_capacity <- function(data) {
  data %>%
    mutate(max_capacity = zoo::rollapplyr(production, width = 24, 
                                     FUN = max, 
                                     fill = NA, 
                                     align = "right", 
                                     partial = TRUE) %>%
             lag(48))
}

# Apply the function to the `tbl_merged` tibble
tbl_merged_2 <- calculate_max_capacity(tbl_merged)

# Remove the NAs created by the `calculate_max_capacity` function
tbl_merged_3 <- tbl_merged_2 %>%
  filter(!is.na(max_capacity))

# Fill the missing values in the `production` column with the production data from 24 hours before
tbl_merged_4 <- tbl_merged_3 %>% 
  mutate(production = ifelse(is.na(production),
                             lag(production, 24),
                             production))

# Calculate the correlation between the `production` and `max_capacity` columns
# List of prefixes to calculate averages for
prefixes <- c("dswrf_surface",
              "tcdc_low.cloud.layer",
              "tcdc_middle.cloud.layer",
              "tcdc_high.cloud.layer",
              "tcdc_entire.atmosphere",
              "uswrf_top_of_atmosphere",
              "csnow_surface",
              "dlwrf_surface",
              "uswrf_surface",
              "tmp_surface")

# Function to calculate the average for each regressor and remove the original columns
calculate_and_remove <- function(data, prefix) {
  avg_column <- rowMeans(select(data, starts_with(prefix)), na.rm = TRUE)
  data <- data %>% 
    select(-starts_with(prefix)) %>%
    mutate(!!paste0(prefix, "_avg") := avg_column)
  return(data)
}

# Apply the function to each prefix
for (prefix in prefixes) {
  tbl_merged_4 <- calculate_and_remove(tbl_merged_4, prefix)
}

# Resulting columns
avg_regressors <- c("dswrf_surface_avg",
                    "tcdc_low.cloud.layer_avg",
                    "tcdc_middle.cloud.layer_avg",
                    "tcdc_high.cloud.layer_avg",
                    "tcdc_entire.atmosphere_avg",
                    "uswrf_top_of_atmosphere_avg",
                    "csnow_surface_avg",
                    "dlwrf_surface_avg",
                    "uswrf_surface_avg",
                    "tmp_surface_avg")

# Multiply the avg regressors with the `max_capacity` column
tbl_merged_5 <- tbl_merged_4 %>% 
  mutate_at(vars(avg_regressors),
            list(~ . * max_capacity))

# Add 42 hours lagged 'production' data and 2 hours before temperature data
tbl_merged_6 <- tbl_merged_5 %>% 
  mutate(production_lag_42 = lag(production, 42),
         tmp_surface_avg_lag_2 = lag(tmp_surface_avg, 2))

# Correlation matrix
## Calculate the correlation matrix
cor_matrix <- cor(na.omit(select(tbl_merged_6, -date)))

## Melt the correlation matrix
melted_cor_matrix <- reshape2::melt(cor_matrix)

## Plot the correlation matrix
ggplot(data = melted_cor_matrix, aes(x = Var1,
                                     y = Var2,
                                     fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno",
                       limits = c(-1, 1),
                       name = "Correlation") +
  theme_minimal() + 
  coord_fixed() +
  theme_solarized() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 1,
                                   hjust = 1,
                                   size = 8),
        axis.text.y = element_text(size = 8))

# Plot the production data
tbl_merged_6 %>%
  mutate(datetime = ymd(date) + hours(hour)) %>%
  arrange(datetime) %>% # Create a `datetime` column
  ggplot(aes(x = datetime,
             y = production,
             color = production)) +
  geom_point() +
  labs(title = "Production data",
       x = "Datetime",
       y = "Production") +
  scale_color_viridis_c(option = "inferno") +
  theme_solarized()


# Delete the hours with negligible or no production (between 20:00 and 05:00)
tbl_merged_7 <- tbl_merged_6 %>% 
  filter(hour > 5 & hour < 20)

# Group the data by hour
tbl_by_hour <- tbl_merged_7 %>%
  group_by(hour) %>%
  group_split()

# Fit the models for each hour
## Create a function for this purpose
fit_models <- function(data) {
  lm(production ~ dswrf_surface_avg +
       tcdc_low.cloud.layer_avg +
       tcdc_middle.cloud.layer_avg +
       tcdc_high.cloud.layer_avg +
       tcdc_entire.atmosphere_avg +
       uswrf_top_of_atmosphere_avg +
       csnow_surface_avg +
       dlwrf_surface_avg +
       uswrf_surface_avg +
       tmp_surface_avg +
       tmp_surface_avg_lag_2 +
       production_lag_42,
     data = data)
}

# Apply the function to each hour (fit linear models)
models_by_hour <- map(tbl_by_hour, fit_models)

# Name the models by the hour for easy reference
names(models_by_hour) <- paste0("model_hour_", unique(tbl_merged_7$hour))

# View the models
models_by_hour

# Function to plot residuals for a single model
plot_residuals <- function(model, hour) {
  residuals_df <- data.frame(residuals = resid(model))
  ggplot(residuals_df, aes(x = 1:nrow(residuals_df), y = residuals), hue = residuals) +
    geom_point() +
    scale_color_viridis(option = "inferno") +
    theme_solarized_2() +
    labs(title = paste("Residuals of Linear Model", hour),
         x = "Observation",
         y = "Residuals",
         color = "Hour") +
    theme(legend.position = "none")  # Hide the legend
}

# Loop through each model in models_by_hour and create a plot
for (i in seq_along(models_by_hour)) {
  p <- plot_residuals(models_by_hour[[i]], i)
  print(p)  # Display the plot
  }


# Create a ARIMA model for comparison
## Check ACF and PACF
astsa::acf2(tbl_merged_6$production)

# Auto ARIMA model 
auto_arima_model <- forecast::auto.arima(tbl_merged_6$production)

# View the ARIMA model
auto_arima_model

# Replicate the model ARIMA(3,1,2) in astsa::sarima 

# Check for missing values
sum(is.na(tbl_merged_6$production))

# Plot the production data to identify outliers
ggplot(tbl_merged_6, aes(x = index, y = production)) +
  geom_line() +
  labs(title = "Production Data", x = "Index", y = "Production")

# If necessary, apply a transformation
tbl_merged_6 <- tbl_merged_6 %>% 
  mutate(log_production = log(production + 1))  # Add 1 to avoid log(0)

# Refit the SARIMA model with transformed data
sarima_model <- astsa::sarima(tbl_merged_6$log_production, p = 3, d = 1, q = 2)

