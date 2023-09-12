library(dplyr)

# Load data
df <- read.csv("Cleaned_Dataset.csv")
View(df)


# Parse datetimes
# df$DATE <- as.POSIXct(df$DATE, format="%m/%d/%Y %H:%M")

df$Year <- lubridate::year(df$DATE)
df$Month <- lubridate::month(df$DATE)


# Reshape data
df <- tidyr::pivot_longer(df,
                          cols = starts_with("HOURLY"),
                          names_to = "Temp_Type",
                          values_to = "Temp_Value")

# Aggregate by mean daily temp
filtered_df <- df %>%
        select(Year, Month, HOURLYSeaLevelPressure) %>%
        group_by(Year, Month) %>%
        summarise(Mean_Monthly_Temp = mean(HOURLYSeaLevelPressure))

# Extract year and month

View(filtered_df)
# Plot
ggplot(data = filtered_df) +
  geom_line(aes(x = Month, y = Mean_Monthly_Temp)) +
  facet_wrap(~Year) +
  labs(x = "Month", y = "Average Monthly Temperature")




###################

# Load and reshape
df <- read.csv("Cleaned_Dataset.csv")
df <- subset(df, select = -c(HOURLYPressureTendencyIncr, HOURLYPressureTendencyDecr))

df <- tidyr::pivot_longer(df,
                          cols = starts_with("HOURLY"),
                          names_to = "Temp_Type",
                          values_to = "Temp_Value")

# Aggregate by mean monthly temperature
df <- df %>%
        group_by(Year = lubridate::year(DATE),
                 Month = lubridate::month(DATE),
                 Temp_Type) %>%
        summarise(Mean_Monthly_Temp = mean(Temp_Value))

# Plot heatmaps
ggplot(data = df) +
  geom_tile(aes(x = Month, y = Year, fill = Mean_Monthly_Temp)) +
  facet_wrap(~Temp_Type) +
  scale_fill_gradient(low="blue", high="red")