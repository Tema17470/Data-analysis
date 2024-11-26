# Required libraries
library(dplyr)
library(rnaturalearth)
library(sf)

# Load your COVID dataset
covid_data <- read.csv("worldometer_coronavirus_daily_data.csv")

# Extract unique country names from the dataset
covid_countries <- unique(covid_data$country)

# Get geographic data for all countries
world_data <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids for each country
world_data <- world_data %>%
  mutate(centroid = st_centroid(geometry)) %>%    # Calculate centroids
  mutate(
    longitude = st_coordinates(centroid)[, 1],   # Extract longitude
    latitude = st_coordinates(centroid)[, 2]     # Extract latitude
  )

# Create a lookup table with country names and their centroids
country_coordinates <- world_data %>%
  st_drop_geometry() %>%                        # Remove spatial geometry to simplify
  select(name, latitude, longitude) %>%         # Select country name and coordinates
  rename(country = name) %>%                    # Standardize column name for matching
  mutate(country = as.character(country))       # Ensure it's a character column

# Attempt to match COVID dataset countries with geographic data
matched_coordinates <- covid_countries %>%
  as.data.frame() %>%
  rename(country = ".") %>%
  left_join(country_coordinates, by = "country")

# Handle unmatched countries (if any)
unmatched_countries <- matched_coordinates %>%
  filter(is.na(latitude) | is.na(longitude))

# Print a warning if unmatched countries are found
if (nrow(unmatched_countries) > 0) {
  print("The following countries could not be matched:")
  print(unmatched_countries)
}

# Save the matched data to a CSV file
write.csv(matched_coordinates, "country_coordinates.csv", row.names = FALSE)

print("Coordinates saved to country_coordinates.csv")
