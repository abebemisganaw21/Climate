library(terra)
library(readxl)
library(ncdf4)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)

# Define coordinates
coordinates <- data.frame(
  No. = 1:10,
  Location = c("Adet", "Farta", "Borena", "Degolo", "Simada", "LayGaynet", "Bahir Dar", "Wereilu", "Wegedie", "Legambo"),
  Lat = c(11.15, 11.725, 10.43, 10.225, 11.205, 11.735, 11.36, 10.4, 10.325, 10.695),
  Long = c(37.99, 37.87, 38.4, 39.34, 38.23, 38.305, 37.195, 39.26, 38.65, 38.89)
)

# Set working directory
setwd("C:/Users/hp/Desktop/TMin")

# Function to process a single year
process_year <- function(year) {
  # Construct file name
  nc_file <- sprintf("tasmin_day_CanESM5_historical_r1i1p1f1_gn_%d.nc", year)
  
  # Check if file exists
  if (!file.exists(nc_file)) {
    warning(paste("File not found:", nc_file))
    return(NULL)
  }
  
  # Load the .nc file
  r <- rast(nc_file)
  
  # Extract data
  extracted_data <- terra::extract(r, coordinates[, c("Long", "Lat")])
  
  if (!is.null(extracted_data)) {
    # Convert to data frame
    df <- as.data.frame(extracted_data)
    df <- cbind(coordinates, df)
    
    # Select desired columns
    desired_columns <- c("Location", names(df)[grepl("^tasmin_", names(df))])
    df <- df[, desired_columns]
    
    # Convert Kelvin to Celsius
    temperature_columns <- grep("tasmin_", names(df), value = TRUE)
    df <- df %>%
      mutate(across(all_of(temperature_columns), ~ .x - 273.15))
    
    # Transpose and format
    df <- as.data.frame(t(df))
    df <- df %>%
      mutate(Year = rep(year, nrow(df)), Day = 1:nrow(df)) %>%
      select(Year, Day, everything())
    
    # Round numeric values
    df[, sapply(df, is.numeric)] <- lapply(df[, sapply(df, is.numeric)], round, 2)
    
    # Save to Excel
    output_file <- sprintf("Tmin%d.xlsx", year)
    write_xlsx(df, output_file)
    
    return(df)
  } else {
    warning(paste("Data extraction failed for year:", year))
    return(NULL)
  }
}

# Process all years from 1983 to 2014
for (year in 1983:2014) {
  print(paste("Processing year:", year))
  process_year(year)
}

# Optional: Combine all years into a single file
combine_years <- function(start_year, end_year) {
  all_data <- list()
  
  for (year in start_year:end_year) {
    file_name <- sprintf("Tminx%d.xlsx", year)
    if (file.exists(file_name)) {
      df <- read_excel(file_name)
      all_data[[as.character(year)]] <- df
    }
  }
  
  combined_df <- bind_rows(all_data)
  write_xlsx(combined_df, "Tmin_Combined_1983_2014.xlsx")
  return(combined_df)
}

# Run the combination (optional)
combined_data <- combine_years(1983, 2014)