library(terra)
library(readxl)
library(ncdf4)
library(dplyr)
library(tidyr)
library(writexl)
library(lubridate)

# Define coordinates
coordinates <- data.frame(
  No. = 1:10,
  Location = c("Adet", "Farta", "Borena", "Degolo", "Simada", "LayGaynet", "Bahir Dar", "Wereilu", "Wegedie", "Legambo"),
  Lat = c(11.15, 11.725, 10.43, 10.225, 11.205, 11.735, 11.36, 10.4, 10.325, 10.695),
  Long = c(37.99, 37.87, 38.4, 39.34, 38.23, 38.305, 37.195, 39.26, 38.65, 38.89)
)

# Set working directory
setwd(" example C:/Users/hp/Desktop/RSDS")

# Function to process a single year for rsds data
process_year_rsds <- function(year) {
  # Construct file name for rsds (solar radiation)
  nc_file <- sprintf("rsds_day_CanESM5_historical_r1i1p1f1_gn_%d.nc", year)
  
  # Check if file exists
  if (!file.exists(nc_file)) {
    warning(paste("File not found:", nc_file))
    return(NULL)
  }
  
  # Load the .nc file
  r <- rast(nc_file)
  
  # Get time information from the raster
  time_info <- time(r)
  
  # Filter for the specific year (in case file contains multiple years)
  target_year <- year
  year_indices <- which(year(time_info) == target_year)
  
  if (length(year_indices) == 0) {
    warning(paste("No data found for year:", target_year, "in file:", nc_file))
    return(NULL)
  }
  
  # Extract data only for the target year
  r_year <- r[[year_indices]]
  
  # Extract data for all coordinates
  extracted_data <- terra::extract(r_year, coordinates[, c("Long", "Lat")])
  
  if (!is.null(extracted_data) && nrow(extracted_data) > 0) {
    # Remove ID column and keep only data columns
    data_columns <- extracted_data[, -1, drop = FALSE]
    
    # Convert to data frame and add location info
    df <- cbind(coordinates, data_columns)
    
    # Get column names (dates)
    date_columns <- names(data_columns)
    
    # Convert W/m² to MJ/m²/day (common unit for solar radiation)
    # 1 W/m² = 0.0864 MJ/m²/day (since 1 W = 1 J/s, and 86400 seconds in a day)
    df <- df %>%
      mutate(across(all_of(date_columns), ~ .x * 0.0864))
    
    # Transpose the data for the desired format
    df_long <- df %>%
      select(Location, Lat, Long, all_of(date_columns)) %>%
      pivot_longer(cols = all_of(date_columns), 
                   names_to = "Date", 
                   values_to = "Solar_Radiation_MJ") %>%
      mutate(Date = as.Date(Date),
             Year = year(Date),
             Day = yday(Date)) %>%
      select(Year, Day, Date, Location, Lat, Long, Solar_Radiation_MJ) %>%
      arrange(Location, Date)
    
    # Round numeric values to 2 decimal places
    df_long$Solar_Radiation_MJ <- round(df_long$Solar_Radiation_MJ, 2)
    
    # Create wide format for Excel output (if preferred)
    df_wide <- df_long %>%
      select(Year, Day, Date, Location, Solar_Radiation_MJ) %>%
      pivot_wider(names_from = Location, values_from = Solar_Radiation_MJ)
    
    # Save to Excel
    output_file <- sprintf("Rsds%d.xlsx", year)
    write_xlsx(list(Long_Format = df_long, Wide_Format = df_wide), output_file)
    
    return(df_long)
  } else {
    warning(paste("Data extraction failed for year:", year))
    return(NULL)
  }
}

# Alternative function for better memory management
process_year_rsds_optimized <- function(year) {
  nc_file <- sprintf("rsds_day_CanESM5_historical_r1i1p1f1_gn_%d.nc", year)
  
  if (!file.exists(nc_file)) {
    warning(paste("File not found:", nc_file))
    return(NULL)
  }
  
  # Use ncdf4 for better time handling
  nc <- nc_open(nc_file)
  
  # Get time information
  time_data <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  origin_date <- as.Date("1850-01-01")
  dates <- origin_date + time_data
  
  # Filter dates for target year
  target_dates <- dates[year(dates) == year]
  
  if (length(target_dates) == 0) {
    nc_close(nc)
    warning(paste("No data found for year:", year))
    return(NULL)
  }
  
  # Get the data for target dates
  all_data <- list()
  
  for (i in 1:nrow(coordinates)) {
    lat <- coordinates$Lat[i]
    lon <- coordinates$Long[i]
    
    # Extract data for this location
    data_values <- ncvar_get(nc, "rsds", 
                             start = c(which.min(abs(ncvar_get(nc, "lon") - lon)),
                                       which.min(abs(ncvar_get(nc, "lat") - lat)),
                                       1),
                             count = c(1, 1, -1))
    
    # Convert to MJ/m²/day and filter for target year
    solar_data <- data_values[year(dates) == year] * 0.0864
    
    all_data[[coordinates$Location[i]]] <- solar_data
  }
  
  nc_close(nc)
  
  # Create output dataframe
  result <- data.frame(
    Year = year,
    Day = yday(target_dates),
    Date = target_dates
  )
  
  # Add solar radiation data for each location
  for (loc in coordinates$Location) {
    result[[loc]] <- round(all_data[[loc]], 2)
  }
  
  # Save to Excel
  output_file <- sprintf("Rsds%d.xlsx", year)
  write_xlsx(result, output_file)
  
  return(result)
}

# Process all years from 1983 to 2014 for rsds
all_years_data <- list()

for (year in 1983:2014) {
  print(paste("Processing year:", year))
  year_data <- process_year_rsds_optimized(year)
  if (!is.null(year_data)) {
    all_years_data[[as.character(year)]] <- year_data
  }
}

# Combine all years into a single file
if (length(all_years_data) > 0) {
  combined_df <- bind_rows(all_years_data)
  write_xlsx(combined_df, "Rsds_Combined_1983_2014.xlsx")
  print("Combined file created: Rsds_Combined_1983_2014.xlsx")
} else {
  print("No data was processed successfully.")
}