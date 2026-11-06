library(terra)
library(readxl)
library(ncdf4)
library(dplyr)
library(tidyr)
library(writexl)

# Set working directory
setwd("C:/Users/hp/Desktop/abebe")

# Define the coordinate points of interest
coordinates <- data.frame(
  No. = 1:10,
  Location = c("Adet", "Farta", "Borena", "Degolo", "Simada", "LayGaynet", 
               "Bahir Dar", "Wereilu", "Wegedie", "Legambo"),
  Lat = c(11.15, 11.725, 10.43, 10.225, 11.205, 11.735, 11.36, 10.4, 10.325, 10.695),
  Long = c(37.99, 37.87, 38.4, 39.34, 38.23, 38.305, 37.195, 39.26, 38.65, 38.89)
)

# Loop through years from 2030 to 2090
for (year in 1983:2014) {
  # Construct the filename
  nc_file <- sprintf("pr_day_IPSL-CM6A-LR_historical_r1i1p1f1_gr_%d.nc", year)
  
  # Check if file exists
  if (!file.exists(nc_file)) {
    message(sprintf("File not found: %s", nc_file))
    next
  }
  
  # Load the .nc file as a SpatRaster
  r <- rast(nc_file)
  
  # Extract data using bilinear interpolation
  RF <- terra::extract(r, coordinates[, c("Long", "Lat")])
  
  # Check if extraction was successful
  if (!is.null(RF)) {
    # Convert extracted data to a data frame
    df <- as.data.frame(RF)
    
    # Add location information
    df <- cbind(coordinates, df)
    
    # Select desired columns except No, Lat, Long, and ID
    desired_columns <- c("Location", names(df)[grepl("^pr_", names(df))])
    df <- df[, desired_columns]
    
    # Remove rows with missing values
    df <- na.omit(df)
    
    # Convert all columns except the first one to numeric and multiply by 86400
    df[, -1] <- lapply(df[, -1], function(x) as.numeric(x) * 86400)
    
    # Transpose the dataframe
    df_transposed <- as.data.frame(t(df))
    
    # Write the transposed dataframe to a temporary file
    temp_file <- "temp_RF.xlsx"
    write_xlsx(df_transposed, temp_file)
    
    # Read the transposed data back in, skipping the first row
    df_final <- read_excel(temp_file, skip = 1)
    
    # Add Year and Day columns
    df_final <- df_final %>%
      mutate(
        Year = year, 
        Day = 1:nrow(df_final)
      ) %>%
      select(Year, Day, everything()) %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    # Write to final Excel file for this year
    output_file <- sprintf("RF%d.xlsx", year)
    write_xlsx(df_final, output_file)
    
    # Clean up temporary file
    file.remove(temp_file)
    
    message(sprintf("Successfully processed year %d", year))
  } else {
    message(sprintf("Error: Data extraction failed for year %d", year))
  }
}

