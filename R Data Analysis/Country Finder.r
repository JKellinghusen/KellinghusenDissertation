if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
library(httr)

library(readxl)
library(jsonlite)

# Define the file path
file_path <- "C:/Users/jtorr/OneDrive/DrPH/Dissertation/Julius TK Dissertation Deliverables/Annex D - Pilot Results.xlsx"

# Read the Excel file
print("Reading Excel file...")
data <- read_excel(file_path, sheet = 1)
print("Excel file read successfully.")

# Read latitude and longitude from columns J and K and convert to numeric
latitudes <- round(as.numeric(data[[10]]), 4)  # Column J
longitudes <- round(as.numeric(data[[11]]), 4)  # Column K

# Initialize a vector to store country names
# countries <- vector("character", length(latitudes))

# Custom function to perform reverse geocoding using the correct URL
reverse_geocode_custom <- function(lat, lon) {
    url <- sprintf("http://photon.komoot.io/reverse?lon=%f&lat=%f", lon, lat)
    print(paste("Accessing URL:", url))  # Print the URL
    response <- GET(url)
    if (status_code(response) == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        print(substr(content, 1, 1000))  # Print the first 1000 characters of the response
        country <- sub(".*\"country\":\"([^\"]+)\".*", "\\1", content)
        if (country != content) {
            return(country)
        } else {
            return(NA)
        }
    } else {
        return(NA)
    }
}

# Loop through each coordinate and find the country name
print("Starting reverse geocoding...")
for (i in 1:length(latitudes)) {
  if (!is.na(latitudes[i]) && !is.na(longitudes[i])) {
    tryCatch({
      country <- reverse_geocode_custom(latitudes[i], longitudes[i])
      if (!is.na(country)) {
        countries[i] <- country
        print(paste("Processed coordinate", i, ":", latitudes[i], longitudes[i], "->", countries[i]))
      } else {
        countries[i] <- NA
        print(paste("Country not found for coordinate", i, ":", latitudes[i], longitudes[i]))
      }
    }, error = function(e) {
      print(paste("Error processing coordinate", i, ":", latitudes[i], longitudes[i], "->", e$message))
      countries[i] <- NA
    })
  } else {
    countries[i] <- NA
    print(paste("Skipped coordinate", i, "due to missing values."))
  }
}

# Add the country names to the data frame
data$Country <- countries

# Save the updated data back to the same Excel file
print("Saving updated data to Excel file...")
write.xlsx(data, file_path, sheetName = "Sheet1", overwrite = TRUE)
print("Data saved successfully.")

# Print success message
print("Success")
