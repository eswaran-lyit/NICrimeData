print(paste("Start of Amalgamation ", Sys.time()))
# Using R, amalgamate all of the crime data from each csv file into one dataset. 
# Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset.
file_location <- paste0(getwd(), '/data')
csv_file_list <- list.files(path = file_location, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
combine_results <- function(csv_file_list) {
    crime_data <- NULL
    for (csv_file in csv_file_list) {
        temp <- read.csv(header = TRUE, stringsAsFactors = FALSE, csv_file)
        crime_data <- rbind(temp, crime_data)
    }
    return(crime_data)
}

crime_data <- combine_results(csv_file_list)
print(paste("Files Merged ", Sys.time()))
nrow(crime_data)
# Modify the structure of the newly created AllNICrimeData csv file and remove the following attributes: 
# CrimeID, Reported by, Falls within, LSOA code, LSOA name. Show the structure of the modified file.

crime_data$Crime.ID <- NULL
crime_data$Falls.within <- NULL
crime_data$Reported.by <- NULL
crime_data$LSOA.code <- NULL
crime_data$LSOA.name <- NULL
crime_data$Last.outcome.category <- NULL
crime_data$Context <- NULL

write.csv(crime_data, 'AllNICrimeData.csv', row.names = FALSE)
sapply(crime_data, function(x) sum(is.na(x)))
crime_data <- crime_data[!(crime_data$Location == 'No Location'),]

# Factorise the Crime type attribute. Show the modified structure.

crime_type_levels <- levels(factor(crime_data$Crime.type))
crime_data$Crime.type = factor(crime_data$Crime.type, levels = crime_type_levels, ordered = FALSE)

# Modify the AllNICrimeData dataset so that the Location attribute contains only a street name. 
# For example, the attribute value “On or near Westrock Square” should be modified to only contain “Westrock Square”.

crime_data$Location <- gsub("On or near ", "", crime_data$Location)

# Create a function called find_a_postcode that takes as an input each location attribute from AllNICrimeData 
# and finds a suitable postcode value from the postcode dataset. Use the CleanNIPostcodeData dataset you created 
# in your previous lab practical as the reference data to find postcodes. If there are several postcodes discovered 
# with the same location, choose the most popular postcode for that location. Store the output from the 
# find_a_postcode function in a suitably named variable. Show the structure and number of values in this variable.

cleaned_post_codes <- read.csv("CleanNIPostcodeData.csv", header = TRUE)[, c("Primary_Thorfare", "Postcode", "Town", "County")]

unique_post_code <- cleaned_post_codes[!duplicated(cleaned_post_codes$Postcode),]
unique_primary_Thorfare <- unique(as.vector(unique_post_code$Primary_Thorfare))

find_a_postcode <- function(location) {
    if (location == "" | location == "On or near ") {
        return("")
    } else {
        popular_post_code <- tail(names(sort(table(unique_post_code$Postcode[grepl(location, unique_primary_Thorfare, ignore.case = TRUE)]))), 1)
        return(if (is.null(popular_post_code)) "" else popular_post_code)
    }
}

unique_crime_location <- crime_data[!duplicated(crime_data$Location),]

print(paste("Start of Finding Post codes for crime location ", Sys.time()))
for (row in unique_crime_location$Location) {
    crime_data$Postcode[crime_data$Location == row] <- sapply(row, find_a_postcode)
}
print(paste("End of Finding Post codes for crime location ", Sys.time()))
write.csv(crime_data, 'Crime-data-after-matching-postcodes.csv', row.names = FALSE)

unique_post_code <- NULL
unique_primary_Thorfare <- NULL

# Append the data output from your find_a_postcode function to the AllNICrimeData dataset. Show the modified structure.
# Some location data in the AllNICrimeData has missing location information eg it contains “On or near”. 
# Instead of deleting it from the dataset, create a function called tidy_location that takes as an input 
# any data that does not have complete location information. Using longitude and latitude information, 
# find a close match to the missing location information

unique_crime_post_code <- crime_data[!duplicated(crime_data$Postcode),]

crime_data_lat_long <- crime_data[order(crime_data$Latitude, crime_data$Longitude),]
crime_data_lat_long_unique <- crime_data_lat_long[!duplicated(crime_data[c("Latitude", "Longitude")]),]
crime_data_lat_long_unique <- crime_data_lat_long_unique[(crime_data_lat_long_unique$Location != ''),]

mising_data <- crime_data[(crime_data$Location == ''),]
str(mising_data)
crime_data <- crime_data[(!crime_data$Location == ''),]
unique_missing_data <- mising_data[!duplicated(mising_data[c("Latitude", "Longitude")]),]

install.packages("geosphere")
library(geosphere)

# create distance matrix
print(paste("Start of Geo Sphere ", Sys.time()))
mat <- distm(unique_missing_data[, c("Longitude", "Latitude")], crime_data_lat_long_unique[, c("Longitude", "Latitude")], fun = distVincentySphere)
unique_missing_data$Location <- crime_data_lat_long_unique$Location[max.col(-mat)]
unique_missing_data$Postcode <- crime_data_lat_long_unique$Postcode[max.col(-mat)]
write.csv(unique_missing_data, 'Postcodes_added_for_missing_location_geosphere.csv', row.names = FALSE)
print(paste("End of Geo Sphere ", Sys.time()))
mat <- NULL
crime_data_lat_long <- NULL

library(stringr)
library(data.table)
unique_missing_data$Latitude <- as.character(unique_missing_data$Latitude)
unique_missing_data$Longitude <- as.character(unique_missing_data$Longitude)

tidy_location <- function(latitude, longitude) {
    matching_lat_rows <- crime_data_lat_long_unique[(crime_data_lat_long_unique$Latitude %like% latitude),]
    no_matching_lat_rows <- nrow(matching_lat_rows)
    matching_post_code <- NULL
    if (no_matching_lat_rows != 0) {
        for (i in nchar(longitude):4) {
            sub_long <- substr(longitude, 1, i)
            matching_long_rows <- matching_lat_rows[(matching_lat_rows$Longitude %like% sub_long),]
            no_matching_long_rows <- nrow(matching_long_rows)
            if (no_matching_long_rows == 1) {
                matching_post_code <- matching_long_rows$Postcode
                break
            } else if (no_matching_long_rows > 1) {
                matching_post_code <- tail(names(sort(table(matching_long_rows$Postcode))), 1)
                break
            } else {
                next
            }
        }
    } else {
        if (nchar(latitude) > 4) {
            sub_lat <- substr(latitude, 1, nchar(latitude) - 1)
            matching_post_code <- tidy_location(sub_lat, longitude)
        }
    }
    return(matching_post_code)
}

print(paste("Start of Location loop ", Sys.time()))
for (row in seq_len(nrow(unique_missing_data[(unique_missing_data$Location == ''), ]))) {
    latitude <- unique_missing_data[row, "Latitude"]
    longitude <- unique_missing_data[row, "Longitude"]
    post_code <- tidy_location(latitude, longitude)
    location <- unique_crime_post_code[(unique_crime_post_code$Postcode == post_code),]$Location
    unique_missing_data$Location[(unique_missing_data$Latitude == latitude & unique_missing_data$Longitude == longitude)] <- location
    unique_missing_data$Postcode[(unique_missing_data$Latitude == latitude & unique_missing_data$Longitude == longitude)] <- post_code
}
write.csv(unique_missing_data, 'Postcodes_added_for_missing_location_through_recursion.csv', row.names = FALSE)
print(paste("End of Location loop ", Sys.time()))
crime_data_lat_long_unique <- NULL

print(paste("Start of final tidying of location", Sys.time()))
mising_data$Latitude <- as.factor(mising_data$Latitude)
mising_data$Longitude <- as.factor(mising_data$Longitude)
mising_data$Postcode <- as.character(mising_data$Postcode)
mising_data$Location <- as.character(mising_data$Location)
str(mising_data)
for (row in seq_len(nrow(unique_missing_data))) {
    lat <- unique_missing_data[row, "Latitude"]
    long <- unique_missing_data[row, "Longitude"]
    postcode <- unique_missing_data[row, "Postcode"]
    location <- unique_missing_data[row, "Location"]
    mising_data$Location[(mising_data$Latitude == lat & mising_data$Longitude == long)] <- location
    mising_data$Postcode[(mising_data$Latitude == lat & mising_data$Longitude == long)] <- postcode
}

crime_data$Latitude <- as.character(crime_data$Latitude)
crime_data$Longitude <- as.character(crime_data$Longitude)
mising_data$Latitude <- as.character(mising_data$Latitude)
mising_data$Longitude <- as.character(mising_data$Longitude)
str(mising_data)
str(crime_data)
crime_data <- rbind(mising_data, crime_data)
write.csv(crime_data, 'crime_data_after_tidying_location.csv', row.names = FALSE)
print(paste("End of final tidying of location", Sys.time()))

# Append the AllNICrimeData dataset with new attributes Town, County and Postcode. 
# Use the NIPostcode dataset and match the location attribute to perform the join between both datasets. 
# Modify Town and County attributes to become unordered factors. Show the modified AllNICrimeData structure.

library(data.table)
unique_post_code_location <- cleaned_post_codes[!duplicated(cleaned_post_codes$Postcode),]

merged_data <- merge(x = crime_data, y = unique_post_code_location[, c("Town", "County", "Postcode")], by = "Postcode", all.x = TRUE)
str(merged_data)
county_levels = levels(factor(merged_data$County))
merged_data$County = factor(merged_data$County, levels = county_levels, ordered = FALSE)
town_levels <- levels(factor(merged_data$Town))
merged_data$Town = factor(merged_data$Town, levels = town_levels, ordered = FALSE)
str(merged_data)

# Save your modified AllNICrimeData dataset in a csv file called FinalNICrimeData.

write.csv(merged_data, 'FinalNICrimeData.csv', row.names = FALSE)
town_levels <- NULL
county_levels <- NULL
cleaned_post_codes <- NULL
unique_post_code_location <- NULL
crime_data <- NULL

# Search for all crime data where location contains Strabane and postcode contains BT82. 
# Show the first 10 rows of this data.
filter_by_location_strabane <- merged_data[grep("Strabane", merged_data$Town, ignore.case = TRUE),]
strabane_Data <- filter_by_location_strabane[grep("BT82", filter_by_location_strabane$Postcode, ignore.case = TRUE),]
head(strabane_Data, 10)
print(paste("End of Amalgamation ", Sys.time()))