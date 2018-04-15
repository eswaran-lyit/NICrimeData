# https://github.com/eswaran-lyit/NICrimeData.git
print(paste("Start of Amalgamation ", Sys.time()))
# To read the csv files relative to the working directory, create the filelocation based on the working directory
crime_file_location <- paste0(getwd(), '/data/crime_data/')
postcode_file_location <- paste0(getwd(), '/data/CleanNIPostcodeData.csv')
write_file_location <- paste0(getwd(), '/data/generated')
# This will return the list of all csv files present under the root data folder by traversing recursively
csv_file_list <- list.files(path = crime_file_location, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
# This function will take the list of CSV files names, read the contents one by one and append them together 
# to a single data frame and return the final data frame
combine_results <- function(csv_file_list) {
    crime_data <- NULL
    for (csv_file in csv_file_list) {
        temp <- read.csv(header = TRUE, stringsAsFactors = FALSE, csv_file)
        crime_data <- rbind(temp, crime_data)
    }
    return(crime_data)
}

# Assign the combined dataset to the variable crime_data
crime_data <- combine_results(csv_file_list)

# returns the number of rows from crime_data
nrow(crime_data)

# shows the top 5 records from combined data
head(crime_data)

# shows the structure of the crime_data
str(crime_data)
print(paste("Files Merged ", Sys.time()))

# Remove the below columns from the crime_data data frame by assigning NULL
crime_data$Crime.ID <- NULL
crime_data$Falls.within <- NULL
crime_data$Reported.by <- NULL
crime_data$LSOA.code <- NULL
crime_data$LSOA.name <- NULL
crime_data$Last.outcome.category <- NULL
crime_data$Context <- NULL

# write the cleaned data frame to the CSR file
write.csv(crime_data, paste0(write_file_location, '/AllNICrimeData.csv'), row.names = FALSE)

# Remove all the data with 'No Location' value under the Location column and assign it back to crime_data
crime_data <- crime_data[!(crime_data$Location == 'No Location'),]

head(crime_data)
str(crime_data)

# Factorise the Crime type attribute and assign the ordered to False
crime_type_levels <- levels(factor(crime_data$Crime.type))
crime_data$Crime.type = factor(crime_data$Crime.type, levels = crime_type_levels, ordered = FALSE)

head(crime_data)
str(crime_data)

# removes the word On or near on all values under Location column by using gsub and assign it back to crime_data dataframe
crime_data$Location <- gsub("On or near ", "", crime_data$Location)
head(crime_data)
str(crime_data)

# reads the already cleaned post code dataset from csv file and assigns it to the cleaned_post_codes dataframe. Only these 
# "Primary_Thorfare", "Postcode", "Town", "County" fields are read instead of all
cleaned_post_codes <- read.csv(postcode_file_location, header = TRUE)[, c("Primary_Thorfare", "Postcode", "Town", "County")]
# from the list of post codes, unique rows are selected based on the post code value and assigned to unique_post_code
unique_post_code <- cleaned_post_codes[!duplicated(cleaned_post_codes$Postcode),]
# from the unique_post_code rows, unique Primary_Thorfare is selected separately and stored on unique_primary_Thorfare
unique_primary_Thorfare <- unique(as.vector(unique_post_code$Primary_Thorfare))

# This function takes in location as argument and if the value of location is empty it returns empty value, else
# to looks up the unique_post_code list for postcodes matching the location value passed
# the lookup will return multiple postcodes, out of that most popular one is selected by sorting the postcodes in ascending order and last
# value is returned from the list
find_a_postcode <- function(location) {
    if (location == "") {
        return("")
    } else {
        popular_post_code <- tail(names(sort(table(unique_post_code$Postcode[grepl(location, unique_primary_Thorfare, ignore.case = TRUE)]))), 1)
        return(if (is.null(popular_post_code)) "" else popular_post_code)
    }
}

# unique location from the crime_data rows are assigned to the variable unique_crime_location
unique_crime_location <- crime_data[!duplicated(crime_data$Location),]

# now iterate through each unique location from the list and find the corresponding postcodes for the same
# assign the postcode to all the rows which has the matching location in the crime_data dataframe
print(paste("Start of Finding Post codes for crime location ", Sys.time()))
for (row in unique_crime_location$Location) {
    crime_data$Postcode[crime_data$Location == row] <- sapply(row, find_a_postcode)
}
head(crime_data)
str(crime_data)
print(paste("End of Finding Post codes for crime location ", Sys.time()))
# write the information to a csv file for verification
write.csv(crime_data, paste0(write_file_location, '/Crime-data-after-matching-postcodes.csv') , row.names = FALSE)

# clear off the variables by assigning NULL as it won't be used further
unique_post_code <- NULL
unique_primary_Thorfare <- NULL

# get the list of unique postcodes from the crime_data rows
unique_crime_post_code <- crime_data[!duplicated(crime_data$Postcode),]
# sort the crime datas by latitude and longitude together
crime_data_lat_long <- crime_data[order(crime_data$Latitude, crime_data$Longitude),]
# remove the duplicate latitude and longitude from the sorted list
crime_data_lat_long_unique <- crime_data_lat_long[!duplicated(crime_data[c("Latitude", "Longitude")]),]
# extract the rows with unique latitude and longitude where the location is empty
crime_data_lat_long_unique <- crime_data_lat_long_unique[(crime_data_lat_long_unique$Location != ''),]

# extract the data with with empty location to the missing_data variable
mising_data <- crime_data[(crime_data$Location == ''),]
str(mising_data)
# remove all the missing data rows from crime_data variable
crime_data <- crime_data[(!crime_data$Location == ''),]
# get the list of unique missing data based on the latitude and longitude
unique_missing_data <- mising_data[!duplicated(mising_data[c("Latitude", "Longitude")]),]

# intall the library geosphere
install.packages("geosphere")
library(geosphere)

# create distance matrix
print(paste("Start of Geo Sphere ", Sys.time()))
# pass the missing data latitude and longitude & crime data with proper lat, long, location and postcode values 
# to the distm function and use distVincentySphere. This will create a distance matrix for the each latitude and longitude
mat <- distm(unique_missing_data[, c("Longitude", "Latitude")], crime_data_lat_long_unique[, c("Longitude", "Latitude")], fun = distVincentySphere)
# get the location and postcode for the missing lat/long by matching the same with proper lat/long
unique_missing_data$Location <- crime_data_lat_long_unique$Location[max.col(-mat)]
unique_missing_data$Postcode <- crime_data_lat_long_unique$Postcode[max.col(-mat)]
# write the data to the file
write.csv(unique_missing_data, paste0(write_file_location, '/Postcodes_added_for_missing_location_geosphere.csv') , row.names = FALSE)
print(paste("End of Geo Sphere ", Sys.time()))
# clear variables which are not used further
mat <- NULL
crime_data_lat_long <- NULL
install.packages("stringr")
install.packages("data.table")
library(stringr)
library(data.table)
# convert the lat/long to character from num, in order to perform substring
unique_missing_data$Latitude <- as.character(unique_missing_data$Latitude)
unique_missing_data$Longitude <- as.character(unique_missing_data$Longitude)

# this is a recursive method, it will take a latitude and longitude value and returns corresponding postcode for the same.
# Method 1: it looks for direct match for latitude, if a direct match is found, it will look for corresponding longitude match for the same. 
#         Lookup for longitude will be based on substr by reducing one number at the end, till we reach 1 number after dot. If there is no 
#         match, no record will be returned
# Method 2: if there is no direct match for latitued, it will reduce a number at the end and do a lookup again, if no match found this process 
#         will go on till there are 2 digits after dot. if a match found, it will look for corresponding longitude match for the same 
#         Lookup for longitude will be based on substr by reducing one number at the end, till we reach 1 number after dot. If there is no 
#         match, no record will be returned
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
# Iterate through the unique missing data to find if there any missing location, still after the geosphere method. if there is any
# pass it to the tidy_location function and get the correspondoing postcode. once the postcode is obtained, get the corresponding location
# and assign both to the unique_missing_data list
for (row in seq_len(nrow(unique_missing_data[(unique_missing_data$Location == ''), ]))) {
    latitude <- unique_missing_data[row, "Latitude"]
    longitude <- unique_missing_data[row, "Longitude"]
    post_code <- tidy_location(latitude, longitude)
    location <- unique_crime_post_code[(unique_crime_post_code$Postcode == post_code),]$Location
    unique_missing_data$Location[(unique_missing_data$Latitude == latitude & unique_missing_data$Longitude == longitude)] <- location
    unique_missing_data$Postcode[(unique_missing_data$Latitude == latitude & unique_missing_data$Longitude == longitude)] <- post_code
}
# write the data to the file
write.csv(unique_missing_data, paste0(write_file_location, '/Postcodes_added_for_missing_location_through_recursion.csv') , row.names = FALSE)
print(paste("End of Location loop ", Sys.time()))
crime_data_lat_long_unique <- NULL
# Convert all the lat, long, postcode, locatiocation to character to avoid error during merge
print(paste("Start of final tidying of location", Sys.time()))
mising_data$Latitude <- as.factor(mising_data$Latitude)
mising_data$Longitude <- as.factor(mising_data$Longitude)
mising_data$Postcode <- as.character(mising_data$Postcode)
mising_data$Location <- as.character(mising_data$Location)
str(mising_data)
# for each row on the unique_missing_data, iterate through and udpate the location and postcode for all the similar missing rows under missing_data
for (row in seq_len(nrow(unique_missing_data))) {
    lat <- unique_missing_data[row, "Latitude"]
    long <- unique_missing_data[row, "Longitude"]
    postcode <- unique_missing_data[row, "Postcode"]
    location <- unique_missing_data[row, "Location"]
    mising_data$Location[(mising_data$Latitude == lat & mising_data$Longitude == long)] <- location
    mising_data$Postcode[(mising_data$Latitude == lat & mising_data$Longitude == long)] <- postcode
}
# Convert all the lat, long, postcode, locatiocation to character to avoid error during merge
crime_data$Latitude <- as.character(crime_data$Latitude)
crime_data$Longitude <- as.character(crime_data$Longitude)
mising_data$Latitude <- as.character(mising_data$Latitude)
mising_data$Longitude <- as.character(mising_data$Longitude)
str(mising_data)
str(crime_data)
head(crime_data, 10)
# combine the crime_data dataframe with all proper values with the missng_data dataframe which was udpated through tidy_location
crime_data <- rbind(mising_data, crime_data)
# store the combined result to a csv file
write.csv(crime_data, paste0(write_file_location, '/crime_data_after_tidying_location.csv') , row.names = FALSE)
print(paste("End of final tidying of location", Sys.time()))

library(data.table)
# get the unique Post code and location details from the cleaned post code
unique_post_code_location <- cleaned_post_codes[!duplicated(cleaned_post_codes$Postcode),]
# merge the crime_data and unique post code to get a matching town and county values for each and every row under crime_data dataframe
merged_data <- merge(x = crime_data, y = unique_post_code_location[, c("Town", "County", "Postcode")], by = "Postcode", all.x = TRUE)
str(merged_data)
# assing the levels to county and town fields under the merged_data
county_levels = levels(factor(merged_data$County))
merged_data$County = factor(merged_data$County, levels = county_levels, ordered = FALSE)
town_levels <- levels(factor(merged_data$Town))
merged_data$Town = factor(merged_data$Town, levels = town_levels, ordered = FALSE)
str(merged_data)
head(merged_data, 10)

# save the merged data to a csv file
write.csv(merged_data, paste0(write_file_location, '/FinalNICrimeData.csv') , row.names = FALSE)
# clear off variables
town_levels <- NULL
county_levels <- NULL
cleaned_post_codes <- NULL
unique_post_code_location <- NULL
crime_data <- NULL

# filter the merged data to get the town list matching strabane
filter_by_location_strabane <- merged_data[grep("Strabane", merged_data$Town, ignore.case = TRUE),]
# filter the strabane town list to get the post codes matching BT82
strabane_Data <- filter_by_location_strabane[grep("BT82", filter_by_location_strabane$Postcode, ignore.case = TRUE),]
# display the top 10 rows of the strabane list
head(strabane_Data, 10)
str(strabane_Data)
print(paste("End of Amalgamation ", Sys.time()))