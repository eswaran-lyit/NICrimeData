# Using R, amalgamate all of the crime data from each csv file into one dataset. 
# Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset.
file_location <- paste0(getwd(), '/data')
csv_file_list <- list.files(path = file_location, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
csv_file_list
combine_results <- function(csv_file_list) {
    crime_data <- NULL
    for (csv_file in csv_file_list) {
        temp <- read.csv(header = TRUE, stringsAsFactors = FALSE, csv_file)
        crime_data <- rbind(temp, crime_data)
    }
    return(crime_data)
}

crime_data <- combine_results(csv_file_list)
write.csv(crime_data, 'AllNICrimeData.csv', row.names = FALSE)
str(crime_data)
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

str(crime_data)
write.csv(crime_data, 'AllNICrimeData.csv', row.names = FALSE)
sapply(crime_data, function(x) sum(is.na(x)))

str(crime_data)
crime_data <- crime_data[!(crime_data$Location == 'No Location'),]
str(crime_data)
# Factorise the Crime type attribute. Show the modified structure.

crime_type_levels <- levels(factor(crime_data$Crime.type))
crime_data$Crime.type = factor(crime_data$Crime.type, levels = crime_type_levels, ordered = FALSE)
str(crime_data)

# Modify the AllNICrimeData dataset so that the Location attribute contains only a street name. 
# For example, the attribute value “On or near Westrock Square” should be modified to only contain “Westrock Square”.

crime_data$Location <- gsub("On or near ", "", crime_data$Location)

# Create a function called find_a_postcode that takes as an input each location attribute from AllNICrimeData 
# and finds a suitable postcode value from the postcode dataset. Use the CleanNIPostcodeData dataset you created 
# in your previous lab practical as the reference data to find postcodes. If there are several postcodes discovered 
# with the same location, choose the most popular postcode for that location. Store the output from the 
# find_a_postcode function in a suitably named variable. Show the structure and number of values in this variable.

cleaned_post_codes <- read.csv("CleanNIPostcodeData.csv", header = TRUE)[, c("Primary_Thorfare", "Postcode", "Town", "County")]
str(cleaned_post_codes)

unique_post_code_count <- cleaned_post_codes[!duplicated(cleaned_post_codes$Postcode),]
str(unique_post_code_count)
unique_primary_Thorfare <- unique(as.vector(unique_post_code_count$Primary_Thorfare))
str(unique_primary_Thorfare)

find_a_postcode <- function(location) {
    if (location == "" | location == "No Location" | location == "On or near ") {
        return("")
    } else {
        popular_post_code <- tail(names(sort(table(unique_post_code_count$Postcode[grepl(location, unique_primary_Thorfare, ignore.case = TRUE)]))), 1)
        return(if (is.null(popular_post_code)) "" else popular_post_code)
    }
}

unique_crime_location <- crime_data[!duplicated(crime_data$Location),]
str(unique_crime_location)

Sys.time()
for (row in unique_crime_location$Location) {
    crime_data$Postcode[crime_data$Location == row] <- sapply(row, find_a_postcode)
}
Sys.time()
str(crime_data)

# Append the data output from your find_a_postcode function to the AllNICrimeData dataset. Show the modified structure.
str(crime_data)

# Some location data in the AllNICrimeData has missing location information eg it contains “On or near”. 
# Instead of deleting it from the dataset, create a function called tidy_location that takes as an input 
# any data that does not have complete location information. Using longitude and latitude information, 
# find a close match to the missing location information

tidy_location <- function(data) {
}

mising_data <- crime_data[(crime_data$Location == '' | crime_data$Location == 'No Location'),]
mising_data$Location <- apply(mising_data, 1, tidy_location)

# Append the AllNICrimeData dataset with new attributes Town, County and Postcode. 
# Use the NIPostcode dataset and match the location attribute to perform the join between both datasets. 
# Modify Town and County attributes to become unordered factors. Show the modified AllNICrimeData structure.
str(crime_data)

str(cleaned_post_codes)

unique_post_code_location <- cleaned_post_codes[!duplicated(cleaned_post_codes$Primary_Thorfare),]
str(unique_post_code_location)
merged_data <- merge(x = crime_data, y = unique_post_code_location[, c("Town", "County", "Postcode")], by = "Postcode", all.x = TRUE)
county_levels = levels(factor(merged_data$County))
merged_data$County = factor(merged_data$County, levels = county_levels, ordered = FALSE)
town_levels <- levels(factor(merged_data$Town))
merged_data$Town = factor(merged_data$Town, levels = town_levels, ordered = FALSE)
str(merged_data)

# Save your modified AllNICrimeData dataset in a csv file called FinalNICrimeData.

write.csv(merged_data, 'FinalNICrimeData.csv', row.names = FALSE)

# Search for all crime data where location contains Strabane and postcode contains BT82. 
# Show the first 10 rows of this data.
str(merged_data)
filter_by_location_strabane <- merged_data[grep("Strabane", merged_data$Town, ignore.case = TRUE),]
strabane_Data <- filter_by_location_strabane[grep("BT82", filter_by_location_strabane$Postcode, ignore.case = TRUE),]
head(strabane_Data, 10)