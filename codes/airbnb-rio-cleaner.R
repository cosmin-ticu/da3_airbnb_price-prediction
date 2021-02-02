# Cleaning London airbnb file
# v.1.2. 2021-01-04 paths changed


# IN data from web
# out: airbnb_london_cleaned.csv

#setting working directory
rm(list=ls())

#location folders
data_in  <- "data/raw/"
data_out <- "data/clean/"

library(tidyverse)
library(dplyr)

# zero step - most basic of cleaning
data<-read.csv(paste0(data_in,"listings_rio_dec-2020.csv"))

data <- data[grep("calculated", colnames(data), invert = TRUE)]
data <- data[grep("maximum", colnames(data), invert = TRUE)]
data <- data[grep("availab", colnames(data), invert = TRUE)]
data <- data[grep("calend", colnames(data), invert = TRUE)]
data <- data[grep("url", colnames(data), invert = TRUE)]

colnames(data)

data <- data %>% select(-c("scrape_id","last_scraped","name","description","neighborhood_overview",
                           "neighbourhood_group_cleansed","latitude","longitude","bathrooms",
                           "number_of_reviews_ltm","license","reviews_per_month","first_review","last_review",
                           "number_of_reviews_l30d","neighbourhood","minimum_minimum_nights","minimum_nights_avg_ntm",
                           "host_id","host_name","host_since","host_location","host_about","host_response_time",
                           "host_neighbourhood","host_total_listings_count","host_verifications"))

write.csv(data,file=paste0(data_in,"airbnb_rio_listing.csv"))


#####################################

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_rio_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)
              
#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
df$price <-gsub("\\$","",as.character(df$price))
df$price <-gsub("\\,","",as.character(df$price))
df$price <- as.numeric(df$price)

sum(colnames(df) %in% c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable"))
# any(colnames(df) %in% c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable"))

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 

# df$amenities <- df$amenities[lapply(df$amenities,function(x) length(grep("\\u2",x,value=FALSE))) == 0]

# df$amenities <- df$amenities[!str_detect(df$amenities,pattern='\u')]

# for (i in nrow(df)) {
#   for (j in length(df$amenities[[i]])) {
#     df$amenities[[i]] <- df$amenities[[i]][j] %>% filter(!grepl("\\u"))
#   }
# }

# df <- df[!grepl("\\u", df$amenities),]

levs <- levels(factor(unlist(df$amenities)))

levs

# grepl('Parking', df$amenities, ignore.case = FALSE)

df <- cbind(df, as.data.frame( do.call(rbind, lapply( lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

# Assign a vector of phrases
# Faster than the aggregate_columns functon


column_names <- c('free.park','paid.park','pool','kitchen','stove','oven','linen','tv','sound','chair','refri','air condi','heat','storage','coffee','breakfast','workspace',
                  'dish','soap','dryer','wifi','laundr|washer','shampoo','sauna','tub','toiletries','restaurant','bar')
# column_names <- c("stove", "parking", "breakfast")
# word <- 'laundr|washer'


for( word in column_names){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- df %>% select(matches(word),"id")
  
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  df <- merge(df, new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename the new column
  names(df)[names(df) == 'col_name'] <- paste0(word,"_agg")
  
}

# Subset all amenities columns and remove any which have '1' less than 5%
amenities_clean <- df %>% select(27:210, "id")
less_than_5per <- amenities_clean %>% select(where(~mean(. == 1) <= 0.05))
less_than_5per <- less_than_5per %>% select(-contains(c("id")))
amenities_clean <- amenities_clean %>% select(-colnames(less_than_5per))

# Check for counts
amenities_clean_df <- as.data.frame(sapply(amenities_clean, function(x){sum(x)}))

# Merge the original and amenities dataframe
df <- df %>% select(-(27:210))
df <- merge(df,amenities_clean, by = "id", all = FALSE)

names(df) <- trimws(names(df))

df <- df[, !duplicated(colnames(df))]

#write csv
write.csv(df,file=paste0(data_out,"airbnb_rio_cleaned.csv"))
