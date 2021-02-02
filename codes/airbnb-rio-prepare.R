# CLEAR MEMORY
rm(list=ls())


library(tidyverse)
library(stargazer)
library(Hmisc)

# set data dir, load theme and functions
source("codes/gabor_textbook/theme_bg.R")
source("codes/gabor_textbook/da_helper_functions.R")

#location folders
data_in  <- "data/clean/"
output <- "output(s)/"

options(digits = 3)


#-------------------------------------------------------
# Import data
data <- read_csv(paste(data_in,"airbnb_rio_cleaned.csv", sep = ""))

# keep if property type is Apartment, House or Townhouse
table(data$property_type)
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire condominium", "Private room in apartment", "Private room in condominium")) %>% 
  filter(between(accommodates, 2, 6))

# bathrooms as factor
table(data$bathrooms_text)

data$bathrooms_text <- gsub("[^0-9.]", "", data$bathrooms_text) %>% factor()

# neighbourhood_cleansed & property type & room type as factors
table(data$neighbourhood_cleansed)

data$neighbourhood_cleansed <- factor(data$neighbourhood_cleansed)

table(data$room_type)

data$room_type <- factor(data$room_type)
data$property_type <- factor(data$property_type)

data$host_response_rate[data$host_response_rate=='N/A'] <- NA
data$host_acceptance_rate[data$host_acceptance_rate=='N/A'] <- NA
data$bathrooms_text[data$bathrooms_text==''] <- NA

# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_in, "airbnb_rio_workfile.csv"))

library(skimr)
##################################
# DESCRIBE
data <- read_csv(paste(data_in,"airbnb_rio_workfile.csv", sep = ""))
data <- data %>%
  filter(neighbourhood_cleansed == "Copacabana")

data <- data %>% select(-c("neighbourhood_cleansed"))

write_csv(data, paste0(data_in, "airbnb_copacabana_workfile.csv"))

#--------------------------------
data <- read_csv(paste(data_in,"airbnb_copacabana_workfile.csv", sep = ""))

N=nrow(data)
N
# N=5632


#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)
describe(data$accommodates)

boxplot(data$price)

# take log of price - can also be used for prediction; will retain level price for analysis
data <- data %>%
  mutate(ln_price = log(price))

# Remove extreme values in the price (over ~1000 euros/6000 brazilian reals)
data <- data %>%
  filter(price <6000)

# Histograms
lnprice_hist <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
lnprice_hist
ggsave(paste0(output, "lnprice_histogram.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

price_hist <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
price_hist
ggsave(paste0(output, "price_histogram.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

######################
# Handling missing values #
######################
glimpse(data)
skim(data)

# where do we have missing values now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
# data <- data %>%
  # drop_na(price)

# 2. imput when few, not that important
data <- data %>%
  mutate(
    bathrooms_text =  ifelse(is.na(bathrooms_text), median(bathrooms_text, na.rm = T), bathrooms_text), #assume at least 1 bath
    beds = ifelse(is.na(beds), round(accommodates / 1.5), beds), #assume that 1 bed corresponds to about 1.5 accommodates
    beds = ifelse(beds == 0, round(accommodates / 1.5), beds), #assume that 1 bed corresponds to about 1.5 accommodates
    bedrooms = ifelse(is.na(bedrooms), accommodates %% 2, bedrooms), #assume that bedrooms correlate to around half the number of accommodates
  )

# 3. drop columns when many missing not imortant
to_drop <- c("review_scores_accuracy", "review_scores_cleanliness","review_scores_checkin","review_scores_communication","review_scores_location","review_scores_value")
data <- data %>%
  select(-one_of(to_drop))


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_review_scores_rating = ifelse(is.na(review_scores_rating),1, 0),
    review_scores_rating =  ifelse(is.na(review_scores_rating), median(review_scores_rating, na.rm = T), review_scores_rating),
    flag_host_acceptance_rate = ifelse(is.na(host_acceptance_rate),1, 0),
    host_acceptance_rate =  ifelse(is.na(host_acceptance_rate), median(host_acceptance_rate, na.rm = T), host_acceptance_rate),
    flag_host_response_rate = ifelse(is.na(host_response_rate),1, 0),
    host_response_rate =  ifelse(is.na(host_response_rate), median(host_response_rate, na.rm = T), host_response_rate),
  )
table(data$flag_review_scores_rating)

# where do we have missing values now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

price_vs_accommodates <- ggplot(data = data, aes(x=accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,6000)+
  xlim(0,8)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()
price_vs_accommodates

ggsave(paste0(output, "price_vs_accommodates.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# Squares and further values to create
data <- data %>%
  mutate(accommodates2=accommodates^2, ln_accommodates=log(accommodates) ,
         ln_accommodates2=log(accommodates)^2)

# Regression 1: ln price and num of accommodates and squares
lm(ln_price ~ accommodates + accommodates2, data=data)
# Regression 2: ln price and log num of accommodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accommodates
lm(ln_price ~ accommodates, data=data)

## Beds
data %>%
  group_by(beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data <- data %>% filter(beds < 9)

# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(beds))

## bathrooms
ggplot(data, aes(bathrooms_text)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()

# Pool accommodations with 0,1,2,10 bathrooms
describe(data$bathrooms_text)
data <- data %>%
  mutate(f_bathroom = cut(bathrooms_text, c(0,1,2,8), labels=c(0,1,2), right = F) )

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

## Number of reviews
nreview_plot <- data %>%
  filter(number_of_reviews < 100)

ggplot(nreview_plot, aes(number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(number_of_reviews, c(0,1,51,max(data$number_of_reviews)), labels=c('None','1-51','51+'), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())

data <- data %>% filter(!is.na(f_number_of_reviews))

# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)
# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)

## review score effect
ggplot(data = data, aes(x=review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,800)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (Brazilian reals)")+
  theme_bg()

# plot logs of both to see if effect changes
ggplot(data = data, aes(x=log(review_scores_rating) , y=ln_price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (Brazilian reals)")+
  theme_bg()

# Regression 1) ln price - num of review scores
lm(ln_price ~ review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ log(review_scores_rating) ,data=data)
#leave as is

## minimum nights
lm(ln_price ~ minimum_nights,data=data)

# Pool and categorize the number of minimum nights: 1,2,3, 5+
describe(data$minimum_nights)

data <- data %>%
  mutate(f_minimum_nights= cut(minimum_nights, c(1,2,3,5,max(data$minimum_nights)), labels=c('1','2','3-4','5+'), right = F))

describe(data$f_minimum_nights)

data <- data %>% filter(!is.na(f_minimum_nights))

lm(ln_price ~ f_minimum_nights,data=data)

## host listings count
describe(data$host_listings_count)
ggplot(data, aes(x=host_listings_count))+
  geom_histogram()
# Pool and categorize the number of listings: 1,2,5, 10+
data <- data %>%
  mutate(f_host_listings_count = cut(host_listings_count, c(0,1,2,5,10,max(data$host_listings_count)), labels=c('0','1','2-4','5-9','10+'), right = F))
table(data$f_host_listings_count)

data <- data %>% filter(!is.na(f_host_listings_count))

###########################
## look at categoricals  ##
###########################

categoricals <- c("property_type", "room_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#####################################

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, paste0(data_in, "airbnb_copacabana_workfile_adj.csv"))

#------------------------------------------------------------------------------------------------

