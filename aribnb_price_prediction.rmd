
```{r setup}
# Load standard libraries
library(tidyverse)
library(dplyr)

# Read file
bnb <- read.csv(file="/Users/Kelly/Desktop/UW/IMT573/pset07/airbnb-vancouver-bc-listings.csv.bz2", sep = ',')

# Data cleaning
bnb <- bnb %>% 
  filter(!is.na(price))
# Convert the data type for pricing
bnb$price <- as.numeric(gsub("[^0-9.]", "", bnb$price))
# Remove NAs
bnb <- bnb %>% 
  filter(!is.na(price)) %>% 
  mutate(bedrooms = ifelse(is.na(bedrooms), 0.5 * accommodates, bedrooms))

# Analyze the distribution of price
hist_price <- hist(bnb$price, main = "Histogram of Airbnb", xlab = "Price", col = "lightblue", border = "black")
hist_price_log <- hist(log(bnb$price), main = "Log Histogram of Airbnb", xlab = "Price", col = "lightblue", border = "black")

# Convert the number of bedrooms into categories
bnb <- bnb %>% 
  mutate(bed_group = cut(bedrooms,
                 breaks=c(0,1,2,3,Inf),
                 labels=c("0","1","2","3+")))


# Is linear regression with log better or without log?
# The model with log price is better since it has a higher R-squared (0.3195) compared to the R-squared of the model without log (0.1138).
```{r}
price_bed_log <- lm(log(price) ~ bed_group, data=bnb)
summary(price_bed_log)

price_bed <- lm(price ~ bed_group, data=bnb)
summary(price_bed)

# Convert the room type into three categories
bnb <- bnb %>% 
    mutate(room_type = ifelse(room_type == "Entire home/apt", "Entire home/apt",
                              ifelse(room_type == "Private room", "Private room", "Other"))) %>% 
    mutate(accommodates = cut(accommodates,
                              breaks=c(1,2,3,Inf),
                              labels=c("1","2","3 or more")))

# Linear regression
price_room_acc_log <- lm(log(price) ~ room_type + accommodates, data=bnb)
summary(price_room_acc_log)

# Prediction
predict <- predict(price_room_acc_log, bnb, type="response")

# RMSE calculation
RMSE <- sqrt(mean(log(bnb$price)-predict)^2)

# Prediction log price for a 2-bedroom apartment that accommodates 4
new_bnb <- bnb %>% 
  filter(bedrooms == 2 & room_type == "Entire home/apt" & accommodates == "3 or more")

predict_new <- predict(price_room_acc_log, new_bnb, type="response")
```











