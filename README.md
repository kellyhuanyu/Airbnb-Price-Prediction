# Goal of the analysis
Analyze AirBnB data in Vancouver, BC with categorical variables by linear Regression and predict the price of the AirBnB.
# Data Source
This dataset is downloaded from Inside Airbnb (http://insideairbnb.com/get-the-data/).
### Column description
```
Variables:
price           listing price in the form “$1,234” (note: character string)
bedrooms        room_type type of listing (a shared room, whole unit, ...)
accommodates    accommodates how many people?
```
# Data Exploration
### 0. Set up
#### Load library and import file.
```ruby
library(tidyverse)
library(dplyr)
bnb <- read.csv(file="../airbnb-vancouver-bc-listings.csv.bz2", sep = ',')
```
### 1. Data cleaning
#### Making sure if there's any missing or unreasonable data, and clean those data.
Filter out the missing value
```ruby
bnb <- bnb %>% 
  filter(!is.na(price))
```
Convert the type of the price column into numeric instead of varchar
```ruby
bnb$price <- as.numeric(gsub("[^0-9.]", "", bnb$price))
```
### 2. Data exploration
#### (1) Histogram: Analyze the price distribution of Airbnb

We can see that the distribution of airbnb price looks more like a right-skewed distribution, and log-transformation makes it more like a normal distribution.

Airbnb price without log:
```ruby
hist_price <- hist(bnb$price, main = "Histogram of Airbnb", xlab = "Price", col = "lightblue", border = "black")
```
<img width="695" alt="Screenshot 2024-01-15 at 5 16 41 PM" src="https://github.com/kellyhuanyu/Airbnb_Price_Prediction/assets/105426157/20f24dec-d1f0-4c3b-9bbc-af3a4ea08254">

Airbnb price with log:
```ruby
hist_price_log <- hist(log(bnb$price), main = "Log Histogram of Airbnb",
                                                xlab = "Price", col = "lightblue", border = "black")
```
<img width="647" alt="Screenshot 2024-01-15 at 5 23 05 PM" src="https://github.com/kellyhuanyu/Airbnb_Price_Prediction/assets/105426157/43708714-bd68-4f23-88ef-a3137465eafe">

#### (2) Linear Regression
To do the linear regression, we can first convert some variables into categorical variables.
```ruby
# Convert the number of bedrooms into categories
bnb <- bnb %>% 
  mutate(bed_group = cut(bedrooms,
                 breaks=c(0,1,2,3,Inf),
                 labels=c("0","1","2","3+")))

# Convert the room type into categories
bnb <- bnb %>% 
    mutate(room_type = ifelse(room_type == "Entire home/apt", "Entire home/apt",
                              ifelse(room_type == "Private room", "Private room", "Other"))) %>% 
    mutate(accommodates = cut(accommodates,
                              breaks=c(1,2,3,Inf),
                              labels=c("1","2","3 or more")))
```
#### Also, we have a question: is this Airbnb linear regression with log better or without log?

We can see from the result that the model with log price is better since it has a higher R-squared (0.3195) compared to the R-squared of the model without log (0.1138).

Regression with log
```
price_bed_log <- lm(log(price) ~ bed_group, data=bnb)
summary(price_bed_log)
```
Result:
```
## Call:
## lm(formula = log(price) ~ bed_group, data = bnb)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8419 -0.3293 -0.0377  0.3287  4.5622 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.64799    0.01032  450.22   <2e-16 ***
## bed_group1   0.49570    0.01858   26.68   <2e-16 ***
## bed_group2   0.86271    0.02883   29.92   <2e-16 ***
## bed_group3+  1.41282    0.04412   32.02   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5358 on 4444 degrees of freedom
## Multiple R-squared:  0.3199, Adjusted R-squared:  0.3195 
## F-statistic: 696.8 on 3 and 4444 DF,  p-value: < 2.2e-16
```
Regression without log
```
price_bed <- lm(price ~ bed_group, data=bnb)
summary(price_bed)
```
Result:
```
## Call:
## lm(formula = price ~ bed_group, data = bnb)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -578.6  -61.6  -26.6   23.4 9872.4 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  126.553      5.261  24.056  < 2e-16 ***
## bed_group1    65.538      9.467   6.923 5.07e-12 ***
## bed_group2   185.808     14.693  12.646  < 2e-16 ***
## bed_group3+  477.043     22.482  21.219  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 273 on 4444 degrees of freedom
## Multiple R-squared:  0.1144, Adjusted R-squared:  0.1138 
## F-statistic: 191.3 on 3 and 4444 DF,  p-value: < 2.2e-16
```
Since we know that regression with log is better, now we can do our analysis. 

#### Let's see how the room type and accomodates affect the price.

From the result, we can see that the average price difference between Private room and Entire home/apt is -0.42, which means the price of the private room is $0.42 lower than the entire home/apt.

The p-value of the “Other” room type is really high which makes it fail to reject the null hypothesis. It means that we do not have much evidence to prove that the Hotel room and Shared room have the correlation with the price. The sample size of “Other” category is much less since there are only 12. It makes it harder to be statistically significant.
```
price_room_acc_log <- lm(log(price) ~ room_type + accommodates, data=bnb)
summary(price_room_acc_log)
```
Result:
```
## Call:
## lm(formula = log(price) ~ room_type + accommodates, data = bnb)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.2043 -0.3385 -0.0679  0.2574  4.8625 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            4.76925    0.01468 324.898  < 2e-16 ***
## room_typeOther         0.20391    0.21415   0.952  0.34107    
## room_typePrivate room -0.42150    0.02467 -17.088  < 2e-16 ***
## accommodates2          0.08616    0.02807   3.069  0.00216 ** 
## accommodates3 or more  0.51089    0.01845  27.696  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5233 on 4155 degrees of freedom
##   (288 observations deleted due to missingness)
## Multiple R-squared:  0.2801, Adjusted R-squared:  0.2794 
## F-statistic: 404.2 on 4 and 4155 DF,  p-value: < 2.2e-16
```
#### (3) Prediction
Now, we can use the model for the pricing prediction.
```
predict <- predict(price_room_acc_log, bnb, type="response")
head(predict)
```
```
##        1        2        3        4        5        6 
## 5.280136 5.280136 4.769246 4.769246 5.280136 5.280136
```

We can compute the root-mean-squared-error (RMSE) to evaluate the quality of prediction
```
RMSE <- sqrt(mean(log(bnb$price)-predict)^2)
```

Now we can predict log price for a 2-bedroom apartment that accommodates 4
```
new_bnb <- bnb %>% 
  filter(bedrooms == 2 & room_type == "Entire home/apt" & accommodates == "3 or more")

predict_new <- predict(price_room_acc_log, new_bnb, type="response")
```





