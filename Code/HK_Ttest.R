
# The package "ezids" (EZ Intro to Data Science) includes a lot of the helper functions we developed for the course. 
# Some of the frequently used functions are loadPkg(), xkabledply(), xkablesummary(), uzscale(), etc.
# Once installed, load the library.
library(ezids)
library(tidyverse)



## Cross-Sell RAW Data

vehicle <- read.csv("train.csv")

str(vehicle)

## Basic EDA


xkabledply(summary(vehicle))

paste0("We will be able to get a idea on the outliers here by the percentiles ( In the Annual_Premium the 3rd quartile is 39400 and the max is 540165 this represents the outliers in this column)")


vehicle$Response =as.factor(vehicle$Response)
vehicle$Driving_License =as.factor(vehicle$Driving_License)
vehicle$Previously_Insured =as.factor(vehicle$Previously_Insured)
vehicle$Vehicle_Age <- ifelse(vehicle$Vehicle_Age=='> 2 Years',2
                              ,ifelse(vehicle$Vehicle_Age=='1-2 Year',1,0))
vehicle$Vehicle_Damage=factor(vehicle$Vehicle_Damage)
vehicle$Vehicle_Age=factor(vehicle$Vehicle_Age)


# T-TEST


accepted <- subset(vehicle,Response==1)
rejected <- subset(vehicle,Response==0)

tage95_acpt = t.test(x=accepted$Age, conf.level=0.95 )
tage95_acpt

tage95_rjct = t.test(x=rejected$Age, conf.level=0.95 )
tage95_rjct

tpsc95_acpt = t.test(x=accepted$Policy_Sales_Channel, conf.level=0.95 )
tpsc95_acpt

tpsc95_rjct = t.test(x=rejected$Policy_Sales_Channel, conf.level=0.95 )
tpsc95_rjct

tap95_acpt = t.test(x=accepted$Annual_Premium, conf.level=0.95 )
tap95_acpt

tap95_rjct = t.test(x=rejected$Annual_Premium, conf.level=0.95 )
tap95_rjct

tvin95_acpt = t.test(x=accepted$Vintage, conf.level=0.95 )
tvin95_acpt

tvin95_rjct = t.test(x=rejected$Vintage, conf.level=0.95 )
tvin95_rjct

paste0("From t-test we can conclude that p-value of all numerical variables for accepted and rejected sub-groups are less than alpha
       (0.05). Hence, the NULL Hypothesis can be rejected, i.e, the mean of accepted and rejected is not same as the mean of the population dataset")
