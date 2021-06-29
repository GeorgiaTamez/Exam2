#clear environment
```{r}
rm(list=ls(all=TRUE))
```

#Import college_scprecard data set
```{r}
library(readxl)
college_scorecard <- read_excel("~/Desktop/Summer 2021/2021_exam2_data.xlsx", 
                                sheet = "college_scorecard")
View(college_scorecard)
```

#summary
summary(college_scorecard)


library (tidyverse)
#create smaller data set
small_scorecard <- subset (college_scorecard,
                   select= c(college_scorecard, year== 2014,
                             state_abbr== "TX",
                             pred_degree_awarded_ipeds== 3))

_____________________________________________________________________________

AVOCADO DATA SET

#import avocado data
library(readxl)
avocados <- read_excel("~/Desktop/Summer 2021/2021_exam2_data.xlsx", 
                       sheet = "avocados")
View(avocados)

#create variable year that only captures the year in which they were sold
library (lubridate)
year= avocados %>%
  dplyr::mutate(year = lubridate::year(avocados$date))

#deflate the average_price
library(WDI)
# Google "GDP deflator World Bank"
# https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS
deflator_data = WDI(country = "all", indicator = c("NY.GDP.DEFL.ZS"),
                    start = 1960, # start of foreign aid data 
                    end = 2018, # end of of foreign aid data 
                    extra = FALSE, cache = NULL)

#rename it using data.table package
library(data.table) 
setnames(deflator_data,"NY.GDP.DEFL.ZS", "deflator")

#gdp deflator for the US
# select only the United States data
usd_deflator = subset(deflator_data, country=="United States")

#figure out the year base deflator
subset(usd_deflator, deflator==100)
>>> 2015

#merge the deflator into our avocados frame. 
#drop deflator_data first, you dont need it anymore
# remove the rest of the deflator data
rm(deflator_data)

# drop unnecessary variables
usd_deflator$country <- NULL 
usd_deflator$iso2c <- NULL

#drop usd_deflator
remove(usd_deflator)

#merge
collapsed_avocados= left_join(year, 
                         usd_deflator, 
                         by=c("year"))

#deflate the data
collapsed_avocados$deflated_price = collapsed_avocados$average_price/ 
  (collapsed_avocados$deflator/100)

#collapse
collapsed_avocados  %>%
  group_by( year) %>% # tell R the unique IDs 
  summarize(across(where(is.numeric), sum)) %>% # summarize numeric vars by sum
  select(c("deflator")) # drop transaction id (no longer relevant)

#show the new data frame
head(collapsed_avocados)

#reshape collapsed_avocados wide


wide_avocados <-
  collapsed_avocados %>%
  pivot_wider(id_cols = c( "year",  "total_volume", "deflated_price"), # unique IDs
              names_from = "year", # names for new wide vars
              values_from = "year", # data to put in new wide vars 
              names_prefix = "year_" ) # prefix to add before years



head (wide_avocados)


# label the variables 
library(labelled)
var_label(wide_avocados) <- list(`total_volume` = "Volume of Sales",
                               `year_2015` = "2015",
                               `deflated_price` = "Deflated Price",
                               `year_2016` = "2016",
                               `year_2017`= "2017",
                               `year_2018` = "2018")

______________________________________________________________________________

TRAINING DATA SET

#import data set
library(readxl)
training <- read_excel("~/Desktop/Summer 2021/2021_exam2_data.xlsx", 
                       sheet = "training")
View(training)

#reshape so that all earning are in a single column
training$id = 1:nrow(training)

library(dplyr)

earnings= coalesce(training$re_74, training$re_75, training$re_78)

training$earnings <- earnings


training_reshape <-
  training %>%
  pivot_longer(cols = starts_with("id"), # use columns starting with "year"
               names_to ="idr", # name of new column
               names_prefix = "id_", # part of string to drop
               values_to = "earnings", # where to put numeric values 
               values_drop_na = FALSE) %>% # don't drop NAs
  
 
  ___________________________________________________________________________

TITANIC DATA SET
#import data set
library(readxl)
titanic <- read_excel("~/Desktop/Summer 2021/2021_exam2_data.xlsx", 
                      sheet = "titanic")
View(titanic)

#summary statistics
summary (titanic)

#cross-tabulation
library(doBy)
summaryBy(survived ~ female, data=titanic, FUN=c(mean,length))

According to these results, on average, more women survived in quantity (1731 vs 470), eventhough
men have a higher mean of survival

