library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)
library(reshape2)

# Read in data sets while in file
cities.all <- read.csv("data/csv/Cities-2010-2015.csv") 
gdp <- read.csv("data/csv/gdp_crime.csv")
long.gdp <- read.csv("for-long.csv")

# read in data sets while in RMD file 
# cities.all <- read.csv("../data/csv/Cities-2010-2015.csv")
# gdp <- read.csv("../data/csv/gdp_crime.csv")

# convert all NA values to 0, ya not great but it makes things work
gdp[is.na(gdp)] <- 0

gdp$gdp_per_person_2010 <- gdp$gdp_2010 * 1000000 / gdp$Population_2010
gdp$gdp_per_person_2011 <- gdp$gdp_2011 * 1000000 / gdp$Population_2011
gdp$gdp_per_person_2012 <- gdp$gdp_2012 * 1000000 / gdp$Population_2012
gdp$gdp_per_person_2013 <- gdp$gdp_2013 * 1000000 / gdp$Population_2013
gdp$gdp_per_person_2014 <- gdp$gdp_2014 * 1000000 / gdp$Population_2014
gdp$gdp_per_person_2015 <- gdp$gdp_2015 * 1000000 / gdp$Population_2015

# Add total crime for each year
gdp <- gdp %>% 
        mutate(total_crime_2010 = (Violent_Crime_2010 + Property_Crime_2010), 
               total_crime_2011 = (Violent_Crime_2011 + Property_Crime_2011),
               total_crime_2012 = (Violent_Crime_2012 + Property_Crime_2012),
               total_crime_2013 = (Violent_Crime_2013 + Property_Crime_2013),
               total_crime_2014 = (Violent_Crime_2014 + Property_Crime_2014),
               total_crime_2015 = (Violent_Crime_2015 + Property_Crime_2015))

gdp <- gdp %>% 
      mutate(crime_per_person_2010 = (total_crime_2010 / Population_2010),
             crime_per_person_2011 = (total_crime_2011 / Population_2011),
             crime_per_person_2012 = (total_crime_2012 / Population_2012),
             crime_per_person_2013 = (total_crime_2013 / Population_2013),
             crime_per_person_2014 = (total_crime_2014 / Population_2014),
             crime_per_person_2015 = (total_crime_2015 / Population_2015))

gdp <- gdp %>% 
  mutate(crime_change_10_11 = (crime_per_person_2011 - crime_per_person_2010),
         crime_change_11_12 = (crime_per_person_2012 - crime_per_person_2011),
         crime_change_12_13 = (crime_per_person_2013 - crime_per_person_2012),
         crime_change_14_13 = (crime_per_person_2014 - crime_per_person_2013),
         crime_change_15_14 = (crime_per_person_2015 - crime_per_person_2014))



# Poisson regression of gdp per person and population vs crime per person. 
poisson.2010 <- glm(crime_per_person_2010 ~ gdp_per_person_2010 + Population_2010, data = gdp, 
    family = poisson(link = "log"))

summary(poisson.2010)
# f(x) = -3.082 + 0.0000009385(GDP) + 0.0000001263(POP)

poisson.2011 <- glm(crime_per_person_2011 ~ gdp_per_person_2011 + Population_2011, data = gdp, 
                    family = poisson(link = "log"))

summary(poisson.2011)

poisson.2012 <- glm(crime_per_person_2012 ~ gdp_per_person_2012 + Population_2012, data = gdp, 
                    family = poisson(link = "log"))

summary(poisson.2012)

poisson.2013 <- glm(crime_per_person_2013 ~ gdp_per_person_2013 + Population_2013, data = gdp, 
                    family = poisson(link = "log"))

summary(poisson.2013)

poisson.2014 <- glm(crime_per_person_2014 ~ gdp_per_person_2014 + Population_2014, data = gdp, 
                    family = poisson(link = "log"))

summary(poisson.2014)

poisson.2015 <- glm(crime_per_person_2015 ~ gdp_per_person_2015 + Population_2015, data = gdp, 
                    family = poisson(link = "log"))

summary(poisson.2015)

total <- cities.all %>% 
          filter(grepl("-", City))
columbia <- cities.all %>% 
            filter(City == "Columbia")

test <- melt(gdp, id.vars = c("State", "City", "id"))

population <- test %>% 
            filter(grepl("Population_", variable))

total.crime <- test %>% 
            filter(grepl("total_crime_", variable))

total.gdp <- test %>% 
            filter(grepl("gdp_per_person", variable))



