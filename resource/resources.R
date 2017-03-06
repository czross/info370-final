library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)

# Read in data sets
cities.all <- read.csv("data/csv/Cities-2010-2015.csv") 
gdp <- read.csv("data/csv/gdp_crime.csv")

# convert all NA values to 0, ya not great but it makes things work
gdp[is.na(gdp)] <- 0

gdp$gdp_per_person_2010 <- gdp$gdp_2010 * 1000000 / gdp$Population_2010

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


# Remove rows with zeros
# row_sub = apply(dd, 1, function(row) all(row !=0 ))
##Subset as usual
# dd[row_sub,]

View(cities.all)

cities.greater <- cities.all %>% 
                  filter(X > 9310)
