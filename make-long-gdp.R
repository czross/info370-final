library(dplyr)

gdp <- read.csv("data/csv/gdp_crime.csv")

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

to.write <- gdp %>% 
            select(State, City, total_crime_2010, total_crime_2011, total_crime_2012, total_crime_2013,
                   total_crime_2014, total_crime_2015, crime_per_person_2010, crime_per_person_2011,
                   crime_per_person_2012, crime_per_person_2013, crime_per_person_2014, crime_per_person_2015,
                   crime_change_10_11, crime_change_11_12, crime_change_12_13, crime_change_14_13, crime_change_15_14)
write.csv(to.write, "for-long.csv")
