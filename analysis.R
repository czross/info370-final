library(dplyr)
library(ggplot2)

# Load in main crime data of all cities wide file format
crime.data <- read.csv("data/csv/Cities-2010-2015.csv", stringsAsFactors = FALSE)

#Load in GDP data 
gdp.data <- read.csv("data/csv/gdp-states.csv", stringsAsFactors = FALSE)

#View(crime.data)

# Make all columns numeric 
crime.data$Violent_crime_2010 <- as.numeric(as.character(crime.data$Violent_crime_2010))
crime.data$Violent_crime_2011 <- as.numeric(as.character(crime.data$Violent_crime_2011))
crime.data$Property_crime_2010 <- as.numeric(as.character(crime.data$Property_crime_2010))
crime.data$Population_2010 <- as.numeric(as.character(crime.data$Population_2010))

# total 2010 crime
total.2010.crime <- sum(crime.data$Violent_crime_2010 + crime.data$Property_crime_2010, na.rm = TRUE) 

# add the difference in number of violent crimes
crime.data <- crime.data %>% 
              mutate(diff.2010.2011 = Violent_crime_2011 - Violent_crime_2010)

# finds the places where crime has increased 
increase.crime <- filter(crime.data, diff.2010.2011 < 0)


# summarises by state and year
summary.states.2010 <- crime.data %>% 
                  group_by(State) %>% 
                  summarise(Population.2010 = sum(Population_2010, na.rm = TRUE), total.Crime.2010 = sum(Violent_crime_2010 + Property_crime_2010, na.rm = TRUE))

summary.states.2011 <- crime.data %>% 
  group_by(State) %>% 
  summarise(Population.2011 = sum(Population_2011, na.rm = TRUE), total.Crime.2011 = sum(Violent_crime_2011 + Property_crime_2011, na.rm = TRUE))

summary.states.2012 <- crime.data %>% 
  group_by(State) %>% 
  summarise(Population.2012 = sum(Population_2012, na.rm = TRUE), total.Crime.2012 = sum(Violent_crime_2012 + Property_crime_2012, na.rm = TRUE))

summary.states.2013 <- crime.data %>% 
  group_by(State) %>% 
  summarise(Population.2013 = sum(Population_2013, na.rm = TRUE), total.Crime.2013 = sum(Violent_crime_2013 + Property_crime_2013, na.rm = TRUE))

summary.states.2014 <- crime.data %>% 
  group_by(State) %>% 
  summarise(Population.2014 = sum(Population_2014, na.rm = TRUE), total.Crime.2014 = sum(Violent_crime_2014 + Property_crime_2014, na.rm = TRUE))

summary.states.2015 <- crime.data %>% 
  group_by(State) %>% 
  summarise(Population.2015 = sum(Population_2015, na.rm = TRUE), total.Crime.2015 = sum(Violent_crime_2015 + Property_crime_2015, na.rm = TRUE))


full.summary <- full_join(summary.states.2010, summary.states.2011, by="State") %>% 
                full_join(., summary.states.2012, by="State") %>% 
                full_join(., summary.states.2013, by="State") %>% 
                full_join(., summary.states.2014, by="State") %>% 
                full_join(., summary.states.2015, by="State")

write.csv(full.summary, "full-summary.csv")

wide.full.summary <- read.csv("full-summary.csv", stringsAsFactors = FALSE)

regression <- lm(wide.full.summary$percent ~ wide.full.summary$Population)

wide.full.summary$percent <- wide.full.summary$total.Crime. /wide.full.summary$Population 

View(wide.full.summary)

regression.percent <- glm(percent ~ Population + total.Crime., data = wide.full.summary, family = binomial(link = "logit"))

summary(regression)

summary(regression.percent)
