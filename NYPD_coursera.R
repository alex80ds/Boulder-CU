library(tidyverse)
library(lubridate)

## Step1: Load data

df = read.csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")
head(df)

## Step 2: Tidy and transform data

df_2 = df %>% select(INCIDENT_KEY, 
                   OCCUR_DATE,
                   OCCUR_TIME,
                   BORO, 
                   STATISTICAL_MURDER_FLAG,
                   PERP_AGE_GROUP,
                   PERP_SEX,
                   PERP_RACE,
                   VIC_AGE_GROUP,
                   VIC_SEX,
                   VIC_RACE,
                   Latitude,
                   Longitude)

lapply(df_2, function(x) sum(is.na(x)))



df_2 = df_2 %>% 
  replace_na(list(PERP_AGE_GROUP = "Unknown", PERP_SEX = "Unknown", PERP_RACE = "Unknown"))

df_2 = subset(df_2, PERP_AGE_GROUP!="1020" & PERP_AGE_GROUP!="224" & PERP_AGE_GROUP!="940")
df_2$PERP_AGE_GROUP = recode(df_2$PERP_AGE_GROUP, UNKNOWN = "Unknown")
df_2$PERP_SEX = recode(df_2$PERP_SEX, U = "Unknown")
df_2$PERP_RACE = recode(df_2$PERP_RACE, UNKNOWN = "Unknown")
df_2$VIC_SEX   = recode(df_2$VIC_SEX, U = "Unknown")
df_2$VIC_RACE   = recode(df_2$VIC_RACE, UNKNOWN = "Unknown")
df_2$INCIDENT_KEY = as.character(df_2$INCIDENT_KEY)
df_2$BORO = as.factor(df_2$BORO)
df_2$PERP_AGE_GROUP = as.factor(df_2$PERP_AGE_GROUP)
df_2$PERP_SEX = as.factor(df_2$PERP_SEX)
df_2$PERP_RACE = as.factor(df_2$PERP_RACE)
df_2$VIC_AGE_GROUP = as.factor(df_2$VIC_AGE_GROUP)
df_2$VIC_SEX = as.factor(df_2$VIC_SEX)
df_2$VIC_RACE = as.factor(df_2$VIC_RACE)

summary(df_2)

## Step 3: Add visualisation and analysis
  
  ### 1. Which part of New York has the most number of incidents?
  
  ### Brooklyn is the 1st in terms of the number of incidents


g <- ggplot(df_2, aes(x = BORO)) +
  geom_bar() +
  labs(title = "Areas of New York City",
       x = "Areas of New York City",
       y = "Count of Incidents") +
  theme_minimal()
g


### 2. Which day and time occur the most number of incidents in New York?
  
  ### Weekends.
  ### In the evening and night time.


df_2$OCCUR_DAY = mdy(df_2$OCCUR_DATE)
df_2$OCCUR_DAY = wday(df_2$OCCUR_DAY, label = TRUE)
df_2$OCCUR_HOUR = hour(hms(as.character(df_2$OCCUR_TIME)))
df_3 = df_2 %>%
  group_by(OCCUR_DAY) %>%
  count()
df_4 = df_2 %>%
  group_by(OCCUR_HOUR) %>%
  count()

g <- ggplot(df_3, aes(x = OCCUR_DAY, y = n)) +
  geom_col() +
  labs(title = "Count of Incidents by day of week",
       x = "Incident Occurence Day",
       y = "Count of Incidents") +
  theme_minimal()
g

g <- ggplot(df_4, aes(x = OCCUR_HOUR, y = n)) +
  geom_line() +
  labs(title = "Count of Incidents by time of day",
       x = "Incident Occurence Hour",
       y = "Count of Incidents") +
  theme_minimal()
g


## Step 4: Conclusion

### Brooklyn ranks first in the number of accidents, followed by the Bronx and Queens. The number of murder cases follows the same pattern as the number of incidents. There are significantly more incidents with men than with women.