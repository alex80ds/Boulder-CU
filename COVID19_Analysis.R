
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)



url_confirmed_global <-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_deaths_global <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_confirmed_us <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
url_deaths_us <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

global_cases <- read_csv(url_confirmed_global) 
global_deaths <- read_csv(url_deaths_global) 
us_cases <- read_csv(url_confirmed_us) 
us_deaths <- read_csv(url_deaths_us)

## load data global_cases

global_cases <- global_cases %>% 
  pivot_longer(cols = -c('Province/State', 'Country/Region', Lat, Long), names_to = "date", values_to = "cases") %>% 
  select(-c(Lat, Long))
head(global_cases)

## load data global_deaths

global_deaths <- global_deaths %>% 
pivot_longer(cols = -c('Province/State', 'Country/Region', Lat, Long), names_to = "date", values_to = "deaths") %>% 
select(-c(Lat, Long)) 
head(global_deaths)

global <- global_cases %>% 
full_join(global_deaths, by = c("Province/State", "Country/Region", "date")) %>% 
  rename(Country_Region = "Country/Region", Province_State = "Province/State") %>% 
  mutate(date = mdy(date))
head(global)

us_cases <- us_cases %>% 
pivot_longer(cols = -(UID:Combined_Key), names_to = "date", values_to = "cases") %>% 
  select(Admin2:cases) %>% 
mutate(date = mdy(date)) %>% 
select(-c(Lat, Long_)) 
head(us_cases)

us_deaths <- us_deaths %>% 
pivot_longer(cols = -(UID:Population), names_to = "date", values_to = "deaths") %>% 
  select(Admin2:deaths) %>% 
mutate(date = mdy(date)) %>% 
select(-c(Lat, Long_)) 
head(us_deaths)

global <- global %>% 
unite("Combined_Key", c(Province_State, Country_Region), sep = ",", na.rm = TRUE, remove = FALSE)

url5 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv" 
uid <- read_csv(url5) %>%
select(-c(Lat, Long_, Combined_Key, code3, iso2, iso3, Admin2))

global <- global %>% 
left_join(uid, by = c("Province_State", "Country_Region")) %>% 
select(-c(UID, FIPS)) %>% 
select(Province_State, Country_Region, date, cases, deaths, Population, Combined_Key)
head(global)

# Let's create a time series plot for the percentage of deaths relative to cases over time

time_series_percentage_deaths <- global %>% 
group_by(date) %>% 
summarise(total_deaths = sum(deaths, na.rm = TRUE), total_cases = sum(cases, na.rm = TRUE))

time_series_percentage_deaths <- time_series_percentage_deaths %>% 
mutate(percentage_deaths = (total_deaths / total_cases) * 100)

time_series_percentage_deaths_plot <- ggplot(time_series_percentage_deaths, aes(x = date, y = percentage_deaths)) + 
geom_line() + 
labs(title = "Percentage of Deaths Relative to Cases Over Time (All Country_Region)", x = "Date", y = "Percentage (%)") + 
theme_minimal()

time_series_percentage_deaths_plot

# 1. Low rates at the beginning of the pandemic may be associated with incorrect diagnosis and establishment of the cause of death

# 2. The fall in the second half of 2020 is due to the introduction of restrictive measures.

# 3. The decrease at the beginning of 2022b to almost 0 is the result of vaccination carried out since mid-2021.

# Let's create a bar chart for the top 20 regions (cases_per_1000_population) in descending order

global <- global %>% 
mutate(cases_per_1000_population = (cases / Population))

global$date <- as.Date(global$date)

latest_data <- global %>% 
group_by(Country_Region) %>% 
filter(date == max(date)) %>%
arrange(desc(cases_per_1000_population)) %>%
top_n(20, wt = cases_per_1000_population) %>%
select(Country_Region, cases, deaths, Population, cases_per_1000_population)

top_20_latest_data <- latest_data %>% 
arrange(desc(cases_per_1000_population)) %>% 
head(20)

bar_chart <- ggplot(top_20_latest_data, aes(x = reorder(Country_Region, -cases_per_1000_population), y = cases_per_1000_population, fill = Country_Region)) + geom_col() +
labs(title = "Top 20 COVID-19 Cases per 1000 Population by Country/Region (Latest Date)", x = "Country/Region", y = "Cases per 1000 Population")+
  theme_minimal() + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
scale_fill_manual(values = rainbow(length(top_20_latest_data$Country_Region)))

bar_chart

# 1. France, UK, Denmark have highest rates of cases per 1000 Population.

# 2. We see that European countries, Korea, and Israel are in the TOP. This may be due to the high population density.

# Let's create a bar chart for the top 20 regions (percentage_deaths) in descending order

global <- global %>% 
mutate(percentage_deaths = (deaths / cases)*100)

global$date <- as.Date(global$date)

latest_data <- global %>% 
group_by(Country_Region) %>%
filter(date == max(date)) %>% 
arrange(desc(percentage_deaths)) %>%
top_n(20, wt = percentage_deaths) %>%
select(Country_Region, cases, deaths, percentage_deaths)

top_20_latest_data <- latest_data %>%
arrange(desc(percentage_deaths)) %>% 
head(20)

top_20_latest_data_filtered <- top_20_latest_data %>% 
filter(!(Country_Region %in% c("Canada", "Korea, North")))

bar_chart <- ggplot(top_20_latest_data_filtered, aes(x = reorder(Country_Region, -percentage_deaths), y = percentage_deaths, fill = Country_Region)) + 
geom_col() + 
labs(title = "Top 20 COVID-19 Percentage of Deaths by Country/Region (Latest Date)", x = "Country/Region", y = "Percentage of Deaths (%)") + 
theme_minimal() + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
scale_fill_manual(values = rainbow(length(top_20_latest_data_filtered$Country_Region)))

bar_chart

# In contrast to the TOP level of cases, where developed countries were mainly present. The TOP countries with a high mortality rate consist of countries in Africa and South America. I think this is due to the low level of medicine.

# 1. In the same way, it is possible to form the TOP 20 countries with a low level of cases, to use their experience in preventing the spread of infection.

#2. TOP 20 countries with the lowest mortality rate, to use advances in medicine to reduce the mortality rate.

# Let's create global monthly cases plot.

global_new_cases <- global %>%
mutate(new_cases = cases - lag(cases)) %>% 
select(date, new_cases)

global_new_cases$date <- as.Date(global_new_cases$date) 

monthly_new_cases <- global_new_cases %>%
group_by(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  summarise(total_new_cases = sum(new_cases)) 

monthly_new_cases$year_month <- paste(monthly_new_cases$year, monthly_new_cases$month, sep = "-")

monthly_new_cases_plot <- ggplot(monthly_new_cases, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = total_new_cases)) + 
geom_point(color = 'blue') + 
labs(title = "Monthly New COVID-19 Cases (Global)", x = "Date", y = "Total New Cases") + theme_minimal()

monthly_new_cases_plot

# And let's try to make a forecast for 6 months using linear regression model.

global_new_cases <- global %>%
mutate(new_cases = cases - lag(cases)) %>%
select(date, new_cases) 

global_new_cases$date <- as.Date(global_new_cases$date)

monthly_new_cases <- global_new_cases %>%
group_by(year = lubridate::year(date), month = lubridate::month(date)) %>% 
  summarise(total_new_cases = sum(new_cases, na.rm = TRUE)) %>%
mutate(date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")), time_num = row_number())

last_9_months <- tail(monthly_new_cases, 9) 
lm_model <- lm(total_new_cases ~ time_num, data = last_9_months)

future_time <- data.frame(time_num = (max(monthly_new_cases$time_num) + 1):(max(monthly_new_cases$time_num) + 6)) 
future_time$predictions <- predict(lm_model, future_time)

last_year <- year(max(monthly_new_cases$date)) 
last_month <- month(max(monthly_new_cases$date)) 
future_months <- data.frame(year = rep(NA, 6), month = rep(NA, 6))

for (i in 1:6){ 
new_month <- last_month + i 
future_months$year[i] <- last_year + floor((new_month - 1) / 12)  
future_months$month[i] <- (new_month - 1) %% 12 + 1 
}

forecast_data <- bind_cols(future_time, future_months) %>%
rename(total_new_cases = predictions)

full_data <- rbind( monthly_new_cases %>%
select(year, month, total_new_cases), forecast_data )

monthly_new_cases_plot <- ggplot(full_data, aes(x = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")), y = total_new_cases)) + 
geom_point(data = monthly_new_cases, color = 'blue') + 
geom_point(data = forecast_data, aes(y = total_new_cases), color = "red") + 
labs(title = "Monthly New COVID-19 Cases (Global) with Forecast", x = "Date", y = "Total New Cases") + 
ylim(0, max(full_data$total_new_cases, na.rm = TRUE)) + 
theme_minimal()

monthly_new_cases_plot

