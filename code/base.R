
# import packages
library(tidyverse)
library(lubridate)

# import raw crime data
# source: https://data.lacity.org/api/views/63jg-8b9z/rows.csv?accessType=DOWNLOAD
setwd("~/Downloads")
crime_raw = read_csv('Crime_Data_from_2010_to_2019.csv')
glimpse(crime_raw)
View(head(crime_raw))

# export crime codes into crime types
crime_types = crime_raw %>% 
  select(`Crm Cd Desc`) %>% pull() %>% unique() %>% as_tibble() %>%
  select(crime_description = value) %>% arrange(crime_description)

# export for manual mapping
write_csv(crime_types, 'crime_types_original.csv')

# clean
crime_clean = crime_raw %>%
  mutate(date_reported = mdy_hms(`Date Rptd`)) %>%
  select(id = DR_NO, date_reported, area_name = `AREA NAME`, crime_description = `Crm Cd Desc`) %>%
  mutate(month_reported = date(floor_date(date_reported, unit = 'month')))

# aggregate
crime_aggregated = crime_clean %>%
  group_by(month_reported, crime_description) %>%
  summarise(count = n_distinct(id)) %>%
  spread(crime_description, count, fill = 0) %>%
  rename(`MONTH REPORTED` = month_reported)

# export
write_csv(crime_aggregated, '5_crime_clean.csv')
