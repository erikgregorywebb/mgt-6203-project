library(tidyverse)
library(lubridate)

### CPI DATA ###

# import
cpi_transportation_raw = readxl::read_xlsx('SeriesReport-20220720002254_606343.xlsx', skip = 11)
cpi_medical_care_raw = readxl::read_xlsx('SeriesReport-20220720002256_3e1e8d.xlsx', skip = 11)
cpi_food_berage_raw = readxl::read_xlsx('SeriesReport-20220720002258_3cd805.xlsx', skip = 11)
cpi_fuel_utilites_raw = readxl::read_xlsx('SeriesReport-20220720002301_d320ce.xlsx', skip = 11)

# define helper funciton
transform_cpi_data = function(table) {
  t = table %>%
    gather(month, value, -Year) %>% 
    filter(month %in% month.abb) %>%
    mutate(date_char = paste(month, '01', Year, sep = '-')) %>%
    mutate(month = mdy(date_char)) %>% select(month, value) %>% 
    arrange(month)
  return(t)
}

# transform (wide to long) cpi data
cpi_transportation = transform_cpi_data(cpi_transportation_raw) %>% rename(cpi_transportation = value)
cpi_medical_care = transform_cpi_data(cpi_medical_care_raw) %>% rename(cpi_medical_care = value)
cpi_food_berage = transform_cpi_data(cpi_food_berage_raw) %>% rename(cpi_food_berage = value)
cpi_fuel_utilites = transform_cpi_data(cpi_fuel_utilites_raw) %>% rename(cpi_fuel_utilites = value)

# combine
cpi_combined = cpi_transportation %>%
  full_join(x = ., y = cpi_medical_care, by = 'month') %>%
  full_join(x = ., y = cpi_food_berage, by = 'month') %>%
  full_join(x = ., y = cpi_fuel_utilites, by = 'month')

# plot to preview
cpi_combined %>%
  gather(metric, value, -month) %>%
  ggplot(., aes(x = month, y = value, col = metric)) +
  geom_point()

### FRED DATA ###

# import
unemployment_rate_raw = read_csv('CALOSA7URN.csv') 
home_price_index_raw = read_csv('LXXRSA.csv')

# clean
unemployment_rate = unemployment_rate_raw %>% select(month = DATE, unemployment_rate = CALOSA7URN)
home_price_index = home_price_index_raw %>% select(month = DATE, home_price_index = LXXRSA)

# joined
fred = unemployment_rate %>% full_join(x = ., y = home_price_index, by = 'month')

# plot to preview
fred %>%
  gather(metric, value, -month) %>%
  ggplot(., aes(x = month, y = value, col = metric)) +
  geom_point()

### ZILLOW DATA ###

# import
zillow_raw = read_csv('Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv')

# clean 
zillow_rent_index = zillow_raw %>%
  select(-RegionID, -SizeRank) %>%
  gather(month, value, -RegionName) %>%
  filter(RegionName == 'Los Angeles-Long Beach-Anaheim, CA') %>%
  mutate(month = ym(month)) %>% select(month, rent_index = value)

### CRIME DATA ###

# import 
crime_raw_part_1 = read_csv('Crime_Data_from_2010_to_2019.csv')
crime_raw_part_2 = read_csv('Crime_Data_from_2020_to_Present.csv')

# stack
crime_raw = rbind(crime_raw_part_1, crime_raw_part_2)
nrow(crime_raw) #2,643,306 rows 

# transform
crime = crime_raw %>%
  mutate(date_reported = mdy_hms(`Date Rptd`)) %>%
  select(id = DR_NO, date_reported, area_name = `AREA NAME`, crime_description = `Crm Cd Desc`) %>%
  mutate(month = date(floor_date(date_reported, unit = 'month'))) %>%
  group_by(month) %>% summarise(crime_count = n_distinct(id))

# preview
crime %>% ggplot(., aes(x = month, y = crime_count)) + geom_point()

### COMBINE EVERYTHING ###

# join 
final = crime %>%
  full_join(x = ., y = cpi_combined, by = 'month') %>%
  full_join(x = ., y = fred, by = 'month') %>%
  full_join(x = ., y = zillow_rent_index, by = 'month') 

# plot to preview
final %>%
  gather(metric, value, -month) %>%
  ggplot(., aes(x = month, y = value, col = metric)) +
  geom_point() + facet_wrap(~metric, scales = 'free')
