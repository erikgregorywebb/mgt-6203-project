
library(tidyverse)
library(lubridate)

### CRIME DATA ### 

# import raw crime data
# source: https://data.lacity.org/api/views/63jg-8b9z/rows.csv?accessType=DOWNLOAD
setwd("~/Downloads")
crime_raw = read_csv('Crime_Data_from_2010_to_2019.csv')
glimpse(crime_raw)

# export crime codes into crime types
crime_types = crime_raw %>% 
  select(`Crm Cd Desc`) %>% pull() %>% unique() %>% as_tibble() %>%
  select(crime_description = value) %>% arrange(crime_description)

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

crime_qtr = crime_clean %>%
  mutate(cy_qtr = paste(year(month_reported), quarter(month_reported), sep = '-0')) %>% 
  group_by(cy_qtr) %>% summarise(crime_count = n_distinct(id))

### UNEMPLOYMENT DATA ###

# import
url = 'https://raw.githubusercontent.com/erikgregorywebb/mgt-6203-project/main/data/3_unemployment_statistics.csv'
unemployment_raw = read_csv(url)
glimpse(unemployment_raw)

# clean
unemployment = unemployment_raw %>%
  filter(`Status (Preliminary / Final)` == 'Final') %>%
  filter(`Seasonally Adjusted (Y/N)` == 'Y') %>%
  mutate(month = mdy(Date)) %>%
  select(month, labor_force = `Labor Force`, unemployment = Unemployment, unemployment_rate = `Unemployment Rate`) %>%
  arrange(month)

# roll-up to quarter level
unemployment_qtr = unemployment %>%
  mutate(cy_qtr = paste(year(month), quarter(month), sep = '-0')) %>%
  group_by(cy_qtr) %>% 
  summarise(labor_force = mean(labor_force), 
            unemployment = mean(unemployment), unemployment_rate = mean(unemployment_rate)) %>%
  mutate(labor_force = round(labor_force, 0), unemployment = round(unemployment, 0),
         unemployment_rate = round(unemployment_rate, 2))

# quick plot, to validate
unemployment %>%
  ggplot(., aes(x = month, y = unemployment_rate)) + geom_point()

### RENT DATA ### 

# import
url = 'https://raw.githubusercontent.com/erikgregorywebb/mgt-6203-project/main/data/4_rent_prices.csv'
rent_raw = read_csv(url)
glimpse(rent_raw)

# preview
rent_raw %>% group_by(Date) %>% count() # report annually, not granular enough; may be present in economic data

### ECONOMIC DATA ###

# import
url = 'https://raw.githubusercontent.com/erikgregorywebb/mgt-6203-project/main/data/2_economic_data.csv'
economic_raw = read_csv(url)
glimpse(economic_raw)

# preview
economic_raw %>% 
  filter(is.null(cy_qtr) != TRUE) %>%
  group_by(category, indicator) %>% count(sort = T)

# summarize metrics 
cpi = economic_raw %>% 
  filter(locality == 'Los Angeles Msa') %>%
  filter(indicator == 'Total All Items') %>%
  select(cy_qtr, cpi = value) %>%
  mutate(cpi = round(cpi, 1))

taxable_sales = economic_raw %>% 
  filter(locality == 'Los Angeles City') %>%
  filter(indicator == 'Taxable Sales By Sector') %>%
  group_by(cy_qtr) %>% summarise(taxable_sales = sum(value)) %>%
  mutate(taxable_sales = round(taxable_sales, 0))

avg_annual_wage = economic_raw %>% 
  filter(locality == 'Los Angeles City') %>%
  filter(indicator == 'Average Annual Wage') %>%
  filter(council_district != 'City Total') %>%
  group_by(cy_qtr) %>% summarise(avg_annual_wage = mean(value)) %>%
  mutate(avg_annual_wage = round(avg_annual_wage, 0))

multi_family_permits = economic_raw %>% 
  filter(locality == 'Los Angeles City') %>%
  filter(indicator == 'Multi-family Permits') %>%
  filter(council_district != 'City Total') %>%
  group_by(cy_qtr) %>% summarise(multi_family_permits = sum(value)) %>%
  mutate(multi_family_permits = round(multi_family_permits, 0))

single_family_permits = economic_raw %>% 
  filter(locality == 'Los Angeles City') %>%
  filter(indicator == 'Single-family Permits') %>%
  filter(council_district != 'City Total') %>%
  group_by(cy_qtr) %>% summarise(single_family_permits = sum(value)) %>%
  mutate(single_family_permits = round(single_family_permits, 0))

# join 
joined = cpi %>%
  full_join(x = ., y = taxable_sales, by = 'cy_qtr') %>%
  full_join(x = ., y = avg_annual_wage, by = 'cy_qtr') %>%
  full_join(x = ., y = multi_family_permits, by = 'cy_qtr') %>%
  full_join(x = ., y = single_family_permits, by = 'cy_qtr') %>%
  full_join(x = ., y = unemployment_qtr, by = 'cy_qtr') %>% 
  full_join(x = ., y = crime_qtr, by = 'cy_qtr')

# reorder columns
final = joined %>% select(cy_qtr, total_crime_count = crime_count, labor_force, unemployment, unemployment_rate,
         cpi, taxable_sales, avg_annual_wage, multi_family_permits, single_family_permits)
glimpse(final)

# export
setwd("~/Downloads")
write_csv(final, 'final_joined_cleaned.csv')
