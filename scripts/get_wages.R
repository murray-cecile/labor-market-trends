#===============================================================================#
# LABOR MARKET TRENDS: GET WAGES/EARNINGS
#   Compensation by industry over time
#
# Cecile Murray
# 2019-01-17
#===============================================================================#

library(here)
source("scripts/setup.R")

#===============================================================================#
# CONSTRUCT QUERY & PULL DATA
#===============================================================================#

ces_data_code <- "03"

# construct series IDs to query
earn_query <- filter(industries, substr(industry_code, 3, 8) == "000000") %>% 
  mutate(seriesID = paste0("CE", "S", industry_code, ces_data_code))

hrearn_data <- bls_api(earn_query$seriesID, startyear = 2007, endyear = 2018,
                       registrationKey = blskey, annualaverage = TRUE) %>% 
  mutate(month = paste0(year, "-", substr(period, 2, 3))) %>% 
  left_join(earn_query, by = "seriesID") 

# perform inflation adjustment (CPI-U)
cpi_18 <- inflation_adjust(base_year = 2018) %>%
  mutate(year = as.numeric(year))

hrearn_data %<>% left_join(cpi_18, by = "year") %>% 
  mutate(real_value = value * adj_value) %>% 
  select(year, month, industry_name, value, real_value, footnotes)

ggplot(hrearn_data, aes(x = month, y = real_value, color = industry_name)) +
  geom_point() +
  theme_minimal()
