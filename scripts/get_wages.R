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

industry_list <- c("Mining and logging", "Construction", "Manufacturing",
                   "Retail trade", "Wholesale Trade", "Utilities",
                   "Transportation and warehousing", "Information",
                   "Financial activities", 
                   "Professional and business services",
                   "Education and health services",
                   "Leisure and hospitality", "Other services", "Government",
                   "Total nonfarm")

# construct series IDs to query: note that government doesn't show up here
earn_query <- filter(industries, industry_name %in% industry_list) %>% 
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
  select(year, month, industry_code, industry_name, value, real_value, footnotes)

ggplot(hrearn_data, aes(x = month, y = value, color = industry_name)) +
  geom_point() +
  theme_minimal()

#===============================================================================#
# CLASSIFY LOW-, MIDDLE-, HIGH-EARNING JOBS
#===============================================================================#

# compare earnings to average for all private employees
all_private_earn <- bls_api(paste0("CE", "S", "05000000", ces_data_code),
                            startyear = 2007, endyear = 2018, 
                            registrationKey = blskey)
avgearn18 <- filter(all_private_earn, year == 2018) %>% 
  summarize(avgearn = mean(value))

earn_cats <- hrearn_data %>% filter(year == 2007 | year == 2018) %>% 
  group_by(industry_name, industry_code, year) %>%
  summarize(avgearn = mean(real_value)) %>% 
  spread(year, avgearn, sep = "_") %>% ungroup() %>% 
  mutate(earncat = case_when(
    year_2018 < avgearn18$avgearn ~ "Below-average earning",
    year_2018 > avgearn18$avgearn ~ "Above-average earning"
  ))

save(earn_cats, file = "temp/naics2_average_earnings_07.Rdata")
