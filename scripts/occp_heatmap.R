#===============================================================================#
# OCCUPATIONAL GROWTH BY SEX
#   Exploring occupational shares by gender and growth rate with a heatmap?
#
# Cecile Murray
# 2019-01-31
#===============================================================================#

library(here)
source("scripts/setup.R")

cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

series_list <- ("LNU02032468")

# construct query
cps_query <- filter(cps_codes, substr(series_id, 1, 8) == "LNU02032", 
                    periodicity_code == "M", occupation_code > 0, ages_code == 0,
                    race_code == 0, orig_code ==0, pcts_code == 0, sexs_code > 0)

year_intervals <- sapply(seq(1983, 2018, 19), function(x) seq(x, x + 19, 19))
year_intervals[2, ncol(year_intervals)] <- 2018

raw_data <- map2(year_intervals[1, ] , year_intervals[2, ],
                 function(x, y) bls_api(cps_query$series_id, startyear = x, endyear = y,
                                        registrationKey = blskey,
                                        annualaverage = TRUE)) %>% bind_rows() %>% 
  dplyr::rename(series_id = seriesID) %>%
  mutate(month = paste0(year, "-", period))

occp_annual <- raw_data %>% filter(period == "M13") %>% 
  left_join(select(cps_query, series_id, series_title), by = "series_id") %>% 
  mutate(occupation = str_remove(series_title, "\\(Unadj\\) Employment Level - "))
