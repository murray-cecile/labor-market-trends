#===============================================================================#
# PULL UNEMPLOYMENT BY RACE AND GENDER
#   U3 and U6 by race and gender from CPS
#
# Cecile Murray
# 2019-01-19
#===============================================================================#

library(here)
source("scripts/setup.R")


cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

# construct query
cps_query <- filter(cps_codes, periodicity_code == "M",
                    substr(series_id, 4, 10) %in% c("10000"),
                    sexs_code == 0)

unemp_raceth_data <- bls_api(cps_query$series_id, startyear = 2000,
                             endyear = 2018, registrationKey = blskey) %>% 
  dplyr::rename(series_id = seriesID) %>% 
  mutate(month = paste0(year, "-", period),
         raceth = case_when(
           substr(series_id, 11, 11) == "0" ~ "all",
           substr(series_id, 11, 11) == "3" ~ "white",
           substr(series_id, 11, 11) == "6" ~ "black",
           substr(series_id, 11, 11) == "9" ~ "hispanic"
         ))

unemp_all <- unemp_raceth_data %>% filter(substr(series_id, 11, 11) == "0") %>% 
  dplyr::rename(unemp_all = value) %>% 
  select(month, unemp_all)

unemp_raceth <- unemp_raceth_data %>% 
  filter(raceth %in% c("white", "black")) %>% select(-series_id) %>% 
  spread(raceth, value) %>% 
  mutate(bw_delta = black - white) %>% 
  left_join(unemp_all, by = "month")

ggplot(unemp_raceth, aes(x = white, y = black)) +
  geom_point(color = "blue")
