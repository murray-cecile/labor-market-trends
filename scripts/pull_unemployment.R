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

epop <- filter(cps_codes, str_detect(series_title, "Employment-Population"),
               periodicity_code == "M", sexs_code ==0 )

# construct query
cps_query <- filter(cps_codes, periodicity_code == "M",
                    substr(series_id, 4, 12) %in% c("02300060", "02300063",
                                                    "02300066", "02300069"),
                    sexs_code == 0)


year_intervals <- sapply(seq(1954, 2018, 19), function(x) seq(x, x + 18, 18))
year_intervals[2,4] <- 2018

raceth_data <- map2(year_intervals[1, ] , year_intervals[2, ],
  function(x, y) bls_api(cps_query$series_id, startyear = x, endyear = y,
                     registrationKey = blskey)) %>% bind_rows() %>% 
  dplyr::rename(series_id = seriesID) %>%
  mutate(month = paste0(year, "-", period),
         raceth = case_when(
           substr(series_id, 11, 11) == "0" ~ "all",
           substr(series_id, 11, 11) == "3" ~ "white",
           substr(series_id, 11, 11) == "6" ~ "black",
           substr(series_id, 11, 11) == "9" ~ "hispanic"
         ))

epop_all <- raceth_data %>% filter(substr(series_id, 11, 11) == "0") %>% 
  dplyr::rename(epop_all = value) %>% 
  select(month, epop_all)

epop_raceth <- raceth_data %>% 
  filter(raceth %in% c("white", "black")) %>% select(-series_id) %>% 
  spread(raceth, value) %>% 
  mutate(bw_delta = black - white) %>% 
  left_join(unemp_all, by = "month")

ggplot(epop_raceth, aes(x = white, y = black, color = year)) +
  geom_point()

ggplot(filter(epop_raceth, year > 1994), 
       aes(x = month, y = bw_delta)) +
  geom_point()
