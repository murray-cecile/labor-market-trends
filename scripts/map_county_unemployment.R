#===============================================================================#
# PULL EMPLOYMENT BY COUNTY
#   
# Cecile Murray
# 2019-02-09
#===============================================================================#

library(here)
source("scripts/setup.R")

library(sf)
library(tidycensus)
library(rmapshaper)

#===============================================================================#
# GET NATIONAL-LEVEL DATA
#===============================================================================#

cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

# grab code for quarterly unemployment rate
cps_query <- filter(cps_codes, periodicity_code == "Q",
                    substr(series_id, 4, 11) == "14000000",
                    sexs_code == 0)

natl_data <- bls_api(cps_query$series_id, startyear = 2007, endyear = 2018,
                     registrationKey = blskey, annualaverage = TRUE)  %>% 
  dplyr::rename(series_id = seriesID) %>%
  mutate(quarter = paste0(year, "-Q", substr(period, 3, 3))) %>% 
  select(quarter, value) %>% dplyr::rename(natl_urate = value)

#===============================================================================#
# GET COUNTY-LEVEL DATA
#===============================================================================#

# get list of all the state codes
counties <- fread("https://download.bls.gov/pub/time.series/la/la.area") %>% 
  filter(area_type_code == "F") %>% 
  mutate(stcofips = substr(area_code, 3, 8)) %>% 
  filter(substr(stcofips, 1, 2) != "72")

# # COUNTY DATA: query the API - ONCE, this is slow; 63 max size queries
# cturate_query <- matrix(sapply(counties$area_code,
#                                function(x) paste0("LAU", x, "03")),
#                         nrow = ceiling(nrow(counties) / 50), 50)
# cturate_data <- purrr::map(seq(1, nrow(cturate_query)),
#                        function(x) bls_api(cturate_query[x, ], startyear = 2007,
#                                            endyear = 2018, registrationKey = blskey,
#                                            annualaverage = TRUE)) %>%
#   bind_rows() %>% dplyr::rename(series_id = seriesID) 


# save(cturate_data, file = "raw/cturate_data.Rdata")
# rm(cturate_data)

load("raw/cturate_data.Rdata")


# determine month of highest unemployments
cturate <- mutate(cturate_data, stfips = substr(series_id, 6, 7),
                  stcofips = substr(series_id, 6, 10),
                  month = paste0(year, "-", period),
                  quarter = case_when(
                    period %in% c("M01", "M02", "M03") ~ paste0(year, "-", "Q1"),
                    period %in% c("M04", "M05", "M06") ~ paste0(year, "-", "Q2"),
                    period %in% c("M07", "M08", "M09") ~ paste0(year, "-", "Q3"),
                    period %in% c("M10", "M11", "M12") ~ paste0(year, "-", "Q4")
                  )) %>% 
  dplyr::rename(urate = value) %>%  group_by(stcofips) %>% 
  mutate(max_urate = max(urate), is_max = ifelse(urate == max_urate, 1, 0))

annual <- cturate %>% filter(period == "M13") %>% dplyr::rename(ann_urate = urate) %>% 
  select(year, stcofips, ann_urate)

adj_factor <- cturate %>% filter(period != "M13", year < 2018) %>% 
  left_join(annual, by = c("stcofips", "year")) %>% 
  mutate(ann_delta = urate - ann_urate) %>% 
  group_by(period) %>% summarize(month_adj = mean(ann_delta))

adj_cturate <- cturate %>% filter(period != "M13", year < 2018) %>% 
  left_join(adj_factor, by = "period") %>% 
  mutate(adj_urate = urate - month_adj) 

#===============================================================================#
# JOIN WITH NATIONAL DATA
#===============================================================================#

by_quarter <- adj_cturate %>% group_by(stcofips, quarter) %>%
  summarize(adj_urate = mean(adj_urate)) %>% ungroup() %>% group_by(stcofips) %>% 
  left_join(natl_data, by = "quarter") %>% 
  mutate(above_natl = ifelse(adj_urate > natl_urate, 1, 0),
         urate_delta = adj_urate - natl_urate,
         delta_quintile = ntile(urate_delta, 5),
         delta_ratio = adj_urate / natl_urate)

qt_above <- by_quarter %>% group_by(stcofips) %>% 
  summarize(qt_over_natl = sum(above_natl)) %>% ungroup() %>% 
  mutate(above_quintile = ntile(qt_over_natl, 5))

ggplot(by_quarter, aes(x = quarter, y = urate_delta)) +
  geom_point()

#===============================================================================#
# PLOT STUFF
#===============================================================================#

ctpop <- get_acs(geography = "county", variable = "B01001_001",
                 geometry = TRUE, shift_geo = TRUE) %>% 
  dplyr::rename(stcofips = GEOID, pop = estimate) %>% 
  filter(!stcofips %in% c("72")) %>% select(-moe) 

oh <- ctpop %>% filter(substr(stcofips, 1, 2) == "39") %>% 
  left_join(by_quarter, by = "stcofips") %>% 
  st_transform(102322)

oh_above <- ctpop %>% filter(substr(stcofips, 1, 2) == "39") %>% 
  left_join(qt_above, by = "stcofips") %>% 
  st_transform(102322)


ggplot(filter(oh, quarter == "2017-Q4")) +
  geom_sf(aes(fill = delta_ratio, alpha = pop), color = NULL,
          size = 0.5) +
  scale_fill_gradient2(low = lt_yellow, mid = "gray90", high = lt_blue, 
                       midpoint = 1) +
  scale_alpha_continuous(trans = "log10", range = c(0.8, 1)) +
  lt_theme(axis.text = element_blank()) +
  coord_sf()

# ggplot(oh_above) +
#   geom_sf(aes(fill = qt_over_natl, alpha = pop)) +
#   scale_alpha_continuous(trans = "log10", range = c(0.5, 1)) +
#   lt_theme(axis.text = element_blank()) +
#   coord_sf()


natl_map <- ctpop %>% left_join(qt_above, by = "stcofips")  

ggplot(natl_map) +
  geom_sf(aes(fill = qt_over_natl, alpha = pop), lwd = 0) + 
  scale_fill_gradient(low = lt_yellow, high = lt_blue) +
  scale_alpha_continuous(trans = "log10", range = c(0.25, 1)) +
  lt_theme(axis.text = element_blank()) +
  coord_sf()

