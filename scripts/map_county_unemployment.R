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

#===============================================================================#
# PREPARE SHAPEFILES
#===============================================================================#

ctpop <- get_acs(geography = "county", variable = "B01001_001",
                 geometry = TRUE, shift_geo = TRUE) %>% 
  dplyr::rename(stcofips = GEOID, pop = estimate) %>% 
  filter(!stcofips %in% c("72")) %>% select(-moe) 

natl_map <- ctpop %>% inner_join(qt_above, by = "stcofips") %>% 
  mutate(qt_cat = case_when(
    qt_over_natl == 0 ~ 1,
    qt_over_natl < 20 ~ 2,
    qt_over_natl < 40 ~ 3,
    qt_over_natl >= 40 ~ 4
  ), pop_alpha = case_when(
    pop < 100000 ~ "A",
    pop < 1000000 ~ "B",
    pop >= 1000000 ~ "C" 
  ), color_cat = paste0(qt_cat, pop_alpha))

# states <- get_acs(geography = "state", table = "B01001",
#                   geometry = TRUE, shift_geo = TRUE)

#===============================================================================#
# PLOT STUFF
#===============================================================================#

# check for population relationship - not super strong?
qt_above %>% left_join(ctpop, by = "stcofips") %>% 
  ggplot(aes(x = qt_over_natl, y = pop)) +
  geom_point(color = lt_blue) +
  scale_y_continuous(trans = "log10")

setwd("/Users/cecilemurray/Documents/coding/xwalks")
cbsa_xwalk <- readstata13::read.dta13("county metro xwalk.dta")
setwd(here::here())
cbsa_xwalk2 <- cbsa_xwalk %>% select(stcofips, cbsa, cbsa_name, top100, metro_micro)

qt_above %>% left_join(cbsa_xwalk2, by = "stcofips") %>% 
  mutate(metro_micro = ifelse(is.na(metro_micro), 0, metro_micro),
         in_metro = ifelse(metro_micro == "Metropolitan Statistical Area", TRUE, FALSE)) %>%
  ggplot(aes(x = in_metro, y = qt_over_natl)) +
  geom_violin()

#===============================================================================#
# THE ACTUAL PLOT
#===============================================================================#


library(gridExtra)

# construct a bivariate color matrix:
# I want it to run from lt_yellow to lt_blue (4 categories),
# and from low to high intensity (3 categories)
color_matrix <- data.frame("colors" = c("#FFD81680", "#FFD816BF", "#FFD816FF",
                                        "#BFA21080", "#BFA210BF", "#BFA210FF",
                                        "#3C657E80", "#3C657EBF", "#3C657EFF",
                                        "#217FBE80", "#217FBEBF", "#217FBEFF"),
                           "xaxis" = rep(c("A", "B", "C"), 4),
                           "yaxis" = matrix(sapply(seq(1, 4), function(x) rep(x, 3)), ncol = 1)) %>% 
  mutate(cat = paste0(yaxis, xaxis))


map_legend <- ggplot(color_matrix, aes(x = xaxis, y = yaxis)) +
  geom_tile(fill = color_matrix$colors, color = "gray50") +
  scale_x_discrete(labels = c("Under 100,000", "100,000 to 1,000,000", "1M or more")) +
  scale_y_continuous(labels = c("0", "1-19", "20-39", "40+"),
                     breaks = seq(1, 4)) +
  labs(title = "Quarters of high unemployment by population", 
       x = "Population", y = "Quarters") +
  lt_theme(panel.grid.major = element_blank(),
           axis.text = element_text(size = rel(1.1)),
           plot.title = element_text(face = "bold"))


metro <- data.frame("x" = 1, "y" = 1) %>% 
  ggplot(aes(x, y)) +
  geom_tile(fill = NA, color = "gray50", lwd = 1.5) +
  labs(title = "Metropolitan areas") +
  lt_theme(axis.text = element_blank(), panel.grid.major = element_blank(),
           axis.title = element_blank())

# create the map itself
mainmap <- ggplot() +
  geom_sf(data = natl_map, aes(fill = color_cat), lwd = 0) + 
  geom_sf(data = cbsa, color = "gray50", fill = NA, lwd = .20) +
  geom_sf(data = anchorage, color = "gray50", fill = NA, lwd = .20) +
  geom_sf(data = fairbanks, color = "gray50", fill = NA, lwd = .20) +
  geom_sf(data = honolulu, color = "gray50", fill = NA, lwd = .20) +
  geom_sf(data = kahului, color = "gray50", fill = NA, lwd = .20) +
  scale_fill_manual(values = c("1A" = "#FFD81680", "1B" = "#FFD816BF",
                               "1C" = "#FFD816FF", "2A" = "#BFA21080",
                               "2B" = "#BFA210BF", "2C" = "#BFA210FF",
                               "3A" = "#217FBE80", "3B" = "#217FBEBF",
                               "3C" = "#217FBEFF", "4A" = "#3C657E80",
                               "4B" = "#3C657EBF", "4C" = "#3C657EFF")) +
  coord_sf() +
  labs(title = "Metropolitan areas and the central U.S. have had shorter spells of
       high unemployment",
       subtitle = "Quarters where county unemployment rates were higher than the nation's, 2007-2017",
       caption = "Source: Bureau of Labor Statistics
       Note: County unemployment rates were seasonally smoothed.") +
  # annotate(geom = "text", 1250000, -2200000, label="Metropolitan\n areas",
  #          family = "Cabin") +
  # annotate(geom = "linerange", x = 1280000, ymin = -2240000, ymax = -2220000, color = "gray50") +
  lt_theme(legend.position = "none", legend.direction = "horizontal",
           legend.text = element_blank(),
           axis.title = element_blank(),
           axis.text = element_blank())
# mainmap

test <- grid.arrange(mainmap, metro, map_legend,
             layout_matrix = rbind(c(1, 1, 1, 1, 1),
                                   c(1, 1, 1, 1, 1),
                                   c(1, 1, 1, 1, 1),
                                   c(1, 1, 1, 1, 1),
                                   c(2, 3, 3, 3, 3)))
