#===============================================================================#
# PULL EMPLOYMENT BY COUNTY
#   
# Cecile Murray
# 2019-02-02s
#===============================================================================#

library(here)
source("scripts/setup.R")

library(tidycensus)

#===============================================================================#
# CONSTRUCT QUERY
#===============================================================================#

# get list of all the state codes
counties <- fread("https://download.bls.gov/pub/time.series/la/la.area") %>% 
  filter(area_type_code == "F") 

# create a list of series IDs
cturate_query <- matrix(sapply(counties$area_code,
                                function(x) paste0("LAU", x, "03")),
                        nrow = nrow(counties) / 50, 50)

# query the API
cturate_names <- names(bls_api(cturate_query[1, 1], startyear = 2007,
                               endyear = 2018, registrationKey = blskey))
cturate_data <- purrr::map(seq(1, 50),
                       function(x) bls_api(cturate_query[x, ], startyear = 2007,
                                           endyear = 2018, registrationKey = blskey,
                                           annualaverage = TRUE)) %>% 
  bind_rows() %>% dplyr::rename(series_id = seriesID)

# determine month of highest unemployments
cturate <- mutate(cturate_data, stcofips = substr(series_id, 6, 10),
                stfips = substr(stcofips, 1, 2),
                month = paste0(year, "-", period)) %>% 
  group_by(stcofips) %>%  dplyr::rename(urate = value) %>% 
  mutate(max_urate = max(urate), is_max = ifelse(urate == max_urate, 1, 0))

max_urate <- cturate %>% filter(is_max == 1, stfips != "72")

ggplot(max_urate, aes(x = month, y = max_urate)) +
  geom_point() +
  lt_theme()

#===============================================================================#
# NOW GET GEOMETRY
#===============================================================================#

countypop <- get_acs(geography = "county", variable = "B01001_001",
                     geometry = TRUE) %>% 
  dplyr::rename(stcofips = GEOID, pop = estimate) %>% 
  filter(substr(stcofips, 1, 2) != "72")

mapdata <- left_join(countypop, max_urate, by = "stcofips") %>% 
  filter(as.numeric(substr(stcofips, 1, 2)) == 39)

ggplot(mapdata) +
  geom_sf(aes(fill = pop))

