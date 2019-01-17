#===============================================================================#
# PULL BLS DATA
#   Pull data to explore potential datasets of interest
#
# Cecile Murray
# 2019-01-12
#===============================================================================#

# setup steps
library(here)

libs <- c("tidyverse", "stringr", "readr", "openxlsx", "janitor", "sp",
          "tigris", "censusapi", "broom", "data.table", "blscrapeR")
lapply(libs, library, character.only=TRUE)

blskey <- Sys.getenv("BLS_KEY")

#===============================================================================#
# CURRENT EMPLOYMENT STATISTICS
#===============================================================================#

# series IDs
ces_ids_raw <- fread("raw/CES_IDS", col.names = c("id")) 

ces_ids <- mutate(ces_ids_raw, 
                  stfips = substr(id, 4, 5),
                  dt = substr(id, 19, 20),
                  area = substr(id, 6, 10),
                  industry = substr(id, 11, 18))

al <- filter(ces_ids, stfips == "01")

test_pull <- bls_api(al$id, startyear = 2007, endyear = 2018, registrationKey = blskey)

test_pull %<>% mutate(yr_mo = paste0(year, "-", substr(period, 2, 3)), 
                      area = substr(seriesID, 9, 13))

ggplot(filter(test_pull, area == "00050"),
       aes(yr_mo, value, group = area)) +
  geom_line()
