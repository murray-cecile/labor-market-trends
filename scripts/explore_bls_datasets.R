#===============================================================================#
# PULL BLS DATA
#   Pull data to explore potential datasets of interest
#
# Cecile Murray
# 2019-01-12
#===============================================================================#

# setup steps
library(here)

libs <- c("tidyverse", "magrittr", "stringr", "readr", "openxlsx", "janitor", "sp",
          "tigris", "censusapi", "broom", "data.table", "foreign", "blscrapeR")
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
