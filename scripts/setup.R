#===============================================================================#
# LABOR MARKET TRENDS: SETUP
#   Load required packages, get API key, get code lists
#
# Cecile Murray
# 2019-01-17
#===============================================================================#

libs <- c("tidyverse", "magrittr", "stringr", "readr", "data.table", "janitor",
          "blscrapeR", "here", "ggrepel", "censusapi")
lapply(libs, library, character.only=TRUE)

blskey <- Sys.getenv("BLS_KEY")

#===============================================================================#
# SERIES ID DICTIONARIES
#===============================================================================#

# pull list of all NAICS codes
industries <- fread("https://download.bls.gov/pub/time.series/ce/ce.industry") %>% 
  mutate(industry_code = str_pad(industry_code, 8, side = "left", pad = "0"))

# CES data types
ces_data_types <- fread("https://download.bls.gov/pub/time.series/ce/ce.datatype") %>% 
  mutate(data_type_code = str_pad(data_type_code, 2, side = "left", pad = "0"),
         data_type_text = tools::toTitleCase(str_to_lower(data_type_text)))

supersectors <- c("Goods-producing", "Service-providing",
                  "Private service-providing", "Total private")
