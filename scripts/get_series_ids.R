#===============================================================================#
# PULL BLS SERIES ID COMPONENTS
#   Extract the components of BLS series IDs from BLS website
#
# Cecile Murray
# 2019-01-12
#===============================================================================#

library(here)

libs <- c("tidyverse", "magrittr", "stringr", "readr", "openxlsx", "janitor", 
          "data.table", "broom")
lapply(libs, library, character.only=TRUE)

#===============================================================================#
# GET IDS
#===============================================================================#

data_types <- fread("https://download.bls.gov/pub/time.series/sm/sm.data_type")
supersectors <- fread("https://download.bls.gov/pub/time.series/sm/sm.supersector")
industries <- fread("https://download.bls.gov/pub/time.series/sm/sm.industry")

states <- fread("https://download.bls.gov/pub/time.series/sm/sm.state")
areas <- fread("https://download.bls.gov/pub/time.series/sm/sm.area")

# this one needs to be tidied a bit
qcew_est_size <- fread("https://data.bls.gov/cew/doc/titles/size/size_titles.txt")
# this one straight didn't work
qcew_own <- fread("https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.txt")


