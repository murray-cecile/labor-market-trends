#===============================================================================#
# LABOR MARKET TRENDS: ADJUST FOR INFLATION
#   Contains inflation adjustment functions
#
# Cecile Murray
# 2019-01-17
#===============================================================================#

libs <- c("tidyverse", "magrittr", "stringr", "readr", "data.table", "janitor",
          "blscrapeR", "here")
lapply(libs, library, character.only=TRUE)

blskey <- Sys.getenv("BLS_KEY")

#===============================================================================#
# QUERY INFLATION DATA
#===============================================================================#

