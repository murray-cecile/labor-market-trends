#===============================================================================#
# TITLE
#   Description
#
# Cecile Murray
# Date
#===============================================================================#

library(devtools)
install_github("mikeasilva/BLSapi")

blskey <- "3bbe592b119044e780cfc92fd3e76a61"

library(blsAPI)
library(rjson)

# test out a query from QCEW
series_test <- c("ENU1703110522", "ENU1703105221", "ENU170311052211")
test_call <- blsAPI(series_test[1], return_data_frame = TRUE)

# test with multiple series/payload approach
payload <- list('seriesid' = c("ENU1703110522", "ENU1703105221", "ENU170311052211"))
test_call <- blsAPI(payload, return_data_frame = TRUE)

#===============================================================================#
# TRY ANOTHER PACKAGE
#===============================================================================#

devtools::install_github("keberwein/blscrapeR")
library(blscrapeR)

blskey <- "3bbe592b119044e780cfc92fd3e76a61"

set_bls_key(blskey)
readRenviron("~/.Renviron")
Sys.getenv("BLS_KEY")

test <- qcew_api(year = 2017, qtr = "a", slice = "industry", sliceCode = 1013)
# this returned county level data for 

