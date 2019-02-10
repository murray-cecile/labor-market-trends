#===============================================================================#
# MAP WAGES IN CALIFORNIA
#
# Cecile Murray
# 2019-02-09
#===============================================================================#

library(here)
source("scripts/setup.R")

library(sf)
library(tidycensus)

#===============================================================================#
# GET DATA
#===============================================================================#

tract_emp <- get_acs(geography = "tract", table = "B23025",
                     state = "17", county = "031", geometry = TRUE) %>% 
  dplyr::rename(tract = GEOID) %>% select(-NAME, -moe) %>% 
  mutate(emp_status = case_when(
    variable == "B23025_001" ~ "total",
    variable == "B23025_002" ~ "in_lf",
    variable == "B23025_003" ~ "in_civlf",
    variable == "B23025_004" ~ "emp",
    variable == "B23025_005" ~ "unemp",
    variable == "B23025_006" ~ "af",
    variable == "B23025_007" ~ "nilf"
  )) %>% select(-variable) %>% 
  filter(emp_status %in% c("emp", "unemp", "nilf")) %>% 
  st_as_sf()


#===============================================================================#
# MAKE DOTS
#===============================================================================#
# based on https://github.com/andybega/mireg-blogs/tree/master/us-2016-dot-density

# Modified version of sf:::st_sample that combines points by sampled polyon
st_sample_by_poly <- function(x, size) {
  x <- st_geometry(x)
  res <- lapply(1:length(x), function(i) {
    y <- st_poly_sample_n(x[i], size[i]) %>%
      st_combine()
    if (st_is_empty(y)) {
      y <- st_as_sfc("MULTIPOINT EMPTY")
    }
    y
  })
  do.call(c, res)
}

# Modified version of sf:::st_poly_sample that always returns correct size
# when sampling a polygon
st_poly_sample_n <- function(x, size) {
  stopifnot(length(x)==1)
  stopifnot(length(size)==1)
  x <- st_geometry(x)
  size <- round(size)
  if (size==0) {
    return(st_as_sfc("POINT EMPTY"))
  } else {
    pts <- st_sample(x, size)
    max_iter <- 10
    iter <- 1
    while(length(pts) < size & !(iter > max_iter)) {
      need <- size - length(pts)
      pts <- c(pts, st_sample(x, need))
      iter <- iter + 1
    }
    if (length(pts)==size) {
      return(pts)
    } else if (length(pts) > size) {
      return(pts[1:size])
    }
  }
}

tract_emp2 <- tract_emp %>%
  mutate(n_dots = random_round(estimate / 100)) %>%
  mutate(dots = suppressMessages(st_sample_by_poly(., size = n_dots)))


#===============================================================================#
# GET SHAPEFILES
#===============================================================================#

# grab Chicago OSM file
setwd("/Users/cecilemurray/Documents/CSDS")
chi_ways <- st_read("ways/chicago_way.geojson")
chi_ways %<>% st_transform(st_crs(tract_emp)$proj4string)
chi_hwy <- filter(chi_ways, highway %in% c("motorway", "motorway_link"))
setwd(here::here())

ggplot() +
  geom_sf(data = chi_hwy) +
  geom_sf(data = st_set_geometry(tract_emp2, tract_emp2$dots), 
          aes(color = emp_status), size = 0.25, alpha = 0.75) +
  geom_sf(data = st_set_geometry(tract_emp2, tract_emp2$geometry),
          fill = NA, lwd = 0.25, color = "gray50") +
  lt_theme(axis.text = element_blank())

