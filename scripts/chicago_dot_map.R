#===============================================================================#
# MAP CHICAGO EMPLOYMENT STATUS
#
# Cecile Murray
# 2019-02-09
#===============================================================================#

library(here)
source("scripts/setup.R")

library(sf)
library(tidycensus)

ways_dir <- "/Users/cecilemurray/Documents/CSDS/ways"

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

# credit to Jens von Bergmann for this algo https://github.com/mountainMath/dotdensity/blob/master/R/dot-density.R
random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

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
  mutate(n_dots = random_round(estimate / 500)) %>%
  mutate(dots = suppressMessages(st_sample_by_poly(., size = n_dots)))

tract_emp3 <- tract_emp2 %>% st_drop_geometry() %>%
  st_set_geometry(tract_emp2$dots) %>% st_as_sf()

#===============================================================================#
# GET SHAPEFILES
#===============================================================================#

# grab pre-processed Chicago OSM file
setwd(ways_dir)
chi_ways <- st_read("chicago_way.geojson")
chi_ways %<>% st_transform(st_crs(tract_emp)$proj4string)
chi_hwy <- filter(chi_ways, highway %in% c("motorway", "motorway_link"))
setwd(here::here())

# create cook county outline
cook <- tract_emp %>% filter(emp_status == "emp") %>% st_union() 

# save.image("plot_data/dotmap.Rdata")

ggplot() +
  geom_sf(data = cook, fill = NA) +
  geom_sf(data = chi_hwy, color = "gray65") +
  geom_sf(data = tract_emp3, aes(color = emp_status),
          size = 0.25, alpha = 0.75) +
  scale_color_manual(values = c("emp" = lt_yellow, "unemp" = lt_blue,
                               "nilf" = "#90BFDE"),
                    name = "Represents 500 people",
                    labels = c("Employed", "Not in labor force", "Unemployed")) +
  labs(title = "Unemployment is spatially uneven at the sub-county level too",
       subtitle = "Labor force status for the population aged 20 and older in Cook County census tracts",
       caption = "Source: American Community Survey 2013-2017 estimates") +
  lt_theme(legend.position = c(0.95, 0.85), axis.text = element_blank(),
           panel.grid = element_blank())

