#===============================================================================#
# CREATE CBSA SHAPEFILES
#   Get metro area shapefiles where AK and HI metros are rotated
#
# Cecile Murray
# 2019-02-09
#===============================================================================#

library(here)
library(sf)
library(rgdal)


# read in the CBSA shapefile, transform projection, grab metros
cbsa <- st_read("shp/cbsa_2017.geojson")  %>%
  st_transform(st_crs(natl_map)$proj4string) %>%
  filter(LSAD == "M1", !str_detect(NAME, "PR"),
         !str_detect(NAME, "AK"), !str_detect(NAME, "HI"))

# grab relevant counties from AK and HI
ak_cty <- filter(ctpop, substr(stcofips, 1, 2) == "02") %>% 
  filter(stcofips %in% (ak$stcofips)) 
anchorage <- st_union(ak_cty[ak_cty$stcofips=="02020",],
                        ak_cty[ak_cty$stcofips=="02170",])
fairbanks <- ak_cty[ak_cty$stcofips == "02090", ]

hi_cty <- filter(ctpop, substr(stcofips, 1, 2) == "15") %>% 
  filter(stcofips %in% (hi$stcofips)) 
kahului <- st_union(hi_cty[hi_cty$stcofips == "15009", ],
                    hi_cty[hi_cty$stcofips == "15005", ])
honolulu <- hi_cty[hi_cty$stcofips == "15003",]

all_ak <- get_acs(geography = "state", table = "B01001",
                  geometry = TRUE, shift_geo = TRUE) %>%
  filter(GEOID == "02")

# ggplot() +
#   geom_sf(data = anchorage, color = "red") +
#   geom_sf(data = fairbanks, color = "red") +
#   geom_sf(data = all_ak, fill = NA)
# 
# ggplot() +
#   geom_sf(data = honolulu, color = "red") +
#   geom_sf(data = kahului, color = "red") 
# 
# ggplot(cbsa) +
#   geom_sf()
# 
# 
