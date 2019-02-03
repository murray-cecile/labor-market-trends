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

# query the API - ONCE, this is slow
# cturate_data <- purrr::map(seq(1, 50),
#                        function(x) bls_api(cturate_query[x, ], startyear = 2007,
#                                            endyear = 2018, registrationKey = blskey,
#                                            annualaverage = TRUE)) %>% 
#   bind_rows() %>% dplyr::rename(series_id = seriesID)
# 
# save(cturate_data, file = "raw/cturate_data.Rdata")
# rm(cturate_data)

load("raw/cturate_data.Rdata")

# determine month of highest unemployments
cturate <- mutate(cturate_data, stfips = substr(series_id, 6, 7),
                  stcofips = substr(series_id, 6, 10),
                  month = paste0(year, "-", period),
                  quarter = case_when(
                    period %in% c("M01", "M02", "M03") ~ paste0(year, "-", "Q1"),
                    period %in% c("M04", "M05", "M06") ~ paste0(year, "-", "Q2"),
                    period %in% c("M07", "M08", "M09") ~ paste0(year, "-", "Q3"),
                    period %in% c("M10", "M11", "M12") ~ paste0(year, "-", "Q4")
                  )) %>% 
  dplyr::rename(urate = value) %>%  group_by(stcofips) %>% 
  mutate(max_urate = max(urate), is_max = ifelse(urate == max_urate, 1, 0))

annual <- cturate %>% filter(period == "M13") %>% dplyr::rename(ann_urate = urate) %>% 
  select(year, stcofips, ann_urate)

adj_factor <- cturate %>% filter(period != "M13", year < 2018) %>% 
  left_join(annual, by = c("stcofips", "year")) %>% 
  mutate(ann_delta = urate - ann_urate) %>% 
  group_by(period) %>% summarize(month_adj = mean(ann_delta))

adj_cturate <- cturate %>% filter(period != "M13", year < 2018) %>% 
  left_join(adj_factor, by = "period") %>% 
  mutate(adj_urate = urate - month_adj)

by_quarter <- adj_cturate %>% group_by(stcofips, quarter) %>%
  summarize(adj_urate = mean(adj_urate)) %>% ungroup() %>% group_by(stcofips) %>% 
  mutate(max_urate = max(adj_urate),
         is_max = ifelse(adj_urate == max_urate, 1, 0))

#===============================================================================#
# PLOT STUFF
#===============================================================================#

ctpop <- get_acs(geography = "county", variable = "B01001_001",
                 geometry = FALSE) %>% 
  dplyr::rename(stcofips = GEOID, pop = estimate) %>% 
  filter(!stcofips %in% c("72"))

# ggplot(ctpop, aes(x = pop)) +
#   geom_histogram(binwidth = 1000)

max_urate <- by_quarter %>% filter(is_max == 1) %>% 
  left_join(ctpop, by = "stcofips")

rm(cturate_data)
save.image("plot_data/county_unemployment_scatter.Rdata")

# quarter_breaks <- unlist(Map(function(x, y) paste0(x, "-Q", y),
#                       rep(seq(2007, 2017), 2),
#                       rep(c(1, 3), 11)))

ggplot(max_urate, aes(x = yq(quarter), y = max_urate/100, alpha = pop)) +
  geom_rect(aes(xmin = yq("2008-Q1"), xmax = yq("2009-Q3"),
                ymin = 0, ymax = .325),
            fill = "gray85", color = "gray85", alpha = 0.75) +
  geom_point(color = lt_blue) +
  scale_alpha_continuous(name = "Population (log), 2017", trans = "log10",
                         label = scales::number_format(big.mark = ",")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = "1 year", labels = scales::date_format(format = "%Y")) +
  labs(title = "Many counties experienced their highest unemployment rates after 
  the technical end of the Great Recession",
       subtitle = "Maximum quarterly average county unemployment rate, seasonally smoothed, 2007-2017",
       x = "Quarter", y = "Unemployment Rate",
       caption = "Source: Bureau of Labor Statistics Local Area Unemployment Statistics
       Note: Data were seasonally smoothed by subtracting the average monthly difference from annual average.") +
  lt_theme(panel.grid.major.x = element_line(color = "gray75", linetype = "dotted"),
           panel.grid.major.y = element_line(color = "gray75", linetype = "dotted"),
           panel.ontop = TRUE,
           legend.position = c(0.95, 0.8))

#===============================================================================#
# NOW GET GEOMETRY
#===============================================================================#


# stpop <- get_acs(geography = "state", variable = "B01001_001",
#                      geometry = TRUE) %>% 
#   dplyr::rename(stfips = GEOID, pop = estimate) %>% 
#   filter(!stfips %in% c("02", "15" ,"72"))
# 
# mapdata <- left_join(stpop, max_urate, by = "stfips")
# 
# ggplot(mapdata) +
#   geom_sf(aes(fill = quarter)) +
#   lt_theme(axis.text = element_blank())

