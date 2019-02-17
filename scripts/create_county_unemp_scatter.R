#===============================================================================#
# PULL EMPLOYMENT BY COUNTY
#   
# Cecile Murray
# 2019-02-02
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
rm(cturate_data)


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
  geom_point(color = lt_orange) +
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


max_urate %<>% mutate(popcat = log10(pop))
max_urate %>% ggplot() + geom_histogram(aes(x = popcat), bindwidth = 1)

max_urate %<>% mutate(popcat = case_when(
  pop < 50000 ~ "1",
  pop < 100000 ~ "2",
  pop < 1000000 ~ "3",
  pop < 10000000 ~ "4"
))


ggplot(max_urate, aes(x = yq(quarter), y = max_urate/100, color = popcat,
                      alpha = pop)) +
  geom_rect(aes(xmin = yq("2008-Q1"), xmax = yq("2009-Q3"),
                ymin = 0, ymax = .325),
            fill = "gray85", color = "gray85", alpha = 0.75) +
  geom_jitter() +
  scale_color_manual(values = c("1" = lt_blue, "2" = lt_green, "3" = lt_orange, "4" = lt_pink)) +
  scale_alpha_continuous(name = "Population (log),\n2017", trans = "log10",
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
           legend.position = c(0.9, 0.75)) 


ggplot(max_urate) +
  geom_rect(aes(xmin = yq("2008-Q1"), xmax = yq("2009-Q3"),
                ymin = 0, ymax = .325),
            fill = "gray85", color = "gray85", alpha = 0.75) +
  geom_hex(aes(x = yq(quarter),
               y = max_urate/100, 
               fill = popcat)) +
             # scale_fill_continuous(trans = "log10") +
             lt_theme()

ggplot(max_urate) +
  geom_rect(aes(xmin = yq("2008-Q1"), xmax = yq("2009-Q3"),
                ymin = 0, ymax = .325),
            fill = "gray85", color = "gray85", alpha = 0.75) +
  geom_hex(aes(x = yq(quarter),
               y = max_urate/100, 
               fill = ..count..,
               weight = log10(pop))) +
  # scale_fill_continuous(trans = "log10") +
  lt_theme()


adj_cturate %<>% left_join(select(ctpop, stcofips, pop), by = "stcofips")  

recession_quarters <- unlist(Map(function(x, y) paste0(x, "-Q", y),
                          c(rep(2008, 4), rep(2009, 3)),
                          c(seq(1, 4), seq(1, 3))))

qbreaks <- sapply(seq(2007, 2014), function(x) paste0(x, "-Q1"))

plotdata <- adj_cturate %>% filter(!is.na(pop), between(year, 2007, 2013)) %>% 
  mutate(quarter = as.factor(quarter),
         recession = ifelse(quarter %in% recession_quarters, "1", "0")) 

plotdata %>% 
ggplot(aes(x = adj_urate/100,
           y = fct_rev(quarter),
           height = ..density..,
           weight = pop,
           fill = recession)) +
  geom_density_ridges(stat = "density", color = "white") +
  scale_x_continuous(limits = c(0, .20), labels = scales::percent) +
  scale_fill_manual(values = c("1" = "gray40", "0" = lt_blue)) +
  scale_y_discrete(breaks = qbreaks) +
  labs(title = "Unemployment stayed high in many counties well after the end of the official recession",
       x = "Unemployment rate (%)", y = "Quarter",
       caption = "Source: Bureau of Labor Statistics \nNote: County unemployment rates were seasonally smoothed.") +
  lt_theme(panel.grid.major.y = element_blank(),
           legend.position = "none") +
  annotate(geom = "text", x = 0.01, y = "2008-Q4", label = "Recession", family = "Cabin",
           color = "gray40")
