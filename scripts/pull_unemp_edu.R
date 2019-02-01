#===============================================================================#
# COMPARE UNEMPLOYMENT BY EDUCATIONAL ATTAINMENT
#   Maybe this is a good use of a waffle chart?
#
# Cecile Murray
# 2019-01-31
#===============================================================================#

library(here)
source("scripts/setup.R")

cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

series_list <- c("LNU03027659", "LNU03027660", "LNU03027662", "LNU03027689")

# construct query
cps_query <- filter(cps_codes, series_id %in% series_list)

year_intervals <- sapply(seq(1954, 2018, 19), function(x) seq(x, x + 18, 18))
year_intervals[2,4] <- 2018

raw_data <- map2(year_intervals[1, ] , year_intervals[2, ],
                 function(x, y) bls_api(cps_query$series_id, startyear = x, endyear = y,
                                        registrationKey = blskey,
                                        annualaverage = TRUE)) %>% bind_rows() %>% 
  dplyr::rename(series_id = seriesID) %>%
  mutate(month = paste0(year, "-", period))

unemp_edu <- raw_data %>% filter(period == "M13") %>% 
  mutate(names = case_when(
    series_id == "LNU03027659" ~ "Less than high school",
    series_id == "LNU03027660" ~ "High school graduate",
    series_id == "LNU03027662" ~ "Some college or associate's degree",
    series_id == "LNU03027689"  ~ "Bachelor's degree or higher"
  ),
  order = case_when(
    names == "Less than high school" ~ 1,
    names == "High school graduate" ~ 2,
    names == "Some college or associate's degree " ~ 3,
    names == "Bachelor's degree or higher" ~ 4
  )) %>% ungroup()

annual_totals <- unemp_edu %>% select(year, value) %>% 
  group_by(year) %>% summarize(total = sum(value)) 

unemp_edu %<>% left_join(annual_totals, by = "year")

#===============================================================================#
# MAKE WAFFLES, FROM SCRATCH
#===============================================================================#

scale_year_data <- function(df, yr, scale = FALSE) {
  rv <- filter(df, year == yr) %>% ungroup() %>% select(names, value)
  yr_tot <- sum(rv$value)
  rv$order <- c(4, 1, 3, 2)
  rv %<>% arrange(order)
  if(scale) {
    rv %<>% mutate(value = value / yr_tot * 100)}
  return(rv)
}

unemp09 <- filter(unemp_edu, year == 2009) %>% ungroup() %>% select(names, value)
unemp18 <- filter(unemp_edu, year == 2018) %>% ungroup() %>% select(names, value)

# create an array of values to plot for a single year
gridfn <- function(df, blockval, per_row) {
  
  scaled_df <- df %>% mutate(value = value / blockval) %>% 
  arrange(order)
  total <- sum(scaled_df$value)
  
  grid <- expand.grid(x = seq(1, per_row), y = seq(1, ceiling(total / per_row)))
  z <- unlist(sapply(unique(scaled_df$names),
                     function(x) rep(x, scaled_df$value[scaled_df$names == x])))
  grid$z <- c(z, rep(NA, nrow(grid) - length(z)))
  return(grid)
}

grid_more_yrs <- function(df, years, blockval, per_row) {
  grids <- data.frame(x = c(0), y = c(0), z = c(""))
  for(yr in years){
    g <- filter(df, year == yr) %>% ungroup() %>% select(names, value, order) %>% 
      gridfn(blockval = blockval, per_row = per_row) %>% mutate(year = yr)
    grids <- bind_rows(grids, data.frame(g))
  }
  grids %<>% filter(x > 0)
  return(grids)
}

grids <- grid_more_yrs(filter(unemp_edu), 
                       seq(1992, 2018), blockval = 100, per_row = 1) 

ggplot(grids, aes(x = year, y = y, fill = z)) +
  geom_tile(color = "white") +
  # facet_wrap(facets = vars(year)) +
  scale_fill_discrete(na.value = "white") +
  coord_equal() +
  theme(legend.position = "bottom")
