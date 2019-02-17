#===============================================================================#
# COMPARE DIFFERENT MEASURES OF UNEMPLOYMENT
#   Mostly U-3 vs U-6
#
# Cecile Murray
# 2019-01-27
#===============================================================================#

library(here)
source("scripts/setup.R")

cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

# composition of U6:
# U‑1:  People who are unemployed for 15 weeks or longer LNS13008516
# U‑2:  Job losers, plus people who completed temporary jobs
# U‑3:  Total number of people who are unemployed  (official unemployment rate). LNS13000000
# U‑4:  Total number of people who are unemployed, plus discouraged workers + 
# U‑5:  Total number of people who are unemployed, plus discouraged workers,
# plus all other persons marginally attached to the labor force + LNU05026642
# U‑6:  Total number of people who are unemployed, plus all persons marginally attached
# to the labor force, plus total employed part time for economic reasons

series_list <- c("LNU03000000", "LNU05026645", "LNU05026642", "LNU02032194")

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

unemp_monthly <- raw_data %>% filter(period != "M13") %>% 
  mutate(names = case_when(
    series_id == "LNU03000000" ~ "Unemployed",
    series_id == "LNU05026645" ~ "Discouraged",
    series_id == "LNU05026642" ~ "Marginally attached",
    series_id == "LNU02032194"  ~ "Part-time for economic reasons"
  ),
  order = case_when(
    names == "Unemployed" ~ 1,
    names == "Discouraged" ~ 2,
    names == "Marginally attached" ~ 3,
    names == "Part-time for economic reasons" ~ 4
  )) %>% ungroup()

monthly_totals <- unemp_monthly %>% select(month, value) %>% 
  group_by(month) %>% summarize(total = sum(value)) #%>% filter(year > 1993) 

unemp_monthly %<>% left_join(monthly_totals, by = "year")

#===============================================================================#
# SMOOTH SEASONAL VARIATION
#===============================================================================#

annual <- raw_data %>% filter(period == "M13") %>% dplyr::rename(ann_value = value) %>% 
  select(year, series_id, ann_value) %>% 
  mutate(names = case_when(
    series_id == "LNU03000000" ~ "Unemployed",
    series_id == "LNU05026645" ~ "Discouraged",
    series_id == "LNU05026642" ~ "Marginally attached",
    series_id == "LNU02032194"  ~ "Part-time for economic reasons"
  ))

adj_factor <- unemp_monthly %>% 
  left_join(annual, by = c("year", "names")) %>% 
  mutate(ann_delta = value - ann_value) %>% 
  group_by(period, names) %>% summarize(month_adj = mean(ann_delta))

adj_levels <- unemp_monthly  %>% 
  left_join(adj_factor, by = c("period", "names")) %>% 
  mutate(adj_value = value - month_adj) 

# save.image("plot_data/different_unemployment_measures.Rdata")

#===============================================================================#
# MAKE A STACKED BAR
#===============================================================================#

underutilized <- adj_levels %>% filter(year > 1993) %>% 
  mutate(month_date = lubridate::ymd(paste0(str_remove(month, "M"), "-01"))) 

ggplot(underutilized, aes(x = month_date, y = adj_value / 1000, group = names)) +
  geom_area(aes(fill = names)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min(underutilized$month_date), max(underutilized$month_date))) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(values = c("Discouraged" = lt_green, 
                               "Marginally attached" = lt_pink,
                               "Part-time for economic reasons" = lt_orange,
                               "Unemployed" = lt_blue),
                    name = "") +
  labs(title = "The official unemployment measure doesn't count millions of workers",
       subtitle = "Alternative measures of underutilized workers, in millions, 1994-2018",
       x = "Year", y = "Workers (millions)",
       caption = "Source: Current Population Survey") +
  lt_theme(legend.position = c(0.23, 0.85),
           axis.text.x = element_text(angle = 45))

#===============================================================================#
# WAFFLE, FROM SCRATCH
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

unemp09 <- scale_year_data(unemp_annual, 2009)
unemp18 <- scale_year_data(unemp_annual, 2018) 

# create an array of values to plot for a single year
gridfn <- function(df, blockval, per_row) {
  
  scaled_df <- df %>% mutate(value = value / blockval) %>% 
    mutate(order = case_when(
    names == "Unemployed" ~ 1,
    names == "Discouraged" ~ 2,
    names == "Marginally attached" ~ 3,
    names == "Part-time for economic reasons" ~ 4
  )) %>% arrange(order)
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
    g <- filter(df, year == yr) %>% ungroup() %>% select(names, value) %>% 
      gridfn(blockval = blockval, per_row = per_row) %>% mutate(year = yr)
    grids <- bind_rows(grids, data.frame(g))
  }
  grids %<>% filter(x > 0)
  return(grids)
}

grids <- grid_more_yrs(filter(unemp_annual), 
                       seq(2007, 2018), blockval = 500, per_row = 5) 

ggplot(grids, aes(x = x, y = y, fill = z)) +
  geom_tile(color = "white") +
  facet_grid(cols = vars(year)) +
  scale_fill_discrete(na.value = "white") +
  coord_equal() +
  theme(legend.position = "bottom")
