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
  )) %>% ungroup()

annual_totals <- unemp_edu %>% select(year, value) %>% 
  group_by(year) %>% summarize(total = sum(value)) 

unemp_edu %<>% left_join(annual_totals, by = "year") %>% 
  mutate(share = value / total)

save.image("plot_data/unemployment_education.Rdata")

#===============================================================================#
# MAKE WAFFLES, FROM SCRATCH
#===============================================================================#

# create an array of values to plot for a single year
gridfn <- function(df, blockval, per_row) {
  
  scaled_df <- df %>% mutate(value = value / blockval)
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

grids <- grid_more_yrs(dplyr::rename(unemp_edu), 
                       c(1997, 2007, 2017), blockval = 100, per_row = 4) %>% 
  mutate(x = x + (year - 1997) / 2,
         order = case_when(
           z == "Less than high school" ~ 1,
           z == "High school graduate" ~ 2,
           z == "Some college or associate's degree " ~ 3,
           z == "Bachelor's degree or higher" ~ 4
         ))

colors <- c("Less than high school" = lt_blue,
            "High school graduate" = lt_pink,
            "Some college or associate's degree" = lt_green,
            "Bachelor's degree or higher" = lt_orange)

ggplot(grids, aes(x = x, y = y, fill = reorder(z, order))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = colors, na.value = "white") +
  scale_x_continuous(breaks = c(2.5, 7.5, 12.5), labels = c(1997, 2007, 2017),
                     limits = c(0, 20)) +
  coord_equal() +
  labs(title = "Today's unemployed population includes more individuals
  with at least some college education",
       subtitle = "Unemployed population by educational attainment near three business
  cycle peaks") +
  annotate(geom = "tile", x = 1, y = 13, fill = "gray50") +
  annotate(geom = "text", x = 4, y = 13, label = "Represents \n100,000 people",
           size = 3) +
  annotate(geom = "text", x = 17, y = 1, label = "Less than\nhigh school", 
           size = 3) +
  annotate(geom = "text", x = 17, y = 4, label = "High school\ngraduate",
           size = 3) +
  annotate(geom = "text", x = 17, y = 8, label = "Some college or\nassociate's degree",
           size = 3) +
  annotate(geom = "text", x = 17, y = 11, label = "Bachelor's degree\n or higher", 
           size = 3) +
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_blank(), legend.position = "none")


