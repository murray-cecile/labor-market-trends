#===============================================================================#
# PULL UNEMPLOYMENT BY RACE AND GENDER
#   U3 and epop by race from CPS
#
# Cecile Murray
# 2019-01-26
#===============================================================================#

library(here)
source("scripts/setup.R")


cps_codes <- fread("https://download.bls.gov/pub/time.series/ln/ln.series")

epop <- filter(cps_codes, str_detect(series_title, "Employment-Population"),
               periodicity_code == "M", sexs_code ==0 )

unemp <- c("04000000", "04000003", "04000006", "04000009")

# prime-aged epop all/white/black/hispanic, not seasonallu adjusted
prime_age_epop <- c("02300060", "02300063","02300066", "02300069")

# construct query
cps_query <- filter(cps_codes, periodicity_code == "M",
                    substr(series_id, 4, 12) %in% c(unemp, prime_age_epop),
                    sexs_code == 0)


year_intervals <- sapply(seq(1954, 2018, 19), function(x) seq(x, x + 18, 18))
year_intervals[2,4] <- 2018

raw_data <- map2(year_intervals[1, ] , year_intervals[2, ],
  function(x, y) bls_api(cps_query$series_id, startyear = x, endyear = y,
                     registrationKey = blskey)) %>% bind_rows() %>% 
  dplyr::rename(series_id = seriesID) %>%
  mutate(month = paste0(year, "-", period),
         raceth = case_when(
           substr(series_id, 11, 11) == "0" ~ "all",
           substr(series_id, 11, 11) == "3" ~ "white",
           substr(series_id, 11, 11) == "6" ~ "black",
           substr(series_id, 11, 11) == "9" ~ "hispanic"
         ))

all <- raw_data %>% filter(substr(series_id, 11, 11) == "0") %>% 
  spread(series_id, value) %>%
  dplyr::rename(epop_all = LNU02300060,
                unemp_all = LNU04000000) %>%
  select(month, contains("_all"))

extract_by_race <- function(df, metric, metric_code, all_df = all) {
  rv <- df %>% filter(raceth %in% c("white", "black", "hispanic"),
                     substr(series_id, 4, 5) == metric_code) %>%
    select(-series_id) %>% spread(raceth, value) %>% 
    mutate(bw_delta = black - white, hw_delta = hispanic - white) %>% 
    left_join(select(all_df, month, contains(metric)), by = "month")
  return(rv)  
}

raceth_unemp <- extract_by_race(raw_data, "unemp", "04")
raceth_epop <- extract_by_race(raw_data, "epop", "02")

ggplot(filter(raceth_unemp, year > 1972), 
       aes(x = white, y = black, color = year)) +
  geom_point() +
  scale_x_continuous(limits = c(3, 24)) +
  scale_y_continuous(limits = c(3, 24))

ggplot(filter(raceth_epop_yr, year > 1994), 
       aes(x = white, y = black, color = year)) +
  geom_line() +
  scale_x_continuous(limits = c(65, 85)) +
  scale_y_continuous(limits = c(65, 85))


raceth_unemp_yr <- filter(raceth_unemp, year > 1971) %>% group_by(year) %>% 
  summarize(black = mean(black), white = mean(white), hispanic = mean(hispanic),
            unemp_all = mean(unemp_all)) %>% 
  mutate(bw_delta = black - white, hw_delta = hispanic - white) 

raceth_epop_yr <- filter(raceth_epop, year > 1994) %>% group_by(year) %>% 
  summarize(black = mean(black), white = mean(white), hispanic = mean(hispanic),
            epop_all = mean(epop_all)) %>% 
  mutate(bw_delta = black - white)

ggplot(data = raceth_unemp_yr, aes(x = year)) +
  geom_line(aes(y = bw_delta), color = "green") +
  geom_line(aes(y = black), color = "orange") +
  geom_line(aes(y = white), color = "blue")

ggplot(raceth_unemp_yr, aes(x = year, ymin = white, ymax = black)) +
  geom_linerange()

ggplot(raceth_unemp_yr, aes(x = year, y = black-white)) +
  geom_line()

ggplot(raceth_unemp_yr, aes(x = year)) +
  geom_errorbar(aes(ymin = white/100, ymax = black/100), color = "gray50", 
                 alpha = 0.75) +
  geom_line(aes(y = (black - white)/100), color = "orange") +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Black unemployment has always been higher than white
  unemployment, but the gap is smaller than ever",
       subtitle = "Average annual unemployment rate by race, 1971-2018",
       x = "Year", y = "Average annual unemployment rate",
       caption = "Source: Bureau of Labor Statistics Current Population Survey") +
  annotate(geom = "errorbar", x = 1988, ymin = 0.18, ymax = 0.195, 
           color = "gray50") +
  annotate(geom = "text", x = 1996, y = 0.195, label = "Black unemployment",
           size = 3.5) +
  annotate(geom = "text", x = 1996, y = 0.18, label = "White unemployment",
           size = 3.5) +
  annotate(geom = "line", x = c(2005, 2007), y = rep(0.1875, 2), color = "orange",
           size = 1.25) +
  annotate(geom = "text", x = 2013, y = 0.1875, label = "Black-white gap", size = 3.5)
