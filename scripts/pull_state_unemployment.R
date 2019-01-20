#===============================================================================#
# PULL EMPLOYMENT BY STATE
#   
#
# Cecile Murray
# 2019-01-19
#===============================================================================#

library(here)
source("scripts/setup.R")

#===============================================================================#
# CONSTRUCT QUERY
#===============================================================================#

# get list of all the state codes
states <- fread("https://download.bls.gov/pub/time.series/la/la.area") %>% 
  filter(area_type_code == "A") 

# create a list of series IDs
sturate_query <- sapply(states$area_code, function(x) paste0("LAU", x, "03"))
stunemp_query <- sapply(states$area_code, function(x) paste0("LAU", x, "06"))

# query the API
stunemp_data <- bls_api(stunemp_query, startyear = 2007, endyear = 2018,
                        registrationKey = blskey, annualaverage = TRUE) %>% 
  dplyr::rename(series_id = seriesID)


sturate_data <- bls_api(sturate_query, startyear = 2007, endyear = 2018,
                        registrationKey = blskey, annualaverage = TRUE) %>% 
  dplyr::rename(series_id = seriesID)



#===============================================================================#
# CODE STATES INTO REGIONS
#===============================================================================#

# create crosswalk to indicate census division and region
sturate_data %<>% mutate(stfips = substr(series_id, 6, 7),
                 division = case_when(
                   stfips %in% c("09", "23", "25", "33", "44", "50") ~ "New England",
                   stfips %in% c("34", "36", "42") ~ "Mid-Atlantic",
                   stfips %in% c("18", "17", "26", "39", "55") ~ "East North Central",
                   stfips %in% c("19", "20", "27", "29", "31", "38", "46") ~ "West North Central",
                   stfips %in% c("10", "11", "12", "13", "24", "37", "45", "51", "54") ~ "South Atlantic",
                   stfips %in% c("01", "21", "28", "47") ~ "East South Central",
                   stfips %in% c("05", "22", "40", "48") ~ "West South Central",
                   stfips %in% c("04", "08", "16", "35", "30", "49", "32", "56") ~ "Mountain",
                   stfips %in% c("02", "06", "15", "41", "53") ~ "Pacific"
                 ),
                 region = case_when(
                   division %in% c("New England", "Mid-Atlantic") ~ "NE",
                   division %in% c("East North Central", "West North Central") ~ "MW",
                   division %in% c("South Atlantic", "East South Central", "West South Central") ~ "S",
                   division %in% c("Mountain", "Pacific") ~ "W"
                 )) 

#===============================================================================#
# PLOTS
#===============================================================================#

stunemp <- stunemp_data %>% dplyr::rename(lf = value) %>% 
  mutate(stfips = substr(series_id, 6, 7)) %>% 
  select(year, period, stfips, lf)

stdata <- sturate_data %>% left_join(stunemp, by = c("year", "period", "stfips")) %>% 
  mutate(value = value/100) %>% 
  left_join(state_fips, by = c("stfips" = "fips_state"))


ggplot(filter(stdata, period == "M10", year %in% c(2009)), 
       aes(x = reorder(state_abb, value), y = value)) +
  geom_point(aes(size = lf), color = "darkcyan") +
  scale_size_continuous(breaks = c(0, 5000000, 10000000, 15000000),
                        labels = c("Under 5M", "5 to 10M", "10 to 15M", "15+ M"),
                        limits = c(0,20000000),
                        name = "Labor Force Size") +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0.1, color = "gray50", linetype = "longdash" ) +
  geom_text(aes(x = 7, y = .102, label = "National unemployment rate: 10%")) +
  labs(title = "States saw widely different unemployment rates in the depth of the recession",
       subtitle = "Unemployment rate and state labor force size (millions), October 2009",
       x = "State", y = "Unemployment Rate",
       caption = "Source: Bureau of Labor Statistics Current Population Survey.
       Note: Peak national unemployment occurred in October 2009.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(), legend.position = "top")

# ggsave("plots/State_unemployment_rate.pdf")
