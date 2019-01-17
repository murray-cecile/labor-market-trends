#===============================================================================#
# PULL NATIONAL EMPLOYMENY BY TWO-DIGIT NAICS 
#   All jobs by 2-digit NAICS sector for the whole country, 2007-2018
#
# Cecile Murray
# 2019-01-16
#===============================================================================#

libs <- c("tidyverse", "magrittr", "stringr", "readr", "data.table", "janitor",
          "blscrapeR", "here")
  lapply(libs, library, character.only=TRUE)

blskey <- Sys.getenv("BLS_KEY")

#===============================================================================#
# CONSTRUCT SERIES IDS & PULL FROM API
#===============================================================================#

# pull list of all NAICS codes
industries <- fread("https://download.bls.gov/pub/time.series/sm/sm.industry") %>% 
  mutate(industry_code = str_pad(industry_code, 8, side = "left", pad = "0"))

# construct series IDs to query
naics2 <- filter(industries, substr(industry_code, 3, 8) == "000000") %>% 
  mutate(seriesID = paste0("CE", "S", industry_code, "01"))

naics2_data <- bls_api(naics2$seriesID, startyear = 2007, endyear = 2018,
                       registrationKey = blskey, annualaverage = TRUE) %>% 
  mutate(month = paste0(year, "-", substr(period, 2, 3))) %>% 
  left_join(naics2, by = "seriesID")

#===============================================================================#
# EXPLORE DATA
#===============================================================================#

supersectors <- c("Total Nonfarm", "Goods Producing", "Service-Providing",
                  "Private Service Providing", "Total Private")

annual_naics2 <- filter(naics2_data, periodName != "Annual",
                        substr(seriesID, 4, 5) == "00" | substr(seriesID, 4, 4) != "0")  %>% 
  select(year, month, value, seriesID) %>%  
  mutate(base_jobs = ifelse(month == "2007-01", value, NA)) %>%
  fill(base_jobs, .direction = c("up")) %>%   dplyr::rename(jobs = value) %>% 
  mutate(index_jobs = jobs / base_jobs) %>% 
  mutate(flag = ifelse(industry_name %in% c("Construction",
                                            "Education and Health Services",
                                            "Leisure and Hospitality"), 1, 0))


ggplot(annual_naics2, aes(x = month, y = index_jobs, color = industry_name)) +
  geom_point() +
  scale_fill_brewer() +
  theme_minimal()


ggplot(annual_naics2, aes(x = year, y = index_jobs,
                          color = ifelse(flag, industry_name, "other"))) +
  geom_point() +
  scale_fill_brewer()


ggplot(annual_naics2, aes(x = year, y = index_jobs,
                          group = ifelse(flag, industry_name, "other"),
                          color=ifelse(flag, industry_name, "#888888"))) +
  geom_point()


# what were the biggest industries just before the recession?
industry_shares <- naics2_data %>% filter(month %in% c("2007-08", "2018-08")) %>% 
  select(year, month, industry_name, value) %>% 
  mutate(tot_nonfarm = ifelse(industry_name == "Total Nonfarm", value, NA)) %>% 
  arrange(month) %>% fill(tot_nonfarm) %>% 
  mutate(industry_share = value / tot_nonfarm) 

ggplot(filter(industry_shares, !industry_name %in% supersectors),
       aes(x = industry_name, y = industry_share, fill = month)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

industry_shares %<>%filter(!industry_name %in% supersectors) %>% 
  ungroup() %>% select(-value, -year, -tot_nonfarm) %>% 
  spread(month, industry_share) %>% clean_names() %>% 
  mutate(delta = x2018_08 - x2007_08) 

ggplot(industry_shares, aes(x = reorder(industry_name, delta), y = delta)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
