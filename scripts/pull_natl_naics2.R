#===============================================================================#
# PULL NATIONAL EMPLOYMENY BY MAJOR INDUSTRY CATEGORY
#   All jobs by major industry groups (grouped 2-digit NAICS), 2007-2018
#   
#
# Cecile Murray
# 2019-01-16
#===============================================================================#

library(here)
source("scripts/setup.R")

#===============================================================================#
# CONSTRUCT SERIES IDS & PULL FROM API
#===============================================================================#

industry_list <- c("Mining and logging", "Construction", "Manufacturing",
                   "Retail trade", "Wholesale Trade", "Utilities",
                   "Transportation and warehousing", "Information",
                   "Financial activities", 
                   "Professional and business services",
                   "Education and health services",
                   "Leisure and hospitality", "Other services", "Government",
                   "Total nonfarm")

# construct series IDs to query
naics2 <- filter(industries, industry_name %in% industry_list) %>% 
  mutate(seriesID = paste0("CE", "S", industry_code, "01"))

naics2_data <- bls_api(naics2$seriesID, startyear = 2007, endyear = 2018,
                       registrationKey = blskey, annualaverage = TRUE) %>% 
  mutate(month = paste0(year, "-", substr(period, 2, 3))) %>% 
  left_join(naics2, by = "seriesID")



#===============================================================================#
# EXPLORE DATA
#===============================================================================#

annual_naics2 <- filter(naics2_data, periodName != "Annual")  %>% 
  select(year, month, value, seriesID, industry_name) %>%  
  mutate(base_jobs = ifelse(month == "2007-01", value, NA)) %>%
  fill(base_jobs, .direction = c("up")) %>%   dplyr::rename(jobs = value) %>% 
  mutate(index_jobs = jobs / base_jobs) %>% 
  filter(industry_name != "Total nonfarm")

# indexed employment - I like where this one is going!s
ggplot(annual_naics2, aes(x = month, y = index_jobs, group = industry_name, 
                          color = industry_name)) +
  geom_path() +
  scale_fill_brewer() +
  scale_x_date(date_breaks = "1 year") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# what were the biggest industries just before the recession?
industry_shares <- naics2_data %>% filter(month %in% c("2007-08", "2018-08")) %>% 
  select(year, month, industry_name, value) %>% 
  mutate(tot_nonfarm = ifelse(industry_name == "Total nonfarm", value, NA)) %>% 
  arrange(month) %>% fill(tot_nonfarm) %>% 
  mutate(industry_share = value / tot_nonfarm) 

#===============================================================================#
# BREAK NAME
#===============================================================================#

load("temp/naics2_average_earnings_07.Rdata")

industry_growth <- industry_shares %>% select(month, industry_name, value) %>% 
  spread(month, value) %>% clean_names() %>% 
  mutate(pct_change = x2018_08 / x2007_08 - 1) %>% 
  left_join(earn_cats, by = "industry_name")

nonfarm_growth <- industry_growth$pct_change[industry_growth$industry_name=="Total nonfarm"]

ann_text <- data.frame(industry_name = "Information", pct_change = nonfarm_growth + .01,
                       lab = "Growth rate across all industries",
                       earncat = factor("High-earning",
                                        levels = c("High-earning", "Low-earning")))


ggplot(filter(industry_growth, !is.na(industry_code)),
       aes(x = reorder(industry_name, pct_change), y = pct_change,
                            fill = earncat)) +
  geom_col() +
  geom_text(aes(label = round(pct_change * 100, 1)), hjust = 0.5, 
            vjust = 1, nudge_y = -0.005) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(cols = vars(earncat), scales = "free", space = "free") +
  geom_hline(aes(yintercept = nonfarm_growth), color = "gray50",
             linetype = "dashed") +
  geom_text(data = ann_text, label = "Growth rate across all industries",
            hjust = "left", nudge_x = -0.5) +
  labs(title = "The fastest-growing industries have had lower average earnings",
       subtitle = "Percent change in employment by industry, 2007-2018",
       x = "Industry", y = "Percent change in employment",
       caption = "Source: Bureau of Labor Statistics Current Employment Statistics
       Note: High-and low-earning categories based on whether an industry's average hourly
       earnings are above or below average hourly earnings for all private jobs.") +
  guides(fill = guide_legend(title = "Earnings Category")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.x = element_blank(), legend.position = "bottom")


#===============================================================================#
# 
#===============================================================================#

# 
# growth_share <- industry_shares %>% ungroup() %>% select(-year, -tot_nonfarm) %>% 
#   gather(type, estimate, -month, -industry_name) %>% 
#   mutate(key = paste0(month, "_", type)) %>% 
#   spread(key, estimate) %>% clean_names() %>%
#   mutate(delta = x2018_08 - x2007_08) %>% 
#   left_join(earn_cats, by = "industry_name")
# 
# ggplot(filter(growth_share, !industry_name %in% c("Government", "Total nonfarm")),
#        aes(x = reorder(industry_name, delta), y = delta * 100, fill = earncat)) +
#   geom_col() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "top") +
#   labs(title = "The labor market has shifted toward lower-earning sectors",
#        subtitle = "Change in share of total employment by industry and average hourly wage, 2007-2018",
#        x = "Industry", y = "",
#        caption = "Source: Bureau of Labor Statistics Current Employment Statistics") +
#   guides(fill = guide_legend(title = NULL))
# 
# ggplot(filter(industry_shares, !industry_name %in% c("Government", "Total nonfarm")),
#        aes(x = reorder(industry_name, delta), y = delta * 100, fill = earncat)) +
#   geom_col() +
#   labs(title = "The labor market has shifted toward lower-earning sectors",
#        subtitle = "Change in share of total employment by industry and average hourly wage, 2007-2018",
#        x = "Industry", y = "",
#        caption = "Source: Bureau of Labor Statistics Current Employment Statistics") +
#   guides(fill = guide_legend(title = NULL)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.ticks.x = element_blank(), panel.background = element_rect(color = "gray"),
#         legend.position = "top")
