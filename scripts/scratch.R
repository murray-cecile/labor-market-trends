#===============================================================================#
# SCRATCH CODE FOR RANDOM CHECKS
#   these snippets are dependent on other code and this isn't best practice
#
# Cecile Murray
# 219-01-19
#===============================================================================#

# check about durable goods
mf <- filter(naics2_data, industry_name == "Manufacturing") %>% 
  group_by(year) %>% summarize(value = sum(value))
durable <- filter(naics2_data, industry_name == "Durable goods") %>% 
  group_by(year) %>% summarize(value = sum(value))
nondurable <- filter(naics2_data, industry_name == "Nondurable goods") %>% 
  group_by(year) %>% summarize(value = sum(value))
compare <- left_join(mf, durable, by = "year", suffix = c("mf", "db")) %>% 
  left_join(nondurable, by = "year") %>% 
  mutate(jobsum = valuedb + value)
rm(mf, durable, nondurable, compare)
# CONCLUSION: DURABLE AND NON-DURABLES ARE SUBSECTORS OF MANUFACTURING  