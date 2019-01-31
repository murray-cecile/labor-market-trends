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

#===============================================================================#
# BREAK NAME
#===============================================================================#


# ggplot(filter(stunemp_data, periodName == "Annual", year %in% c(2007, 2017)),
#        aes(x = stfips, y = value, fill = region)) +
#   geom_violin()
# 
# ggplot(filter(stunemp_data, year %in% c(2008, 2018)),
#        aes(x = region, y = value, fill = region)) +
#   geom_violin() +
#   facet_grid(cols = vars(year))

#===============================================================================#
# TEST NBERSHADE
#===============================================================================#

rshade <- nberDates() %>% mutate(Start = lubridate::ymd)

#===============================================================================#
# DEVELOPING A GRID FUNCTION
#===============================================================================#

# goal: one block for each 100,000 people
grid09 <- expand.grid(x = seq(1, 8), y = seq(1, 32)) 
grid09 <- grid09[1:262, ]
grid09$z <- c(rep("a", 143), rep("b", 8), rep("c", 22), rep("d", 89))

grid18 <- expand.grid(x = seq(10, 17), y = seq(1, ceiling(sum(unemp18$value)/100/8)))
# grid18$z <- c(rep("a", 62), rep("b", 4), rep("c", 15), rep("d", 47))

u18 <- unemp18 %>% mutate(value = value/100)
# z <- unlist(sapply(unique(u18$names), function(x) rep(x, u18$value[u18$names == x])))
# grid18$z <- c(z, rep(NA, nrow(grid18) - length(z)))

grid18 %<>% bind_rows(data.frame(x = seq(10,14), y = rep(17, 5),
                                 z = c("a", "a", "b","c","d")))

# grid18 <- expand.grid(x = seq(1, 8), y = seq(1, 16))
# grid18$z <- c(rep("a", 62), rep("b", 4), rep("c", 15), rep("d", 47))
# gridcombo <- bind_rows(mutate(grid09, year = "09"),
#                        mutate(grid18, year = "18"))
# 
# ggplot(gridcombo, aes(x = x, y = y, fill = z), color = "white") +
#   geom_tile(size = 4) +
#   facet_wrap(facets = vars(year)) +
#   coord_equal()


ggplot() +
  geom_tile(data = grid09, aes(x = x, y = y, fill = z), color = "white") +
  geom_tile(data = grid18, aes(x = x, y = y, fill = z), color = "white") +
  scale_fill_discrete(na.value = "white") +
  coord_equal() +
  theme_minimal()