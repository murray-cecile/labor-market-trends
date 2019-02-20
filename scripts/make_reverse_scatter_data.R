#===============================================================================#
# SELECTED DATA FOR REVERSE SCATTER PLOT
#===============================================================================#

library(here)
source("scripts/create_black_white_gap_wk3.R")

bw <- raceth_unemp_yr %>% select(year, black, white, unemp_all) %>%
  filter(year > 1998)

library(jsonlite)
# jsonlite::write_json(bw, "temp/black_white_unemployment.json")

bw %<>% gather("raceth", "unemp", -year)
jsonlite::write_json(bw, "temp/black_white_unemployment_long.json")

ggplot(bw, aes(x = year, y = unemp, color = raceth)) +
  geom_point() +
  scale_color_manual(values = c("black" = lt_green, "white" = lt_blue,
                                "unemp_all" = "#ffffffff")) +
  lt_theme()
