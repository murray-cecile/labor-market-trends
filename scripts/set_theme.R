#===============================================================================#
# LABOR MARKET TRENDS: THEME
#   Define standard visual appearance of charts
#
# Cecile Murray
# 2019-02-01
#===============================================================================#

# Font and text size choices for titles, subtitles, captions, labels and annotations
# Panel and/or background color
# Major and Minor Gridlines
library(extrafont)
library(extrafontdb)

# Colors: 
lt_blue <- "#217FBE"
lt_orange <- "#FFA220"
lt_pink <- "#E817A9"
lt_yellow <- "#FFD816"
lt_green <- "#17E849"

# construct a bivariate color matrix
# I want it to run from lt_yellow to lt_blue (4 categories),
# and from low to high intensity (3 categories)

color_matrix <- data.frame("colors" = c("#FFD81680", "#FFD816BF", "#FFD816FF",
                                        "#BFA21080", "#BFA210BF", "#BFA210FF",
                                        "#3C657E80", "#3C657EBF", "#3C657EFF",
                                        "#217FBE80", "#217FBEBF", "#217FBEFF"),
                       "xaxis" = rep(c("A", "B", "C"), 4),
                       "yaxis" = matrix(sapply(seq(1, 4), function(x) rep(x, 3)), ncol = 1)) %>% 
  mutate(cat = paste0(yaxis, xaxis))


map_colors <- setNames(color_matrix$colors, color_matrix$cat)

ggplot(color_matrix, aes(x = xaxis, y = yaxis)) +
  geom_tile(fill = color_matrix$colors, color = "gray50") 

# Fonts:
# font_import()

lt_theme <- function (...) {
  theme(text = element_text(family = "Cabin Medium"),
  plot.title = element_text(size = rel(1.25), face = "bold"),
  plot.subtitle = element_text(family = "Cabin"),
  plot.caption = element_text(family = "Cabin", face = "italic"),
  panel.background = element_rect(fill = NA), 
  panel.grid.major = element_line(color = "gray75", size = rel(0.75),
                                  linetype = "dotted"),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_text(family = "Cabin SemiBold"),
  legend.background = element_blank(),
  legend.key = element_blank()) +
  theme(...)
  
}

lt_map_theme <- function (...) {
  theme(text = element_text(family = "Cabin Medium"),
        plot.title = element_text(size = rel(1.25), face = "bold"),
        plot.subtitle = element_text(family = "Cabin"),
        plot.caption = element_text(family = "Cabin", face = "italic"),
        panel.background = element_rect(fill = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank()) +
    theme(...)
  
}


# ggplot(countypop, aes(x = pop, y = as.numeric(stcofips))) +
#   geom_point() +
#   labs(title = "This graph says something important",
#        subtitle = "Description of the data",
#        x = "X Variable", y = "Y Variable",
#        caption = "Source: authoritative") +
#   lt_theme()


