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




