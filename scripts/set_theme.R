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


# Colors: 
lt_blue <- "#2666ff"
lt_orange <- "#FFA220"
lt_pink <- "#E817A9"
lt_yellow <- "#FFD816"
lt_green <- "#17E849"

lt_theme <- theme(panel.background = element_rect(fill = "#FFFFFF"), 
                  panel.grid.major = element_line(color = "gray60"))
