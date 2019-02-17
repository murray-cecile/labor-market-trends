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

map_legend <- ggplot(color_matrix, aes(x = xaxis, y = yaxis)) +
  geom_tile(fill = color_matrix$colors, color = "gray50") +
  scale_x_discrete(labels = c("Under 100,000", "100,000 to 1,000,000", "1M or more")) +
  scale_y_continuous(labels = c("0", "1-19", "20-39", "40+"),
                     breaks = seq(1, 4)) +
  labs(title = "Quarters of high unemployment by population", 
       x = "Population", y = "Quarters of high unemployment") +
  lt_theme(panel.grid.major = element_blank(),
           axis.text = element_text(size = rel(1.1)))

library(gridExtra)
