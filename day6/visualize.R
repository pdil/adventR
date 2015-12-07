
# >> run 2-brightness.R script first <<

library(tidyr)
library(ggplot2)
library(grid)

df_lights <- data.frame(x = as.vector(row(lights)), y = as.vector(col(lights)), brightness = as.vector(lights))

colorFunc <- colorRampPalette(c("black", "red", "orange", "yellow", "white"), space = "rgb")

ggplot(data = df_lights) + 
  geom_tile(aes(x = x, y = y, fill = brightness)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = colorFunc(10), guide = FALSE) +
  theme(panel.background = element_rect(fill = "black", colour = NA),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.grid = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))

ggsave("heatmap.png", height = 7, width = 7)
