
# >> run 2-santa-robo.R script first <<

library(animation)
library(ggplot2)
library(scales)
library(grid)

combined$group <- factor(c(rep("Santa", length(santa_coords$x)), rep("Robo-Santa", length(robo_coords$x))))

ggplot(data = combined) + 
  geom_point(aes(x = x, y = y, colour = group), 
             alpha = 1/3, size = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values = c("#00BFC4", "#F8766D")) +
  guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
  theme(panel.background = element_rect(fill = "black", colour = NA),
        plot.background = element_rect(fill = "black", colour = NA),
        panel.grid = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1, 0),
        legend.title = element_blank(), legend.background = element_rect(fill = alpha("black", 0)),
        legend.key = element_blank(), legend.text = element_text(colour = "white", size = 12),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))

ggsave("paths.png", height = 7, width = 7)


plot_iteration <- function(i) {
  .e <- environment()
  
  df_santa <- santa_coords[1:i, ]
  df_robo <- robo_coords[1:i, ]
  df <- rbind(df_santa, df_robo)
  df$group <- factor(c(rep("Santa", length(df_santa$x)), rep("Robo-Santa", length(df_robo$x))))
  
  santa_x <- tail(df_santa, 1)$x
  santa_y <- tail(df_santa, 1)$y
  
  robo_x <- tail(df_robo, 1)$x
  robo_y <- tail(df_robo, 1)$y
  
  print(ggplot(data = df, environment = .e) + 
    geom_point(aes(x = x, y = y, colour = group), 
               alpha = 1/3, size = 1) +
    scale_colour_manual(values = c("#00BFC4", "#F8766D")) +
    annotate("text", x = -20, y = 40, colour = "white", size = 4, hjust = 0,
             label = paste0("Santa (", santa_x, ", ", santa_y, ")\nRobo-Santa (", robo_x, ", ", robo_y, ")")) +
    guides(colour = guide_legend(override.aes = list(size = 5, alpha = 1))) +
    scale_x_continuous(limits = c(-20, 110)) + scale_y_continuous(limits = c(-100, 40)) +
    theme(panel.background = element_rect(fill = "black"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          legend.position = c(1, 0), legend.justification = c(1, 0),
          legend.title = element_blank(), legend.background = element_rect(fill = alpha("black", 0)),
          legend.key = element_blank(), legend.text = element_text(colour = "white", size = 12)))
}

i <- 1:(length(combined$x) / 2)

#suppressMessages(
#  saveGIF({lapply(i, plot_iteration)}, interval = 0.01, ani.height = 720, ani.width = 720)
#)

suppressMessages(
  saveVideo({lapply(i, plot_iteration)}, interval = 0.007, 
            other.opts = "-pix_fmt yuv420p -r 30 -s:v 720x720 -profile:v high -c:v libx264")
)
