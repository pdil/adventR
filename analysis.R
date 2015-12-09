
library(ggplot2)
library(grid)

df <- data.frame(
  day = 1:9,
  gold = c(15765, 11531, 9303, 8139, 6478, 5438, 3316, 2931, 1938)
)

glm_fit <- glm(gold ~ day, data = df, family = poisson(link = "log"))
glm_fit

coefs <- coef(glm_fit)

new_df <- data.frame(
  day = 1:25,
  gold = exp(coefs[[1]] + coefs[[2]] * (1:25))
)

new_df$gold[25]

ggplot() + 
  geom_line(data = df, aes(x = day, y = gold)) + 
  geom_point(data = df, aes(x = day, y = gold)) + 
  geom_line(data = new_df, aes(x = day, y = gold), colour = "red") +
  geom_point(data = new_df, aes(x = day, y = gold), colour = "red") +
  geom_segment(aes(x = 24.5, y = 3000, xend = 24.9, yend = new_df$gold[25] * 10), arrow = arrow(length = unit(0.5, "cm")),
               colour = "black", size = 1) +
  labs(x = "Day", y = "Number Completed", title = "Advent of Code Stats") +
  annotate(geom = "text", x = 21.5, y = 3500, label = paste0("Day 25: ~", round(new_df$gold[25], 0)), hjust = 0) +
  theme(panel.border = element_rect(fill = NA, colour = "black"))

ggsave("glm.png", width = 8, height = 7)
