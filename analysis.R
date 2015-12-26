
library(ggplot2)
library(MASS)
library(grid)

df <- data.frame(
  day = 1:14,
  #gold = c(15791, 11556, 9325, 8159, 6500, 5453, 3327, 2952)
  #gold = c(17413, 12879, 10477, 9181, 7440, 6358, 4072, 3724, 3044, 3247, 2260)
  gold = c(19418, 14513, 11917, 10502, 8629, 7478, 4889, 4556, 3875, 4164, 3444, 2925, 2598, 2529)
)

glm_nb <- glm.nb(gold ~ day, data = df)

coefs <- coef(glm_nb)

new_df <- data.frame(
  day = 1:25,
  gold = predict(glm_nb, newdata = data.frame(day = 1:25), type = "response")
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
