library(ggart)
library(tidyverse)

set.seed(10000)

n <- 10000
d_max <- 50

df <- data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n))

df[1, c("x", "y", "xend", "yend")] <- runif(4, 0, 10000)

for(i in 1:(n-1)) {
  pt <- runif(2, 0, 10000)
  temp <- df[1:i, ] %>%
    rowwise() %>%
    mutate(d = sqrt((x - pt[1])^2 + (y - pt[2])^2)) %>%
    arrange(d)
  df[i+1, c("x", "y", "xend", "yend")] <- c(pt[1], pt[2], temp$x[1], temp$y[1])
  print(i)
}

p <- ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend), df) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)
p

ggsave("plots/030-rrt.png", p, width = 20, height = 20, units = "in")
