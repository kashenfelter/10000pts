# 10,000 points project

# Packages ----
library(deldir)
library(ggart)
library(pts10000)
library(tidyverse)

# Points ----
points <- pts10000::points
p <- ggplot() +
  geom_point(aes(x, y), points, size = 1) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)
ggsave("plots/001-points.png", p, width = 20, height = 20, units = "in")

# Delaunay ----
result <- deldir(points)
delaunay <- result$delsgs
p2 <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), delaunay, lineend = "round")
ggsave("plots/002-delaunay.png", p2, width = 20, height = 20, units = "in")

# Voronoi ----
result <- deldir(points)
voronoi <- result$dirsgs
p3 <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), voronoi, lineend = "round")
ggsave("plots/003-voronoi.png", p3, width = 20, height = 20, units = "in")

# Nearest neigbour ----
find_nearest <- function(points, id, n) {
  xi <- points$x[id]
  yi <- points$y[id]
  temp <- points %>% mutate(dist = sqrt((x - xi)^2 + (y - yi)^2)) %>% arrange(dist)
  result <- temp[seq(2, n + 1), ] %>% mutate(xend = xi, yend = yi)
}

result <- 1:10000 %>%
  map_df(~find_nearest(points, ., 8), .id = "id")

p4 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
ggsave("plots/004-08-nn.png", p4, width = 20, height = 20, units = "in")

# Travelling salesman (Lin-Kernighan heuristic) ----
result <- read.table("pts10000/data/lin-kernighan-union.cyc") %>%
  mutate(id = V1 + 1) %>%
  select(id) %>%
  left_join(points %>% mutate(id = 1:nrow(points)) %>% select(id, x, y), by = "id") %>%
  mutate(xend = lead(x, default = points$x[1]), yend = lead(y, default = points$y[1]))
p5 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, size = 1, lineend = "round")
ggsave("plots/005-tsp.png", p5, width = 20, height = 20, units = "in")
