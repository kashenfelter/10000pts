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
p2 <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), delaunay)
ggsave("plots/002-delaunay.png", p2, width = 20, height = 20, units = "in")

# Voronoi ----
result <- deldir(points)
voronoi <- result$dirsgs
p3 <- p + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), voronoi)
ggsave("plots/003-voronoi.png", p3, width = 20, height = 20, units = "in")
