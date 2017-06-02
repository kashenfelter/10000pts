# 10,000 points project

# Packages ----
library(ape)
library(cccd)
library(deldir)
library(ggart)
library(pts10000)
library(SyNet)
library(tidyverse)
library(TSP)

# Make reproducible
set.seed(10000)

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

# TSP (exact)
# etsp <- ETSP(points)
# tour <- solve_TSP(etsp)
# tour
# tour_length(tour)
# plot(etsp, tour)
# etsp

# k-means ----
temp <- kmeans(points, 1000)
centers <- as.data.frame(temp[["centers"]]) %>% mutate(centreID = 1:nrow(.))
temp2 <- as.data.frame(temp[["cluster"]])
names(temp2) <- c("centreID")
result <- points %>% cbind(temp2) %>% left_join(centers, by = "centreID") %>%
  rename(x = x.x, y = y.x, xend = x.y, yend = y.y)
p6 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
ggsave("plots/006-kmeans.png", p6, width = 20, height = 20, units = "in")

# Minimum spanning tree ----
result <- read_delim("pts10000/data/mst.qs", delim = " ", col_names = FALSE, skip = 10001) %>%
  rename(p1 = X1, p2 = X2) %>%
  select(p1, p2) %>%
  left_join(points %>% mutate(id = seq(0, nrow(.) - 1)), by = c("p1" = "id")) %>%
  left_join(points %>% mutate(id = seq(0, nrow(.) - 1)) %>% rename(xend = x, yend = y), by = c("p2" = "id")) %>%
  select(x, y, xend, yend)
p7 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
ggsave("plots/007-mst.png", p7, width = 20, height = 20, units = "in")

 # Quadrand nearest neighbour ----
result <- read_delim("pts10000/data/qnn.qs", delim = " ", col_names = FALSE, skip = 10001) %>%
  rename(p1 = X1, p2 = X2) %>%
  select(p1, p2) %>%
  left_join(points %>% mutate(id = seq(0, nrow(.) - 1)), by = c("p1" = "id")) %>%
  left_join(points %>% mutate(id = seq(0, nrow(.) - 1)) %>% rename(xend = x, yend = y), by = c("p2" = "id")) %>%
  select(x, y, xend, yend)
p8 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
ggsave("plots/008-qnn.png", p8, width = 20, height = 20, units = "in")

# # Relative neighbourhood graph ----
# result <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))
# for(i in seq(1, nrow(points) - 1)) {
#   for(j in seq(i + 1, nrow(points))) {
#     d <- sqrt(sum((points[i, ] - points[j, ])^2))
#     temp <- points %>%
#       filter(row.names(.) != i & row.names(.) != j) %>%
#       mutate(di = sqrt((x - points$x[i])^2 + (y - points$y[i])^2),
#              dj = sqrt((x - points$x[j])^2 + (y - points$y[j])^2))
#     dimin <- min(temp$di)
#     djmin <- min(temp$dj)
#     if(d < dimin & d < djmin) {
#       result <- rbind(result, data.frame(x = points$x[i], y = points$y[i], xend = points$x[j], yend = points$y[j]))
#     }
#     print(j)
#   }
#   print(i)
# }
# p6 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
# 
# 
# 
# x <- matrix(runif(100),ncol=2)
# g <- rng(test)
# 

# Gilbert model ----
# Connect two points if the distance is less than a threshold
# result <- data.frame(x = rep(points$x, times = 10000))
# 
# p7 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
# ggsave("plots/007-gilbert.png", p7, width = 20, height = 20, units = "in")
