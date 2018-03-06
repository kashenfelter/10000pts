# 10,000 points project

# Packages ----
library(ape)
library(cccd)
library(clue)
library(deldir)
library(gganimate)
library(ggforce)
library(ggalt)
library(ggart)
library(packcircles)
library(patchwork)
library(polyclip)
library(pts10000)
library(reshape2)
library(sp)
library(steiner)
library(SyNet)
library(tidyverse)
library(TSP)
library(tweenr)
library(viridis)

# Make reproducible
set.seed(10000)

# Points ----
points <- pts10000::points

# Lattice
points0 <- expand.grid(x = seq(1, 10000, 100), y = seq(1, 10000, 100))

p <- ggplot() +
  geom_point(aes(x, y), points, size = 1.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

p0 <- ggplot() +
  geom_point(aes(x, y), points0, size = 1.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

ggsave("plots/001-points0.png", p0, width = 20, height = 20, units = "in")

# Delaunay ----
result <- deldir(points)
delaunay <- result$delsgs
p2 <- ggplot() +
  #geom_point(aes(x, y), points, size = 0.25) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), delaunay, lineend = "round",
                 size = 0.5) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_void()
ggsave("plots/002-delaunay-2.png", p2, width = 24, height = 24, units = "cm", dpi = 600)

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

# 2D kernel density estimation ----
p9 <- ggplot() +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  stat_density_2d(aes(x, y), points, n = 350, h = c(105, 105), colour = "black")
ggsave("plots/009-density.png", p9, width = 20, height = 20, units = "in")

# k-d tree
result <- kdtree(points)
p10 <- ggplot() +
  #geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  geom_segment(aes(x, y, xend = xend, yend = yend), result)
ggsave("plots/010-kdtree.png", p10, width = 20, height = 20, units = "in")

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

# Bipartite matching
x <- matrix(c(5, 1, 4, 3, 5, 2, 2, 4, 4), nrow = 3)
test <- solve_LSAP(x)
str(test)
test
test[4]

# Graham's scan ----

# Function for determining if v2 makes a right turn w.r.t. v1
right_turn <- function(v1, v2){
  angle <- atan2(v2[2], v2[1]) - atan2(v1[2], v1[1])
  if(angle < 0) {
    TRUE
  } else {
    FALSE
  }
}

n <- nrow(points)
sorted <- points %>% arrange(x)
i <- 1
j <- 2
k <- 3
l <- 1 # index into df
df <- data.frame(x = numeric(n*10), y = numeric(n*10), xend = numeric(n*10), yend = numeric(n*10),
                 id1 = integer(n*10), id2 = integer(n*10), hull = logical(n*10))
df[1, ] <- c(sorted$x[1], sorted$y[1], sorted$x[2], sorted$y[2], 1, 2, TRUE)
r <- 2
while(k <= 100) {
  v1 <- c(sorted$x[j] - sorted$x[i], sorted$y[j] - sorted$y[i])
  v2 <- c(sorted$x[k] - sorted$x[j], sorted$y[k] - sorted$y[j])
  if(right_turn(v1, v2) | !df$hull[l]) {
    df[r, ] <- c(sorted$x[i], sorted$y[i], sorted$x[j], sorted$y[j], i, j, TRUE)
    r <- r + 1
    i <- i + 1
    j <- j + 1
    k <- k + 1
    l <- r - 1
  } else {
    df$hull[l] <- FALSE
    l <- l - 1
    i <- df$id1[l]
    j <- df$id2[l]
  }
  print(paste(i, j, k), sep = ", ")
}

df <- df %>% filter(id1 != 0)

p11 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), df, lineend = "round", alpha = 0.1)
ggsave("plots/011-graham.png", p11, width = 20, height = 20, units = "in")

# Circles ----
df <- points %>% mutate(r = 10)
for(i in 1:10000) {
  xi <- points$x[i]
  yi <- points$y[i]
  temp <- df %>% mutate(dist = sqrt((x - xi)^2 + (y - yi)^2) - r) %>% arrange(dist)
  df$r[i] <- temp$dist[2]
  print(i)
}

p12 <- ggplot() +
  coord_equal() +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 10000)) +
  theme_blankcanvas(margin_cm = 0) +
  geom_circle(aes(x0 = x, y0 = y, r = r), df, n = 720, size = 0.6)
ggsave("plots/012-circles.png", p12, width = 20, height = 20, units = "in", dpi = 720)

# Pack circles ----
r <- 50
df <- points %>% mutate(size = r)
test <- circleRepelLayout(df, xlim = 10000, ylim = 10000, sizetype = "radius")
df2 <- test$layout

p13 <- ggplot() +
  coord_equal() +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 10000)) +
  theme_blankcanvas(margin_cm = 0) +
  geom_circle(aes(x0 = x, y0 = y, r = radius), df2, n = 720, size = 0.6)
ggsave("plots/013-packcircles.png", p13, width = 20, height = 20, units = "in", dpi = 720)

# Gilbert model ----
points_id <- points %>% mutate(id = 1:nrow(.))
df <- melt(as.matrix(dist(points)), varnames = c("v1", "v2"))
dmax <- 0.015 * 10000
df2 <- df %>% filter(value > 0, value < dmax) %>%
  left_join(points_id, by = c("v2" = "id")) %>% rename(xend = x, yend = y) %>%
  left_join(points_id, by = c("v1" = "id"))
p14 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), df2, lineend = "round")
ggsave("plots/014-gilbert-0015.png", p14, width = 20, height = 20, units = "in")

# Vector field ----
l <- 100
df <- points %>% mutate(vx = ((2*y - x) / sqrt(x^2+y^2)), vy = sin((0.5*(x)/ sqrt(x^2+y^2))),
                        r = sqrt(vx^2 + vy^2), xend = x + vx / r * l, yend = y + vy / r * l)
p15 <- ggplot() +
  coord_equal() +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 10000)) +
  theme_blankcanvas(margin_cm = 0) +
  geom_segment(aes(x, y, xend = xend, yend = yend), df, lineend = "round", arrow = arrow(length = unit(0.2, "cm")), size = 0.35)
ggsave("plots/015-vectorfield.png", p15, width = 20, height = 20, units = "in")

# Squares ----
n <- nrow(points)
min_width <- 25
min_height <- 25
max_width <- 150
max_height <- 150
df <- points %>%
  mutate(id = 1:n, width = runif(n, min_width, max_width), height = runif(n, min_height, max_height),
         x1 = x - width / 2, y1 = y - height / 2,
         x2 = x - width / 2, y2 = y + height / 2,
         x3 = x + width / 2, y3 = y + height / 2,
         x4 = x + width / 2, y4 = y - height / 2)
df2 <- (df %>% select(id, x1, y1) %>% rename(x = x1, y = y1)) %>%
  rbind(df %>% select(id, x2, y2) %>% rename(x = x2, y = y2)) %>%
  rbind(df %>% select(id, x3, y3) %>% rename(x = x3, y = y3)) %>%
  rbind(df %>% select(id, x4, y4) %>% rename(x = x4, y = y4))

p16 <- ggplot() +
  coord_equal() +
  coord_cartesian(xlim = c(0, 10000), ylim = c(0, 10000)) +
  theme_blankcanvas(margin_cm = 0) +
  geom_polygon(aes(x, y, group = id), df2, colour = "black", fill = "black", alpha = 0.15)
ggsave("plots/016-squares.png", p16, width = 20, height = 20, units = "in")

# Minimal directed spanning tree
# Reference: http://www.maths.dur.ac.uk/users/andrew.wade/research/graphs.html#mdst
find_nearest <- function(points, id) {
  xi <- points$x[id]
  yi <- points$y[id]
  temp <- points %>% mutate(dist = sqrt((x - xi)^2 + (y - yi)^2)) %>% filter(x < xi, y < yi) %>% arrange(dist)
  result <- temp[1, ] %>% mutate(xend = xi, yend = yi)
}

result <- 1:10000 %>%
  map_df(~find_nearest(points, .), .id = "id")
p17 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), result, lineend = "round")
ggsave("plots/017-directspan.png", p17, width = 20, height = 20, units = "in")

# On-line nearest-neighbour graph ----
# Reference: http://www.maths.dur.ac.uk/users/andrew.wade/research/graphs.html#mdst
df <- points %>% mutate(xend = NA, yend = NA, id = 1:nrow(.))

for(i in 2:nrow(points)) {
  xi <- points$x[i]
  yi <- points$y[i]
  temp <- df %>%
    filter(i < id) %>%
    mutate(dist = sqrt((xi - x)^2 + (yi - y)^2)) %>%
    arrange(dist)
  df[i, c("xend", "yend")] <- c(temp$x[1], temp$y[1])
  print(i)
}

p18 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), df, lineend = "round")
ggsave("plots/018-online.png", p18, width = 20, height = 20, units = "in")

# Quadtree ----
df <- quadtree(points)
test <- df %>% group_by(id) %>% summarise(xmin = min(x), xmax = max(x)) %>% mutate(delta = xmax - xmin)
test2 <- df %>% left_join(test %>% select(id, delta), by = "id") %>% filter(delta > 10)
p19 <- ggplot() +
  #geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  geom_polygon(aes(x, y, group = id), test2, colour = "black", fill = "transparent", size = 0.5)
ggsave("plots/019a-quadtree.png", p19, width = 20, height = 20, units = "in")

df2 <- test2 %>% mutate(frame = ceiling(id / (nrow(.) / 2000)))

p19 <- ggplot() +
  #geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  geom_polygon(aes(x, y, group = id, frame = frame, cumulative = TRUE), df2, colour = "black", fill = "transparent", size = 0.4)

animation::ani.options(interval = 1/15/4)
gganimate(p19, "gifs/019-quadtree.gif", title_frame = FALSE, ani.width = 740, ani.height = 740)

# Weiszfeld ----
set.seed(10000)
terminals <- data.frame(x = runif(10, 0, 10000), y = runif(10, 0, 10000))
df <- 1:10000 %>%
  map_df(~weiszfeld(terminals, c(points$x[.], points$y[.])), .id = "id")

p20 <- ggplot() +
  geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  geom_point(aes(x, y), terminals, size = 5, alpha = 1) +
  geom_line(aes(x, y, group = id), df, colour = "black", size = 0.5, alpha = 0.03) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)
ggsave("plots/020-weiszfeld.png", p20, width = 20, height = 20, units = "in")

# k-d tree (remix) ----
result <- kdtree(points[1:499, ], minmax = TRUE)
p21 <- ggplot() +
  #geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  geom_segment(aes(x, y, xend = xend, yend = yend), result)
ggsave("plots/021-kdtree.png", p21, width = 20, height = 20, units = "in")

# Hexagonal heatmap of 2d bin counts ----
p22 <- ggplot() +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0) +
  stat_bin_hex(aes(x, y), points, bins = 75, colour = "white") +
  #geom_point(aes(x, y), points, size = 1, alpha = 0.25) +
  scale_fill_gradient(low = "black", high = "black")
ggsave("plots/022-hexbin.png", p22, width = 20, height = 20, units = "in")

# Hexagonal heatmap of 2d bin counts ----
points2 <- points %>% mutate(id = 1:nrow(.), used = FALSE)
df <- data.frame(x = numeric(10000), y = numeric(10000), xend = numeric(10000), yend = numeric(10000))
df[1, c("x", "y")] <- points[1, ]

for(i in 1:9999) {
  xi <- points2$x[i]
  yi <- points2$y[i]
  temp <- points2 %>%
    filter(!used) %>%
    mutate(dist = sqrt((x - xi)^2 + (y - yi)^2)) %>%
    filter(dist < 250)
  if(nrow(temp) == 0) next
  chosen <- sample_n(temp, 1)
  df[i, ] <- c(xi, yi, chosen$x[1], chosen$y[1])
  points2[points2$id == chosen$id[1], "used"] <- TRUE
  print(i)
}

df <- df %>% filter(x > 0)

p23 <- p + geom_segment(aes(x, y, xend = xend, yend = yend), df)

ggsave("plots/023-randwalk-4.png", p23, width = 20, height = 20, units = "in")

# Convex hulls ----
nhulls <- 1000
points2 <- points %>% mutate(id = 1:nrow(.))
span <- 500
df <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))

for(i in 1:nhulls) {
  pt <- sample_n(points2, 1)
  pt_id <- pt$id[1]
  pt_x <- pt$x[1]
  pt_y <- pt$y[1]
  temp <- points %>%
    mutate(dist = sqrt((pt_x - x)^2 + (pt_y - y)^2)) %>%
    filter(dist < span) %>%
    sample_n(min(10, nrow(.))) %>%
    select(x, y) %>%
    chull_edges() %>%
    mutate(id = i)
  df <- df %>% rbind(temp)
  print(i)
}

#p24 <- p0 + geom_segment(aes(x, y, xend = xend, yend = yend), df, alpha = 1)
p24 <- p0 + geom_polygon(aes(x, y, group = id), df, alpha = 0.1, colour = "black")
ggsave("plots/024-convexhulls.png", p24, width = 20, height = 20, units = "in")

# Complete graph ----
set.seed(10000)
directions <- runif(8, 0, pi)
n <- 2000
span <- 500
df <- data.frame(x = numeric(0), y = numeric(0), id = integer(0))
for(i in 1:n) {
  pt <- sample_n(points, 1)
  pt_id <- pt$id[1]
  pt_x <- pt$x[1]
  pt_y <- pt$y[1]
  temp <- points %>%
    mutate(dist = sqrt((pt_x - x)^2 + (pt_y - y)^2)) %>%
    filter(dist < span) %>%
    sample_n(1)
  p1 <- round(c(pt$x[1], pt$y[1]), 0)
  p2 <- round(c(temp$x[1], temp$y[1]), 0)
  temp2 <- compute_edge_parallelogram(p1, p2, directions) %>% mutate(id = as.integer(id)) %>% mutate(id2 = i)
  df <- df %>% rbind(temp2)
}

p25 <- p0 + geom_polygon(aes(x, y, group = id2), df, color = "black", fill = "black", alpha = 0.1)
ggsave("plots/025-edgeparallel.png", p25, width = 20, height = 20, units = "in")

# k-means-regions ----
temp <- kmeans(points, 1000)
centers <- as.data.frame(temp[["centers"]]) %>% mutate(centreID = 1:nrow(.))
temp2 <- as.data.frame(temp[["cluster"]])
names(temp2) <- c("centreID")
result <- points %>% cbind(temp2) %>% left_join(centers, by = "centreID") %>%
  rename(x = x.x, y = y.x, xend = x.y, yend = y.y)
edges <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), group = integer(0))
for(i in 1:max(result$centreID)) {
  temp <- chull_edges(result %>% filter(centreID == i) %>% select(x, y)) %>% mutate(group = i)
  edges <- edges %>% rbind(temp)
}
p26 <- p0 + geom_polygon(aes(x, y, group = group, fill = group), edges, colour = "black")+ scale_fill_gradient(low = "white", high = "black")
  geom_segment(aes(x, y, xend = xend, yend = yend), edges, lineend = "round")
ggsave("plots/026-kmeans-regions.png", p26, width = 20, height = 20, units = "in")

# Interpolate ----
get_quad <- function(pt, vertices) {
  valid <- FALSE
  while(!valid) {
    df1 <- points %>%
      mutate(dist = sqrt((x - pt$x[1])^2 + (y - pt$y[1])^2)) %>%
      filter(dist < 1500) %>%
      sample_n(vertices) %>%
      select(x, y) %>%
      chull_edges()
    
    if(nrow(df1) == vertices) {
      valid <- TRUE
    }
  }
  
  eps <- 250
  
  df2 <- data.frame(x = c(min(df1$x) + runif(1, -eps, eps),
                          min(df1$x) + runif(1, -eps, eps),
                          max(df1$x) + runif(1, -eps, eps),
                          max(df1$x) + runif(1, -eps, eps)),
                    y = c(max(df1$y) + runif(1, -eps, eps),
                          min(df1$y) + runif(1, -eps, eps),
                          min(df1$y) + runif(1, -eps, eps),
                          max(df1$y) + runif(1, -eps, eps))) %>%
    chull_edges()
  
  df <- list(df1, df2)
  
  tf <- tween_states(df, tweenlength = 1, statelength = 0,
                     ease = "exponential-in", nframes = 1000)
  
  tf
}

result <- 1:100 %>%
  map_df(~get_quad(points %>% sample_n(1), 4), .id = "id")

p27 <- p0 +
  geom_segment(aes(x, y, xend = xend, yend = yend, colour = .frame), result, alpha = 0.05,
               size = 0.25, lineend = "round") +
  scale_color_gradient(low = "black", high = "black")

ggsave("plots/027-interpolate.png", p27, width = 20, height = 20, units = "in")

result <- 1:50 %>%
  map_df(~get_quad(points %>% sample_n(1), 4), .id = "id")

p28 <- p0 +
  geom_segment(aes(x, y, xend = xend, yend = yend, colour = .frame), result, alpha = 0.05,
               size = 0.25, lineend = "round") +
  scale_color_gradient(low = "black", high = "black")

ggsave("plots/028-interpolate.png", p28, width = 20, height = 20, units = "in")
