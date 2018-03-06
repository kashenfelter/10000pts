# 10,000 points project

# Packages ----
library(deldir)
library(gganimate)
library(ggart)
library(patchwork)
library(tidyverse)

# Make reproducible
set.seed(10000)

# Points ----
pts0 <- expand.grid(x = seq(1, 10000, 100), y = seq(1, 10000, 100))

pts10 <- pts0 %>% mutate(x = x + runif(nrow(.), -250, 250),
                        y = y + runif(nrow(.), -250, 250),
                        x = ifelse(x < 0, 0, ifelse(x > 10000, 10000, x)),
                        y = ifelse(y < 0, 0, ifelse(y > 10000, 10000, y)))

temp <- cbind(pts0, pts10 %>% rename(xend = x, yend = y)) %>%
  mutate(x1 = 0.9 * x + (1 - 0.9) * xend, y1 = 0.9 * y + (1 - 0.9) * yend,
         x2 = 0.8 * x + (1 - 0.8) * xend, y2 = 0.8 * y + (1 - 0.8) * yend,
         x3 = 0.7 * x + (1 - 0.7) * xend, y3 = 0.7 * y + (1 - 0.7) * yend,
         x4 = 0.6 * x + (1 - 0.6) * xend, y4 = 0.6 * y + (1 - 0.6) * yend,
         x5 = 0.5 * x + (1 - 0.5) * xend, y5 = 0.5 * y + (1 - 0.5) * yend,
         x6 = 0.4 * x + (1 - 0.4) * xend, y6 = 0.4 * y + (1 - 0.4) * yend,
         x7 = 0.3 * x + (1 - 0.3) * xend, y7 = 0.3 * y + (1 - 0.3) * yend,
         x8 = 0.2 * x + (1 - 0.2) * xend, y8 = 0.2 * y + (1 - 0.2) * yend,
         x9 = 0.1 * x + (1 - 0.1) * xend, y9 = 0.1 * y + (1 - 0.1) * yend)

pts1 <- temp[, c("x1", "y1")] %>% rename(x = x1, y = y1) %>% mutate(frame = 1)
pts2 <- temp[, c("x2", "y2")] %>% rename(x = x2, y = y2) %>% mutate(frame = 2)
pts3 <- temp[, c("x3", "y3")] %>% rename(x = x3, y = y3) %>% mutate(frame = 3)
pts4 <- temp[, c("x4", "y4")] %>% rename(x = x4, y = y4) %>% mutate(frame = 4)
pts5 <- temp[, c("x5", "y5")] %>% rename(x = x5, y = y5) %>% mutate(frame = 5)
pts6 <- temp[, c("x6", "y6")] %>% rename(x = x6, y = y6) %>% mutate(frame = 6)
pts7 <- temp[, c("x7", "y7")] %>% rename(x = x7, y = y7) %>% mutate(frame = 7)
pts8 <- temp[, c("x8", "y8")] %>% rename(x = x8, y = y8) %>% mutate(frame = 8)
pts9 <- temp[, c("x9", "y9")] %>% rename(x = x9, y = y9) %>% mutate(frame = 9)

pts <- rbind(pts0 %>% mutate(frame = 0),
             pts1 %>% mutate(frame = 1),
             pts2 %>% mutate(frame = 2),
             pts3 %>% mutate(frame = 3),
             pts4 %>% mutate(frame = 4),
             pts5 %>% mutate(frame = 5),
             pts6 %>% mutate(frame = 6),
             pts7 %>% mutate(frame = 7),
             pts8 %>% mutate(frame = 8),
             pts9 %>% mutate(frame = 9),
             pts10 %>% mutate(frame = 10))

result0 <- deldir(pts0)
voronoi0 <- result0$dirsgs

result1 <- deldir(pts1)
voronoi1 <- result1$dirsgs

result2 <- deldir(pts2)
voronoi2 <- result2$dirsgs

result3 <- deldir(pts3)
voronoi3 <- result3$dirsgs

result4 <- deldir(pts4)
voronoi4 <- result4$dirsgs

result5 <- deldir(pts5)
voronoi5 <- result5$dirsgs

result6 <- deldir(pts6)
voronoi6 <- result6$dirsgs

result7 <- deldir(pts7)
voronoi7 <- result7$dirsgs

result8 <- deldir(pts8)
voronoi8 <- result8$dirsgs

result9 <- deldir(pts9)
voronoi9 <- result9$dirsgs

result10 <- deldir(pts10)
voronoi10 <- result10$dirsgs

df <- rbind(voronoi0 %>% mutate(frame = 0),
            voronoi1 %>% mutate(frame = 1),
            voronoi2 %>% mutate(frame = 2),
            voronoi3 %>% mutate(frame = 3),
            voronoi4 %>% mutate(frame = 4),
            voronoi5 %>% mutate(frame = 5),
            voronoi6 %>% mutate(frame = 6),
            voronoi7 %>% mutate(frame = 7),
            voronoi8 %>% mutate(frame = 8),
            voronoi9 %>% mutate(frame = 9),
            voronoi10 %>% mutate(frame = 10))

p <- ggplot() +
  geom_point(aes(x, y, frame = frame), pts, size = 0.25) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, frame = frame), df, lineend = "round",
               size = 0.25) +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

animation::ani.options(interval = 1/3)

gganimate(p, "voronoi.gif", title_frame = FALSE, ani.width = 1000, 
          ani.height = 1000)

# Static
p1 <- ggplot() +
  geom_point(aes(x, y), pts0, size = 0.2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), voronoi0, lineend = "round") +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

p2 <- ggplot() +
  geom_point(aes(x, y), pts10, size = 0.2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), voronoi10, lineend = "round") +
  coord_equal() +
  xlim(0, 10000) +
  ylim(0, 10000) +
  theme_blankcanvas(margin_cm = 0)

p <- p1 + p2

ggsave("voronoi.png", p, width = 20, height = 10, units = "in", dpi = 600)
