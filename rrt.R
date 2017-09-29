# Rapidly-exploring random tree
# https://en.wikipedia.org/wiki/Rapidly-exploring_random_tree

# Load packages
library(gganimate)
library(ggart)
library(steiner)
library(tidyverse)
library(viridis)

# Make reproducible
set.seed(10000)

# Set parameters
n <- 1000000 # Number of iterations
X <- 10000 # Square canvas dimension
delta <- 2

# Create data frame
points <- data.frame(x = numeric(n), y = numeric(n))
points[1, ] <- runif(2, 0, X)
edges <- data.frame(x = numeric(n), y = numeric(n), xend = numeric(n), yend = numeric(n))
edges[1, ] <- c(as.numeric(points[1, ]), as.numeric(points[1, ]))

# Main loop
i <- 2
while(i <= n) {
  valid <- FALSE
  while(!valid) {
    # Sample a random point
    rp <- runif(2, 0, X)
    # Find the nearest neighbour to rp
    temp <- points[1:(i-1), ] %>%
      mutate(dist = sqrt((rp[1] - x)^2 + (rp[2] - y)^2)) %>%
      arrange(dist)
    np <- as.numeric(temp[1, c("x", "y")])
    # Limit the maximum edge length
    if(temp$dist[1] > delta) {
      rp2 <- np + (rp - np) / temp$dist[1] * delta
      rp <- rp2
    }
    # Check if the line segment between rp and np intersects an existing edge
    temp2 <- edges[1:(i-1), ] %>%
      mutate(intersects = does_intersect(rp, np, c(x, y), c(xend, yend)))
    if(sum(temp2$intersects) <= 0) {
      points[i, ] <- rp
      edges[i, ] <- c(np, rp)
      valid <- TRUE
    }
  }
  i <- i + 1
  print(i)
}

edges2 <- edges[1:50000, ] %>% mutate(id = 1:nrow(.), frame = floor(id / (50000 / 100)))

# Create plot
p <- ggplot(mapping = aes(frame = frame, cumulative = FALSE)) +
  geom_segment(aes(x, y, xend = xend, yend = yend),
             edges2, lineend = "round", size = 2.5, colour = "white") +
  #xlim(0, 10000) +
  #ylim(0, 10000) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0, bg_col = "black")
  #theme_void() + theme(legend.position = "none")

# Save plot
#ggsave(paste("rrt_delta___", delta, ".png", sep = ""), p, width = 20, height = 20, units = "cm", dpi = 300)
ggsave("rrt001.png", p, width = 20, height = 20, units = "cm", dpi = 300)


animation::ani.options(interval = 1/30)

gganimate(p, "rrt001.gif", title_frame = FALSE, ani.width = 750, 
           ani.height = 750)