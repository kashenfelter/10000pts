#' k-d tree
#'
#' Compute a k-d tree for a given set of points
#' @param points A data frame with columns for x and y coordinates, and each point in a row.
#' @keywords k-d tree
#' @export
#' @examples
#' kdtree()

kdtree <- function(points) {
  n <- nrow(points)
  df <- data.frame(xmin = numeric(n), xmax = numeric(n), ymin = numeric(n), ymax = numeric(n),
                   dir = character(n),
                   x    = numeric(n), y    = numeric(n), xend = numeric(n), yend = numeric(n)) %>%
    mutate(dir = as.character(dir))
  
  i <- 1
  k <- 2
  l <- n
  
  df[1, c("xmin", "xmax", "ymin", "ymax")] <- c(0, 10000, 0, 10000)
  df[1, "dir"] <- "v"
  
  while(i < l) {
    if(df$dir[i] == "v") {
      temp <- points %>%
        filter(x > df[i, "xmin"], x < df[i, "xmax"], y > df[i, "ymin"], y < df[i, "ymax"]) %>%
        summarise(x = median(x))
      df[i, c("x", "xend", "y", "yend")] <- c(temp, temp, df$ymin[i], df$ymax[i])
      df[k, ] <- df[i, ]
      df[k+1, ] <- df[i, ]
      df$xmax[k] <- temp
      df$xmin[k+1] <- temp
      df$dir[k] <- "h"
      df$dir[k+1] <- "h"
      k <- k + 2
      i <- i + 1
      print(i)
    } else {
      temp <- points %>%
        filter(x > df[i, "xmin"], x < df[i, "xmax"], y > df[i, "ymin"], y < df[i, "ymax"]) %>%
        summarise(y = median(y))
      df[i, c("x", "xend", "y", "yend")] <- c(df$xmin[i], df$xmax[i], temp, temp)
      df[k, ] <- df[i, ]
      df[k+1, ] <- df[i, ]
      df$ymax[k] <- temp
      df$ymin[k+1] <- temp
      df$dir[k] <- "v"
      df$dir[k+1] <- "v"
      k <- k + 2
      i <- i + 1
      print(i)
    }
  }
  df
}
