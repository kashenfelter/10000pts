#' Convex hull
#'
#' Plot the convex hull for the 10,000 points data set
#' @param points The 10,000 points data set
#' @export
#' @examples
#' convex hull()

convex_hull <- function(points) {
  # Compute convex hull
  hull <- points[chull(points), ]
  
  
}
