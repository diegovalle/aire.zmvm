#' Inverse Distance Weighting with Directional Data
#'
#' function for inverse distance weighted interpolation with directional data. Useful for when you
#' are working with data whose unit of measurement is degrees (i.e. the average of 35 degrees and
#' 355 degrees should be 15 degrees). It works by finding the shortest distance between two degree
#' marks on a circle.
#'
#' @param values the dependent variable
#' @param coords the spatial data locations where the values were measured. First column x/longitud,
#' second y/latitude
#' @param grid data frame or Spatial object with the locations to predict. First column x/longitud,
#' second y/latitude
#' @param idp The inverse distance weighting power
#'
#' @importFrom sp spDists
#'
#' @return data.frame with the interpolated values for each of the grid points
#' @export
#' @examples
#' library(sp)
#' # Location of sensors. First column x/longitud, second y/latitude
#' locations <- data.frame(lon = c(1, 2), lat = c(1, 2))
#' coordinates(locations) <- ~lon+lat
#' # Could be wind direction values in degrees
#' values <- c(55, 355)
#' # The grid for which to extrapolate values
#' grid <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 2, 2, 1))
#' coordinates(grid) <- ~lon+lat

#' idw360(values, locations, grid)
#'
idw360 <- function(values, coords, grid, idp = 2) {
  stopifnot(length(values) == nrow(coords))
  stopifnot(is.numeric(idp))
  distance <- t(spDists(coords, grid))
  w <- 1 / (distance ^ idp)

  for (i in 1:nrow(w)) {
    if (sum(is.infinite(w[i, ])) > 0){
      w[i, !is.infinite(w[i, ])] <- 0
      w[i, is.infinite(w[i, ])] <- 1
    }
  }

  y <- (sin(values * (pi / 180)))
  w.sum <- apply(w, 1, sum, na.rm = TRUE)
  wy <- w %*% diag(y)
  uy <- apply(wy / w.sum, 1, sum, na.rm = TRUE)

  x <- (cos(values * (pi / 180)))
  w.sum <- apply(w, 1, sum, na.rm = TRUE)
  wx <- w %*% diag(x)
  ux <- apply(wx / w.sum, 1, sum, na.rm = TRUE)

  res <- atan2(uy, ux) * (180 / pi)
  res <- ifelse(res < 0, 360 + res, res)

  data.frame(pred = res)
}
