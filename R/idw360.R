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
#' library("sp")
#' library("ggplot2")
#'
#' ## Could be wind direction values in degrees
#' values <- c(55, 355)
#'
#' ## Location of sensors. First column x/longitud, second y/latitude
#' locations <- data.frame(lon = c(1, 2), lat = c(1, 2))
#' coordinates(locations) <- ~lon+lat
#'
#' ## The grid for which to extrapolate values
#' grid <- data.frame(lon = c(1, 2, 1, 2), lat = c(1, 2, 2, 1))
#' coordinates(grid) <- ~lon+lat
#'
#' ## Perform the inverse distance weighted interpolation
#' res <- idw360(values, locations, grid)
#' head(res)
#'
#' df <- cbind(res, as.data.frame(grid))
#' ## The wind direction compass starts where the 90 degree mark is located
#' ggplot(df, aes(lon, lat)) +
#'   geom_point() +
#'   geom_spoke(aes(angle = ((90 - pred) %% 360) * pi / 180),
#'              radius = 1,
#'              arrow=arrow(length = unit(0.2,"cm")))
#' \dontrun{
#' library("mapproj")
#' ## Random values in each of the measuring stations
#' locations <- stations[, c("lon", "lat")]
#' coordinates(locations) <- ~lon+lat
#' proj4string(locations) <- CRS("+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0")
#' values <- runif(length(locations), 0, 360)
#' pixels <- 10
#' grid <- expand.grid(lon = seq((min(coordinates(locations)[, 1]) - .1),
#'                               (max(coordinates(locations)[, 1]) + .1),
#'                               length.out = pixels),
#'                     lat = seq((min(coordinates(locations)[, 2]) - .1),
#'                               (max(coordinates(locations)[, 2]) + .1),
#'                               length.out = pixels))
#' grid <- SpatialPoints(grid)
#' proj4string(grid) <- CRS("+proj=longlat +ellps=WGS84 +no_defs +towgs84=0,0,0")
#' ## bind the extrapolated values for plotting
#' df <- cbind(idw360(values, locations, grid), as.data.frame(grid))
#' ggplot(df, aes(lon, lat)) +
#'   geom_point(size = .1) +
#'   geom_spoke(aes(angle = ((90 - pred) %% 360) * pi / 180),
#'              radius = .07,
#'              arrow=arrow(length = unit(0.2,"cm"))) +
#'   coord_map()
#' }
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
