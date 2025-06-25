# Position-Funktionen für geom_sf
library(sf)
library(ggplot2)

#' Add random jitter to sf point geometries
#'
#' @description
#' Adds random noise to the coordinates of sf point geometries to avoid overplotting.
#'
#' @param width numeric, amount of horizontal jitter. Defaults to 0.01.
#' @param height numeric, amount of vertical jitter. Defaults to 0.01.
#' @param seed integer or NULL, random seed for reproducible jitter. Defaults to NULL.
#'
#' @return A Position object that can be used with geom_sf.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' points <- st_sfc(st_point(c(8, 52)), st_point(c(8, 52)), crs = 4326)
#' sf_data <- st_sf(id = 1:2, geometry = points)
#' ggplot(sf_data) +
#'   geom_sf(position = position_sf_jitter())
#' }
position_sf_jitter <- function(width = 0.01, height = 0.01, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data)) {
        return(data)
      }

      # Extrahiere Koordinaten
      coords <- st_coordinates(data$geometry)

      # Füge Jitter hinzu
      if (nrow(coords) > 0) {
        coords[, "X"] <- coords[, "X"] + runif(nrow(coords), -width, width)
        coords[, "Y"] <- coords[, "Y"] + runif(nrow(coords), -height, height)

        # Erstelle neue Geometrie
        if (st_geometry_type(data$geometry[1]) == "POINT") {
          new_geom <- st_sfc(
            lapply(1:nrow(coords), function(i) {
              st_point(c(coords[i, "X"], coords[i, "Y"]))
            }),
            crs = st_crs(data$geometry)
          )

          data$geometry <- new_geom
        }
      }

      data
    }
  )
}

#' Horizontally dodge overlapping sf point geometries by group
#'
#' @description
#' Creates a Position object that arranges overlapping points in a horizontal line
#' with equal spacing between them, using ggplot2's group aesthetic.
#'
#' @param width numeric, amount of horizontal spacing between points. Defaults to 0.01.
#' @param bound sf polygon object that defines the boundary within which points should stay.
#'        If NULL, points are not constrained.
#'
#' @return A Position object that can be used with geom_sf.
#' @export
position_sf_dodge <- function(width = 0.01, bound = NULL) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      # Nutze die ggplot2 Gruppierung
      if (!"group" %in% names(data)) {
        group_col <- rep(1, nrow(data))
      } else {
        group_col <- data$group
      }

      # Für jede Gruppe separat verarbeiten
      unique_groups <- unique(group_col)
      for (g in unique_groups) {
        group_mask <- group_col == g
        group_data <- data[group_mask, ]

        coords <- st_coordinates(group_data$geometry)
        n_points <- nrow(coords)

        if (n_points > 1) {
          # Original center point für die Gruppe
          center_point <- st_point(c(mean(coords[, "X"]), mean(coords[, "Y"])))

          # Berechne initiale Offsets
          offsets <- seq(-width / 2, width / 2, length.out = n_points)

          # Wenn bound gegeben, stelle sicher dass Punkte innerhalb bleiben
          if (!is.null(bound)) {
            scale_factor <- 1
            repeat {
              new_coords <- coords
              new_coords[, "X"] <- coords[, "X"] + offsets * scale_factor

              new_points <- st_sfc(
                lapply(1:nrow(new_coords), function(i) {
                  st_point(c(new_coords[i, "X"], new_coords[i, "Y"]))
                }),
                crs = st_crs(group_data$geometry)
              )

              if (all(st_within(new_points, bound, sparse = FALSE))) {
                coords <- new_coords
                break
              }

              scale_factor <- scale_factor * 0.9
              if (scale_factor < 0.01) {
                warning(
                  "Could not place all points within boundary, using original positions"
                )
                break
              }
            }
          } else {
            coords[, "X"] <- coords[, "X"] + offsets
          }

          if (st_geometry_type(group_data$geometry[1]) == "POINT") {
            new_geom <- st_sfc(
              lapply(1:nrow(coords), function(i) {
                st_point(c(coords[i, "X"], coords[i, "Y"]))
              }),
              crs = st_crs(group_data$geometry)
            )
            data$geometry[group_mask] <- new_geom
          }
        }
      }

      data
    }
  )
}

#' Arrange sf point geometries in a grid pattern by group
#'
#' @description
#' Creates a Position object that arranges overlapping points in a grid pattern,
#' using ggplot2's group aesthetic.
#'
#' @param ncol integer, number of columns in the grid. Defaults to 3.
#' @param spacing numeric, spacing between grid points. Defaults to 0.01.
#' @param bound sf polygon object that defines the boundary within which points should stay.
#'        If NULL, points are not constrained.
#'
#' @return A Position object that can be used with geom_sf.
#' @export
position_sf_grid <- function(ncol = 3, spacing = 0.01, bound = NULL) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      # Nutze die ggplot2 Gruppierung
      if (!"group" %in% names(data)) {
        group_col <- rep(1, nrow(data))
      } else {
        group_col <- data$group
      }

      # Für jede Gruppe separat verarbeiten
      unique_groups <- unique(group_col)
      for (g in unique_groups) {
        group_mask <- group_col == g
        group_data <- data[group_mask, ]

        coords <- st_coordinates(group_data$geometry)
        n_points <- nrow(coords)

        if (n_points > 0) {
          # Original center point für die Gruppe
          center_point <- st_point(c(mean(coords[, "X"]), mean(coords[, "Y"])))

          # Berechne Grid-Layout
          cols <- min(ncol, n_points)
          rows <- ceiling(n_points / cols)

          # Wenn bound gegeben, stelle sicher dass Punkte innerhalb bleiben
          scale_factor <- 1
          repeat {
            grid_x <- rep(
              seq(-(cols - 1) / 2, (cols - 1) / 2, length.out = cols),
              length.out = n_points
            ) *
              spacing *
              scale_factor

            grid_y <- rep(
              seq((rows - 1) / 2, -(rows - 1) / 2, length.out = rows),
              each = cols
            )[1:n_points] *
              spacing *
              scale_factor

            new_coords <- coords
            new_coords[, "X"] <- coords[, "X"] + grid_x
            new_coords[, "Y"] <- coords[, "Y"] + grid_y

            if (
              is.null(bound) ||
                all(st_within(
                  st_sfc(
                    lapply(1:nrow(new_coords), function(i) {
                      st_point(c(new_coords[i, "X"], new_coords[i, "Y"]))
                    }),
                    crs = st_crs(group_data$geometry)
                  ),
                  bound,
                  sparse = FALSE
                ))
            ) {
              coords <- new_coords
              break
            }

            scale_factor <- scale_factor * 0.9
            if (scale_factor < 0.01) {
              warning(
                "Could not place all points within boundary, using original positions"
              )
              break
            }
          }

          if (st_geometry_type(group_data$geometry[1]) == "POINT") {
            new_geom <- st_sfc(
              lapply(1:nrow(coords), function(i) {
                st_point(c(coords[i, "X"], coords[i, "Y"]))
              }),
              crs = st_crs(group_data$geometry)
            )
            data$geometry[group_mask] <- new_geom
          }
        }
      }

      data
    }
  )
}

#' Arrange sf point geometries in a triangular pattern by group
#'
#' @description
#' Creates a Position object that arranges overlapping points in a triangular pattern,
#' using ggplot2's group aesthetic.
#'
#' @param spacing numeric, spacing between points. Defaults to 0.01.
#' @param bound sf polygon object that defines the boundary within which points should stay.
#'        If NULL, points are not constrained.
#'
#' @return A Position object that can be used with geom_sf.
#' @export
position_sf_triangle <- function(spacing = 0.01, bound = NULL) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      # Nutze die ggplot2 Gruppierung
      if (!"group" %in% names(data)) {
        group_col <- rep(1, nrow(data))
      } else {
        group_col <- data$group
      }

      # Für jede Gruppe separat verarbeiten
      unique_groups <- unique(group_col)
      for (g in unique_groups) {
        group_mask <- group_col == g
        group_data <- data[group_mask, ]

        coords <- st_coordinates(group_data$geometry)
        n_points <- nrow(coords)

        if (n_points > 0) {
          # Original center point für die Gruppe
          center_point <- st_point(c(mean(coords[, "X"]), mean(coords[, "Y"])))

          # Berechne Dreiecks-Positionen
          scale_factor <- 1
          repeat {
            positions <- data.frame(x_offset = 0, y_offset = 0)
            level <- 1

            while (nrow(positions) < n_points) {
              level <- level + 1
              for (i in 1:level) {
                x_off <- (i - (level + 1) / 2) * spacing * scale_factor
                y_off <- -(level - 1) * spacing * 0.866 * scale_factor
                positions <- rbind(
                  positions,
                  data.frame(x_offset = x_off, y_offset = y_off)
                )
              }
            }

            positions <- positions[1:n_points, ]
            new_coords <- coords
            new_coords[, "X"] <- coords[, "X"] + positions$x_offset
            new_coords[, "Y"] <- coords[, "Y"] + positions$y_offset

            if (
              is.null(bound) ||
                all(st_within(
                  st_sfc(
                    lapply(1:nrow(new_coords), function(i) {
                      st_point(c(new_coords[i, "X"], new_coords[i, "Y"]))
                    }),
                    crs = st_crs(group_data$geometry)
                  ),
                  bound,
                  sparse = FALSE
                ))
            ) {
              coords <- new_coords
              break
            }

            scale_factor <- scale_factor * 0.9
            if (scale_factor < 0.01) {
              warning(
                "Could not place all points within boundary, using original positions"
              )
              break
            }
          }

          if (st_geometry_type(group_data$geometry[1]) == "POINT") {
            new_geom <- st_sfc(
              lapply(1:nrow(coords), function(i) {
                st_point(c(coords[i, "X"], coords[i, "Y"]))
              }),
              crs = st_crs(group_data$geometry)
            )
            data$geometry[group_mask] <- new_geom
          }
        }
      }

      data
    }
  )
}

# # Beispiel-Verwendung:
# library(ggplot2)
# library(sf)

# # Erstelle Beispieldaten
# points <- st_sfc(
#   st_point(c(8, 52)),
#   st_point(c(8, 52)),
#   st_point(c(8, 52)),
#   st_point(c(8.5, 52.5)),
#   st_point(c(8.5, 52.5)),
#   crs = 4326
# )
# sf_data <- st_sf(id = 1:5, geometry = points)

# # Verwendung:
# ggplot(sf_data) +
#   geom_sf(position = position_sf_jitter(width = 0.01, height = 0.01),
#           color = "red", size = 3) +
#   theme_void()

# ggplot(sf_data) +
#   geom_sf(position = position_sf_triangle(spacing = 0.005),
#           color = "blue", size = 3) +
#   theme_void()

# ggplot(sf_data) +
#   geom_sf(position = position_sf_grid(ncol = 3, spacing = 0.005),
#           color = "green", size = 3) +
#   theme_void()

# ggplot(sf_data) +
#   geom_sf(position = position_sf_dodge(width = 0.03),
#           color = "purple", size = 3) +
#   theme_void()
