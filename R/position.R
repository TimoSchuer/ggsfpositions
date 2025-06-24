# Position-Funktionen für geom_sf
library(sf)
library(ggplot2)

# 1. Jitter Position für SF-Objekte
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

# 2. Horizontale Anordnung für SF-Objekte
position_sf_dodge <- function(width = 0.01) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      coords <- st_coordinates(data$geometry)

      if (nrow(coords) > 0) {
        # Berechne horizontale Offsets
        n_points <- nrow(data)
        if (n_points > 1) {
          offsets <- seq(-width / 2, width / 2, length.out = n_points)
          coords[, "X"] <- coords[, "X"] + offsets
        }

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

# 3. Rechteckige Anordnung für SF-Objekte
position_sf_grid <- function(ncol = 3, spacing = 0.01) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      coords <- st_coordinates(data$geometry)
      n_points <- nrow(data)

      if (n_points > 0) {
        # Berechne Raster-Layout
        cols <- min(ncol, n_points)
        rows <- ceiling(n_points / cols)

        # Erstelle Offset-Positionen
        grid_x <- rep(
          seq(-(cols - 1) / 2, (cols - 1) / 2, length.out = cols),
          length.out = n_points
        ) *
          spacing
        grid_y <- rep(
          seq((rows - 1) / 2, -(rows - 1) / 2, length.out = rows),
          each = cols
        )[1:n_points] *
          spacing

        coords[, "X"] <- coords[, "X"] + grid_x
        coords[, "Y"] <- coords[, "Y"] + grid_y

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

# 4. Dreieckige Anordnung für SF-Objekte
position_sf_triangle <- function(spacing = 0.01) {
  ggproto(
    NULL,
    PositionIdentity,
    compute_layer = function(self, data, params, layout) {
      if (!"geometry" %in% names(data) || nrow(data) == 0) {
        return(data)
      }

      coords <- st_coordinates(data$geometry)
      n_points <- nrow(data)

      if (n_points > 0) {
        # Berechne Dreiecks-Positionen
        positions <- data.frame(x_offset = 0, y_offset = 0)
        level <- 1

        while (nrow(positions) < n_points) {
          level <- level + 1
          for (i in 1:level) {
            x_off <- (i - (level + 1) / 2) * spacing
            y_off <- -(level - 1) * spacing * 0.866 # sqrt(3)/2
            positions <- rbind(
              positions,
              data.frame(x_offset = x_off, y_offset = y_off)
            )
          }
        }

        positions <- positions[1:n_points, ]
        coords[, "X"] <- coords[, "X"] + positions$x_offset
        coords[, "Y"] <- coords[, "Y"] + positions$y_offset

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
