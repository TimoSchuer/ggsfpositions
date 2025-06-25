#' Position Adjustments for sf Points in ggplot2
#'
#' @description
#' The ggsfpositions package provides position adjustments for sf point geometries
#' in ggplot2. It includes functions to jitter points, arrange them in grids,
#' dodge them horizontally, or arrange them in triangular patterns. These
#' adjustments help to avoid overplotting and create more informative
#' visualizations of spatial point data.
#'
#' @details
#' The package provides four main position adjustment functions:
#'
#' * `position_sf_jitter()`: Adds random noise to point positions
#' * `position_sf_dodge()`: Arranges overlapping points horizontally
#' * `position_sf_grid()`: Arranges points in a grid pattern
#' * `position_sf_triangle()`: Arranges points in a triangular pattern
#'
#' All position functions work with grouped data using ggplot2's group aesthetic
#' and can optionally ensure points stay within a specified boundary polygon.
#'
#' @section Position Adjustments:
#' \describe{
#'   \item{position_sf_jitter}{
#'     Adds random noise to point positions to avoid overplotting:
#'     \preformatted{
#'     ggplot(points) +
#'       geom_sf(position = position_sf_jitter(
#'         width = 0.01,
#'         height = 0.01,
#'         seed = 123
#'       ))
#'     }
#'   }
#'   \item{position_sf_dodge}{
#'     Arranges overlapping points horizontally:
#'     \preformatted{
#'     ggplot(points) +
#'       geom_sf(aes(group = category, size = value),
#'         position = position_sf_dodge(
#'           width = 0.01,
#'           bound = boundary_polygon,
#'           hjust = 0,    # align left
#'           vjust = 1     # align top
#'         )
#'       )
#'     }
#'   }
#'   \item{position_sf_grid}{
#'     Arranges points in a grid pattern:
#'     \preformatted{
#'     ggplot(points) +
#'       geom_sf(aes(group = category, size = value),
#'         position = position_sf_grid(
#'           ncol = 3,
#'           x_spacing = 0.01,  # horizontal spacing
#'           y_spacing = 0.008, # vertical spacing
#'           bound = boundary_polygon,
#'           hjust = 1,    # align right
#'           vjust = 0     # align bottom
#'         )
#'       )
#'     }
#'   }
#'   \item{position_sf_triangle}{
#'     Arranges points in a triangular pattern:
#'     \preformatted{
#'     ggplot(points) +
#'       geom_sf(aes(group = category, size = value),
#'         position = position_sf_triangle(
#'           spacing = 0.01,
#'           bound = boundary_polygon,
#'           hjust = 0.5,  # center horizontally
#'           vjust = 1     # align top
#'         )
#'       )
#'     }
#'   }
#' }
#'
#' @section Size-based Ordering:
#' When using the size aesthetic (`aes(size = ...)`), points are automatically arranged
#' with larger points appearing first in reading direction (left-to-right, top-to-bottom).
#' This feature helps ensure that smaller points do not get hidden behind larger ones.
#' Size-based ordering is available for position_sf_dodge, position_sf_grid, and
#' position_sf_triangle.
#'
#' @section Boundary Constraints:
#' The bound parameter accepts an sf POLYGON object that defines the area within
#' which points should stay. When a boundary is specified, the functions will
#' automatically scale the spacing between points to ensure they remain within
#' the boundary. If points cannot be placed within the boundary while maintaining
#' their relative positions, a warning will be issued.
#'
#' @section Justification:
#' The hjust and vjust parameters control the alignment of point groups:
#' * hjust: 0 = left, 0.5 = center, 1 = right
#' * vjust: 0 = bottom, 0.5 = center, 1 = top
#'
#' This is particularly useful when working with boundary constraints or when
#' specific alignments are needed for visual clarity.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#'
#' # Create example data
#' points <- st_sfc(
#'   st_point(c(8, 52)),
#'   st_point(c(8, 52)),
#'   st_point(c(8.5, 52.5)),
#'   crs = 4326
#' )
#' data <- st_sf(
#'   category = c("A", "A", "B"),
#'   size = c(3, 2, 1),
#'   geometry = points
#' )
#'
#' # Example with jittering
#' ggplot(data) +
#'   geom_sf(position = position_sf_jitter(width = 0.01, height = 0.01))
#'
#' # Example with dodging, size-based ordering and left alignment
#' ggplot(data) +
#'   geom_sf(
#'     aes(group = category, size = size),
#'     position = position_sf_dodge(width = 0.01, hjust = 0)
#'   )
#'
#' # Example with grid arrangement, different spacings and right-bottom alignment
#' ggplot(data) +
#'   geom_sf(
#'     aes(group = category, size = size),
#'     position = position_sf_grid(
#'       ncol = 2,
#'       x_spacing = 0.01,
#'       y_spacing = 0.008,
#'       hjust = 1,
#'       vjust = 0
#'     )
#'   )
#'
#' # Example with triangular arrangement and top-center alignment
#' ggplot(data) +
#'   geom_sf(
#'     aes(group = category, size = size),
#'     position = position_sf_triangle(
#'       spacing = 0.01,
#'       hjust = 0.5,
#'       vjust = 1
#'     )
#'   )
#' }
#'
#' @import ggplot2
#' @import sf
#'
#' @name ggsfpositions
#' @docType package
NULL
