# ggsfpositions

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

## Overview

`ggsfpositions` is an R package that provides position adjustments for sf point geometries in ggplot2. It enhances the visualization of spatial point data by offering various positioning strategies to avoid overplotting and create more informative visualizations.

## Features

The package includes several position adjustment functions:

-   `position_sf_jitter()`: Adds random noise to point positions to reduce overplotting
-   `position_sf_grid()`: Arranges overlapping points in a grid pattern
-   `position_sf_dodge()`: Shifts overlapping points horizontally
-   `position_sf_triangle()`: Arranges points in triangular patterns

## Installation

You can install the development version of ggsfpositions from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("timoschuermann/ggsfpositions")
```

## Usage

Here's a complete example that demonstrates how to use the package with multiple overlapping points:

``` r
library(ggplot2)
library(sf)
library(ggsfpositions)

# Create sample point data
set.seed(123)  # for reproducibility

# Create points with some overlapping coordinates
points_df <- data.frame(
  x = rep(c(1, 2, 3), each = 5) + rnorm(15, 0, 0.1),
  y = rep(c(1, 2, 3), each = 5) + rnorm(15, 0, 0.1),
  group = rep(letters[1:3], 5)
)

# Convert to sf object
points_sf <- st_as_sf(points_df, coords = c("x", "y"))
st_crs(points_sf) <- 4326  # Set CRS to WGS84

# Basic plot with overlapping points
p1 <- ggplot(points_sf) +
  geom_sf(aes(color = group)) +
  ggtitle("Original (overlapping points)")

# Using position_sf_jitter
p2 <- ggplot(points_sf) +
  geom_sf(aes(color = group), position = position_sf_jitter(width = 0.1)) +
  ggtitle("With position_sf_jitter")

# Using position_sf_grid
p3 <- ggplot(points_sf) +
  geom_sf(aes(color = group), position = position_sf_grid(width = 0.2)) +
  ggtitle("With position_sf_grid")

# Display plots side by side
library(patchwork)
p1 + p2 + p3 + plot_layout(ncol = 3)
```

This example creates a dataset with multiple points per location and demonstrates how different position adjustments can help visualize overlapping points.

## Requirements

-   ggplot2
-   sf

## Author

Timo Schürmann (info\@timoschuermann.org)

## License

This project is licensed under the Creative Commons Attribution 4.0 International License - see the [LICENSE.md](LICENSE.md) file for details.