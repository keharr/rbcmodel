#' Create a plot of the dependent variable in a 3D slice
#'
#' Create a plot of the dependent variable (4-th index) in a 3D slice
#' (created using, e.g., slice_4D_grid()) against its two non-constant
#' independent dimensions. In the plot, the dependent variable is
#' represented by contours and false color, while the two independent
#' variables form the horizontal and the vertical axes.
#'
#' @param grid_slice The 3D slice to be plotted
#' @param contours The values of dependent variable at which the contours
#'   are drawn, as a single numeric vector. If NULL no contours are produced.
#' @param cmin The lower bound for the color mapping. If NULL defaults to the
#'   minimum value of the contour/color variable.
#' @param cmax The upper bound for the color mapping. If NULL defaults to the
#'   maximum value of the contour/color variable.
#' @param dims Length 3 integer vector, e.g., c(1,2,4), that specify the
#'   horizontal, vertical, and contour/color variables of the plot. If NULL,
#'   the function will attempt to automatically determine horizontal coordinate
#'   and the vertical coordinate, while the last element of the slice is
#'   treated as the dependent variable. Note that the horizontal coordinate
#'   should ALWAYS correspond the first index of the two-dimension arrays,
#'   and the vertical coordinates should ALWAYS be the second index. (Use
#'   transpose_3D_slice() to modify which variables are horizontal/vertical)
#' @param colors The vector of colors used for rendering the false color
#'   tiles of the plot. If the string "default" is supplied instead, the
#'   default blue-white-red color scale is used, where the color is bluer
#'   the more negative the dependent variable, redder the more positive the
#'   dependent variable, and which white is anchored at 0.
#' @param NA_color The color used to represent undefined or out-of-range values
#' @param contour_col The color for the contour lines
#' @param xlabel The label for the horizontal axis
#' @param ylabel The label for the vertical axis
#' @param clabel The label for the color bar. Defaults to c("Carbon", "(C/s)")
#' @param lwd The linewidth of contour lines
#' @param ... Addtional arguments are passed to plot3D::image2D(), which is the
#'   "bottom" plot created by this function (to be overlaid by the contour plot)
#' @returns No explicit return (plot generated as side effect)
#' @export
#' @examples
#' # create 3D slices
#' f <- function(x, y, z) { x + y * z }
#' g1 <- make_4D_grid(f, seq(1, 3, 0.1), seq(-2, 2, 0.2), seq(4, 10, 0.2))
#' s1 <- slice_4D_grid(g1, 2, 2)
#' # plot the 3D slice
#' plot_slice_3D(s1, contours=seq(5, 25, 2.5), dims=c(1, 3, 4))
plot_slice_3D <- function(
  grid_slice, contours = NULL, cmin = NULL, cmax = NULL, dims = NULL,
  colors = "default", NA_color = "grey", contour_col = "black",
  xlabel = "", ylabel = "", clabel=c("Carbon", "(C/s)"), lwd = 2, ...
) {

  if (is.null(dims)) { # default: automatically determine coordinates

    tol = 1e-10
    x <- NA
    y <- NA

    for (m in grid_slice){
      if (is.matrix(m) && length(m) > 1){
        mrange <- max(m) - min(m)
        coldiffsum <- sum(abs(diff(m)))
        rowdiffsum <- sum(abs(diff(t(m))))
        if (rowdiffsum < tol * mrange && is.na(x)){
          x <- m
        }
        if (coldiffsum < tol * mrange && is.na(y)){
          y <- m
        }
      }
    }
    if ((length(x)==1 && is.na(x)) || (length(y)==1 && is.na(y))){
      stop("Cannot determine horizontal and/or vertical coordinates.")
    }
    z <- grid_slice[[length(grid_slice)]]
  } else { # customized: resolve x and y coordinates using dims
    x <- grid_slice[[dims[1]]]
    y <- grid_slice[[dims[2]]]
    z <- grid_slice[[dims[3]]]
  }

  # lower and upper bounds for the color fill
  if (is.null(cmax)){
    upper <- max(z, na.rm=TRUE)
  } else {
    upper <- cmax
  }

  if (is.null(cmin)){
    lower <- min(z, na.rm=TRUE)
  } else {
    lower <- cmin
  }

  # resolve default color
  if (length(colors) == 1) {
  if (colors == "default") {
    if (upper <= 0){ # color range negative, use segment of blue-white
      colors <- plot3D::ramp.col(col = c("blue", "white"), n = 125)
      c_cut <- as.integer(max(1 - upper/lower, 0.8) * 125)
      colors <- colors[1:c_cut]
    } else if (lower >= 0) { # color range positive, use segment of white-red
      colors <- plot3D::ramp.col(col = c("white", "red"), n = 125)
      c_cut <- as.integer(min(lower/upper, 0.2) * 125)
      colors <- colors[1 + c_cut:124]
    } else { # color range includes 0, use red-white-blue
      colors <- plot3D::ramp.col(col = c("blue", "white", "red"), n = 200)
      # trim so that white always represents 0
      low_abs = abs(lower)
      if (upper > low_abs){
        c_cut <- as.integer(100 * (1 - low_abs/upper))
        colors <- colors[1 + c_cut:199]
      } else {
        c_cut <- as.integer(100 + 100 * upper / low_abs)
        colors <- colors[1:c_cut]
      }
    }
  }
  }

  # plot with color fill based on values...
  plot3D::image2D(
    x = x, y = y, z = z,
    clim = c(lower, upper), xlab = xlabel, ylab = ylabel,
    col = colors, NAcol = NA_color, clab=clabel, ...
  )

  # ... overlay with line contours
  if (!is.null(contours)){
    plot3D::contour2D(
      x = x[, 1], y = y[1, ], z = z,
      lwd = lwd, col = contour_col, add = TRUE,
      levels = contours
    )
  }
  # no explicit return (Plot generated as side effect)
}
