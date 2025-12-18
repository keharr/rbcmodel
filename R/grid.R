#' Construct a 4D grid
#'
#' Construct a 4D grid, where the first 3 coordinates are independent
#' variables, while the 4th coordinate is dependent variable arising from
#' evaluating a function against the values of the first 3 coordinates.
#'
#' The grid is represented by a 4-element list, where each member is a
#' 3-dimensional array representing grid points. The first array is the
#' value of first independent variable on each grid point, etc., and the
#' fourth array is the value of function evaluated at each grid point.
#'
#' @param func The function to apply on the grid points to obtain the
#'   value of the dependent variable.
#' @param x_seq A vector of coordinates for the first independent variable.
#' @param y_seq A vector of coordinates for the second independent variable.
#' @param z_seq A vector of coordinates for the third independent variable.
#' @param var_names Names for the 3 independent and 1 dependent variables, 
#'   in a single character vector.
#' @returns A 4D grid as a 4-element list of 3-dimensional arrays
#' @export
#' @examples
#' f <- function(x, y, z) { x + y * z }
#' g1 <- make_4D_grid(f, 1:3, c(-2,2), seq(4, 10, 2))
#' g1
make_4D_grid <- function(
  func, x_seq, y_seq, z_seq, var_names = c("x", "y", "z", "f")
) {

  # use the mesh function from plot3D
  mesh <- plot3D::mesh(x_seq, y_seq, z_seq)

  # applying function on the mesh
  f <- func(mesh[[1]], mesh[[2]], mesh[[3]])

  # assemble all coordinates into a single list
  out <- list(
    x = mesh[[1]],
    y = mesh[[2]],
    z = mesh[[3]],
    f = f
  )

  # assign meaningful names to the elements of the list
  names(out) <- var_names

  # return grid
  out

}

#' Permute the independent variables in a 4D grid created by make_4D_grid().
#'
#' In the new grid, every element in the list is a permuted array. Moreover,
#' the order of the independent variables in the list is also permuted.
#'
#' @param grid The original grid to be permuted.
#' @param perm the subscript permutation vector, a permutation of the integers
#'   c(1, 2, 3).
#' @returns A permuted 4D grid
#' @export
#' @examples
#' f <- function(x, y, z) { x + y * z }
#' g1 <- make_4D_grid(f, 1:3, c(-2,2), seq(4, 10, 2))
#' g2 <- permute_4D_grid(g1, c(2,3,1))
#' g2
permute_4D_grid <- function(grid, perm) {

  # permute each array in the grid
  mod <- list()
  for (elem in grid) {
    mod <- append(mod, list(aperm(elem, perm)))
  }

  # permute the top-level list structure
  in_names <- names(grid)
  out_names <- character(0)
  out <- list()
  for (i in 1:3) {
    out <- append(out, mod[perm[i]])
    out_names <- c(out_names, in_names[perm[i]])
  }
  out <- append(out, mod[length(mod)])
  out_names <- c(out_names, in_names[length(mod)])

  # assign permuted names
  names(out) <- out_names

  #return the modified grid
  out
}

#' Create a 3D slice of 4D grid created by make_4D_grid().
#'
#' The resulting slice remains a list of 4 elements, but each element except
#' the dimension being sliced will be a 2-dimensional array. For the dimension
#' being sliced, the element will be a scalar showing the constant value of the
#' sliced coordinate.
#'
#' @param grid The original grid to be sliced.
#' @param dim The dimension to be sliced. Its corresponding coordinates will
#'   take constant values in the resulting slice.
#' @param val The constant value that the dim-th coordinates will take in the
#'   resulting slice.
#' @param approx Whether approximate value matching is allowed.
#' @param tol the tolerance for approximate value matching.
#' @export
#' @returns A 3D slice of the supplied 3D grid.
#' @examples
#' f <- function(x, y, z) { x + y * z }
#' g1 <- make_4D_grid(f, 1:3, c(-2,2), seq(4, 10, 2))
#' s1 <- slice_4D_grid(g1, 2, 2)
#' s1
slice_4D_grid <- function(grid, dim, val, approx = TRUE, tol = 1e-3) {

  # construct array of values for the dimension being sliced
  if (dim == 1) {
    val_vec <- grid[[1]][, 1, 1]
  } else if (dim == 2) {
    val_vec <- grid[[2]][1, , 1]
  } else if (dim == 3) {
    val_vec <- grid[[3]][1, 1, ]
  } else {
    stop("Unrecognized value of dim. No slicing performed.")
  }

  # find the index of the (possibly closest) match
  idx <- which(val_vec == val)
  if (length(idx) == 0) {
    if (approx) { # approximate match
      idx <- which.min(abs(val_vec - val))
      if (abs(val_vec[idx] - val) > tol) {
        stop("No matching slice found. Abort.")
      }
    } else {
      stop("No matching slice found. Abort.")
    }
  }

  # take the first match in case multiple matches are found
  idx <- idx[1]

  # construct new grid (slice of old grid)
  if (dim == 1) {
    out <- list(
      x = grid[[1]][idx, 1, 1],
      y = grid[[2]][idx, , ],
      z = grid[[3]][idx, , ],
      f = grid[[4]][idx, , ]
    )
  } else if (dim == 2) {
    out <- list(
      x = grid[[1]][, idx, ],
      y = grid[[2]][1, idx, 1],
      z = grid[[3]][, idx, ],
      f = grid[[4]][, idx, ]
    )
  } else if (dim == 3) {
    out <- list(
      x = grid[[1]][, , idx],
      y = grid[[2]][, , idx],
      z = grid[[3]][1, 1, idx],
      f = grid[[4]][, , idx]
    )
  }

  # copy names from the grid to the slice
  names(out) <- names(grid)

  # return slice
  out
}

#' Transpose the non-constant independent variables in a 3D grid created by
#' slice_4D_grid().
#'
#' In the new slice, every non-constant element in the list is transposed.
#' Moreover, the order of the 2 non-constant independent variables in the list
#' is also swapped.
#'
#' @param slice The original slice to be transposed.
#' @returns A transposed 3D slice.
#' @export
#' @examples
#' f <- function(x, y, z) { x + y * z }
#' g1 <- make_4D_grid(f, 1:3, c(-2,2), seq(4, 10, 2))
#' s1 <- slice_4D_grid(g1, 2, 2)
#' s2 <- transpose_3D_slice(s1)
#' s2
transpose_3D_slice <- function(slice) {

  # initialize containers
  swap_idx <- integer(0)
  mod <- list()

  # extract the indices where swap happen
  # transpose individual matrix in the slice
  for (i in 1:3){
    if (!is.null(dim(slice[[i]]))) { # transposable case
      swap_idx <- c(swap_idx, i)
      mod <- append(mod, list(t(slice[[i]])))
    } else { # no op case
      mod <- append(mod, slice[i])
    }
  }

  # swapping the list entries using classical swap pattern (!)
  tmp <- mod[[swap_idx[1]]]
  mod[[swap_idx[1]]] <- mod[[swap_idx[2]]]
  mod[[swap_idx[2]]] <- tmp

  # append the function value entry to the list
  mod <- append(mod, list(t(slice[[length(slice)]])))

  # transpose the names of the list entries, then assign
  in_names <- names(slice)
  out_names <- in_names
  out_names[swap_idx[1]] <- in_names[swap_idx[2]]
  out_names[swap_idx[2]] <- in_names[swap_idx[1]]
  names(mod) <- out_names

  # return transposed slice
  mod

}
