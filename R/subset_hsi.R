#' Subset an hsi array while preserving attributes
#'
#' @description
#' Efficiently subsets an hsi array while preserving all hsi-specific attributes
#' (header, thumbnail, thumbnail_wavelength). This function is optimized for
#' memory efficiency and speed by using direct array indexing without unnecessary copying.
#'
#' @param x an hsi array object
#' @param x_range a vector of x indices or range (e.g., c(1, 10) or 1:10)
#' @param y_range a vector of y indices or range (e.g., c(1, 10) or 1:10)
#' @param wl_range a vector of wavelength indices or range (e.g., c(1, 10) or 1:10)
#' @param drop logical indicating whether to drop dimensions of length 1 (default: FALSE)
#' @return a subsetted hsi array with preserved attributes
#' @export
#' @examples
#' \dontrun{
#' # Assuming sample_cube is an hsi object
#' subset_cube <- subset_hsi(sample_cube, x_range = 1:50, y_range = 1:50)
#'
#' # Subset by wavelength range
#' subset_cube <- subset_hsi(sample_cube, wl_range = 1:10)
#'
#' # Check that attributes are preserved
#' print(subset_cube)
#' }
#' @seealso read_pam
subset_hsi <- function(x, x_range = NULL, y_range = NULL, wl_range = NULL, drop = FALSE) {
  # Validate input
  if (!inherits(x, "hsi")) {
    stop("Input must be an hsi array object")
  }

  # Get current dimensions
  dims <- dim(x)
  x_dim <- dims[1]
  y_dim <- dims[2]
  wl_dim <- dims[3]

  # Validate ranges
  if (!is.null(x_range)) {
    if (!is.numeric(x_range)) stop("x_range must be numeric")
    x_range <- unique(round(x_range))
    x_range <- x_range[x_range > 0 & x_range <= x_dim]
    if (length(x_range) == 0) stop("x_range contains no valid indices")
  } else {
    x_range <- 1:x_dim
  }

  if (!is.null(y_range)) {
    if (!is.numeric(y_range)) stop("y_range must be numeric")
    y_range <- unique(round(y_range))
    y_range <- y_range[y_range > 0 & y_range <= y_dim]
    if (length(y_range) == 0) stop("y_range contains no valid indices")
  } else {
    y_range <- 1:y_dim
  }

  if (!is.null(wl_range)) {
    if (!is.numeric(wl_range)) stop("wl_range must be numeric")
    wl_range <- unique(round(wl_range))
    wl_range <- wl_range[wl_range > 0 & wl_range <= wl_dim]
    if (length(wl_range) == 0) stop("wl_range contains no valid indices")
  } else {
    wl_range <- 1:wl_dim
  }

  # Perform subsetting using direct indexing (memory efficient)
  # This avoids creating intermediate copies
  subsetted <- x[x_range, y_range, wl_range, drop = drop]

  # Preserve all hsi attributes
  if (!is.null(attr(x, "header"))) {
    attr(subsetted, "header") <- attr(x, "header")
  }

  # Preserve class
  class(subsetted) <- class(x)

  # Update dimnames to reflect subsetting
  if (!is.null(dimnames(x))) {
    current_dimnames <- dimnames(x)

    if (!is.null(x_range) && length(current_dimnames) >= 1) {
      current_dimnames[[1]] <- current_dimnames[[1]][x_range]
    }
    if (!is.null(y_range) && length(current_dimnames) >= 2) {
      current_dimnames[[2]] <- current_dimnames[[2]][y_range]
    }
    if (!is.null(wl_range) && length(current_dimnames) >= 3) {
      current_dimnames[[3]] <- current_dimnames[[3]][wl_range]
    }

    dimnames(subsetted) <- current_dimnames
  }

  # Update header dimensions if they exist
  if (!is.null(attr(subsetted, "header"))) {
    header <- attr(subsetted, "header")
    header["width"] <- dim(subsetted)[1]
    header["height"] <- dim(subsetted)[2]
    header["depth"] <- dim(subsetted)[3]
    attr(subsetted, "header") <- header
  }

  return(subsetted)
}

