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

  if (!is.null(attr(x, "thumbnail"))) {
    # Update thumbnail if it exists
    original_thumb <- attr(x, "thumbnail")
    thumb_dims <- dim(original_thumb)
    
    # Calculate new thumbnail dimensions based on subsetting
    # Original thumbnail was downsampled by 4x
    new_x_thumb <- ceiling(length(x_range) / 4)
    new_y_thumb <- ceiling(length(y_range) / 4)
    
    # Only update thumbnail if the subset affects the spatial dimensions
    if (length(x_range) < dim(original_thumb)[1] * 4 || 
        length(y_range) < dim(original_thumb)[2] * 4) {
      
      # Create new thumbnail by subsetting the original thumbnail
      # Map subsetted indices back to original indices
      orig_x_indices <- x_range
      orig_y_indices <- y_range
      
      # Find corresponding thumbnail indices
      thumb_x_indices <- ceiling(orig_x_indices / 4)
      thumb_y_indices <- ceiling(orig_y_indices / 4)
      
      # Ensure indices are within thumbnail bounds
      thumb_x_indices <- pmin(thumb_x_indices, dim(original_thumb)[1])
      thumb_y_indices <- pmin(thumb_y_indices, dim(original_thumb)[2])
      
      # Subset the thumbnail
      new_thumb <- original_thumb[unique(thumb_x_indices), unique(thumb_y_indices), drop = FALSE]
      
      if (nrow(new_thumb) > 0 && ncol(new_thumb) > 0) {
        attr(subsetted, "thumbnail") <- new_thumb
      } else {
        # If thumbnail becomes too small, remove it
        attr(subsetted, "thumbnail") <- NULL
      }
    } else {
      # Keep original thumbnail if spatial subsetting doesn't affect it
      attr(subsetted, "thumbnail") <- original_thumb
    }
  }

  if (!is.null(attr(x, "thumbnail_wavelength"))) {
    attr(subsetted, "thumbnail_wavelength") <- attr(x, "thumbnail_wavelength")
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

#' Subset an hsi array by wavelength range
#'
#' @description
#' Convenience function to subset an hsi array by wavelength range while preserving all attributes.
#' This is a specialized version of subset_hsi for wavelength-based subsetting.
#'
#' @param x an hsi array object
#' @param wl_min minimum wavelength to include (inclusive)
#' @param wl_max maximum wavelength to include (inclusive)
#' @return a subsetted hsi array with preserved attributes
#' @export
#' @examples
#' \dontrun{
#' # Assuming sample_cube is an hsi object with wavelengths 400-700nm
#' visible_cube <- subset_by_wavelength(sample_cube, wl_min = 400, wl_max = 700)
#' }
#' @seealso subset_hsi
subset_by_wavelength <- function(x, wl_min, wl_max) {
  if (!inherits(x, "hsi")) {
    stop("Input must be an hsi array object")
  }

  # Get wavelength values from dimnames
  wl_values <- as.numeric(dimnames(x)[[3]])
  
  # Find indices for the wavelength range
  wl_indices <- which(wl_values >= wl_min & wl_values <= wl_max)
  
  if (length(wl_indices) == 0) {
    stop(paste("No wavelengths found in range", wl_min, "-", wl_max))
  }

  # Use subset_hsi to perform the actual subsetting
  subset_hsi(x, wl_range = wl_indices)
}

#' Subset an hsi array by spatial region (ROI)
#'
#' @description
#' Convenience function to subset an hsi array by spatial region of interest (ROI) 
#' while preserving all attributes. This is useful for extracting specific regions 
#' from the hyperspectral cube.
#'
#' @param x an hsi array object
#' @param x_min minimum x coordinate (inclusive)
#' @param x_max maximum x coordinate (inclusive)
#' @param y_min minimum y coordinate (inclusive)
#' @param y_max maximum y coordinate (inclusive)
#' @return a subsetted hsi array with preserved attributes
#' @export
#' @examples
#' \dontrun{
#' # Assuming sample_cube is an hsi object
#' roi_cube <- subset_by_roi(sample_cube, x_min = 10, x_max = 50, y_min = 20, y_max = 60)
#' }
#' @seealso subset_hsi
subset_by_roi <- function(x, x_min, x_max, y_min, y_max) {
  if (!inherits(x, "hsi")) {
    stop("Input must be an hsi array object")
  }

  # Validate coordinates
  if (x_min < 1 || x_max > dim(x)[1] || x_min > x_max) {
    stop("Invalid x coordinates")
  }
  if (y_min < 1 || y_max > dim(x)[2] || y_min > y_max) {
    stop("Invalid y coordinates")
  }

  # Use subset_hsi to perform the actual subsetting
  subset_hsi(x, x_range = x_min:x_max, y_range = y_min:y_max)
}

#' Fast subsetting by logical mask
#'
#' @description
#' Efficiently subsets an hsi array using a logical mask while preserving all attributes.
#' This function is optimized for memory efficiency by using the mask to extract 
#' only the required elements.
#'
#' @param x an hsi array object
#' @param mask a logical matrix of the same x,y dimensions as the hsi array
#' @return a subsetted hsi array with preserved attributes
#' @export
#' @examples
#' \dontrun{
#' # Assuming sample_cube is an hsi object
#' # Create a mask (e.g., based on some condition)
#' mask <- matrix(rbinom(100*100, 1, 0.5), nrow = 100, ncol = 100)
#' masked_cube <- subset_by_mask(sample_cube, mask)
#' }
#' @seealso subset_hsi
subset_by_mask <- function(x, mask) {
  if (!inherits(x, "hsi")) {
    stop("Input must be an hsi array object")
  }

  if (!is.matrix(mask) || !is.logical(mask)) {
    stop("mask must be a logical matrix")
  }

  # Check dimensions
  if (nrow(mask) != dim(x)[1] || ncol(mask) != dim(x)[2]) {
    stop("mask dimensions must match x,y dimensions of the hsi array")
  }

  # Find indices where mask is TRUE
  valid_indices <- which(mask, arr.ind = TRUE)
  
  if (nrow(valid_indices) == 0) {
    stop("No TRUE values in mask - resulting subset would be empty")
  }

  # Extract unique x and y indices
  x_indices <- unique(valid_indices[, "row"])
  y_indices <- unique(valid_indices[, "col"])

  # Use subset_hsi to perform the actual subsetting
  subset_hsi(x, x_range = x_indices, y_range = y_indices)
}
