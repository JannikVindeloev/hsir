#' Read hsi `.pam` files (optimized with grayscale thumbnail)
#'
#' @description
#' Reads a binary PAM (Portable Arbitrary Map) file and returns the image data as a 3-D array 
#' (width \u00d7 height \u00d7 depth). The function uses optimized binary reading and optionally 
#' generates a downsampled grayscale thumbnail from a single wavelength slice for ROI selection.
#'
#' @param file_path the path for the file
#' @param min_wl the minimum wavelength for dimension naming (default: 430)
#' @param thumbnail logical whether to generate a thumbnail (default: TRUE)
#' @param wl wavelength for the thumbnail (default: middle wavelength)
#' @return an `integer` `array` of class `hsi` with optional `thumbnail` attribute
#' @export
#' @examples
#' \dontrun{
#' base_path <- '/z/datasets/ds_mcr_data/departments/MicrobialPhysiology/HSI Files/HTP Gel Firmness/20260410/'
#' sample_path <- paste0(base_path, '20260410_Premium-11.pam')
#' sample_cube <- read_pam(sample_path, wl = 550)
#' str(sample_cube)
#' class(sample_cube)
#'
#' # Show the thumbnail
#' thumbnail <- attr(sample_cube, "thumbnail")
#' graphics::image(thumbnail, col = gray(0:255 / 255))
#' }
#' @importFrom data.table fread setDT
#' @importFrom matrixStats rowMeans2 colMeans2
read_pam <- function(file_path, min_wl = 430, thumbnail = TRUE, wl = NULL) {
  stopifnot(file.exists(file_path))

  con <- file(file_path, "rb")
  on.exit(close(con))

  ## --- Read header (robust: line by line until ENDHDR) ---
  header <- character()
  repeat {
    line <- readLines(con, n = 1)
    if (!length(line)) stop("Unexpected end of file while reading PAM header")
    header <- c(header, line)
    if (trimws(line) == "ENDHDR") break
  }

  # Parse header using base R (optimized with pre-compiled regex)
  get_val <- function(key) {
    pattern <- paste0("^\", key, "\\s+")
    x <- grep(pattern, header, value = TRUE)
    if (!length(x)) stop("Missing PAM header field: ", key)
    as.integer(sub(pattern, "", x[1]))
  }

  width  <- get_val("WIDTH")
  height <- get_val("HEIGHT")
  depth  <- get_val("DEPTH")
  maxval <- get_val("MAXVAL")

  bytes_per_sample <- if (maxval <= 255) 1L else 2L

  ## --- Read binary payload (optimized) ---
  n <- width * height * depth
  data <- readBin(con, what = "raw", size = bytes_per_sample, n = n, signed = FALSE)
  if (length(data) != n) stop("File truncated: data size does not match header")

  # Convert to integer (optimized for speed)
  if (bytes_per_sample == 1L) {
    data <- as.integer(data)
  } else {
    data <- as.integer(readBin(rawConnection(data), integer0 = FALSE, n = n / 2, size = 2, signed = FALSE))
  }

  ## --- Build array (optimized) ---
  a <- array(data, dim = c(width, height, depth))
  attr(a, 'header') <- c(width = width, height = height, depth = depth, maxval = maxval)
  class(a) <- c('hsi', class(a))

  # Set wavelength dimnames (optimized)
  wl_values <- seq_len(depth) + (min_wl - 1)
  dimnames(a) <- list(
    x = as.character(seq_len(dim(a)[1])),
    y = as.character(seq_len(dim(a)[2])),
    wl = as.character(wl_values)
  )

  ## --- Generate grayscale thumbnail ---
  if (thumbnail) {
    a <- .generate_thumbnail(a, wl)
  }

  return(a)
}

#' Generate grayscale thumbnail from hsi array
#'
#' @description
#' Generates a downsampled grayscale thumbnail from a single wavelength slice of an hsi array.
#' Uses subset_hsi for efficient extraction and downsampling.
#'
#' @param a an `hsi` array
#' @param wl wavelength for the thumbnail (default: middle wavelength)
#' @return the input array with added `thumbnail` and `thumbnail_wavelength` attributes
#' @noRd
.generate_thumbnail <- function(a, wl = NULL) {

  # subset indexes
  x_idx <- seq(0, dim(a)[1], by = 4)
  y_idx <- seq(0, dim(a)[2], by = 4)

  # Default to middle wavelength if not specified
  wl_values <- as.integer(dimnames(a)[[3]])
  depth <- dim(a)[3]

  if (is.null(wl)) wl <- wl_values[round(depth / 2)]

  # Find index for the specified wavelength
  wl_idx <- which(wl_values == wl)
  if (length(wl_idx) == 0) {
    warning("Specified wavelength not found in data. Using middle wavelength.")
    wl_idx <- round(depth / 2)
    wl <- wl_values[wl_idx]
  }

  # Extract the wavelength slice as a matrix and scale to 0-255 (storage mode integer)
  downsampled <- subset_hsi(a, x_idx, y_idx, wl_idx)
  maxval <- max(downsampled)
  minval <- min(downsampled)
  downsampled <- ((downsampled - minval) / maxval) * 255
  storage.mode(downsampled) <- 'integer'

  # set attributes on thumbnail
  downsampled

  # Store the downsampled slice directly as the thumbnail
  attr(a, 'thumbnail') <- downsampled
  attr(a, 'thumbnail_wavelength') <- wl

  return(a)
}

#' Print method for hsi objects
#'
#' @description
#' Custom print method for hsi array objects that displays header information and thumbnail status.
#'
#' @param x an hsi array object
#' @param ... additional arguments
#' @return No return value, called for side effects
#' @export
#' @method print hsi
print.hsi <- function(x, ...) {
  cat("HSI Array Object\n")
  cat("================\n")
  cat("Dimensions:", paste(dim(x), collapse = " \u00d7 "), "\n")
  cat("Class:", paste(class(x), collapse = ", "), "\n")
  
  if (!missing(x) && inherits(x, "array") && "header" %in% names(attributes(x))) {
    header <- attr(x, "header")
    cat("\nHeader Information:\n")
    cat("  Width:", header["width"], "\n")
    cat("  Height:", header["height"], "\n")
    cat("  Depth:", header["depth"], "\n")
    cat("  Max Value:", header["maxval"], "\n")
  }
  
  if ("thumbnail" %in% names(attributes(x))) {
    thumb <- attr(x, "thumbnail")
    cat("\nThumbnail Information:\n")
    cat("  Dimensions:", paste(dim(thumb), collapse = " \u00d7 "), "\n")
    cat("  Wavelength:", attr(x, "thumbnail_wavelength"), "\n")
  }
  
  cat("\nWavelength Range:", min(as.integer(dimnames(x)[[3]])), "-", 
      max(as.integer(dimnames(x)[[3]])), " nm\n")
  
  invisible(x)
}

#' Summary method for hsi objects
#'
#' @description
#' Custom summary method for hsi array objects that provides statistical overview.
#' For memory efficiency, this method uses subsampling (every 4th x and y, every 10th wavelength)
#' to compute statistics on large HSI cubes.
#'
#' @param object an hsi array object
#' @param ... additional arguments
#' @param subsample logical indicating whether to use subsampling for large arrays (default: TRUE)
#' @param x_step step size for x dimension subsampling (default: 4)
#' @param y_step step size for y dimension subsampling (default: 4)
#' @param wl_step step size for wavelength dimension subsampling (default: 10)
#' @return A list containing summary statistics
#' @export
#' @method summary hsi
summary.hsi <- function(object, ..., subsample = TRUE, x_step = 4, y_step = 4, wl_step = 10) {
  if (!inherits(object, "hsi")) {
    stop("Object must be of class 'hsi'")
  }
  
  header <- attr(object, "header")
  wl_values <- as.integer(dimnames(object)[[3]])
  
  # Basic information
  result <- list(
    dimensions = dim(object),
    class = class(object),
    header = header,
    wavelength_range = range(wl_values),
    thumbnail_available = "thumbnail" %in% names(attributes(object))
  )
  
  if (result$thumbnail_available) {
    thumb <- attr(object, "thumbnail")
    result$thumbnail_dimensions <- dim(thumb)
    result$thumbnail_wavelength <- attr(object, "thumbnail_wavelength")
  }
  
  # For statistics, use subsampling for memory efficiency on large arrays
  if (subsample && prod(dim(object)) > 100000) {
    # Use subset_hsi for efficient subsampling
    x_indices <- seq(1, dim(object)[1], by = x_step)
    y_indices <- seq(1, dim(object)[2], by = y_step)
    wl_indices <- seq(1, dim(object)[3], by = wl_step)
    
    # Create subsampled version for statistics
    subsampled <- subset_hsi(object, x_range = x_indices, y_range = y_indices, wl_range = wl_indices)
    
    # Compute statistics on subsampled data
    result$statistics <- tapply(subsampled, INDEX = dimnames(subsampled)[[3]], 
                                FUN = function(x) c(
                                  min = min(x, na.rm = TRUE),
                                  max = max(x, na.rm = TRUE),
                                  mean = mean(x, na.rm = TRUE),
                                  sd = sd(x, na.rm = TRUE),
                                  median = median(x, na.rm = TRUE)
                                ))
    
    # Add subsampling information
    result$subsampling <- list(
      used = TRUE,
      x_step = x_step,
      y_step = y_step,
      wl_step = wl_step,
      subsampled_dimensions = dim(subsampled),
      original_dimensions = dim(object)
    )
    
    # Add note about subsampling
    result$note <- "Statistics computed on subsampled data for memory efficiency"
    
  } else {
    # For smaller arrays, use full data
    result$statistics <- tapply(object, INDEX = dimnames(object)[[3]], 
                                FUN = function(x) c(
                                  min = min(x, na.rm = TRUE),
                                  max = max(x, na.rm = TRUE),
                                  mean = mean(x, na.rm = TRUE),
                                  sd = sd(x, na.rm = TRUE),
                                  median = median(x, na.rm = TRUE)
                                ))
    
    result$subsampling <- list(used = FALSE)
    result$note <- "Statistics computed on full data"
  }
  
  class(result) <- "summary.hsi"
  return(result)
}

#' Plot method for hsi objects
#'
#' @description
#' Custom plot method for hsi array objects that displays the thumbnail if available.
#'
#' @param x an hsi array object
#' @param ... additional arguments
#' @return No return value, called for side effects
#' @export
#' @method plot hsi
plot.hsi <- function(x, ...) {
  if ("thumbnail" %in% names(attributes(x))) {
    thumb <- attr(x, "thumbnail")
    graphics::image(thumb, col = gray(0:255 / 255), 
                   main = paste("HSI Thumbnail - Wavelength:", attr(x, "thumbnail_wavelength")),
                   xlab = "X", ylab = "Y")
  } else {
    # If no thumbnail, plot the middle wavelength
    wl_values <- as.integer(dimnames(x)[[3]])
    depth <- dim(x)[3]
    wl_idx <- round(depth / 2)
    slice <- x[,, wl_idx, drop = TRUE]
    
    # Scale to 0-255 for display
    maxval <- attr(x, 'header')['maxval']
    slice_scaled <- (slice / maxval) * 255
    
    graphics::image(slice_scaled, col = gray(0:255 / 255),
                   main = paste("HSI Slice - Wavelength:", wl_values[wl_idx]),
                   xlab = "X", ylab = "Y")
  }
  
  invisible(x)
}
