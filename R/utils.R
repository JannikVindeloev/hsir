#' High-Speed Image Recognition Utilities
#'
#' @description A collection of lightweight, fast and memory-efficient functions
#'   for image recognition and processing tasks.
#' @name hsir
#' @docType package
#' @importFrom data.table data.table setDT setnames := .N .SD .I .BY
#' @importFrom matrixStats rowMins rowMaxs colMins colMaxs rowMeans2 colMeans2
#' @importFrom fastmatrix fastMatrix
#' @importFrom collapse fmean fmedian fsum fprod
NULL

#' Check if required packages are available
#'
#' @description Verifies that all required optimized packages are available.
#' @return A logical vector indicating package availability.
#' @export
#' @examples
#' check_packages()
check_packages <- function() {
    required_packages <- c("data.table", "matrixStats", "fastmatrix", "collapse")
    sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))
}

#' Create a lightweight data structure for image data
#'
#' @description Creates an optimized data structure for storing image data
#'   using data.table for memory efficiency.
#' @param data A matrix or array of image data
#' @param ... Additional arguments to pass to data.table
#' @return A data.table object optimized for image processing
#' @export
#' @examples
#' img_data <- matrix(1:100, ncol = 10)
#' img_dt <- create_image_dt(img_data)
create_image_dt <- function(data, ...) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("data.table package is required for this function")
    }
    
    if (is.matrix(data)) {
        dt <- as.data.table(data, ...)
        setDT(dt)
        return(dt)
    } else if (is.array(data)) {
        # For multi-dimensional arrays, we'll flatten and add dimension info
        dims <- dim(data)
        flat_data <- as.vector(data)
        dt <- data.table(
            value = flat_data,
            row = rep(seq_len(dims[1]), each = prod(dims[-1])),
            col = rep(seq_len(dims[2]), times = dims[1], each = prod(dims[-1:-2])),
            ...
        )
        setDT(dt)
        return(dt)
    } else {
        stop("Input data must be a matrix or array")
    }
}

#' Fast matrix operations for image processing
#'
#' @description Performs fast matrix operations optimized for image data.
#' @param x A numeric matrix
#' @param operation Character string specifying the operation
#'   ("mean", "median", "sum", "min", "max")
#' @param na.rm Logical indicating whether to remove NA values
#' @return A vector of results for each row or column
#' @export
#' @examples
#' mat <- matrix(1:100, ncol = 10)
#' fast_matrix_ops(mat, "mean")
fast_matrix_ops <- function(x, operation = c("mean", "median", "sum", "min", "max"), na.rm = TRUE) {
    operation <- match.arg(operation)
    
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
        stop("matrixStats package is required for this function")
    }
    
    switch(operation,
           "mean" = {
               if (nrow(x) > ncol(x)) {
                   matrixStats::rowMeans2(x, na.rm = na.rm)
               } else {
                   matrixStats::colMeans2(x, na.rm = na.rm)
               }
           },
           "median" = {
               if (nrow(x) > ncol(x)) {
                   matrixStats::rowMedians(x, na.rm = na.rm)
               } else {
                   matrixStats::colMedians(x, na.rm = na.rm)
               }
           },
           "sum" = {
               if (nrow(x) > ncol(x)) {
                   matrixStats::rowSums2(x, na.rm = na.rm)
               } else {
                   matrixStats::colSums2(x, na.rm = na.rm)
               }
           },
           "min" = {
               if (nrow(x) > ncol(x)) {
                   matrixStats::rowMins(x, na.rm = na.rm)
               } else {
                   matrixStats::colMins(x, na.rm = na.rm)
               }
           },
           "max" = {
               if (nrow(x) > ncol(x)) {
                   matrixStats::rowMaxs(x, na.rm = na.rm)
               } else {
                   matrixStats::colMaxs(x, na.rm = na.rm)
               }
           }
    )
}

#' Convert hsi array to data.table
#'
#' @description
#' Efficiently converts an hsi array to a data.table with x, y, wl, and intensity columns.
#' This function is optimized for memory efficiency by avoiding unnecessary copies and 
#' using direct array indexing.
#'
#' @param x an hsi array object
#' @param ... additional arguments (currently unused)
#' @return A data.table with columns: x (integer), y (integer), wl (integer), intensity (integer)
#' @export
#' @examples
#' \dontrun{
#' # Assuming sample_cube is an hsi object
#' dt <- as_dt(sample_cube)
#' head(dt)
#' }
as_dt <- function(x, ...) {
  if (!inherits(x, "hsi")) {
    stop("Input must be an hsi array object")
  }

  # Get dimensions
  x_dim <- dim(x)[1]
  y_dim <- dim(x)[2]
  wl_dim <- dim(x)[3]
  
  # Create index vectors without copying data
  x_indices <- rep(seq_len(x_dim), each = y_dim * wl_dim)
  y_indices <- rep(rep(seq_len(y_dim), each = wl_dim), times = x_dim)
  wl_indices <- rep(seq_len(wl_dim), times = x_dim * y_dim)
  
  # Extract intensity values directly from the array (no copy)
  # Using as.vector which is efficient for arrays
  intensity_values <- as.vector(x)
  
  # Ensure intensity is integer (should already be from read_pam)
  storage.mode(intensity_values) <- "integer"
  
  # Create data.table efficiently
  dt <- data.table::data.table(
    x = x_indices,
    y = y_indices,
    wl = as.integer(wl_indices),
    intensity = intensity_values
  )
  
  return(dt)
}
