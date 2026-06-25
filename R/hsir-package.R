#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage 
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @export
## usethis namespace: end
NULL

#' High-Speed Image Recognition Package
#'
#' @description
#' The hsir package provides lightweight, fast and memory-efficient functions for 
#' image recognition and processing tasks, with a focus on hyperspectral imaging (HSI) data.
#'
#' @details
#' The package includes:
#' \itemize{
#'   \item Optimized functions for reading and processing HSI data in PAM format
#'   \item Memory-efficient data structures using data.table
#'   \item Fast matrix operations using matrixStats and fastmatrix
#'   \item Grayscale thumbnail generation for ROI selection
#'   \item Custom print, summary, and plot methods for hsi objects
#' }
#'
#' @name hsir
#' @docType package
#' @importFrom data.table data.table setDT setnames := .N .SD .I .BY
#' @importFrom matrixStats rowMins rowMaxs colMins colMaxs rowMeans2 colMeans2 rowMedians colMedians rowSums2 colSums2
#' @importFrom fastmatrix fastMatrix
#' @importFrom collapse fmean fmedian fsum fprod
#' @examples
#' # Check if all required packages are available
#' check_packages()
#'
#' # Create an optimized data structure for image data
#' img_data <- matrix(1:100, ncol = 10)
#' img_dt <- create_image_dt(img_data)
#'
#' # Perform fast matrix operations
#' mat <- matrix(1:100, ncol = 10)
#' fast_matrix_ops(mat, "mean")
NULL
