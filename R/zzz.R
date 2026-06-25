#' @importFrom data.table data.table setDT setnames := .N .SD .I .BY
#' @importFrom matrixStats rowMins rowMaxs colMins colMaxs rowMeans2 colMeans2 rowMedians colMedians rowSums2 colSums2
#' @importFrom fastmatrix fastMatrix
#' @importFrom collapse fmean fmedian fsum fprod
"_PACKAGE"

# On load messages
.onLoad <- function(libname, pkgname) {
    # Check if required packages are available
    if (!all(hsir::check_packages())) {
        warning("Some recommended packages are not available. " \
                "Install them for optimal performance: install.packages(c('data.table', 'matrixStats', 'fastmatrix', 'collapse'))")
    }
}

# On attach messages
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("hsir - High-Speed Image Recognition v", utils::packageVersion(pkgname))
}
