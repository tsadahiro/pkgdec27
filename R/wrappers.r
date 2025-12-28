#' Sample max-cell x trace from shifted SYT
#'
#' @param n size parameter
#' @return integer vector
#' @useDynLib pkgdec27, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @export
shifted_hook_walk_xtrace <- function(n) {
  .Call(`_pkgdec27_shifted_hook_walk_xtrace`, n)
}
