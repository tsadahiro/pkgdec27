#' Sample max-cell x trace from shifted SYT
#'
#' @param n size parameter
#' @return integer vector
#' @useDynLib pkgdec27, .registration=TRUE
#' @importFrom Rcpp sourceCpp
#' @examples
#' w <- shifted_hook_walk_xtrace(30)
#' tr <- trajectory(w, particles = c(1, 5 * (1:6)))
#' plot_trajectory(tr)
#' @export
shifted_hook_walk_xtrace <- function(n) {
  .Call(`_pkgdec27_shifted_hook_walk_xtrace`, n)
}
