#' Specify the colors used in the charts
#'
#' @param tau taucharts object
#' @param vector of colors, ideally names (e.g. "\code{black}") or
#'        hex-format (e.g. "\code{#ffeeaa}")
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
tau_color_manual <- function(tau, values=NULL) {
  if (is.null(values)) return(tau)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau$x$forCSS <- sprintf(".%s { fill: %s; }", eids, values)
  tau
}
