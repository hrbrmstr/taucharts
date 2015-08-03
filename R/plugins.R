#' Add a TauCharts tooltip
#'
#' @param tau taucharts object
#' @param fields \code{vector} of fields you would like to see in the tooltip.
#'          If \code{NULL} then will assume all fields.
#' @export
tau_tooltip <- function(tau, fields = NULL) {

  if(is.null(fields)){
    fields <- colnames(tau$x$datasource)
  }

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "tooltip"
    ,fields = fields
  )

  tau
}

#' Add a TauCharts tooltip
#'
#' @param tau taucharts object
#' @importFrom jsonlite toJSON
#' @export
tau_legend <- function(tau, fields = NULL) {

  if(is.null(fields)){
    fields <- colnames(tau$x$datasource)
  }

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "legend"
  )

  tau
}
