#' Add a TauCharts tooltip
#'
#' @param tau taucharts object
#' @param fields character vector of fields to display in the tooltip
#'        (default is to use all columns in \code{data})
#' @seealso \code{\link{cars_data}} dataset
#' @export
#' @examples
#' if (interactive()) {
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_tooltip(c("vehicle", "year", "class", "price", "milespergallon"))
#' }
tau_tooltip <- function(tau, fields = NULL) {

  if (is.null(fields)){
    fields <- colnames(tau$x$datasource)
  }

  if (is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "tooltip"
    ,fields = fields
  )

  tau
}

#' Add a TauCharts legend
#'
#' @param tau taucharts object
#' @export
#' @seealso \code{\link{cars_data}} dataset
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_legend()
tau_legend <- function(tau) {

  if (is.null(tau$x$color) & is.null(tau$x$size)) {
    message("Neither color nor size aesthetics have been mapped. Legend plugin will be active but not displayed.")
  }

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "legend"
  )

  tau
}

#' Add a TauCharts trendline
#'
#' @param tau taucharts object
#' @param type \code{character} either 'linear', 'exponential', or 'logarithmic'
#'                representing the default trend line to show.  NOTE:  this does not
#'                seem to work as expected.  Use the \code{models} parameter instead.
#' @param hideError \code{logical} to show errors.
#' @param showPanel \code{logical} to show the panel next to the chart to allow a user
#'                to manipulate the trendlines.  When \code{FALSE}, the trendlines will
#'                still appear though.
#' @param showTrend \code{logical} to show the trendlines on initial display.  If
#'                \code{showPanel = TRUE}, then the user will have the opportunity
#'                to add/delete the trendlines.
#' @param models \code{character} or \code{vector} of \code{characters} for the models
#'                to show in the trendline panel if \code{showPanel = TRUE}.  As discussed
#'                above in \code{type}, \code{models} also seems to be the only way
#'                to change the initial \code{type} of the trendline.  So, if you would like
#'                \code{exponential} to display, then set \code{models = "exponential"}. If you
#'                would like to change the order of the options, then you can do
#'                \code{models = c("logarithmic","exponential")}, and the first provided
#'                will be the initial model type used.
#' @seealso \code{\link{cars_data}} dataset
#' @export
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_trendline()
tau_trendline <- function(
  tau,
  type = 'linear',
  hideError = FALSE,
  showPanel = TRUE,
  showTrend = TRUE,
  models = c('linear', 'exponential', 'logarithmic')
) {

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "trendline"
    ,settings = list(
      type = 'linear',
      hideError = hideError,
      showPanel = showPanel,
      showTrend = showTrend,
      models = models
    )
  )

  tau
}

#' Add a TauCharts quick filter plugin
#'
#' @param tau taucharts object
#' @seealso \code{\link{cars_data}} dataset
#' @export
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_quick_filter()
tau_quick_filter <- function(
  tau
) {

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "quick-filter"
  )

  tau
}

#' Add a TauCharts export plugin
#'
#' @param tau taucharts object
#' @seealso \code{\link{cars_data}} dataset
#' @export
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_quick_filter()
tau_export_plugin <- function(
  tau
) {

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "exportTo"
    # ,settings = list(
    #   type = 'linear',
    #   hideError = hideError,
    #   showPanel = showPanel,
    #   showTrend = showTrend,
    #   models = models
    # )
  )

  tau
}

#' Add a TauCharts annotations plugin
#'
#' @param tau taucharts object
#' @seealso \code{\link{cars_data}} dataset
#' @export
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_annotations(data.frame(dim = "y", val = 50000,
#'            text = "Whoa there!", position = "front",
#'            color = '#4300FF'))
tau_annotations <- function(
  tau, annotation_df
) {
  if(!all(colnames(annotation_df) %in% c("dim", "val", "text", "position", "color"))){
    warning('Columns must be in c("dim", "val", "text", "position", "color")')
  }

  if(is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

# need to do d3 date conversion magic for
# annotations_df$val
# if(!all(annotations_df$value %in% c("x", "y"))){
#   warning("All values must be either numeric or date")
# }

  annotation_df$text <- as.character(annotation_df$text)
  if(!all(grepl("^#[[:alnum:]]{1,6}$", annotation_df$color))){
    warning("All colors must be hex")
  }

  if(!all(annotation_df$dim %in% c("x", "y"))){
    # warning("All dim must be either 'x' or 'y'")
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "annotations",
    items = annotation_df
  )

  tau
}
