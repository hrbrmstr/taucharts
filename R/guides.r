
#' Set overall chart padding
#'
#' @param tau taucharts object
#' @param bottom,top,left,right amount of padding
#' @references \url{http://api.taucharts.com/basic/guide.html}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#' tau_point("mpg", "wt") %>%
#'   tau_guide_padding(40, 40, 40, 40)
#' }
tau_guide_padding <- function(tau, bottom=0, left=0, top=0, right=0) {
  tau$x$guide$padding <- list(b=bottom, l=left, t=top, r=right)
  tau
}

#' Control showing of axis gridlines
#'
#' @param tau taucharts object
#' @param show_x,show_y if \code{TRUE}, show the gridline
#' @references \url{http://api.taucharts.com/basic/guide.html}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("mpg", "wt") %>%
#'   tau_guide_gridlines(FALSE, FALSE)
#' }
tau_guide_gridlines <- function(tau, show_x=TRUE, show_y=TRUE) {
  tau$x$guide$showGridLines <- paste0(switch(as.character(show_x),
                                             `TRUE`="x", `FALSE`=""),
                                      switch(as.character(show_y),
                                             `TRUE`="y", `FALSE`=""))
  tau
}

#' Control x-axis padding, label, scale & tick format
#'
#' @param tau taucharts object
#' @param padding space between axis ticks and chart panel
#' @param label text of label for axis (overrides default use of variable name)
#' @param label_padding space between axis ticks and axis label (can be negative)
#' @param nice Taucharts engine tries to make axis scale "nice" by trying to start measure-based scale from 0 and adds some margins to complete scale with "nice" numbers. For example, if original scale domain contains values [8, 20, ... 40], then axis will have ticks from 0 to 45.
#' (default: \code{TRUE}, if min and max values are set: \code{FALSE}).
#' @param auto_scale (Deprecated) auto-pick "best" scale for axis? (default: \code{TRUE} (yes))
#' @param tick_period if axis is auto-determined to be a "period scale",
#'        this allows specification of the period size. See \code{References} for more
#'        information.
#' @param tick_format can be any
#'   \href{https://github.com/mbostock/d3/wiki/Formatting#d3_format}{D3 format specifier}
#' @param min,max manual minimum and maximum for the axis
#' @references \url{http://api.taucharts.com/basic/guide.html},
#'             \url{https://github.com/mbostock/d3/wiki/Formatting#d3_format}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'  tau_point("mpg", "wt") %>%
#'  tau_guide_x(label="Miles/gallon", nice=FALSE) %>%
#'  tau_guide_y(label="Weight", nice=FALSE)
#' }
tau_guide_x <- function(tau, padding=NULL,
                        label=NULL, label_padding=NULL, nice=NULL,
                        auto_scale=NULL, tick_period=NULL, tick_format=NULL,
                        min=NULL, max=NULL) {

  tau$x$guide$x$autoScale <- auto_scale %^^% tau[["x"]][["guide"]][["x"]][["autoScale"]] %^^% TRUE

  if (!is.null(label_padding)) tau$x$guide$x$label <- list(padding=label_padding)
  if (!is.null(label)) tau$x$guide$x$label$text <- label
  if (!is.null(tick_format)) tau$x$guide$x$tickFormat <- tick_format
  if (!is.null(tick_period)) tau$x$guide$x$tickPeriod <- tick_period
  if (!is.null(padding)) tau$x$guide$x$padding <- padding
  if (!is.null(min)) tau$x$guide$x$min <- min
  if (!is.null(max)) tau$x$guide$x$max <- max
  if (!is.null(nice)) tau$x$guide$x$nice <- nice
  else {
    if (!is.null(min) && !is.null(max)) tau$x$guide$x$nice <- FALSE
    else tau$x$guide$x$nice <- TRUE
  }

  tau
}

#' Control y-axis padding, label, scale & tick format
#'
#' @param tau taucharts object
#' @param padding space between axis ticks and chart panel
#' @param label text of label for axis (overrides default use of variable name)
#' @param label_padding space between axis ticks and axis label (can be negative)
#' @param nice Taucharts engine tries to make axis scale "nice" by trying to start measure-based scale from 0 and adds some margins to complete scale with "nice" numbers. For example, if original scale domain contains values [8, 20, ... 40], then axis will have ticks from 0 to 45.
#' (default: \code{TRUE}, if min and max values are set: \code{FALSE}).
#' @param auto_scale (Deprecated) auto-pick "best" scale for axis? (default: \code{TRUE} (yes))
#' @param tick_period if axis is auto-determined to be a "period scale",
#'        this allows specification of the period size. See \code{References} for more
#'        information.
#' @param tick_format can be any
#'   \href{https://github.com/mbostock/d3/wiki/Formatting#d3_format}{D3 format specifier}
#' @param min,max manual minimum and maximum for the axis
#' @references \url{http://api.taucharts.com/basic/guide.html},
#'             \url{https://github.com/mbostock/d3/wiki/Formatting#d3_format}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'  tau_point("mpg", "wt") %>%
#'  tau_guide_x(label="Miles/gallon", nice=FALSE) %>%
#'  tau_guide_y(label="Weight", nice=FALSE)
#' }
tau_guide_y <- function(tau, padding=NULL,
                        label=NULL, label_padding=NULL, nice=NULL,
                        auto_scale=NULL, tick_period=NULL, tick_format=NULL,
                        min=NULL, max=NULL) {

  tau$x$guide$x$autoScale <- auto_scale %^^% tau[["x"]][["guide"]][["y"]][["autoScale"]] %^^% TRUE

  if (!is.null(label_padding)) tau$x$guide$y$label <- list(padding=label_padding)
  if (!is.null(label)) tau$x$guide$y$label$text <- label
  if (!is.null(tick_format)) tau$x$guide$y$tickFormat <- tick_format
  if (!is.null(tick_period)) tau$x$guide$y$tickPeriod <- tick_period
  if (!is.null(padding)) tau$x$guide$y$padding <- padding
  if (!is.null(min)) tau$x$guide$y$min <- min
  if (!is.null(max)) tau$x$guide$y$max <- max
  if (!is.null(nice)) tau$x$guide$y$nice <- nice
  else {
    if (!is.null(min) && !is.null(max)) tau$x$guide$y$nice <- FALSE
    else tau$x$guide$y$nice <- TRUE
  }

  tau
}
