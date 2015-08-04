#' Create a new TauChart
#'
#' Performs basic widget setup and returns an object suitable for
#' use with the other \code{tau_} functions.
#'
#' @param data data.frame
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @references \url{http://api.taucharts.com/}
#' @export
tauchart <- function(data, width = NULL, height = NULL) {

  # try to accomodate xts objects
  #  but this will require a dependency on xts
  if( inherits( data, "xts" ) ){
    data <- data.frame(
      "Date" = index(data)
      ,as.data.frame(data)
      ,stringsAsFactors = FALSE
    )
  }

  # this takes care of silly tbl_df/tbl_dt/data.table issues
  data <- data.frame(data)

  # try to handle dates smoothly between JS and R
  #   this is very much a work in progress
  date_columns <- which(sapply(data,function(x) inherits(x,c("Date","POSIXct"))))
  if (length(date_columns) > 0) {
    data[,date_columns] <- asISO8601Time(data[,date_columns])
    # temporarily set class to iso8601 for dimension logic below
    class(data[,date_columns]) <- "iso8601"
  }

  # try to determine the associated tau-type based on
  # column type/class.
  #
  # TODO: this is far from robust. amongst other things
  # it should figure out the date/time better if a character
  # and it should add the ordering of ordered factors

  dimensions <- lapply(data, function(v) {
    # if factor handle separately
    if(inherits(v, "factor")) {
      if(inherits(v, "ordered")){
        list(type = "order", order = levels(v) )
      } else {
        list(type = "category")
      }
    } else if(inherits(v, c("Date","iso8601"))) {
      # some crude handling of dates
      list( type = "order", scale = "time" )
    } else {
      list(`type` =
             switch(typeof(v),
                    double= "measure",
                    integer="measure",
                    logical="category",
                    character="category",
                    "measure")
      )

    }

  })

  # remove our temporary iso8601 class
  class(data[,which(sapply(data,function(x) inherits(x,"iso8601")))]) <- "character"

  # forward options using x
  x <- list(
    datasource=data,
    dimensions=dimensions,
    x=NULL,
    y=NULL,
    padding=NULL,
    guide=list(x=NULL, y=NULL, padding=NULL, color=NULL),
    forCSS=NULL
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'taucharts',
    x,
    width = width,
    height = height,
    package = 'taucharts'
  )
}

#' Create a TauCharts scatterplot
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to
#' @param size quoted name of \code{data} column to make size aesthetic to
#' @references \url{http://api.taucharts.com/basic/scatterplot.html}
#' @export
tau_point <- function(tau, x, y, color=NULL, size=NULL) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- "scatterplot"
  tau
}

#' Create a TauCharts bar chart (horizontal or vertical)
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to
#' @param size quoted name of \code{data} column to make size aesthetic to
#' @param horizontal should the bar chart be horizontal? (default: \code{FALSE} (no))
#' @references \url{http://api.taucharts.com/basic/line.html}
#' @export
tau_bar <- function(tau, x, y, color=NULL, size=NULL, horizontal=FALSE) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- switch(as.character(horizontal),
                       `TRUE`="horizontalBar",
                       `FALSE`="bar")
  tau
}

#' Create a TauCharts line chart
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to
#' @param size quoted name of \code{data} column to make size aesthetic to
#' @references \url{http://api.taucharts.com/basic/bar.html},
#'             \url{http://api.taucharts.com/basic/horizontal-bar.html}
#' @export
tau_line <- function(tau, x, y, color=NULL, size=NULL) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- "line"
  tau
}


#' Set overall chart padding
#'
#' @param tau taucharts object
#' @param bottom,top,left,right amount of padding
#' @references \url{http://api.taucharts.com/basic/guide.html}
#' @export
tau_guide_padding <- function(tau, bottom=0, left=0, top=0, right=0) {
  # padding: {b: 70, l: 70, t: 10, r: 10},
  tau$x$guide$padding <- list(b=bottom, l=left, t=top, r=right)
  tau
}

#' Control showing of axis gridlines
#'
#' @param tau taucharts object
#' @param show_x,show_y if \code{TRUE}, show the gridline
#' @references \url{http://api.taucharts.com/basic/guide.html}
#' @export
tau_guide_gridlines <- function(tau, show_x=TRUE, show_y=TRUE) {
  # showGridLines: 'xy',
  tau$x$showGridLines <- paste0(switch(as.character(show_x),
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
#' @param auto_scale auto-pick "best" scale for axis? (default: \code{TRUE} (yes))
#' @param tick_period if axis is auto-determined to be a "period scale",
#'        this allows specification of the period size. See \code{References} for more
#'        information.
#' @param tick_format can be any
#'   \href{https://github.com/mbostock/d3/wiki/Formatting#d3_format}{D3 format specifier}
#' @references \url{http://api.taucharts.com/basic/guide.html},
#'             \url{https://github.com/mbostock/d3/wiki/Formatting#d3_format}
#' @export
tau_guide_x <- function(tau, padding=NULL,
                        label=NULL, label_padding=NULL,
                        auto_scale=TRUE, tick_period=NULL, tick_format=NULL) {
  tau$x$guide$x$autoScale <- auto_scale
  if (!is.null(label_padding)) tau$x$guide$x$label <- list(padding=label_padding)
  if (!is.null(label)) tau$x$guide$x$label$text <- label
  if (!is.null(tick_format)) tau$x$guide$x$tickFormat <- tick_format
  if (!is.null(tick_period)) tau$x$guide$y$tickPeriod <- tick_period
  if (!is.null(padding)) tau$x$guide$x$padding <- padding
  tau
}

#' Control y-axis padding, label, scale & tick format
#'
#' @param tau taucharts object
#' @param padding space between axis ticks and chart panel
#' @param padding space between axis ticks and chart panel
#' @param label text of label for axis (overrides default use of variable name)
#' @param label_padding space between axis ticks and axis label (can be negative)
#' @param auto_scale auto-pick "best" scale for axis? (default: \code{TRUE} (yes))
#' @param tick_period if axis is auto-determined to be a "period scale",
#'        this allows specification of the period size. See \code{References} for more
#'        information.
#' @param tick_format can be any
#'   \href{https://github.com/mbostock/d3/wiki/Formatting#d3_format}{D3 format specifier}
#' @references \url{http://api.taucharts.com/basic/guide.html},
#'             \url{https://github.com/mbostock/d3/wiki/Formatting#d3_format}
#' @export
tau_guide_y <- function(tau, padding=NULL,
                        label=NULL, label_padding=NULL,
                        auto_scale=TRUE, tick_period=NULL, tick_format=NULL) {
  tau$x$guide$y$autoScale <- auto_scale
  if (!is.null(label_padding)) tau$x$guide$y$label <- list(padding=label_padding)
  if (!is.null(label)) tau$x$guide$y$label$text <- label
  if (!is.null(tick_period)) tau$x$guide$y$tickPeriod <- tick_period
  if (!is.null(tick_format)) tau$x$guide$y$tickFormat <- tick_format
  if (!is.null(padding)) tau$x$guide$y$padding <- padding
  tau
}

#' Specify the colors used in the charts
#'
#' @param tau taucharts object
#' @param vector of colors, ideally names (e.g. "\code{black}") or
#'        hex-format (e.g. "\code{#ffeeaa}")
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
tau_color_manual <- function(tau, values=NULL) {
  if (is.null(values)) return(tau)
  eids <- sapply(1:length(values), function(i) { sprintf("tau-fill-%d-%s", i,
                                                         paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau$x$forCSS <- sprintf(".%s { fill: %s; }", eids, values)
  tau
}

# # @export
# tau_color_brewer <- function(tau, palette="", n=5) {
#   if (palette %in% brewers) {
#     tau$x$guide$color <- JS(sprintf("tauBrewer('%s',%d)", palette, n))
#   }
#   tau
# }

#' Shiny bindings for taucharts
#'
#' Output and render functions for using taucharts within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a taucharts
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name taucharts-shiny
#'
#' @export
tauchartsOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'taucharts', width, height, package = 'taucharts')
}

#' @rdname taucharts-shiny
#' @export
renderTaucharts <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, tauchartsOutput, env, quoted = TRUE)
}
