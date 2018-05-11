#' Create a TauCharts scatterplot
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to
#' @param size quoted name of \code{data} column to make size aesthetic to
#' @references \url{http://api.taucharts.com/basic/scatterplot.html}
#' @export
#' @examples
#' tauchart(mtcars) %>% tau_point("mpg", "wt")
#' mtcars$cyl <- factor(mtcars$cyl)
#' tauchart(mtcars) %>% tau_point("mpg", "wt", "cyl", "hp")
tau_point <- function(tau, x, y, color=NULL, size=NULL) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- "scatterplot"
  tau
}

#' Create a TauCharts box plot (horizontal or vertical)
#'
#' There is no option for color mappings as the tauCharts plugin does not support this yet.
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param horizontal should the bar chart be horizontal? (default: \code{FALSE} (no))
#' @param mode when to show scatter points? e.g. "hide-scatter", "show-scatter", "outliers-only"
#' @export
#' @examples
#' tauchart(mtcars) %>%
#'   tau_boxplot("gear", "mpg", mode="hide-scatter") %>%
#'   tau_guide_y(nice=FALSE)
tau_boxplot <- function(tau, x, y, horizontal=FALSE, mode="outliers-only") {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$type <- "scatterplot"

  if (is.null(tau$x$plugins)){
    tau$x$plugins = list()
  }

  tau$x$plugins[[length(tau$x$plugins) + 1]] =  list(
    type = "box-whiskers"
    ,settings = list(
      flip = horizontal,
      mode = `mode`
    )
  )

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
#' @examples
#' data(mpg, package="ggplot2")
#' tauchart(dplyr::count(mpg, class)) %>%
#'   tau_bar("class", "n")
#'
#' # facets
#' tauchart(dplyr::count(mpg, manufacturer, class)) %>%
#'   tau_bar("class", c("manufacturer", "n"))
#' #facets
#' mfclass <- dplyr::count(mpg, manufacturer, model, class)
#' tauchart(mfclass) %>%
#'   tau_bar(c("manufacturer", "model"), "n")
#'
#' # ordered factors on x-axis
#' mfclass$class <- factor(mfclass$class,
#'                         levels=c("2seater", "subcompact", "compact",
#'                                  "midsize", "minivan",  "suv", "pickup"),
#'                         ordered=TRUE)
#' tauchart(mfclass) %>%
#'   tau_bar(c("class", "manufacturer"), "n")
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

#' Create a TauCharts stacked bar chart (experimental)
#'
#' The API supports it but it's not documented at all
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to.
#'        NOTE that the parameter to this is what really defines the stacking.
#' @param size quoted name of \code{data} column to make size aesthetic to
#' @param horizontal should the bar chart be horizontal? (default: \code{FALSE} (no))
#' @references \url{http://api.taucharts.com/basic/line.html}
#' @export
#' @examples
#' data(mpg, package="ggplot2")
#' tauchart(dplyr::count(mpg, class, drv)) %>%
#'   tau_stacked_bar("class", "n", "drv") %>%
#'   tau_guide_gridlines(FALSE, FALSE) %>%
#'   tau_tooltip()
tau_stacked_bar <- function(tau, x, y, color=NULL, size=NULL, horizontal=FALSE) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- switch(as.character(horizontal),
                       `TRUE`="horizontal-stacked-bar",
                       `FALSE`="stacked-bar")
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
#' @examples
#' data(economics, package="ggplot2")
#' tauchart(economics) %>%
#'   tau_line("date", "unemploy") %>%
#'   tau_guide_x(tick_format="%Y")
#'
#' # facets
#' library(dplyr)
#' library(tidyr)
#'
#' crimes <- gather(add_rownames(USArrests, "State"), Crime, Amount, -State)
#' tauchart(crimes) %>%
#'   tau_line("State", c("Crime", "Amount"), "Crime") %>%
#'   tau_guide_y(auto_scale = FALSE)
tau_line <- function(tau, x, y, color=NULL, size=NULL) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$size <- size
  tau$x$type <- "line"
  tau
}

#' Create a TauCharts area chart
#'
#' @param tau taucharts object
#' @param x quoted name of \code{data} column to use for x-axis values
#' @param y quoted name of \code{data} column to use for y-axis values
#' @param color quoted name of \code{data} column to map color aesthetic to
#' @references \url{http://api.taucharts.com/basic/bar.html},
#'             \url{http://api.taucharts.com/basic/horizontal-bar.html}
#' @export
#' @examples
#' data(economics, package="ggplot2")
#' tauchart(economics) %>%
#'   tau_area("date", "unemploy") %>%
#'   tau_guide_x(tick_format="%Y")
#'
#' # facets
#' library(dplyr)
#' library(tidyr)
#'
#' crimes <- gather(add_rownames(USArrests, "State"), Crime, Amount, -State)
#' tauchart(crimes) %>%
#'   tau_area("State", c("Crime", "Amount"), "Crime") %>%
#'   tau_guide_y(auto_scale = FALSE)
tau_area <- function(tau, x, y, color=NULL) {
  tau$x$x <- x
  tau$x$y <- y
  tau$x$color <- color
  tau$x$type <- "area"
  tau
}
