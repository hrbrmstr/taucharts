#' Turn a simple (single-geom) ggplot plot into an tauchart object
#'
#' Takes a ggplot object that has a single geom (it can be geom_line,
#' geom_point or geom_histogram) and converts it to it's taucahrt counterpart.
#' It will do it's best to identify plot labels, mapped size & color aesthetics,
#' and x/y limits.\cr
#' \cr
#' If there are aesthetic mappings, \code{as_tauchart} will automaticlly add
#' a legend.
#'
#' @note More aesthetic mappings are planned
#' @param gg ggplot object
#' @return tauchart object
#' @export
#' @examples
#' dat <- data.frame(year=seq(1790, 1970, 10),
#'                   uspop=as.numeric(uspop))
#' set.seed(5689)
#' data(movies, package="ggplot2")
#' movies <- movies[sample(nrow(movies), 1000), ]
#'
#' gg <- ggplot(dat, aes(x=year, y=uspop)) + geom_line()
#' as_tauchart(gg)
#'
#' gg <- ggplot(dat, aes(x=year, y=uspop)) + geom_point()
#' as_tauchart(gg)
#'
#' gg <- ggplot(dplyr::count(movies, rating), aes(rating, n)) + geom_bar(stat="identity")
#' as_tauchart(gg)
#'
#' gg <- ggplot(mtcars) + geom_point(aes(x=mpg, y=wt, color=cyl))
#' as_tauchart(gg)
#'
#' gg <- ggplot(mtcars, aes(x=mpg, y=wt, color=am, size=wt)) + geom_point()
#' as_tauchart(gg)
#'
#' data(economics, package="ggplot2")
#' gg <- ggplot(economics) + geom_line(aes(x=date, y=unemploy))
#' as_tauchart(gg) %>% tau_guide_x(tick_format="%Y")
as_tauchart <- function(gg) {

  if (!inherits(gg, c("gg", "ggplot"))) {
    stop("as_tauchart only works with ggplot objects", call.=FALSE)
  }

  gb <- ggplot_build(gg)

  if (length(gb$plot$layers) > 1) {
    stop("as_tauchart only works with single-layer-geoms", call.=FALSE)
  }

  plot_type <- gb$plot$layers[[1]]$geom$objname

  x <- as.character(gb$plot$mapping$x %||% gb$plot$layers[[1]]$mapping$x %||% NULL)
  y <- as.character(gb$plot$mapping$y %||% gb$plot$layers[[1]]$mapping$y %||% NULL)
  color <- gb$plot$mapping$colour %||% gb$plot$layers[[1]]$mapping$colour %||% NULL
  size <- gb$plot$mapping$size %||% gb$plot$layers[[1]]$mapping$size %||% NULL

  color <- grep("factor", as.character(color), value=TRUE, invert=TRUE) %||% NULL
  size <- grep("factor", as.character(size), value=TRUE, invert=TRUE) %||% NULL

  r_x <- gb$panel$ranges[[1]]$x.range
  r_y <- gb$panel$ranges[[1]]$y.range

  data <- gb$plot$data

  tc <- NULL

  if (plot_type=="line") {
    tau_guide_y(
      tau_guide_x(
        tau_line(tauchart(data), x=x, y=y, color=color, size=size),
        auto_scale=FALSE, label=gb$plot$labels$x, min=r_x[1], max=r_x[2]),
      auto_scale=FALSE, label=gb$plot$labels$y, min=r_y[1], max=r_y[2]) -> tc
  } else if (plot_type=="point") {
    tau_guide_y(
      tau_guide_x(
        tau_point(tauchart(data), x=x, y=y, color=color, size=size),
        auto_scale=FALSE, label=gb$plot$labels$x, min=r_x[1], max=r_x[2]),
      auto_scale=FALSE, label=gb$plot$labels$y, min=r_y[1], max=r_y[2]) -> tc
  } else if (plot_type=="bar") {
    tau_guide_y(
      tau_guide_x(
        tau_bar(tauchart(data), x=x, y=y, color=color, size=size),
        auto_scale=FALSE, label=gb$plot$labels$x, min=r_x[1], max=r_x[2]),
      auto_scale=FALSE, label=gb$plot$labels$y, min=r_y[1], max=r_y[2]) -> tc
  } else {
    stop("as_tauchart only works with geom_line, geom_point and geom_histogram", call.=FALSE)
  }

  if (!is.null(color) | !is.null(size)) tc <- tau_legend(tc)

  tc

}

