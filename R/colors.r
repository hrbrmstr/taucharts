#' Specify the colors used in the charts
#'
#' Vector can be optionally named in order to give explicit color/value mapping.
#'
#' @param tau taucharts object
#' @param values vector of colors, ideally names (e.g. "\code{black}") or
#'        hex-format (e.g. "\code{#ffeeaa}")
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_manual(c("blue", "maroon", "black"))
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_manual(c(`4`="blue",`6`= "maroon",`8`= "black"))
#' }
tau_color_manual <- function(tau, values=NULL) {
  tau$x$dimensions[tau$x$color] <- "category"
  if (is.null(values)) return(tau)
  if(!is.null(names(values))) {
    tau$x$guide$color$brewer <- lapply(values,function(x) {
      rgb <- grDevices::col2rgb(x)
      sprintf("rgb(%d,%d,%d)",rgb[1],rgb[2],rgb[3])
    })
    return(tau)
  }
  eids <- lapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}

#' Use the ColorBrewer palette in the charts
#'
#' @param tau taucharts object
#' @param n number of colors to generate (no checking is done to ensure the palette
#'        has \code{n} colors).
#' @param palette ColorBrewer palette name. One of \code{"Set2"}, \code{"BrBG"},
#'   \code{"PiYG"}, \code{"PRGn"}, \code{"PuOr"}, \code{"RdBu"}, \code{"RdGy"}, \code{"RdYlBu"},
#'   \code{"RdYlGn"}, \code{"Spectral"}, \code{"Accent"}, \code{"Dark2"}, \code{"Paired"},
#'   \code{"Pastel1"}, \code{"Pastel2"}, \code{"Set1"}, \code{"Set3"}, \code{"Blues"},
#'   \code{"BuGn"}, \code{"BuPu"}, \code{"GnBu"}, \code{"Greens"}, \code{"Greys"}, \code{"Oranges"},
#'   \code{"OrRd"}, \code{"PuBu"}, \code{"PuBuGn"}, \code{"PuRd"}, \code{"Purples"},
#'   \code{"RdPu"}, \code{"Reds"}, \code{"YlGn"}, \code{"YlGnBu"}, \code{"YlOrBr"}, \code{"YlOrRd"}.
#' @import RColorBrewer
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @note It is highly suggested that callers use the ColorBrewer qualitative palettes:
#'       ["Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"],
#'       especially if you are plotting categorical values (which you most liklely are
#'       since you're using this package).
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_brewer(3, "Set3")
#' }
tau_color_brewer <- function(tau, n=5, palette="Set2") {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- RColorBrewer::brewer.pal(n, palette)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}


#' Use the Tableau palette in the charts
#'
#' @param tau taucharts object
#' @param palette Tableau palette name. One of \code{"tableau20"},
#'  \code{"tableau10medium"}, \code{"gray5"}, \code{"colorblind10"},
#'  \code{"purplegray12"}, \code{"bluered12"}, \code{"greenorange12"}, \code{"cyclic"}.
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_tableau()
#' }
tau_color_tableau <- function(tau, palette="tableau20") {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- tableau_colors(palette)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}

#' Use the "Economist" palette used in the charts
#'
#' The hues in the palette are blues, grays, and greens. Red is not included in
#' these palettes and should be used to indicate important data.
#'
#' @param tau taucharts object
#' @param n number of desired colors
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @seealso \code{\link[ggthemes]{economist_pal}}
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_economist()
#' }
tau_color_economist <- function(tau, n=5) {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- ggthemes::economist_pal()(n)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}

#' Use the "Few" palette used in the charts
#'
#' Qualitative color palettes from Stephen Few, "Practical Rules for Using Color in Charts".
#'
#' @param tau taucharts object
#' @param n number of desired colors
#' @param palette name of Few palette. One of \code{"medium"}, \code{"dark"} or \code{"light"}
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @seealso \code{\link[ggthemes]{few_pal}}
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_few()
#' }
tau_color_few <- function(tau, n=5, palette="medium") {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- ggthemes::few_pal(palette)(n)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}


#' Use the "fivethirtyeight.com" palette used in the charts
#'
#' The standard fivethirtyeight.com palette for line plots is blue, red, green.
#'
#' @param tau taucharts object
#' @param n number of desired colors
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @seealso \code{\link[ggthemes]{fivethirtyeight_pal}}
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_538()
#' }
tau_color_538 <- function(tau, n=5) {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- ggthemes::fivethirtyeight_pal()(n)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}

#' Use the HighchartsJS palette used in the charts
#'
#' Qualitative color palettes from Stephen Few, "Practical Rules for Using Color in Charts".
#'
#' @param tau taucharts object
#' @param n number of desired colors
#' @param palette name of Few palette. One of \code{"default"} or \code{"darkunica"}
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @seealso \code{\link[ggthemes]{hc_pal}}
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_highcharts()
#' }
tau_color_highcharts <- function(tau, n=5, palette="default") {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- ggthemes::hc_pal(palette)(n)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}


#' Use the "Wall Street Journal" palette used in the charts
#'
#' A subset of the plethora of palettes used by the WSJ.
#'
#' @param tau taucharts object
#' @param n number of desired colors
#' @param palette name of Few palette. One of \code{"rgby"}, \code{"green_red"},
#'   \code{"green_black"}, \code{"dem_rep"} or \code{"colors6"}
#' @references \url{http://api.taucharts.com/advanced/encoding.html}
#' @export
#' @seealso \code{\link[ggthemes]{wsj_pal}}
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_wsj()
#' }
tau_color_wsj <- function(tau, n=4, palette="rgby") {
  tau$x$dimensions[tau$x$color] <- "category"
  values <- ggthemes::wsj_pal(palette)(n)
  eids <- sapply(1:length(values), function(i) {
    sprintf("tau-fill-%d-%s", i,
            paste(sample(c(letters[1:6], 0:9), 6, replace=TRUE), collapse="")) })
  tau$x$guide$color$brewer <- eids ;
  tau_add_css_rule(tau, c(
    sprintf("{{ID}} .%s { fill: %s; }", eids, values),
    sprintf("{{ID}} div .graphical-report__legend__guide.%s { background: %s; border: 1px solid %s; }", eids, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__trendline.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__svg .graphical-report__line.%s { background: %s; border: 1px solid %s; stroke: %s; }", eids, values, values, values),
    sprintf("{{ID}} div .graphical-report__legend__item.disabled .graphical-report__legend__guide.%s { background: 0 0; background-color: transparent; }", eids, values, values)
  ))
}
