#' Add a title to the tauchart plot
#'
#' This provides a hack-ish way to add a title to a tauchart. Heed the
#' warning in \code{return} well. This is the last function you should use
#' when building a \code{tauchart}.\cr
#' \cr
#' In interactive sessions this function uses \code{htmltools::html_print} to
#' display the chart with title, otherwise it just returns the \code{shiny.tag}-
#' classed object since that works well in R Markdown documents.\cr
#' \cr
#' This is most certainly a temporary hack.
#'
#' @param title title for the chart
#' @param div_css style to apply to the surrounding \code{div}. You should probably set
#'        the width here.
#' @param css_style CSS style to apply to the title \code{span}. The default is to center-align
#'        the text and using a bold font. Go. Crazy.
#' @param Not tested in a Shiny context yet
#' @return THIS DOES NOT RETURN AN \code{htmlwidget}!! It returns a \code{shiny.tag}
#'         class HTML (the widget is wrapped in a \code{<div>}). It should be the LAST
#'         call in a magrittr pipe chain or called to wrap a streamgraph object for
#'         output
#' @export
#' @examples
#' tauchart(mtcars) %>%
#'   tau_point("mpg", "wt") %>%
#'   tau_guide_y(auto_scale=FALSE) %>%
#'   tau_title("Some Really Good Plot Title",
#'             "font-weight:bold;margin:auto;text-align:center;font-size:24px;color:#7f7f7f;font-family:sans-serif")
#'
#' tauchart(mtcars) %>%
#'   tau_point("mpg", "wt") %>%
#'   tau_guide_y(auto_scale=FALSE) %>%
#'   tau_title("Some Really Good Plot Title",
#'             "font-weight:bold;margin:auto;text-align:right;font-size:24px;color:#7f7f7f;font-family:sans-serif")
#'
#' tauchart(mtcars, width="500px") %>%
#'   tau_point("mpg", "wt") %>%
#'   tau_guide_y(auto_scale=FALSE) %>%
#'   tau_title("Some Really Good Plot Title",
#'             "width:500px;font-weight:bold;margin:auto;text-align:left;font-size:24px;color:#7f7f7f;font-family:sans-serif")
tau_title <- function(tau, title="",
                      div_css="width:100%",
                      span_css="font-weight:bold;margin:auto;text-align:center") {
  plot_with_title <- div(style=div_css, span(style=span_css, title), br(), tau)
  if (interactive()) return(html_print(plot_with_title))
  return(plot_with_title)
}
