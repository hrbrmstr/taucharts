#' Set the font family for the TauCharts chart
#'
#' Set the font family (standard CSS \code{font-family} specification, comma-separated
#' font names only) and an optional font CSS URL to load the font from.
#'
#' @param family CSS \code{font-family} spec (just the comma-separated font names)
#' @param import_url the \code{<link>} href if the fonts come from an external source
#' @return taucharts object
#' @export
#' @examples
#' tauchart(mtcars) %>%
#'   tau_point("mpg", "wt", "cyl") %>%
#'   tau_legend() %>%
#'   tau_color_brewer() %>%
#'   tau_tooltip() %>%
#'   tau_set_font("'Montserrat', sans-serif", "http://fonts.googleapis.com/css?family=Montserrat:400,700")
tau_set_font <- function(tau,
                         family="OpenSans, 'Helvetica Neue', Helvetica, Arial, sans-serif",
                         import_url="https://fonts.googleapis.com/css?family=Open+Sans:400,400italic,600,700,600italic,700italic,800,300,300italic,800italic") {

 if (!is.null(import_url)) tau$x$forFonts <- c(tau$x$forFonts, import_url)

 tau_add_css_rule(tau, sprintf("{{ID}} { font-family: %s }", family))

}

