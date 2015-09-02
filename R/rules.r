#' Add a CSS rule to the rendered htmlwidget
#'
#' This function will add a CSS rule to a widget-created
#' DOM stylesheet. \code{rule} should be a valid CSS rule as you
#' would enter in a \code{<style>...</style>} block. No checking is done
#' to ensure validity.
#'
#' Use \code{\{\{ID\}\}} (followed by a space) to target the CSS rule
#' just to the widget vs the whole DOM.
#'
#' Vectorized over \code{rule}
#'
#' @param tau taucharts object
#' @param rule character vector of CSS rule(s) to add to the widget DOM
#' @param warn show warnings for global CSS rules? (default: \code{TRUE})
#' @return taucharts htmlwidget object
#' @note This is for expert use only. You need to know quite a bit about the visualization
#'       and target DOM to effectively use this function. CSS rules without the \code{\{\{ID\}\}}
#'       are applied to the entire DOM.
#' @export
#' @examples
#' # change the default white tooltip to black
#'
#' if (interactive()) {
#' make_black_tooltip <- function(tau) {
#'  tau %>%
#'   tau_add_css_rule(
#'    ".graphical-report__tooltip__gray-text { color: white; font-weight: bold; }") %>%
#'   tau_add_css_rule(
#'    ".graphical-report__tooltip__list__elem:first-child {color:white;font-weight:bold;}") %>%
#'   tau_add_css_rule(
#'    ".graphical-report__tooltip__exclude { color: white; }") %>%
#'   tau_add_css_rule(
#'     paste0(c(".graphical-report__tooltip__exclude:hover { color: #65717f; ",
#'              "background: linear-gradient(to right, rgba(255, 255, 255, 0) 100%, ",
#'              "rgba(235, 238, 241, 0.9) 0%); }"), collapse="\n")) %>%
#'  tau_add_css_rule(".graphical-report__tooltip { background: black; color: white; }")
#' }
#'
#' tauchart(mtcars) %>%
#'   tau_point("wt", "mpg", color="cyl") %>%
#'   tau_color_manual(c("blue", "maroon", "black")) %>%
#'   tau_tooltip() %>%
#'   make_black_tooltip()
#' }
tau_add_css_rule <- function(tau, rule, warn=TRUE) {

  # if any of the CSS statements in 'rule' do not have {{ID}} targets, warn the user
  if (warn) {
    if (!any(grepl("\\{\\{ID\\}\\}", rule))) {
      # special case for the tooltip since that resides outsie the widget div
      # it has to be targeted globally unless the TauCharts folks change the behavior
      if (!all(grepl("graphical-report__tooltip", rule[which(!grepl("\\{\\{ID\\}\\}", rule))]))) {
        message("NOTE: CSS rules without {{ID}} are applied to the entire DOM.")
      }
    }
  }
  tau$x$forCSS <- c(tau$x$forCSS, rule)
  tau
}
