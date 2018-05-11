#' Add post-render JavaScript tasks to taucharts
#'
#' @param tau taucharts object
#' @param task either a \code{character} or \code{\link[htmlwidgets]{JS}}
#'          representing a JavaScript function to run after a tauchart has
#'          been rendered.  This provides the ability for advanced customization.
#'          The JavaScript function will be \code{call}ed so \code{this.el} will be
#'          the containing \code{div} element and \code{this.chart} will be the
#'          \code{taucharts} object.
#'
#' @importFrom htmlwidgets JS
#' @export
#' @examples
#' if (interactive()) {
#' tauchart(mtcars) %>%
#'   tau_point("mpg", "wt") %>%
#'   tau_tasks("function(){alert('I drew a chart')}") %>%
#'   tau_tasks(htmlwidgets::JS(
#'     "function(){alert('Yep, I really did.')}"
#'   )) %>%
#'   tau_tasks(
#'     "
#' function(){
#'   d3.select(this.el).selectAll('.tau-chart__dot')
#'     .transition().style('opacity',0.1)
#'     .transition().style('opacity',0.9)
#'     .transition().style('opacity',0.1)
#'     .transition().style('opacity',0.9);
#' }
#' "
#'  )
#' }
tau_tasks <- function ( tau, task = NULL ){
  if(is.null(task)) stop("please provide a non-NULL task.", call. = FALSE)

  if(is.null(tau$x$tasks)){
    tau$x$tasks <- list()
  }

  if(!inherits(task,"JS_EVAL")){
    # convert to htmlwidgets::JS if just a character
    task <- htmlwidgets::JS( task )
  }

  tau$x$tasks[[length(tau$x$tasks) + 1]] <- task

  tau
}
