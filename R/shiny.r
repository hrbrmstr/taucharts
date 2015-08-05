#' Run a built-in example Shiny app
#'
#' When run without specifying \code{app_name}, this function will return
#' a list of built-in TauCharts Shiny example applications. To run one of the
#' built-in example applications, specify only the name in the \code{app_name}
#' parameter and the function will display the full path to the app (making it
#' easier to find and view the source) and then execute the app.
#'
#' @param app_name name of built-in Shiny app to run (if \code{NULL} displays a list
#'        of built-in apps)
#' @export
#' @examples \dontrun{
#' # list the built-in apps
#'
#' run_tau_app()
#' Available shiny example apps:
#' -----------------------------
#' tau01 - TauCharts Shiny Example 01
#'
#' # run an app
#'
#' run_tau_app("tau01")
#' Running shiny app [tau01] from:
#  /Library/Frameworks/R.framework/Versions/3.2/Resources/library/taucharts/shinyapps/tau01
#  Listening on http://127.0.0.1:6077
#' }
run_tau_app <- function(app_name=NULL) {

  pkg_dir <- system.file(package="taucharts")

  if (is.null(app_name)) {

    cat("Available shiny example apps:\n")
    cat("-----------------------------\n")

    app_files <- list.files(pkg_dir, "ui.R", recursive=TRUE)
    invisible(sapply(app_files, function(x) {

      cat(gsub("^# META: ", "",
           grep("^# META:", readLines(paste0(pkg_dir, "/", x)), value=TRUE)))
      cat("\n")

    }))

  } else {

    app_dir <- sprintf("%s/shinyapps/%s", pkg_dir, app_name)
    if (file.exists(app_dir)) {
      message(paste0("Running shiny app [", app_name, "] from:\n  ", app_dir))
      shiny::runApp(app_dir)
    } else {
      message("Shiny example app not found.\nExecute run_tau_app() without parameters to see available shiny apps.")
    }
  }

}

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
