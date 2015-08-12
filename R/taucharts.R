#' Create a new TauChart
#'
#' Performs basic widget setup and returns an object suitable for
#' use with the other \code{tau_} functions.
#'
#' @param data \code{data.frame}-like or \code{xts} object
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @references \url{http://api.taucharts.com/}
#' @export
#' @examples
#' tauchart(mtcars) %>% tau_point("mpg", "wt")
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
  date_columns <- which(sapply(data,function(x) inherits(x,c("Date", "POSIXct", "date", "yearmon", "yearqtr"))))
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
    forCSS=NULL,
    forFonts="https://fonts.googleapis.com/css?family=Open+Sans:400italic,600italic,400,600&subset=latin,cyrillic-ext"
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
