#' @title Automobile statistics from 1997-2013
#' @description This dataset contains statistics on cars released from 1997 through 2013.
#'              It consists of a data.frame containing the columns:
#'
#' \itemize{
#'   \item \code{vehicle}: Vehicle model.
#'   \item \code{year}: Year vehicle released.
#'   \item \code{date}: Price (new) of vehicle at release year (USD).
#'   \item \code{accelrate}: Vehicle acceleration rate.
#'   \item \code{milespergallon}: Vehicle MPG
#'   \item \code{class}: Vehicle class (according to U.S. driver's license code)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name cars_data
#' @usage data(cars_data)
#' @note Last updated 2015-08-04.
#' @format A data frame with 135 rows and 7 variables
#' @examples
#' data(cars_data)
#' tauchart(cars_data) %>%
#'   tau_point("milespergallon", c("class", "price"), color="class") %>%
#'   tau_legend() %>%
#'   tau_trendline() %>%
#'   tau_tooltip(c("vehicle", "year", "class", "price", "milespergallon"))

NULL
