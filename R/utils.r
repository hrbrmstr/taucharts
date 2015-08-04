brewers <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn",
"Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
"Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens",
"Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples",
"RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")


# from rstudio/dygraphs https://github.com/rstudio/dygraphs
asISO8601Time <- function(x) {
  if (!inherits(x, "POSIXct"))
    x <- try({as.POSIXct(x, tz = "GMT")})
  # if posix conversion worked
  if (inherits(x, "POSIXct")) {
    format(x, format="%04Y-%m-%dT%H:%M:%SZ", tz='GMT')
  } else {
    # if not just return x and keep pluggin away
    x
  }
}
