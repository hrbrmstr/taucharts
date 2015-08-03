<!-- README.md is generated from README.Rmd. Please edit that file -->
taucharts is an R htmlwidget interface to the TauCharts javascript library

NOTE: Only basic scatterplots, bar charts and line charts are working and even then there's no custom color support yet. Most of the "guide" functionality is, however, working. The plan is to support all the features of the TauCharts library.

Have a look [on RPubs](http://rpubs.com/hrbrmstr/taucharts).

The following functions are implemented:

-   `taucharts`: Create a new TauChart
-   `tau_line`: Create a TauCharts line chart
-   `tau_point`: Create a TauCharts scatterplot
-   `tau_bar`: Create a TauCharts bar chart (horizontal or vertical)
-   `tau_guide_gridlines`: Control showing of axis gridlines
-   `tau_guide_padding`: Set overall chart padding
-   `tau_guide_x`: Control x-axis padding, label, scale & tick format
-   `tau_guide_y`: Control y-axis padding, label, scale & tick format

### News

-   Version 0.0.0.9000 released

### Installation

``` r
devtools::install_github("hrbrmstr/taucharts")
```

### Usage

``` r
library(taucharts)

# current verison
packageVersion("taucharts")
#> [1] '0.0.0.9000'
```

### Test Results

``` r
library(taucharts)
library(testthat)

date()
#> [1] "Mon Aug  3 05:23:32 2015"

test_dir("tests/")
#> testthat results ========================================================================================================
#> OK: 0 SKIPPED: 0 FAILED: 0
#> 
#> DONE
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
