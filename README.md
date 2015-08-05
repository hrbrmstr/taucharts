<!-- README.md is generated from README.Rmd. Please edit that file -->
taucharts is an R htmlwidget interface to the TauCharts javascript library

Take a look at the [TODO list](https://github.com/hrbrmstr/taucharts/issues/1) and chip in!

Right now, you can make & customize (including manual color scales & ordered factors + legends + tooltips + trendlines):

-   scatterplots
-   scatterplot matrices
-   line charts
-   bar charts (veritical, horizontal & stacked)

Composite plots are on the road map for support but not in the R package yet.

Have a look [on RPubs](http://rpubs.com/hrbrmstr/taucharts) to see what `taucharts` can do!

The following functions are implemented:

-   `tauchart`: Create a new TauChart
-   `tau_line`: Create a TauCharts line chart
-   `tau_point`: Create a TauCharts scatterplot
-   `tau_bar`: Create a TauCharts bar chart (horizontal or vertical)
-   `tau_stacked_bar`: Create a TauCharts stacked bar chart (veritcal only)
-   `tau_guide_gridlines`: Control showing of axis gridlines
-   `tau_guide_padding`: Set overall chart padding
-   `tau_guide_x`: Control x-axis padding, label, scale & tick format
-   `tau_guide_y`: Control y-axis padding, label, scale & tick format
-   `tau_color_manual`: Specify the colors used in the charts
-   `tau_legend`: Add a TauCharts tooltip
-   `tau_tooltip`: Add a TauCharts tooltip
-   `tau_trendline`: Add a TauCharts trendline
-   `run_tau_app`: Run a built-in example Shiny app

The following datasets are included:

-   `cars_data`: statistics on cars released from 1997 through 2013 (a data frame with 135 rows and 7 variables)

### News

-   Version 0.2.5.9000 released : list & run example Shiny apps
-   Version 0.2.5.9000 released : stacked bar charts
-   Version 0.1.0 released : trendline, tooltips & dev fork (prod is pretty much stable & functional)
-   Version 0.0.1.9003 released : facet-based ordering + legends
-   Version 0.0.1.9000 released : auto-detects column classes, can add manual colors & faceted plots are now working (see the Rpub for an example)
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
#> [1] '0.2.5.9000'
```

### Test Results

``` r
library(taucharts)
library(testthat)

date()
#> [1] "Wed Aug  5 06:57:23 2015"

test_dir("tests/")
#> testthat results ========================================================================================================
#> OK: 0 SKIPPED: 0 FAILED: 0
#> 
#> DONE
```

### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
