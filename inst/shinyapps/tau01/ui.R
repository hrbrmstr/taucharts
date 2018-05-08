# META: tau01 - TauCharts Shiny Example 01

# AUTHOR: https://github.com/ThoDuyNguyen
# VIA: https://github.com/hrbrmstr/taucharts/issues/1#issuecomment-127898024

library(shiny)
library(htmlwidgets)
library(taucharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("TauCharts Demo"),

  # Sidebar with a slider input for number of bins
  sidebarPanel(width = 3,
               selectInput(
                   "data", label = h4("Data available:"),
                   choices = c("none"=0, "little"=1, "some"=2, "lots"=3),
                   selected = 1
               )),
  mainPanel(
    tabsetPanel(
        tabPanel("bar", tauchartsOutput("bar")),
        tabPanel("line", tauchartsOutput("line"))
    )
  )
))
