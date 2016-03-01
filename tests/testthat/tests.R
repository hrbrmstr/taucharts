testdata <- data.frame(date = seq(as.Date("2016-02-29") - 300, as.Date("2016-02-29"), by = "day"), val = rnorm(43))

# Test selecting specific fields for quick filter
tauchart(mtcars) %>%
  tau_point("mpg", "wt") %>%
  tau_quick_filter(fields = c("mpg", "wt"))

# Double date scales currently result in confusing warning...
testdate <- testdata
testdate$date2 <- testdate$date
tauchart(testdate) %>%
  tau_point("date", "date2")


# Trendline default can be changed to exponential
tauchart(mtcars) %>%
  tau_point("mpg", "wt") %>%
  tau_trendline(type = "exponential", models = c("lastvalue", "logarithmic", "exponential"))

# Autoscale fails for time data
tauchart(head(testdata, 6)) %>%
  tau_line("date", "val") %>%
  tau_guide_x(tick_format = "%b-%y", tick_period = 7)

# width & height fail...
tauchart(testdata, width = "100px", height = "100px") %>%
  tau_line("date", "val")

# Tick period fails fails for time data
tauchart(testdata) %>%
  tau_line("date", "val") %>%
  tau_guide_x(tick_format = "%b-%y", tick_period = 28)

# Tick period fails fails for continuous data
tauchart(testdata) %>%
  tau_line("val", "val") %>%
  tau_guide_x(tick_period = 50)

# Tick period fails fails for continuous data
tauchart(testdata) %>%
  tau_line("val", "val") %>%
  tau_export_plugin()
