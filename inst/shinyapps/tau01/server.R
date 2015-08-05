# AUTHOR: https://github.com/ThoDuyNguyen
# VIA: https://github.com/hrbrmstr/taucharts/issues/1#issuecomment-127898024

library(shiny)
library(htmlwidgets)
library(taucharts)

shinyServer(function(input, output) {
    scatter_dat <-
        structure(
            list(
                team = c("d", "d", "d", "d", "l", "l", "l", "l",
                         "k", "k", "k", "k"), cycleTime = c(1L, 2L, 3L, 4L, 2L, 3L, 4L,
                                                            5L, 2L, 3L, 4L, 5L), effort = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L,
                                                                                            4L, 5L, 6L, 8L), count = c(1L, 5L, 8L, 3L, 1L, 5L, 8L, 3L, 1L,
                                                                                                                       5L, 8L, 3L), priority = c(
                                                                                                                           "low", "low", "medium", "high", "low",
                                                                                                                           "low", "medium", "high", "low", "low", "medium", "high"
                                                                                                                       )
            ), .Names = c("team",
                          "cycleTime", "effort", "count", "priority"), class = "data.frame", row.names = c(NA, 12L)
        )

    line_dat <-
        structure(
            list(
                type = c(
                    "us", "us", "us", "us", "us", "us", "bug",
                    "bug", "bug", "bug", "bug"
                ), count = c(0L, 10L, 15L, 12L, 16L,
                             13L, 21L, 19L, 23L, 26L, 23L), date = c(
                                 "12-2013", "01-2014",
                                 "02-2014", "03-2014", "04-2014", "05-2014", "01-2014", "02-2014",
                                 "03-2014", "04-2014", "05-2014"
                             )
            ), .Names = c("type", "count",
                          "date"), class = "data.frame", row.names = c(NA, 11L)
        )

    bar_dat <-
        structure(
            list(
                team = c("d", "d", "d", "d", "l", "l", "l", "l",
                         "k", "k", "k", "k"), cycleTime = c(1L, 2L, 3L, 4L, 2L, 3L, 4L,
                                                            5L, 2L, 3L, 4L, 5L), effort = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L,
                                                                                            4L, 5L, 6L, 8L), count = c(1L, 5L, 8L, 3L, 1L, 5L, 8L, 3L, 1L,
                                                                                                                       5L, 8L, 3L), priority = c(
                                                                                                                           "low", "low", "medium", "high", "low",
                                                                                                                           "low", "medium", "high", "low", "low", "medium", "high"
                                                                                                                       )
            ), .Names = c("team",
                          "cycleTime", "effort", "count", "priority"), class = "data.frame", row.names = c(NA, 12L)
        )

    car_dat <-
        structure(
            list(
                car = c(
                    "Toyota Prius+", "Volvo S60", "BMV X5",
                    "Infinity FX", "Mercedes Vito", "Peugeot 3008", "Subaru Forester",
                    "Lexus RX", "Bentley Continental"
                ), co2 = c(96L, 135L, 197L,
                           238L, 203L, 155L, 186L, 233L, 246L), hp = c(99L, 150L, 306L,
                                                                       238L, 95L, 120L, 150L, 188L, 507L), euroEco = c(
                                                                           "eco", "eco",
                                                                           "non-eco", "non-eco", "non-eco", "non-eco", "non-eco", "non-eco",
                                                                           "non-eco"
                                                                       ), power = c(
                                                                           "low", "normal", "high", "high", "low",
                                                                           "low", "normal", "normal", "high"
                                                                       )
            ), .Names = c("car", "co2",
                          "hp", "euroEco", "power"), class = "data.frame", row.names = c(NA, 9L)
        )

    splom_dat <-
        structure(
            list(
                param1 = c(
                    "hp", "hp", "hp", "co2", "co2", "co2",
                    "mpg", "mpg", "mpg", "hp", "hp", "hp", "co2", "co2", "co2", "mpg",
                    "mpg", "mpg", "hp", "hp", "hp", "co2", "co2", "co2", "mpg", "mpg",
                    "mpg", "hp", "hp", "hp", "co2", "co2", "co2", "mpg", "mpg", "mpg",
                    "hp", "hp", "hp", "co2", "co2", "co2", "mpg", "mpg", "mpg", "hp",
                    "hp", "hp", "co2", "co2", "co2", "mpg", "mpg", "mpg", "hp", "hp",
                    "hp", "co2", "co2", "co2", "mpg", "mpg", "mpg", "hp", "hp", "hp",
                    "co2", "co2", "co2", "mpg", "mpg", "mpg", "hp", "hp", "hp", "co2",
                    "co2", "co2", "mpg", "mpg", "mpg"
                ), param2 = c(
                    "hp", "co2", "mpg",
                    "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp",
                    "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2",
                    "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg",
                    "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp",
                    "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2",
                    "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg",
                    "hp", "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg", "hp",
                    "co2", "mpg", "hp", "co2", "mpg", "hp", "co2", "mpg"
                ), value1 = c(
                    99,
                    99, 99, 96, 96, 96, 3.8, 3.8, 3.8, 150, 150, 150, 135, 135, 135,
                    7.4, 7.4, 7.4, 306, 306, 306, 197, 197, 197, 11.2, 11.2, 11.2,
                    238, 238, 238, 238, 238, 238, 11.2, 11.2, 11.2, 95, 95, 95, 203,
                    203, 203, 9.4, 9.4, 9.4, 120, 120, 120, 155, 155, 155, 9.2, 9.2,
                    9.2, 150, 150, 150, 186, 186, 186, 10.4, 10.4, 10.4, 188, 188,
                    188, 233, 233, 233, 13.3, 13.3, 13.3, 507, 507, 507, 246, 246,
                    246, 15.4, 15.4, 15.4
                ), value2 = c(
                    99, 96, 3.8, 99, 96, 3.8,
                    99, 96, 3.8, 150, 135, 7.4, 150, 135, 7.4, 150, 135, 7.4, 306,
                    197, 11.2, 306, 197, 11.2, 306, 197, 11.2, 238, 238, 11.2, 238,
                    238, 11.2, 238, 238, 11.2, 95, 203, 9.4, 95, 203, 9.4, 95, 203,
                    9.4, 120, 155, 9.2, 120, 155, 9.2, 120, 155, 9.2, 150, 186, 10.4,
                    150, 186, 10.4, 150, 186, 10.4, 188, 233, 13.3, 188, 233, 13.3,
                    188, 233, 13.3, 507, 246, 15.4, 507, 246, 15.4, 507, 246, 15.4
                )
            ), .Names = c("param1", "param2", "value1", "value2"), class = "data.frame", row.names = c(NA,
                                                                                                       81L)
        )

    output$bar <- renderTaucharts({
        tauchart(bar_dat) %>% tau_bar("team", "effort", color = "priority") %>%
            tau_legend() %>% tau_tooltip()
    })

    output$line <- renderTaucharts({
        tauchart(line_dat) %>%
            tau_line("date", "count", color = "type") %>%
            tau_guide_x(label="Month") %>%
            tau_guide_y(label="Count of completed entities", label_padding=50) %>%
            tau_guide_padding(70, 70, 10, 10) %>%
            tau_legend() %>%
            tau_tooltip()
    })
})
