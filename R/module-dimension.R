# # © Copyright World Health Organization (WHO) 2016-2021.
# # This file is part of the WHO Health Equity Assessment Toolkit
# # (HEAT and HEAT Plus), a software application for assessing
# # health inequalities in countries.
# #
# # This program is free software: you can redistribute it and/or modify
# # it under the terms of the GNU Affero General Public License as
# # published by the Free Software Foundation, either version 3 of the
# # License, or (at your option) any later version.
# #
# # This program is distributed in the hope that it will be useful,
# # but WITHOUT ANY WARRANTY; without even the implied warranty of
# # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# # GNU Affero General Public License for more details.
# #
# # You should have received a copy of the GNU Affero General Public License
# # along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# dimensionComponentTemplate <- function(id) {
#   ns <- NS(id)
#
#   navContent(
#     navPane(
#       id = ns("pane_1"),
#       fade = FALSE,
#       dimensionControlTemplate(
#         id = ns("max_1"),
#         max = 1
#       )
#     ),
#     navPane(
#       id = ns("pane_5"),
#       fade = FALSE,
#       dimensionControlTemplate(
#         id = ns("max_5"),
#         max = 5
#       )
#     ),
#     navPane(
#       id = ns("pane_none"),
#       fade = FALSE,
#       dimensionControlTemplate(
#         id = ns("max_none"),
#         max = Inf
#       )
#     ),
#     navPane(
#       id = ns("pane_hidden"),
#       fade = FALSE
#     )
#   )
# }
#
# dimensionComponentServer <- function(input, output, session, data, visual) {
#   ns <- session$ns
#
#   max_1 <- callModule(dimensionControlServer, "max_1", data = data, max = 1)
#   max_5 <- callModule(dimensionControlServer, "max_5", data = data, max = 5)
#   max_none <- callModule(dimensionControlServer, "max_none", data = data, max = Inf)
#
#   view <- reactive({
#     if (visual() == "exp_detail_dis" && !getOption("heat.plus", FALSE)) {
#       return("Subnational region")
#     } else if (visual() == "exp_detail_dis" && getOption("heat.plus", FALSE)) {
#       return("1")
#     }
#
#     if (visual() == "exp_map_dis") {
#       "Subnational region"
#     } else if (grepl("^(exp_line|exp_bar|exp_detail)_", visual())) {
#       "5"
#     } else if (grepl("^(exp_default|com_default)_", visual())) {
#       "1"
#     } else if (grepl("^(exp_table|com_table)_", visual())) {
#       "none"
#     }
#   })
#
#   value <- reactive({
#     switch(
#       view(),
#       `5` = max_5$value(),
#       `1` = max_1$value(),
#       `none` = max_none$value(),
#       "Subnational region"
#     )
#   })
#
#   observe({
#     new_value <- value()
#
#     if (all_equal(new_value, view())) {
#       return()
#     }
#
#     if (view() != "5") {
#       max_5$change(new_value)
#     }
#
#     if (view() != "1") {
#       max_1$change(new_value)
#     }
#
#     if (view() != "none") {
#       max_none$change(new_value)
#     }
#   })
#
#   observe({
#     switch(
#       view(),
#       `5` = showNavPane(ns("pane_5")),
#       `1` = showNavPane(ns("pane_1")),
#       `none` = showNavPane(ns("pane_none")),
#       showNavPane(ns("pane_hidden"))
#     )
#   })
#
#   value
# }
#
# dimensionControlTemplate <- function(id, max) {
#   ns <- NS(id)
#
#   formGroup(
#     label = if (max == 1) "Select inequality dimension" else "Select inequality dimensions",
#     if (max == 1) {
#       selectInput(
#         id = ns("value")
#       )
#     } else {
#       chipInput(
#         id = ns("value"),
#         max = max,
#         fill = TRUE
#       ) %>%
#         active("blue") %>%
#         width("full")
#     }
#   )
# }
#
# dimensionControlServer <- function(input, output, session, data, max) {
#
#   browser()
#   updateDimensionInput <- if (max == 1) updateSelectInput else updateChipInput
#
#   observe({
#     req(data())
#
#     new_dimensions <- data() %>%
#       dplyr::distinct(dimension) %>%
#       dplyr::pull(dimension)
#
#     updateDimensionInput(
#       id = "value",
#       choices = sort(new_dimensions),
#       values = sort(new_dimensions),
#       selected = if (getOption("heat.plus", FALSE)) new_dimensions[1] else "Economic status",
#       session = session
#     )
#   })
#
#   changeDimension <- function(selected) {
#     updateDimensionInput(
#       id = "value",
#       selected = head(selected, n = max),
#       session = session
#     )
#   }
#
#   value <- reactive({
#     input$value
#   })
#
#   list(
#     value = value,
#     change = changeDimension
#   )
# }
