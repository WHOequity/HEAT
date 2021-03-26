# © Copyright World Health Organization (WHO) 2016-2021.
# This file is part of the WHO Health Equity Assessment Toolkit
# (HEAT and HEAT Plus), a software application for assessing
# health inequalities in countries.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

indicatorComponentTemplate <- function(id) {
  ns <- NS(id)

  navContent(
    navPane(
      id = ns("pane_1"),
      fade = FALSE,
      indicatorControlTemplate(
        id = ns("max_1"),
        max = 1
      )
    ),
    navPane(
      id = ns("pane_3"),
      fade = FALSE,
      indicatorControlTemplate(
        id = ns("max_3"),
        max = 3
      )
    ),
    navPane(
      id = ns("pane_5"),
      fade = FALSE,
      indicatorControlTemplate(
        id = ns("max_5"),
        max = 5
      )
    ),
    navPane(
      id = ns("pane_none"),
      fade = FALSE,
      indicatorControlTemplate(
        id = ns("max_none"),
        max = Inf
      )
    )
  )
}

indicatorComponentServer <- function(input, output, session, data, visual) {
  ns <- session$ns

  max_1 <- callModule(indicatorControlServer, "max_1", max = 1, data = data)
  max_3 <- callModule(indicatorControlServer, "max_3", max = 3, data = data)
  max_5 <- callModule(indicatorControlServer, "max_5", max = 5, data = data)
  max_none <- callModule(indicatorControlServer, "max_none", max = Inf, data = data)

  view <- reactive({
    if (grepl("^(exp_line|exp_bar)_", visual())) {
      "5"
    } else if (grepl("^exp_detail_", visual())) {
      "3"
    } else if (grepl("^(exp_map|com_default)_", visual())) {
      "1"
    } else if (grepl("^(exp_table|com_table)_", visual())) {
      "none"
    }
  })

  observe({
    switch(
      view(),
      `5` = showNavPane(ns("pane_5")),
      `3` = showNavPane(ns("pane_3")),
      `1` = showNavPane(ns("pane_1")),
      `none` = showNavPane(ns("pane_none"))
    )
  })

  value <- reactive({
    switch(
      view(),
      `5` = max_5$value(),
      `3` = max_3$value(),
      `1` = max_1$value(),
      `none` = max_none$value()
    )
  })

  observe({
    new_values <- value()

    if (view() != "5") {
      max_5$change(new_values)
    }

    if (view() != "3") {
      max_3$change(new_values)
    }

    if (view() != "1") {
      max_1$change(new_values)
    }

    if (view() != "none") {
      max_none$change(new_values)
    }
  })

  value
}

indicatorControlTemplate <- function(id, max) {
  ns <- NS(id)

  formGroup(
    label = if (max == 1) "Select health indicator" else "Select health indicators",
    if (max == 1) {
      selectInput(
        id = ns("value")
      )
    } else {
      chipInput(
        id = ns("value"),
        max = max,
        inline = FALSE
      ) %>%
        active("grey") %>%
        shadow("small")
    }
  )
}

indicatorControlServer <- function(input, output, session, max, data) {
  updateIndicatorInput <- if (max == 1) updateSelectInput else updateChipInput

  observeEvent(data(), {


    data_sorted <- data() %>%
      dplyr::distinct(indicator_name, indicator_abbr) %>%
      dplyr::arrange(indicator_name)

    new_names <- data_sorted$indicator_name
    new_abbrs <- data_sorted$indicator_abbr

    updateIndicatorInput(
      id = "value",
      choices = new_names,
      values = new_abbrs,
      selected = if (getOption("heat.plus", FALSE)) new_abbrs[[1]] else "sba",
      session = session
    )
  })

  value <- reactive({
    input$value
  })

  changeIndicator <- function(selected) {
    updateIndicatorInput(
      id = "value",
      selected = head(selected, n = max),
      session = session
    )
  }

  list(
    value = value,
    change = changeIndicator
  )
}
