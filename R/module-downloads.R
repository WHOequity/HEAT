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

downloadsUI <- function(id, chart, map = FALSE) {
  ns <- NS(id)

  list(
    if (isTRUE(chart)) {
      list(
        h6(i18n("downloads.labels.graph")),
        hr() %>% margin(top = 0) %>% background("black"),
        tags$p(
          i18n(if (isTRUE(map)) "downloads.text.map" else "downloads.text.graph")
        ),
        formGroup(
          label = div(
            i18n("downloads.labels.image")
          ),
          radiobarInput(
            id = ns("chart_extension"),
            choices = c("PNG", "JPG", "PDF"),
            values = c("png", "jpg", "pdf"),
            selected = "png"
          ) %>%
            background("grey") %>%
            active("blue") %>%
            border(round = "all") %>%
            shadow("small")
        ),
        buttonInput(
          id = ns("chart_download"),
          label = div(
            span(i18n("downloads.buttons.graph")),
            icon("download") %>% margin(left = 2)
          )
        ) %>%
          width("full") %>%
          font(color = "white") %>%
          background("blue") %>%
          shadow("small") %>%
          margin(top = 3, bottom = 4)
      )
    },
    list(
      h6(i18n("downloads.labels.data")),
      hr() %>% margin(top = 0) %>% background("black"),
      tags$p(
        i18n("downloads.text.data")
      ),
      formGroup(
        label = div(
          i18n("downloads.labels.separator"),
        ),
        radiobarInput(
          id = ns("data_extension"),
          choices = list(
            div(i18n("downloads.values.commas")), # "Commas",
            div(i18n("downloads.values.tabs")) # "Tabs"
          ),
          values = c("csv", "tsv"),
          selected = "csv"
        ) %>%
          background("grey") %>%
          active("blue") %>%
          border(round = "all") %>%
          shadow("small")
      ),
      tags$a(
        id = ns("data_download"),
        class = "btn shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        div(
          span(i18n("downloads.buttons.data")),
          icon("download") %>% margin(left = 2)
        )
      ) %>%
        width("full") %>%
        font(color = "white") %>%
        background("blue") %>%
        shadow("small") %>%
        margin(top = 3)
    )
  )
}

downloadsServer <- function(input, output, session,
                            chart = NULL, data = NULL,
                            language,
                            is_who_dataset) {

  if (!is.null(chart)) {
    chart_id <- gsub("-[^-]+$", "-visual", session$ns(NULL))
    chart_selector <- paste0("#", chart_id)
    observeEvent(input$chart_download, {
      session$sendCustomMessage("heat:download.image", list(
        selector = chart_selector,
        type = input$chart_extension,
        filename = glue("{ get_heat_prefix() }-chart.{ input$chart_extension }")
      ))
    })
  }

  output$data_download <- downloadHandler(
    filename = reactive({
      glue::glue("{ get_heat_prefix() }-data.{ input$data_extension }")
    }),

    content = function(temp) {

      cat(
        file = temp,
        table_disclaimer(language(), is_who_dataset),
        sep = ""
      )
      if (input$data_extension == "csv") {
        suppressWarnings(write.table(
          x = data(), file = temp, sep = ",", row.names = FALSE,
          append = TRUE, na = ""
        ))
      } else if (input$data_extension == "tsv") {
        suppressWarnings(write.table(
          x = data(), file = temp, sep = "\t", row.names = FALSE,
          append = TRUE, na = ""
        ))
      }
    }
  )
}
