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

benchmarkUI <- function(id) {
  ns <- NS(id)

  list(
    formGroup(
      label = div( # "Filter by country-income group",
        i18n(
          key = "benchmarks.labels.income"
        )
      ),
      chipInput(
        i18n(
          ns = "values.groups"
        ),
        id = ns("income"),
        choices = NULL,
        values = NULL,
        selected = NULL,
        inline = TRUE,
        placeholder = "inputs.placeholders.selection"
      ) %>%
        shadow()
    ),
    formGroup(
      label = div( # "Filter by WHO region",
        i18n(
          key = "benchmarks.labels.region"
        )
      ),
      chipInput(
        i18n(
          ns = "values.regions"
        ),
        id = ns("region"),
        choices = NULL,
        values = NULL,
        selected = NULL,
        inline = TRUE,
        placeholder = "inputs.placeholders.selection"
      ) %>%
        shadow()
    ),
    formGroup(
      label = div( # "Select comparison settings",
        i18n("benchmarks.labels.setting")
      ),
      chipInput(
        i18n(
          ns = "values.settings"
        ),
        id = ns("comparison"),
        choices = NULL,
        values = NULL,
        selected = NULL,
        inline = TRUE,
        placeholder = "inputs.placeholders.selection"
      ) %>%
        shadow()
    ),
    formGroup(
      label = div( # "Select years",
        i18n("benchmarks.labels.year")
      ),
      help = div(
        i18n("benchmarks.text.year")
      ),
      # help = glue("By how many years can the benchmark countries' \\
      #              data vary from focus setting's data?"),
      radiobarInput(
        id = ns("variance"),
        choices = 0:5,
        selected = 2
      ) %>%
        border(round = "all") %>%
        shadow("small")
    )
  )
}

benchmarkServer <- function(input, output, session, parent, Data,
                            Events, visible, language) {

  this <- session$ns(NULL)


  # helpers ----
  ignoreMessage <- function(msg) {
    msg$from == this && !isTRUE(msg$force)
  }

  immediateMessage <- function(msg) {
    msg$from == parent || msg$from == this ||
      msg$from == "init"
  }

  # income ----
  r_income <- reactive(input$income)
  d_income <- debounce(r_income, 350)

  # ├ observe income ----
  observeEvent(d_income(), {
    req(visible())


    Events$set_benchmark_income <- list(
      from = this,
      selected = input$income
    )

    new_comparisons <- benchmark_comparisons(
      Data$country_info(), input$income, input$region
    )

    Events$set_benchmark_comparison <- list(
      from = this,
      force = TRUE,
      choices = new_comparisons$choices,
      values = new_comparisons$values,
      selected = new_comparisons$values
    )
  })

  # ├ set_benchmark_income ----
  observeEvent(Events$set_benchmark_income, {


    msg <- Events$set_benchmark_income
    do_update <- function() {
      if (!ignoreMessage(msg)) {
        updateChipInput(
          id = "income",
          choices = msg$choices,
          values = msg$values,
          selected = msg$selected,
          session = session
        )
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # region ----
  r_region <- reactive(input$region)
  d_region <- debounce(r_region, 350)

  # ├ observe region ----
  observeEvent(d_region(), {
    req(visible())

    Events$set_benchmark_region <- list(
      from = this,
      selected = input$region
    )

    new_comparisons <- benchmark_comparisons(Data$country_info(), input$income, input$region)

    Events$set_benchmark_comparison <- list(
      from = this,
      force = TRUE,
      choices = new_comparisons$choices,
      values = new_comparisons$values,
      selected = new_comparisons$values
    )
  })

  # ├ set_benchmark_region ----
  observeEvent(Events$set_benchmark_region, {
    msg <- Events$set_benchmark_region
    sort_order <- if (!is.null(msg$choices)) order(msg$choices)

    do_update <- function() {
      if (!ignoreMessage(msg)) {
        updateChipInput(
          id = "region",
          choices = msg$choices[sort_order],
          values = msg$values[sort_order],
          selected = msg$selected,
          session = session
        )
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # comparison ----
  r_comparison <- reactive(input$comparison)
  d_comparison <- debounce(r_comparison, 450)

  # ├ observe comparison ----
  observeEvent(d_comparison(), {
    req(visible())

    Events$set_benchmark_comparison <- list(
      from = this,
      selected = input$comparison
    )
  })

  # ├ set_benchmark_comparison ----
  observeEvent(Events$set_benchmark_comparison, {
    msg <- Events$set_benchmark_comparison

    do_update <- function() {
      if (!ignoreMessage(msg)) {
        updateChipInput(
          id = "comparison",
          choices = msg$choices,
          values = msg$values,
          selected = msg$selected,
          session = session
        )
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # variance ----
  r_variance <- reactive(as.numeric(input$variance))

  # ├ observe variance ----
  observeEvent(r_variance(), {
    req(visible())

    Events$set_benchmark_variance <- list(
      from = this,
      force = TRUE,
      selected = input$variance
    )
  })

  # ├ set_benchmark_variance ----
  observeEvent(Events$set_benchmark_variance, {
    msg <- Events$set_benchmark_variance

    do_update <- function() {
      if (!ignoreMessage(msg)) {
        updateRadiobarInput(
          id = "variance",
          choices = 0:5,
          selected = msg$selected,
          session = session
        )
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # set all benchmark fields ----
  observeEvent(Events$set_benchmark_setting, {
    msg <- Events$set_benchmark_setting

    selection_setting <- Data$country_info() %>%
      dplyr::filter(setting == !!msg$selected) %>%
      dplyr::slice(1)

    if (NROW(selection_setting) > 0) {
      new_income <- selection_setting$wbincome_name
      new_region <- selection_setting$whoreg6
      new_comparisons <- benchmark_comparisons(Data$country_info(), new_income, new_region)
    } else {
      new_income <- NULL
      new_region <- NULL
      new_comparisons <- NULL
    }

    possible_incomes <- benchmark_income_groups(Data$country_info())


    Events$set_benchmark_income <- list(
      from = msg$from,
      choices = possible_incomes,
      values = possible_incomes,
      selected = new_income
    )


    possible_regions <- benchmark_regions(Data$country_info())

    Events$set_benchmark_region <- list(
      from = msg$from,
      choices = possible_regions$choices,
      values = possible_regions$values,
      selected = new_region
    )

    # new_comparisons, see above

    Events$set_benchmark_comparison <- list(
      from = msg$from,
      choices = new_comparisons$choices,
      values = new_comparisons$values,
      selected = new_comparisons$values
    )

  })

  # return comparisons settings and variance ----
  list(
    comparison = r_comparison,
    variance = r_variance
  )
}
