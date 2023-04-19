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
      label = div(
        i18n(
          key = "inputs.labels.source_plural",  # Select data source(s)
          plural = TRUE, # source
        )
      ),
      chipInput(
        i18n(
          ns = "values.sources"
        ),
        id = ns("benchmark_source"),
        choices = " ",
        values = " ",
        selected = " ",
        placeholder = "inputs.placeholders.selection",
        sort = "fixed"
      ) %>%
        active("grey") %>%
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
      label = div( # "Select comparison settings",
        i18n("benchmarks.labels.date")#"Date of benchmark survey"
      ),
      list(
        checkboxInput(
          i18n(
            ns = "benchmarks.labels",
            target = "label",
            assign = "benchmarks.labels.checkbox" # nearest date
          ),
          id = ns("recent"),
          choices = "recent",
          values = "recent",
          selected = NULL
        ) %>%
          margin(bottom = 2),
        collapsePane(
          id = ns("year_collapse"),
          show = TRUE,
          animate = FALSE,
          formRow(
            div(class = 'bench-label',
              i18n(key = "benchmarks.labels.startdate")#"Range: start date"
            ),
            selectInput(
              # i18n(
              #   ns = "inputs.labels"
              # ),

              id = ns("benchmark_year_beg"),
              choices = NULL,
              values = NULL,
              selected = NULL,
              placeholder = "inputs.placeholders.selection"
            ) %>%
              margin(bottom = 2),
            div(class = 'bench-label',
                i18n(key = "benchmarks.labels.enddate")#"Range: end date"
            ),
            selectInput(
              # i18n(
              #   ns = "inputs.labels"
              # ),
              id = ns("benchmark_year_end"),
              choices = NULL,
              values = NULL,
              selected = NULL,
              placeholder = "inputs.placeholders.selection"
            )
          )

        )
      )
    )

  )
}

benchmarkServer <- function(input, output, session, parent, Data,
                            Events, visible, language) {


  #data_setting_yr_src <- Data$setting_yr_src()
  #data_country_info <- Data$country_info()
  #data_date_to_integer <- Data$date_to_integer()



  ns <- session$ns
  this <- ns(NULL)


  add_time("enter benchmarkServer", this)

  # ├ setters ----
  setBenchmarkYear <- function(values) {
    benchmark_yrs <- default_benchmark_year(values)
    state$benchmark_year_beg <- comp_yrs$beg
    state$benchmark_year_end <- comp_yrs$end
  }


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
      Data$setting_yr_src(),
      Data$country_info(),
      input$income,
      input$region,
      input$benchmark_source
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
    add_time("observe d_region()", this)
    new_comparisons <- benchmark_comparisons(
      Data$setting_yr_src(),
      Data$country_info(),
      input$income,
      input$region,
      input$benchmark_source)

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

    add_time("observe set_benchmark_region", this)
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

  # benchmark source ----


  r_benchmark_source <- reactive(input$benchmark_source)
  d_benchmark_source <- debounce(r_benchmark_source, 350)


  # ├ observe benchmark source ----

  observeEvent(input$benchmark_source, {

    req(Data$setting_yr_src())
    add_time("observe input$benchmark_source 1", this)
    src <- Data$setting_yr_src() |>
      dplyr::pull(source) |>
      unique() |>
      sort()

    updateChipInput(
      id = "benchmark_source",
      choices = src,
      values = src,
      selected = src,
      session = session
    )
  }, once = TRUE)


  observeEvent(d_benchmark_source(), {
    req(visible())

    add_time("observe d_benchmark_source()", this)
    Events$set_benchmark_source <- list(
      from = this,
      selected = input$benchmark_source
    )

    new_comparisons <- benchmark_comparisons(
      Data$setting_yr_src(),
      Data$country_info(),
      input$income,
      input$region,
      d_benchmark_source()
    )

    Events$set_benchmark_comparison <- list(
      from = this,
      force = TRUE,
      choices = new_comparisons$choices,
      values = new_comparisons$values,
      selected = new_comparisons$values
    )
    add_time("end observe d_benchmark_source()", this)

  }, ignoreInit = TRUE)

  # ├ set_benchmark_source ----
  observeEvent(Events$set_benchmark_source, {

    msg <- Events$set_benchmark_source

    do_update <- function() {
      if (!ignoreMessage(msg)) {
        updateChipInput(
          id = "benchmark_source",
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






  #
  # r_benchmark_source <- reactive(input$benchmark_source)
  # d_benchmark_source <- debounce(r_benchmark_source, 250)
  #
  # observeEvent(input$benchmark_source, {
  #
  #   src <- data_setting_yr_src |>
  #     dplyr::pull(source) |>
  #     unique() |>
  #     sort()
  #
  #   updateChipInput(
  #     id = "benchmark_source",
  #     choices = src,
  #     values = src,
  #     selected = src,
  #     session = session
  #   )
  # }, once = TRUE)



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
    msg_yr <- Events$set_year
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

  # recent year ----

  r_benchmark_year_near <- reactive(input$recent)
  d_benchmark_year_near <- debounce(r_benchmark_year_near, 250)

  observeEvent(d_benchmark_year_near(), ignoreNULL = FALSE, {
    req(visible())


    Events$toggle_year_near <- list(
      from = this,
      force = TRUE,
      value = input$recent,
      selected = input$benchmark_year_beg#,
      #no_years = is.null(2003)
    )
  })

  observeEvent(Events$toggle_year_near, {

    msg <- Events$toggle_year_near

    do_update <- function() {

      checked <- length(msg$value) > 0

      if (checked) {
        #state$benchmark_year_beg <- 2001
        #state$benchmark_year_end <- 2003
      } else if (FALSE) { #!msg$no_years
        #setBenchmarkYear(c(2001, 2003))
      }

      if (!ignoreMessage(msg)) {
        #if (!msg$no_years) {
          updateCheckboxInput(
            id = "recent",
            selected = checked,
            session = session
          )
        #}

        (if (checked) hideCollapsePane else showCollapsePane)(
          id = ns("year_collapse"),
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




  # begin year ----
  r_benchmark_year_beg <- reactive(input$benchmark_year_beg)

  # ├ observe year ----
  observeEvent(r_benchmark_year_beg(), {

    req(visible())


    Events$set_benchmark_year_beg <- list(
      from = this,
      selected = input$benchmark_year_beg
    )


  })

  # ├ set_benchmark_year_beg ----
  observeEvent(Events$set_benchmark_year_beg, {
    msg <- Events$set_benchmark_year_beg

    beg_year <- input$benchmark_year_beg
    end_year <- input$benchmark_year_end

    do_update <- function() {

      # checked <- length(msg$recent) > 0
      #
      # selected_year <- setYear(msg$selected)
      #
      # if (checked) {
      #   state$year <- msg$recent
      # }

      if (!ignoreMessage(msg)) {


        updateSelectInput(
          id = "benchmark_year_beg",
          choices = msg$choices,
          values = msg$values,
          selected = msg$selected,
          session = session
        )


        iv <- shinyvalidate::InputValidator$new()

        f <- function(value){

          if(is.null(value))
            return()

          beg_year <- get_date_integer(
            isolate(input$benchmark_year_beg),
            Data$date_to_integer()
          )
          end_year <- get_date_integer(
            isolate(input$benchmark_year_end),
            Data$date_to_integer()
          )

          if(!is.null(input$recent))
            return()
          if(!is.null(value) && as.numeric(end_year) < as.numeric(beg_year))
            translate(c(language, "benchmarks", "text", "error"))  #Start date should be before end date"
            #"text"
        }

        iv$add_rule("benchmark_year_beg", f)
        iv$add_rule("benchmark_year_end", f)
        iv$enable()

      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # end year ----
  r_benchmark_year_end <- reactive(input$benchmark_year_end)

  # ├ observe year ----
  observeEvent(r_benchmark_year_end(), {

    req(visible())

    Events$set_benchmark_year_end <- list(
      from = this,
      selected = input$benchmark_year_end
    )


  })

  # ├ set_benchmark_year_end ----
  observeEvent(Events$set_benchmark_year_end, {
    msg <- Events$set_benchmark_year_end

    beg_year <- Events$set_benchmark_year_beg$selected
    end_year <- Events$set_benchmark_year_end$selected

    do_update <- function() {


      if (!ignoreMessage(msg)) {


        updateSelectInput(
          id = "benchmark_year_end",
          choices = msg$choices,
          values = msg$values,
          selected = msg$selected,
          session = session
        )

        iv <- shinyvalidate::InputValidator$new()

        f <- function(value){

          if(is.null(value))
            return()

          beg_year <- get_date_integer(
            isolate(input$benchmark_year_beg),
            Data$date_to_integer()
          )
          end_year <- get_date_integer(
            isolate(input$benchmark_year_end),
            Data$date_to_integer()
          )

          if(!is.null(input$recent))
            return()
          if(!is.null(value) && as.numeric(end_year) < as.numeric(beg_year))
            translate(c(isolate(language()), "benchmarks", "text", "error")) #Start date should be before end date"
            #"text"
        }

        iv$add_rule("benchmark_year_beg", f)
        iv$add_rule("benchmark_year_end", f)
        iv$enable()


      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })




  # set all benchmark fields ----
  observeEvent(c(Events$set_benchmark_setting), {

    req(Events$set_benchmark_setting)

    add_time("observe set_benchmark_setting", this)
    msg <- Events$set_benchmark_setting

    selection_setting <- Data$country_info() %>%
      dplyr::filter(setting == !!msg$selected) %>%
      dplyr::slice(1)

    src <- sort(unique(Data$setting_yr_src()$source))

    if (NROW(selection_setting) > 0) {
      new_income <- selection_setting$wbincome_name
      new_region <- selection_setting$whoreg6
      new_comparisons <- benchmark_comparisons(Data$setting_yr_src(),
                                               Data$country_info(),
                                               new_income,
                                               new_region,
                                               src)
    } else {
      new_income <- NULL
      new_region <- NULL
      new_comparisons <- NULL
    }


    Events$set_benchmark_source <- list(
      from = msg$from,
      choices = src,
      values = src,
      selected = src
    )


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
      selected = new_comparisons$values,
      trigger = rnorm(1)
    )



    possible_benchmark_years <- get_benchmark_years(
      Data$setting_yr_src(),
      selection_setting$setting,
      new_comparisons$choices
      )

    Events$set_benchmark_year_beg <- list(
      from = msg$from,
      choices = possible_benchmark_years,
      values = possible_benchmark_years,
      selected = min(possible_benchmark_years)
    )

    Events$set_benchmark_year_end <- list(
      from = msg$from,
      choices = possible_benchmark_years,
      values = possible_benchmark_years,
      selected = max(possible_benchmark_years)
    )


    Events$toggle_year_near <- list(
      from = msg$from,
      choices = translate(c(isolate(language()), "inputs", "labels", "recent")),
      values = max(possible_benchmark_years),
      selected = TRUE
    )


    add_time("end observe set_benchmark_setting", this)
  })

  r_comparison_wtrigger <- reactive({
    Events$set_benchmark_comparison
    input$comparison
  })

  # return comparisons settings and variance ----
  list(
    comparison = r_comparison_wtrigger,
    benchmark_source = r_benchmark_source,
    benchmark_year_beg = r_benchmark_year_beg,
    benchmark_year_end = r_benchmark_year_end,
    benchmark_year_near = d_benchmark_year_near
  )
}
