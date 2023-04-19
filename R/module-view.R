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

NAV <- list(
  SELECTION = "selection",
  BENCHMARK = "benchmark",
  OPTIONS = "options",
  SUMMARIES = "summaries",
  DOWNLOADS = "downloads"
)

ICONS <- list(
  selection = icon("filter"),
  benchmark = icon("sliders"),
  options = icon("gear"),
  summaries = icon("table-list"),
  downloads = icon("file-arrow-down")
)

viewUI <- function(id, nav, source, year, indicator, dimension, measure,
                   summaries, benchmarks, output, options = list()) {
  ns <- NS(id)

  container(
    columns(
      # class="overflow-x",
      column(
        class = "pr-0",
        width = 4,
        card(
          # side panel nav ----
          header = navInput(
            i18n(ns = "sidepanel.labels"),
            id = ns("nav"),
            appearance = "tabs",
            choices = lapply(
              nav,
              function(item) {
                span(
                  span(
                    class = "nav-label"
                  ),
                  ICONS[[item]]
                ) %>%
                  font(size = "sm")
              }
            ),
            values = nav,
            selected = nav[[1]]
          ),
          navContent(
            style = "margin: -5px; padding: 5px;",
            navPane(
              id = ns(paste0("pane_", NAV$SELECTION)),
              fade = FALSE,
              # setting ----
              formGroup(
                label = div(
                  i18n(
                    key = "inputs.labels.setting"
                  )
                  # "Select setting (e.g. country, province, district)"
                ),
                selectInput(
                  i18n(
                    ns = "values.settings"
                  ),
                  id = ns("setting"),
                  choices = NULL,
                  values = NULL,
                  selected = NULL,
                  placeholder = "inputs.placeholders.selection"
                )
                # selectInput(
                #   id = ns("setting"),
                #   choices = NULL,
                #   values = NULL,
                #   selected = NULL
                # )
              ),
              # source ----
              formGroup(
                label = div(
                  i18n(
                    key = "inputs.labels.source_plural", # Select data source(s)
                    plural = TRUE, # source
                  )
                ),
                if (source == 1) {
                  selectInput(
                    i18n(
                      ns = "values.sources"
                    ),
                    id = ns("source"),
                    choices = NULL,
                    values = NULL,
                    selected = NULL,
                    placeholder = "inputs.placeholders.selection"
                  )
                } else {
                  chipInput(
                    i18n(
                      ns = "values.sources"
                    ),
                    id = ns("source"),
                    choices = NULL,
                    values = NULL,
                    selected = NULL,
                    placeholder = "inputs.placeholders.selection",
                    sort = "fixed"
                  ) %>%
                    active("grey") %>%
                    shadow()
                }
              ),
              # year ----
              formGroup(
                label = div(
                  i18n(
                    key = "inputs.labels.year",
                    plural = year
                  )
                  # Year or Years
                ),
                list(
                  checkboxInput(
                    i18n(
                      ns = "inputs.labels",
                      target = "label",
                      assign = "inputs.labels.recent"
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
                    if (year == 1) {
                      selectInput(
                        i18n(
                          ns = "inputs.labels"
                        ),
                        id = ns("year"),
                        choices = NULL,
                        values = NULL,
                        selected = NULL,
                        placeholder = "inputs.placeholders.selection"
                      )
                    } else {
                      list(
                        div(
                          class = "alldates-btn",
                          yonder::buttonInput(
                            id = ns("alldates"),
                            label = div(
                              span(i18n("inputs.buttons.all_dates"))
                            )
                          ),
                          yonder::buttonInput(
                            id = ns("removedates"),
                            label = div(
                              span(i18n("inputs.buttons.remove_dates"))
                            )
                          )
                        ),
                        chipInput(
                          i18n(
                            ns = "inputs.labels"
                          ),
                          id = ns("year"),
                          choices = NULL,
                          values = NULL,
                          selected = NULL,
                          placeholder = "inputs.placeholders.selection",
                          sort = "fixed"
                        ) %>%
                          active("grey") %>%
                          shadow()
                      )
                    }
                  )
                )
              ),
              # indicator ----
              formGroup(
                label = div(
                  i18n(
                    key = "inputs.labels.indicator",
                    plural = indicator
                  )
                  # Health indicator or Health indicators
                ),
                if (indicator == 1) {
                  selectInput(
                    i18n(
                      ns = "values.indicators",
                    ),
                    id = ns("indicator"),
                    choices = NULL,
                    values = NULL,
                    selected = NULL
                  )
                } else {
                  chipInput(
                    i18n(
                      ns = "values.indicators"
                    ),
                    id = ns("indicator"),
                    choices = NULL,
                    values = NULL,
                    selected = NULL,
                    placeholder = "inputs.placeholders.selection",
                    max = indicator,
                    inline = FALSE,
                    sort = "queue"
                  ) %>%
                    active("grey") %>%
                    shadow()
                }
              ),
              if (!is.null(dimension)) {
                # dimension ----
                formGroup(
                  label = div(
                    i18n(
                      key = "inputs.labels.dimension",
                      plural = dimension
                    )
                    # Select inequality dimension(s)
                  ),
                  if (dimension == 1) {
                    selectInput(
                      i18n(
                        ns = "values.dimensions"
                      ),
                      id = ns("dimension"),
                      choices = NULL,
                      values = NULL,
                      selected = NULL
                    )
                  } else {
                    chipInput(
                      i18n(
                        ns = "values.dimensions"
                      ),
                      id = ns("dimension"),
                      choices = NULL,
                      values = NULL,
                      selected = NULL,
                      placeholder = "inputs.placeholders.selection",
                      max = dimension,
                      sort = "queue"
                    ) %>%
                      active("grey") %>%
                      shadow()
                  }
                )
              },
              if (!is.null(measure)) {
                # measure ----
                formGroup(
                  label = div(
                    i18n(
                      key = "inputs.labels.measure",
                      plural = measure
                    )
                    # Select summary measure
                  ),
                  if (measure == 1) {
                    selectInput(
                      i18n(
                        ns = "values.measures"
                      ),
                      id = ns("measure"),
                      choices = NULL,
                      values = NULL,
                      selected = NULL,
                      placeholder = "inputs.placeholders.selection"
                    )
                  } else {
                    chipInput(
                      i18n(
                        ns = "values.measures"
                      ),
                      id = ns("measure"),
                      choices = NULL,
                      values = NULL,
                      selected = NULL,
                      placeholder = "inputs.placeholders.selection",
                      max = measure,
                      sort = "queue"
                    ) %>%
                      active("grey") %>%
                      shadow()
                  }
                )
              }
            ),
            # downloads ----
            navPane(
              id = ns(paste0("pane_", NAV$DOWNLOADS)),
              fade = FALSE,
              downloadsUI(
                id = ns("downloads"),
                chart = !is.null(options$titles),
                map = isTRUE(options$titles == "main")
              )
            ),
            # summaries ----
            if (isTRUE(summaries)) {
              navPane(
                id = ns(paste0("pane_", NAV$SUMMARIES)),
                fade = FALSE,
                summariesUI(
                  id = ns("summaries")
                )
              )
            },
            # benchmarks ----
            if (isTRUE(benchmarks)) {
              navPane(
                id = ns(paste0("pane_", NAV$BENCHMARK)),
                fade = FALSE,
                benchmarkUI(ns("benchmark"))
              )
            },
            # options ----
            if (length(options)) {
              navPane(
                id = ns(paste0("pane_", NAV$OPTIONS)),
                fade = FALSE,
                rlang::exec(optionsUI, id = ns("options"), !!!options)
              )
            }
          )
        )
      ),
      # main ----
      column(
        # class = "pl-0",
        width = 8,
        uiOutput(
          outputId = ns("disclaimer"),
          container = function(...) {
            tags$div(...) %>%
              #display("flex") %>%
              # flex(justify = "center") %>%
              font(size = "xs")
          }
        ),
        output(ns("visual"))
      )
    )
  )
}

viewServer <- function(input, output, session, Events, Data, visible,
                       year, source, indicator, dimension, measure, summaries,
                       benchmarks, render, visual, options = list(),
                       downloads = list(), language, most_recent = NULL, dataset_name,
                       is_who_dataset, is_map_dataset) {



  ns <- session$ns
  this <- ns(NULL)


  # state ----
  state <- reactiveValues(
    setting = NULL,
    source = NULL,
    year = NULL,
    indicator = NULL,
    dimension = NULL,
    measure = NULL,
    recent = NULL,
    comparisons = NULL,
    after_first_module_load = FALSE
  )


  # ├ setters ----
  setSetting <- function(values) {
    (state$setting <- default_setting(values))
  }

  setSource <- function(values) {
    state$source <- default_source(values, max = source)
  }

  setYear <- function(values) {
    state$year <- default_year(values, max = year)
  }

  setIndicator <- function(values) {
    state$indicator <- default_indicator(values, max = indicator)
  }

  setDimension <- function(values) {
    state$dimension <- default_dimension(values, max = dimension)
  }

  setMeasure <- function(values) {
    state$measure <- default_measure(values, max = measure)
  }



  # messages helper ----
  immediateMessage <- function(msg) {
    msg$from == this || msg$from == "init"
  }

  ignoreMessage <- function(msg) {
    msg$from == this && !isTRUE(msg$force)
  }

  # debug ----
  if (getOption("heat.debug", 0) >= 1) {
    debug_msg <- function(title, field, value) {
      is_visible <- isolate(isTRUE(visible()))

      cli::cat_rule(
        left = title,
        right = this,
        col = if (is_visible) cli::style_bold else cli::style_dim
      )

      cli::cat_bullet(field, col = if (is_visible) cli::style_bold else cli::style_dim)

      if (isTRUE(nzchar(value))) {
        cli::cat_line("  ", value)
      }

      cli::cat_line()
    }

    lapply(isolate(names(state)), function(x) {
      observeEvent(state[[x]], {
        val <- glue::glue_collapse(state[[x]], sep = ", ")

        debug_msg("State", x, val)
      })
    })
  }

  # nav ----
  observeEvent(input$nav, {
    showNavPane(ns(paste0("pane_", input$nav)))
  })

  observeEvent(visible(), {
    if (visible()) {
      session$sendCustomMessage("heat:activate-plot-output", list(
        id = ns("visual")
      ))
    }
  })


  observeEvent(dataset_name(), {
    m_options$reset(
      horizontal_title = if (!is.null(measure) && benchmarks) {
        translate(c(language(), "options", "labels", "average")) # "Setting average" # @translate
      }
    )
  })



  r_setting <- reactive(input$setting)
  d_setting <- debounce(r_setting, 500)

  # setting ----
  # ├ observe setting ----
  observeEvent(d_setting(), {


    req(visible())

    if(session$userData$app_init && this != "heat-explore_disag_line"){
      #Events$set_setting$prev <- Events$set_setting$from
      return()
    }

    add_time("observe d_setting()", this)


    #Events$set_setting$prev <- Events$set_setting$from
    Events$set_setting$from <- this
    Events$set_setting$selected <- input$setting

    # ││├ benchmarks ----
    benchmark_selection <- Data$country_info() %>%
      dplyr::filter(setting == !!input$setting) %>%
      dplyr::slice(1)

    if (nrow(benchmark_selection) == 0) {
      return()
    }

    new_incomes <- benchmark_selection$wbincome_name
    new_regions <- benchmark_selection$whoreg6
    new_comparisons <- Data$country_info() %>%
      dplyr::filter(
        wbincome_name == !!new_incomes,
        whoreg6 == !!new_regions
      ) %>%
      dplyr::distinct(choices = setting, values = iso3)

    Events$set_benchmark_setting <- list(
      from = this,
      choices = new_comparisons$choices,
      values = new_comparisons$values,
      selected = input$setting
    )

    new_sources <- Data$setting_yr_src() %>%
      dplyr::filter(setting == !!input$setting) %>%
      dplyr::distinct(source) %>%
      dplyr::pull() %>%
      sort()

    Events$set_source <- list(
      from = this,
      force = TRUE,
      choices = new_sources,
      values = new_sources,
      selected = new_sources
    )

    x_years <- Data$setting_yr_src() %>%
      dplyr::filter(
        setting == !!input$setting,
        source %in% new_sources
      ) %>%
      dplyr::distinct(year) %>%
      dplyr::pull()



    new_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = state$indicator, # input$indicator,
      current_dimension = state$dimension, # input$dimension,
      new_country = input$setting,
      new_source = default_source(new_sources, source),
      new_year = default_year(x_years, year)
    )

    Events$set_indicator <- list(
      from = this,
      force = TRUE,
      choices = new_indicator_dimension$indicator_choices,
      values = new_indicator_dimension$indicator_values,
      selected = new_indicator_dimension$indicator_selected
    )


    Events$set_dimension <- list(
      from = this,
      force = TRUE,
      choices = new_indicator_dimension$dimension,
      values = new_indicator_dimension$dimension,
      selected = new_indicator_dimension$dimension_selected
    )

    Events$set_year <- list(
      from = this,
      force = TRUE,
      choices = x_years,
      values = x_years,
      selected = x_years,
      recent = input$recent
    )

    state$set_comparison_null <- rnorm(1)

  })

  # ├ set_setting ----
  observeEvent(Events$set_setting, {
    msg <- Events$set_setting

    add_time("observe set_setting", this)

    if (!visible() && input$nav != NAV$SELECTION) {
      updateNavInput(id = "nav", selected = NAV$SELECTION, session = session)
    }

    if (Events$set_setting$from == "init") {
      updateNavInput(id = "nav", selected = NAV$SELECTION, session = session) #803
      Events$set_year$recent <- NULL
    }



    do_update <- function() {
      selected_setting <- setSetting(msg$selected)

      if (!ignoreMessage(msg)) {
        updateSelectInput(
          id = "setting",
          choices = msg$choices,
          values = msg$values,
          selected = selected_setting,
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

  # source ----
  if (source == 1) {
    updateSource <- updateSelectInput
    d_source <- r_source <- reactive(input$source)
  } else {
    updateSource <- updateChipInput
    r_source <- reactive(input$source)
    d_source <- debounce(r_source, 250)
  }

  # ├ observe source ----
  observeEvent(d_source(), {
    req(
      visible(),
      !all_equal(r_source(), state$source)
    )

    add_time("observe d_source()", this)

    Events$set_source <- list(
      from = this,
      selected = r_source()
    )

    new_years <- Data$setting_yr_src() %>%
      dplyr::filter(
        setting == !!input$setting,
        source %in% !!input$source
      ) %>%
      dplyr::distinct(year) %>%
      dplyr::pull()

    Events$set_year <- list(
      from = this,
      force = TRUE,
      choices = new_years,
      values = new_years,
      selected = new_years,
      recent = input$recent
    )

    new_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = input$indicator,
      current_dimension = input$dimension,
      new_country = input$setting,
      new_source = input$source,
      new_year = default_year(new_years, year)
    )

    Events$set_indicator <- list(
      from = this,
      force = TRUE,
      choices = new_indicator_dimension$indicator_choices,
      values = new_indicator_dimension$indicator_values,
      selected = new_indicator_dimension$indicator_selected
    )


    Events$set_dimension <- list(
      from = this,
      force = TRUE,
      choices = new_indicator_dimension$dimension,
      values = new_indicator_dimension$dimension,
      selected = new_indicator_dimension$dimension_selected
    )
  })

  # ├ set_source ----
  observeEvent(Events$set_source, {
    msg <- Events$set_source


    add_time("observe set_source", this)

    do_update <- function() {
      selected_source <- setSource(msg$selected)

      if (!ignoreMessage(msg)) {
        updateSource(
          id = "source",
          choices = msg$choices,
          values = msg$value,
          selected = selected_source,
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

  # year ----
  if (year == 1) {
    updateYear <- updateSelectInput
    d_year <- r_year <- reactive(input$year)
  } else {
    updateYear <- updateChipInput
    r_year <- reactive(input$year)
    d_year <- debounce(r_year, 250)
  }

  # ├ observe year ----
  observeEvent(d_year(), {
    req(
      visible(),
      req(d_year()),
      is.null(input$recent),
      !all_equal(r_year(), state$year)
    )

    add_time("observe d_year", this)

    selected_year <- r_year()


    Events$set_year <- list(
      from = this,
      selected = selected_year
    )


    x_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = input$indicator,
      current_dimension = input$dimension,
      new_country = input$setting,
      new_source = state$source,
      new_year = default_year(selected_year, year)
    )


    Events$set_year <- list(
      from = this,
      selected = selected_year
    )

    Events$set_indicator <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$indicator_choices,
      values = x_indicator_dimension$indicator_values,
      selected = x_indicator_dimension$indicator_selected
    )


    Events$set_dimension <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$dimension,
      values = x_indicator_dimension$dimension,
      selected = x_indicator_dimension$dimension_selected
    )
  })

  # ├ set_year ----
  observeEvent(Events$set_year, {
    msg <- Events$set_year

    add_time("observe set_year", this)

    do_update <- function() {
      checked <- length(msg$recent) > 0

      selected_year <- setYear(msg$selected)

      if (checked) {
        state$year <- msg$recent
      }

      if (!ignoreMessage(msg)) {
        disable <- NULL

        updateYear(
          id = "year",
          choices = msg$choices,
          values = msg$values,
          selected = selected_year,
          session = session
        )

        if (!is.null(msg$values)) {
          # updateCheckboxInput(
          #   id = "recent",
          #   choices = translate(c(isolate(language()), "inputs", "labels", "recent")),
          #   values = max(msg$values),
          #   selected = checked,
          #   session = session
          # )
        }
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  r_recent <- reactive(input$recent)
  d_recent <- debounce(r_recent, 250)
  # recent year ----
  observeEvent(d_recent(), ignoreNULL = FALSE, {
    req(visible())

    add_time("observe d_recent", this)

    Events$set_recent_year <- list(
      from = this,
      force = TRUE,
      value = input$recent,
      selected = input$year,
      no_years = is.null(input$year)
    )
  })

  observeEvent(Events$set_recent_year, {
    msg <- Events$set_recent_year

    add_time("observe set_recent_year", this)

    do_update <- function() {
      checked <- length(msg$value) > 0

      if (checked) {
        state$year <- msg$value
        state$recent <- TRUE
      } else if (!msg$no_years) {
        setYear(isolate(input$year))
        state$recent <- FALSE
      }

      if (!ignoreMessage(msg)) {
        if (!msg$no_years) {
          updateCheckboxInput(
            id = "recent",
            selected = checked,
            session = session
          )
        }

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


  # all dates ----
  observeEvent(input$alldates, {
    req(visible())

    add_time("observe input$alldates", this)

    x_years <- Data$setting_yr_src() %>%
      dplyr::filter(
        setting == !!input$setting,
        source %in% !!input$source
      ) %>%
      dplyr::distinct(year) %>%
      dplyr::pull()

    x_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = input$indicator,
      current_dimension = input$dimension,
      new_country = input$setting,
      new_source = state$source,
      new_year = default_year(x_years, year)
    )

    Events$set_indicator <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$indicator_choices,
      values = x_indicator_dimension$indicator_values,
      selected = x_indicator_dimension$indicator_selected
    )


    Events$set_dimension <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$dimension,
      values = x_indicator_dimension$dimension,
      selected = x_indicator_dimension$dimension_selected
    )

    Events$set_year <- list(
      from = this,
      force = TRUE,
      choices = x_years,
      values = x_years,
      selected = x_years,
      recent = input$recent
    )
  })


  # remove dates ----
  observeEvent(input$removedates, {
    req(visible())

    add_time("observe input$removedates", this)

    all_years <- Data$strata() %>%
      dplyr::filter(
        setting == !!input$setting,
        source %in% !!input$source
      )

    x_years <- all_years %>%
      dplyr::filter(
        indicator_abbr %in% !!input$indicator,
        dimension %in% !!input$dimension
      ) %>%
      dplyr::group_by(indicator_abbr, dimension) |>
      dplyr::summarise(
        maxyr = max(year)
      ) |>
      dplyr::pull(maxyr) |>
      unique()

    all_years <- all_years |>
      dplyr::pull(year) |>
      unique()



    x_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = input$indicator,
      current_dimension = input$dimension,
      new_country = input$setting,
      new_source = state$source,
      new_year = default_year(x_years, year)
    )

    Events$set_indicator <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$indicator_choices,
      values = x_indicator_dimension$indicator_values,
      selected = x_indicator_dimension$indicator_selected
    )


    Events$set_dimension <- list(
      from = this,
      force = TRUE,
      choices = x_indicator_dimension$dimension,
      values = x_indicator_dimension$dimension,
      selected = x_indicator_dimension$dimension_selected
    )

    Events$set_year <- list(
      from = this,
      force = TRUE,
      choices = all_years,
      values = all_years,
      selected = x_years,
      recent = input$recent
    )
  })


  # indicator ----
  if (indicator == 1) {
    updateIndicator <- updateSelectInput
    d_indicator <- r_indicator <- reactive(input$indicator)
  } else {
    updateIndicator <- updateChipInput
    r_indicator <- reactive(input$indicator)
    d_indicator <- debounce(r_indicator, 250)
  }

  # ├ observe indicator ----
  observeEvent(d_indicator(), {
    req(visible())

    add_time("observe d_indicator", this)

    Events$set_indicator <- list(
      from = this,
      selected = input$indicator
    )
  })

  # ├ set_indicator ----
  observeEvent(Events$set_indicator, {
    msg <- Events$set_indicator

    add_time("observe set_indicator", this)

    if (isTRUE(options$sorting)) {
      selected_indicator <- default_indicator(msg$selected, indicator)

      indicator_selection <- Data$strata() %>%
        dplyr::select(indicator_name, indicator_abbr) %>%
        dplyr::filter(indicator_abbr %in% !!selected_indicator) %>%
        dplyr::distinct(choices = indicator_name, values = indicator_abbr) %>%
        dplyr::arrange(choices)

      m_options$update_sort_indicator(
        choices = indicator_selection$choices,
        values = indicator_selection$values,
        selected = indicator_selection$values[1]
      )
    }

    #msg$choices <- do_indicator_indent(msg$choices, dataset_name())

    do_update <- function() {
      selected_indicator <- setIndicator(msg$selected)

      if (!ignoreMessage(msg)) {
        disable <- NULL

        # updateYear(
        #   id = "year",
        #   #choices = msg$choices,
        #   #values = msg$values,
        #   #selected = selected_year,
        #   disable = disable,
        #   session = session
        # )

        updateIndicator(
          id = "indicator",
          choices = msg$choices,
          values = msg$values,
          selected = rev(selected_indicator),
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


  observeEvent(language(), {

    add_time("observe language()", this)

    if (is.null(dimension)) {
      state$dimension <- translate(c(language(), "values", "dimensions", "Subnational region"))
    }

    m_options$reset(
      horizontal_title = if (!is.null(measure) && benchmarks) {
        translate(c(language(), "options", "labels", "average")) # "Setting average" # @translate
      }
    )
  })
  # dimension ----
  if (!is.null(dimension)) {
    if (dimension == 1) {
      updateDimension <- updateSelectInput
      d_dimension <- r_dimension <- reactive(input$dimension)
    } else {
      updateDimension <- updateChipInput
      r_dimension <- reactive(input$dimension)
      d_dimension <- debounce(r_dimension, 250)
    }

    # ├ observe dimension ----
    observeEvent(d_dimension(), {
      req(visible())

      add_time("observe d_dimension", this)

      Events$set_dimension <- list(
        from = this,
        selected = input$dimension
      )

      # IMPORTANT, need to always update _other_ view measures
      # previously (and incorrectly) the set_measure event was only fired from
      # views with a measure input
      new_measures <- pick_measures(Data$measures(), input$dimension, language())

      Events$set_measure <- list(
        from = this,
        force = TRUE,
        choices = new_measures$choices,
        values = new_measures$values,
        selected = "D", # default_measure(new_measures$values, max = measure),
        previous = state$measure
      )
    })

    # ├ set_dimension ----
    observeEvent(Events$set_dimension, {
      msg <- Events$set_dimension

      #git707
      #session$sendCustomMessage("window-resize", "msg")

      add_time("observe set_dimension", this)


      do_update <- function() {
        selected_dimension <- setDimension(msg$selected)

        if (!ignoreMessage(msg)) {
          disable <- NULL

          updateYear(
            id = "year",
            # choices = msg$choices,
            # values = msg$values,
            # selected = selected_year,
            # disable = disable,
            session = session
          )

          updateDimension(
            id = "dimension",
            choices = msg$choices,
            values = msg$values,
            selected = rev(selected_dimension),
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
  }

  # measure ----
  if (!is.null(measure)) {
    if (measure == 1) {
      updateMeasure <- updateSelectInput
      d_measure <- r_measure <- reactive(input$measure)
    } else {
      updateMeasure <- updateChipInput
      r_measure <- reactive(input$measure)
      d_measure <- debounce(r_measure, 250)
    }

    observeEvent(d_measure(), {
      req(visible())

      add_time("observe d_measure", this)

      Events$set_measure <- list(
        from = this,
        selected = input$measure,
        previous = state$measure
      )
    })

    # ├ set_measure ----
    observeEvent(Events$set_measure, {
      msg <- Events$set_measure

      add_time("observe set_measure", this)

      do_update <- function() {
        if (any(msg$previous %in% msg$values)) {
          selected_measure <- setMeasure(msg$previous)
        } else {
          selected_measure <- setMeasure(msg$selected)
        }

        if (!ignoreMessage(msg)) {
          updateMeasure(
            id = "measure",
            choices = msg$choices,
            values = msg$values,
            selected = selected_measure,
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
  }


  # gray ----


  r_setsrcyrind <- reactive({
    req(!is.null(input$indicator))
    input$setting
    input$source
    input$year
    input$indicator
    input$dimension
  })

  d_setsrcyrind <- debounce(r_setsrcyrind, 1000)

  observeEvent(d_setsrcyrind(), {

    if(session$userData$app_init && this == "heat-compare_summary_table")
      session$userData$app_init <- FALSE

    add_time("observe d_setsrcyrind", this)

    grey_out_options <- function() {
      if (!grepl("explore_disag_map", this)) {
        isolate({
          all_vals <- Data$strata() |>
            dplyr::filter(
              setting == Events$set_setting$selected,
              source %in% Events$set_source$selected
            )

          all_years <- all_vals |>
            dplyr::pull(year)

          all_dimensions <- all_vals |>
            dplyr::pull(dimension) |>
            unique()

          sel_indicator <- Events$set_indicator$selected
          sel_dimension <- Events$set_dimension$selected
          l_indicator <- length(sel_indicator)
          l_dimension <- length(sel_dimension)


          max_ind <- ifelse(is.infinite(indicator) | l_indicator < indicator, l_indicator, indicator)
          max_dim <- ifelse(is.infinite(dimension) | l_dimension < dimension, l_dimension, dimension)

          valid_vals <- Data$strata() |>
            dplyr::filter(
              setting == Events$set_setting$selected,
              source %in% Events$set_source$selected,
              indicator_abbr %in% Events$set_indicator$selected[1:max_ind]
            )

          valid_dimensions <- valid_vals |>
            dplyr::pull(dimension) |>
            unique()

          valid_years <- valid_vals |>
            dplyr::filter(
              dimension %in% Events$set_dimension$selected[1:max_dim]
            ) |>
            dplyr::pull(year)


          invalid <- all_years[!all_years %in% valid_years]
          if (length(invalid) == 0) invalid <- 9999

          session$sendCustomMessage("make-invalid-year", list(
            valid = unique(valid_years),
            invalid = unique(invalid),
            from = this
          ))


          invalid <- all_dimensions[!all_dimensions %in% valid_dimensions]
          if (length(invalid) == 0) invalid <- 9999

          session$sendCustomMessage("make-invalid-dimension", list(
            valid = unique(valid_dimensions),
            invalid = unique(invalid),
            from = this
          ))
        })
      }
    }


    if (visible()) {
      grey_out_options()
    } else {
      session$onFlushed(grey_out_options)
    }
  })



  # benchmarks module ----
  if (isTRUE(benchmarks)) {
    benchmark <- callModule(
      benchmarkServer, "benchmark",
      parent = this,
      Data = Data, Events = Events,
      visible = reactive(input$nav == NAV$BENCHMARK),
      language = language # ,
      # state = state
    )

    r_comparison <- benchmark$comparison
    r_benchmark_source <- benchmark$benchmark_source

    observe(state$comparison <- r_comparison())
    observe(state$benchmark_source <- r_benchmark_source())


    r_benchmark_year_beg <- benchmark$benchmark_year_beg
    r_benchmark_year_end <- benchmark$benchmark_year_end
    r_benchmark_year_near <- benchmark$benchmark_year_near


  }

  observeEvent(state$set_comparison_null, {
    state$comparison <- NULL
  }, priority = 100000)

  # options module ----
  m_options <- callModule(
    optionsServer, "options",
    Events = Events,
    parent = this,
    visible = reactive(input$nav == NAV$OPTIONS),
    is_log_scale = if (!is.null(measure)) reactive(is_log_scale(input$measure)),
    language = language
  )

  if (getOption("heat.debug", 0) >= 1) {
    lapply(names(m_options), function(opt) {
      if (shiny::is.reactive(m_options[[opt]])) {
        observeEvent(m_options[[opt]](), {
          debug_msg("Option", opt, m_options[[opt]]())
        })
      }
    })
  }




  # ├ observe update highlight subgroup ----
  if (isTRUE(options$subgroup_highlight)) {
    observe({
      req(state$setting, state$year, state$dimension, language())

      new_subgroups <- isolate(Data$main()) %>%
        dplyr::filter(
          setting == !!state$setting,
          year %in% !!state$year,
          dimension == !!state$dimension
        ) %>%
        dplyr::distinct(subgroup) %>%
        dplyr::pull(subgroup)

      m_options$update_highlight_subgroup(
        choices = new_subgroups,
        values = new_subgroups,
        selected = ""
      )
    })
  }

  # ├ observe title_main ----
  if (!is.null(options$title)) {
    observeEvent(r_data(), {

      add_time("observe r_data for titles", this)
      m_options$set_title_main(options$title(r_data(), language()))

      if (!is.null(measure)) {
        if(state$measure %in% c("R", "D")){
          m_options$set_title_vertical(measure_name(state$measure, language()))
          #m_options$set_title_vertical(get_summary_vtitle(r_data(), language()))
        } else {
          m_options$set_title_vertical(measure_name(state$measure, language()))
        }


      }
      add_time("end observe r_data for titles", this)
    })

  }

  # disclaimer ----
  output$disclaimer <- renderUI({

    #req(all(isolate(Events$set_benchmark_comparison$selected) %in% state$comparison))

    is_map_dataset <- isolate(is_map_dataset()) && !is_heat_plus()
    is_who_dataset <- isolate(is_who_dataset()) && !is_heat_plus()
    is_compare_page <- is_compare(this)

    shiny::tagList(
      tags$p(
        class = "text-muted p-no-margin-bottom p-margin-top",
        if(!is_who_dataset){
          #get_nonwho_disclaimer()
          i18n(key = "charts.disclaimers.disclaimer")
        }
      ),
      tags$p(
        class = "text-muted p-no-margin-bottom p-margin-top",
        if(is_compare_page){
          i18n(key = "charts.disclaimers.compareineq")
        }
      ),
      tags$p(
        class = "text-muted p-no-margin-bottom p-margin-top",
        if (
          NROW(r_data()) == 0 |
          !state$setting %in% r_data()$setting |
          (isTRUE(options$disclaimer == "map") && !(unique(r_data()$iso3) %in% heatdata::spatial_national$iso3))
        ) {
          if (isTRUE(options$disclaimer == "map") && is_map_dataset) {
            # "There is no data for this combination of variables. Maps are not available for MICS and RHS data."
            i18n(key = "charts.disclaimers.nomap")
          } else if (isTRUE(options$disclaimer == "map") && !is_map_dataset) {
            i18n(key = "charts.disclaimers.nomapdataset")
            #"There are no maps associated with this dataset (we need to translate this)"
            # i18n(key = "charts.disclaimers.nonmapdataset")
          } else {
            # "There is no data for the combination of variables"
            i18n(key = "charts.disclaimers.nodefault")
          }
        } else {
          if (isTRUE(options$disclaimer == "map")) {
            # "If estimates are not shown for a selected combination of variables, then data are not available. Maps are not available for MICS and RHS data."
            i18n("charts.warnings.availablemap")
          } else {
            # "If estimates are not shown for a selected combination of variables, then data are not available."
            i18n("charts.warnings.available")
          }
        }
      )
    )

  })

  # data ----
  if (benchmarks && is.null(measure)) {
    # ├ Compare disaggregated ----


    r_data <- reactive({



      req(
        visible(),
        state$setting,
        state$source,
        state$year,
        state$indicator,
        state$dimension,
        state$comparison,
        grepl("compare_disag_", this),
        setequal(Events$set_benchmark_comparison$selected, state$comparison),#git695
        cancelOutput = TRUE
      )


      add_time("begin r_data compare-disag", this)

      heat_data <- isolate(Data$main())
      year_to_int <- isolate(Data$date_to_integer())

      heat_data <- heat_data %>%
        dplyr::filter(
          indicator_abbr %in% state$indicator,
          dimension %in% state$dimension
        )


      if (isTruthy(d_recent())) {
        max_focus_year <- heat_data |>
          dplyr::filter(
            setting == state$setting
          ) |>
          dplyr::pull(year) |>
          max() |>
          suppressWarnings()
      } else {
        max_focus_year <- state$year
      }

      focus_year_int <- year_to_int %>%
        dplyr::filter(
          year == max_focus_year
        ) %>%
        dplyr::pull(year_int)


      if (!isTruthy(focus_year_int)) {
        return()
      }

      year_beg_int <- year_to_int %>%
        dplyr::filter(
          year == r_benchmark_year_beg()
        ) %>%
        dplyr::pull(year_int)

      year_end_int <- year_to_int %>%
        dplyr::filter(
          year == r_benchmark_year_end()
        ) %>%
        dplyr::pull(year_int)

      has_bench_years <- all(
        is_value(r_benchmark_year_beg()),
        is_value(r_benchmark_year_end())
      )


      if (!has_bench_years | is_value(r_benchmark_year_near())) {
        setting_data <- heat_data %>%
          dplyr::filter(setting %in% state$setting & year == max_focus_year)

        comparison_data <- heat_data %>%
          dplyr::filter(iso3 %in% state$comparison | setting %in% state$comparison) %>%
          dplyr::filter(source %in% state$benchmark_source) %>%
          dplyr::filter(setting != state$setting) %>%
          dplyr::mutate(year_diff = abs(year_int - focus_year_int)) %>%
          dplyr::arrange(year_diff) %>%
          dplyr::group_by(setting) %>%
          dplyr::mutate(min_diff = suppressWarnings(min(year_diff, na.rm = TRUE))) %>%
          dplyr::filter(year_diff == min_diff) %>%
          dplyr::select(-year_diff, -min_diff)

        benchmark_data <- dplyr::bind_rows(
          setting_data,
          comparison_data
        ) %>%
          dplyr::select(-year_int)

        # git680
        cntry_count <- benchmark_data |>
          dplyr::distinct(setting, year) |>
          dplyr::group_by(setting) |>
          dplyr::summarise(
            n = dplyr::n(),
            maxyr = suppressWarnings(max(year))
          )

        if (any(cntry_count$n > 1)) {
          benchmark_data <- dplyr::semi_join(
            benchmark_data,
            cntry_count,
            by = c("setting", "year" = "maxyr")
          )
        }
      } else {
        if (length(year_beg_int) == 0) {
          return()
        }

        setting_data <- heat_data %>%
          dplyr::filter(setting %in% state$setting & year == max_focus_year)

        comparison_data <- heat_data %>%
          dplyr::filter(iso3 %in% state$comparison | setting %in% state$comparison) %>%
          dplyr::filter(setting != state$setting) %>%
          dplyr::filter(
            year_int >= year_beg_int,
            year_int <= year_end_int
          )


        benchmark_data <- dplyr::bind_rows(
          setting_data,
          comparison_data
        ) %>%
          dplyr::select(-year_int)
      }

      add_time("end r_data compare-disag", this)
      benchmark_data
    })
  } else if (!is.null(measure)) {
    # ├ Compare summary ----
    if (benchmarks) {
      r_data <- reactive({

        data_measures <- isolate(Data$measures())
        year_to_int <- isolate(Data$date_to_integer())

        st_setting <- state$setting
        st_source <- state$source
        st_year <- state$year
        d_recent <- d_recent()
        st_indicator <- state$indicator
        st_dimension <- state$dimension
        st_comparison <- state$comparison
        st_measure <- state$measure
        benchmark_year_beg <- r_benchmark_year_beg()
        benchmark_year_end <- r_benchmark_year_end()
        benchmark_year_near <- r_benchmark_year_near()
        benchmark_source <- r_benchmark_source()
        all_comparison <- setequal(Events$set_benchmark_comparison$selected, state$comparison)#git695


        req(
          visible(),
          st_setting,
          st_source,
          st_year,
          st_indicator,
          st_dimension,
          st_comparison,
          all_comparison,
          grepl("compare_summary_", this)
        )
        add_time("begin r_data compare-summary", this)

        has_bench_years <- all(
          is_value(benchmark_year_beg),
          is_value(benchmark_year_end)
        )

        data_measures <- data_measures %>%
          dplyr::filter(
            indicator_abbr %in% st_indicator,
            dimension %in% st_dimension,
            measure %in% st_measure
          )


        if (isTruthy(d_recent)) {
          max_focus_year <- data_measures |>
            dplyr::filter(
              setting == st_setting
            ) |>
            dplyr::pull(year) |>
            max() |>
            suppressWarnings()
        } else {
          max_focus_year <- st_year
        }

        focus_year_int <- year_to_int %>%
          dplyr::filter(
            year == max_focus_year
          ) %>%
          dplyr::pull(year_int)

        if (!isTruthy(focus_year_int)) {
          return()
        }

        year_beg_int <- year_to_int %>%
          dplyr::filter(
            year == benchmark_year_beg
          ) %>%
          dplyr::pull(year_int)

        year_end_int <- year_to_int %>%
          dplyr::filter(
            year == benchmark_year_end
          ) %>%
          dplyr::pull(year_int)

        if (!has_bench_years | is_value(benchmark_year_near)) {
          setting_data <- data_measures %>%
            dplyr::filter(setting %in% st_setting & year == max_focus_year)


          comparison_data <- data_measures %>%
            dplyr::filter(iso3 %in% st_comparison | setting %in% st_comparison) %>%
            dplyr::filter(source %in% benchmark_source) %>%
            dplyr::filter(setting != st_setting) %>%
            dplyr::mutate(year_diff = abs(year_int - focus_year_int)) %>%
            dplyr::arrange(year_diff) %>%
            dplyr::group_by(setting) %>%
            dplyr::mutate(min_diff = suppressWarnings(min(year_diff, na.rm = TRUE))) %>%
            dplyr::filter(year_diff == min_diff) %>%
            dplyr::select(-year_diff, -min_diff)

          if(all(is.na(setting_data$r_national)) || all(is.na(comparison_data$r_national)))
            return()

          benchmark_data <- dplyr::bind_rows(
            setting_data,
            comparison_data
          ) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(setting == st_setting)) %>%
            dplyr::select(-year_int)

          # git680
          cntry_count <- benchmark_data |>
            dplyr::distinct(setting, year) |>
            dplyr::group_by(setting) |>
            dplyr::summarise(
              n = dplyr::n(),
              maxyr = suppressWarnings(max(year))
            )

          if (any(cntry_count$n > 1)) {
            benchmark_data <- dplyr::semi_join(
              benchmark_data,
              cntry_count,
              by = c("setting", "year" = "maxyr")
            )
          }

          if (any(cntry_count$n > 1)) {
            benchmark_data <- dplyr::semi_join(
              benchmark_data,
              cntry_count,
              by = c("setting", "year" = "maxyr")
            )
          }
        } else {
          if (length(year_beg_int) == 0) {
            return()
          }

          setting_data <- data_measures %>%
            dplyr::filter(setting %in% st_setting & year == max_focus_year)

          comparison_data <- data_measures %>%
            dplyr::filter(iso3 %in% st_comparison | setting %in% st_comparison) %>%
            dplyr::filter(setting != st_setting) %>%
            dplyr::filter(
              year_int >= year_beg_int,
              year_int <= year_end_int
            )

          if(all(is.na(setting_data$r_national)) || all(is.na(comparison_data$r_national)))
            return()

          benchmark_data <- dplyr::bind_rows(
            setting_data,
            comparison_data
          ) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(setting == st_setting)) %>%
            dplyr::select(-year_int)
        }

        add_time("end r_data compare-summary", this)
        benchmark_data
      })
    } else {
      # ├ Explore summary ----
      r_data <- reactive({
        req(
          visible(),
          state$setting,
          state$source,
          state$year,
          state$indicator,
          state$dimension,
          state$measure
        )

        add_time("begin r_data explore-summary", this)

        recent <- isolate(state$recent)

        summary_dat <- isolate(Data$measures()) %>%
          dplyr::filter(
            setting %in% state$setting,
            source %in% state$source,
            indicator_abbr %in% state$indicator,
            dimension %in% state$dimension,
            measure %in% state$measure
          )

        if (!is.null(recent) && recent) {
          summary_dat <- summary_dat |>
            dplyr::filter(year == max(summary_dat$year))
        } else {
          summary_dat <- summary_dat |>
            dplyr::filter(
              year %in% state$year
            )
        }
        add_time("end r_data explore-summary", this)
        summary_dat
      })
    }
    # ├ Explore disaggregated bar, line, and table ----
  } else {
    r_data <- reactive({


      #browser()
      data_main <- isolate(Data$main())

      lang <- isolate(language())
      st_setting <- state$setting
      st_source <- state$source
      st_year <- state$year
      st_recent <- isolate(state$recent)
      st_indicator <- state$indicator
      st_dimension <- state$dimension

      req(
        visible(),
        st_setting,
        st_source,
        st_year,
        st_indicator,
        st_dimension
      )

      add_time("begin r_data explore-disag", this)

      if (lang != "en" && "Subnational region" %in% st_dimension) {
        trans_dim <- translate(c(lang, "values", "dimensions", "Subnational region"))
        st_dimension[st_dimension == "Subnational region"] <- trans_dim
      }

      if (!is.null(st_recent) && st_recent) {
        dat <- data_main %>%
          dplyr::filter(
            setting %in% st_setting,
            source %in% st_source,
            indicator_abbr %in% st_indicator,
            dimension %in% !!st_dimension,
          ) %>%
          dplyr::group_by(
            setting,
            indicator_abbr,
            dimension
          ) %>%
          dplyr::mutate(
            maxyr = suppressWarnings(max(year))
          ) %>%
          dplyr::filter(
            year == maxyr
          ) %>%
          dplyr::ungroup()
      } else {
        dat <- data_main %>%
          dplyr::filter(
            setting %in% st_setting,
            source %in% st_source,
            year %in% st_year,
            indicator_abbr %in% st_indicator,
            dimension %in% !!st_dimension,
          )
      }

      add_time("end r_data explore-disag", this)
      dat
    })
  }

  # visual reactive ----
  r_visual <- reactive({

    req(
      NROW(r_data()) > 0 &
        state$setting %in% r_data()$setting,
        !grepl("compare", this) ||
        (grepl("compare", this) && all(isolate(Events$set_benchmark_comparison$selected) %in% state$comparison))
        )


    add_time("begin creating visual", this)
    visual(
      .data = r_data(),
      title_main = m_options$title_main(), # state$title_main,
      title_horizontal = m_options$title_horizontal(),
      title_vertical = m_options$title_vertical(),
      axis_min = m_options$axis_min(),
      axis_max = m_options$axis_max(),
      axis_horizontal_min = m_options$axis_horizontal_min(),
      axis_horizontal_max = m_options$axis_horizontal_max(),
      axis_vertical_min = m_options$axis_vertical_min(),
      axis_vertical_max = m_options$axis_vertical_max(),
      sort_by = m_options$sort_by(),
      sort_order = m_options$sort_order(),
      sort_indicator = m_options$sort_indicator(),
      highlight_subgroup = m_options$highlight_subgroup(),
      plot_lines = m_options$reference_lines(),
      data_labels = m_options$data_labels(),
      conf_int = m_options$confidence_interval(),
      label_style = m_options$label_format()$style,
      label_size = m_options$label_format()$size,
      focus_setting = state$setting,
      decimal_places = m_options$decimal_places(),
      table_decimals = m_options$table_decimals(),
      columns = m_options$columns(),
      data_only = FALSE, # isolate(input$nav != NAV$DOWNLOADS),
      rename = TRUE,
      language = language(),
      recent = isTRUE(state$recent),
      dataset_name = dataset_name(),
      is_who_dataset = isolate(is_who_dataset())
    )
  })

  # render visual ----
  output$visual <- render({
    req(visible())
    if (getOption("heat.debug", 0) >= 1) {
      debug_msg("Visual", "render", NULL)
    }

    add_time("render visual", this)
    r_visual()
  })



  # download visual ----
  # data download, chart or table data?
  if (isTRUE(downloads$chart)) {
    if (!is.null(measure)) {
      r_data_download <- reactive({
        # summary specific columns
        r_data() %>%
          dplyr::transmute(
            setting, year, source, indicator_abbr, indicator_name, dimension,
            measure_abbr = measure, measure_name = measure_name(measure_abbr, language()),
            estimate = inequal, se, ci_lb = se.lowerci, ci_ub = se.upperci,
            setting_average = r_national, iso3
          ) %>%
          dplyr::arrange(
            dplyr::desc(setting == state$setting), setting,
            dplyr::desc(year), source, indicator_name, dimension, measure_abbr
          )
      })
    } else {
      r_data_download <- reactive({
        add_time("reactive r_data_download 1", this)
        # disaggregated specific columns
        r_data() %>%
          dplyr::select(
            setting, year, source, indicator_abbr, indicator_name,
            dimension, subgroup, estimate, se, ci_lb, ci_ub, population,
            flag, setting_average, iso3, favourable_indicator,
            indicator_scale, ordered_dimension, subgroup_order,
            reference_subgroup
          ) %>%
          dplyr::arrange(
            dplyr::desc(setting == state$setting), setting,
            dplyr::desc(year), source, indicator_name, dimension,
            subgroup_order, subgroup
          )
      })
    }
  } else {
    r_data_download <- reactive({
      i_data <- r_data()
      req(NROW(i_data) > 0)
      add_time("reactive r_data_download 2", this)
      visual(
        .data = i_data,
        focus_setting = state$setting,
        table_decimals = Inf,
        columns = m_options$columns(),
        data_only = TRUE, # isolate(input$nav == NAV$DOWNLOADS),
        rename = FALSE,
        language = language()
      )
    })
  }
  callModule(
    downloadsServer, "downloads",
    chart = if (isTRUE(downloads$chart)) r_visual,
    data = r_data_download,
    language = language,
    is_who_dataset = is_who_dataset
  )


  # summaries ----
  if (isTRUE(summaries)) {
    callModule(
      summariesServer, "summaries",
      r_data = r_data,
      language = language
    )
  }

  # nullify ----
  observeEvent(Events$nullify, {
    state$setting <- NULL
    updateSelectInput(
      id = "setting", choices = list(), selected = FALSE,
      session = session
    )

    state$source <- NULL
    updateSource(
      id = "source", choices = list(), selected = FALSE,
      session = session
    )

    state$year <- NULL
    updateYear(
      id = "year", choices = list(), selected = FALSE,
      session = session
    )

    state$indicator <- NULL
    updateIndicator(
      id = "indicator", choices = list(), selected = FALSE,
      session = session
    )

    if (!is.null(dimension)) {
      state$dimension <- NULL
      updateDimension(
        id = "dimension", choices = list(), selected = FALSE,
        session = session
      )
    }

    if (!is.null(measure)) {
      state$measure <- NULL
      updateMeasure(
        id = "measure", choices = list(), selected = FALSE,
        session = session
      )
    }

    state$title_main <- NULL
    m_options$set_title_main("")

    m_options$update_highlight_subgroup(list(), list(), NULL)
  })
}
