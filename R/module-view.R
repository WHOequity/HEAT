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
  benchmark = icon("sliders-h"),
  options = icon("cog"),
  summaries = icon("th-list"),
  downloads = icon("file-download")
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
                    key = "inputs.labels.source_plural",  # Select data source(s)
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
        uiOutput(class = "col add-margin-bottom",
          outputId = ns("disclaimer"),
          container = function(...) {
            tags$div(...) %>%
              display("flex") %>%
              #flex(justify = "center") %>%
              font(size = "xs") %>%
              height(2)
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
                       downloads = list(), language) {
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
    comparisons = NULL

    # title_main = NULL
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

        debug_msg("State",  x, val)
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




  # setting ----
  # ├ observe setting ----
  observeEvent(c(input$setting), {
    req(
      visible()#
      # !all_equal(input$setting, state$setting)
    )

    Events$set_setting <- list(
      from = this,
      selected = input$setting
    )

    # ││├ benchmarks ----
    benchmark_selection <- Data$country_info() %>%
      dplyr::filter(setting == !!input$setting) %>%
      dplyr::slice(1)

    new_incomes <- benchmark_selection$wbincome_name
    new_regions <- benchmark_selection$whoreg6
    new_comparisons <- Data$country_info() %>%
      dplyr::filter(
        wbincome_name == !!new_incomes,
        whoreg6 == !!new_regions
      ) %>%
      dplyr::distinct(choices = setting, values = iso3)

    state$comparison <- new_comparisons$values


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

    Events$set_year <- list(
      from = this,
      force = TRUE,
      choices = x_years,
      values = x_years,
      selected = x_years,
      recent = as.numeric(input$recent)
    )

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
  })

  # ├ set_setting ----
  observeEvent(Events$set_setting, {
    msg <- Events$set_setting

    # showNavPane(ns(paste0("pane_", NAV$SELECTION)), session = session)
    if (!visible() && input$nav != NAV$SELECTION) {
      updateNavInput(id = "nav", selected = NAV$SELECTION, session = session)
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
      recent = as.numeric(input$recent)
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
    d_year <- r_year <- reactive(as.numeric(input$year))
  } else {
    updateYear <- updateChipInput
    r_year <- reactive(as.numeric(input$year))
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

    # if(length(x_indicator_dimension$indicator_choices) == 0){
    #
    #   Events$set_indicator <- list(
    #     from = this,
    #     force = TRUE,
    #     selected = input$indicator
    #   )
    #
    #   Events$set_dimension <- list(
    #     from = this,
    #     force = TRUE,
    #     selected = input$dimension
    #   )
    # }else{
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
    # }


  })

  # ├ set_year ----
  observeEvent(Events$set_year, {
    msg <- Events$set_year

    do_update <- function() {
      checked <- length(msg$recent) > 0

      selected_year <- setYear(msg$selected)

      if (checked) {
        state$year <- msg$recent
      }

      if (!ignoreMessage(msg)) {
        updateYear(
          id = "year",
          choices = msg$choices,
          values = msg$values,
          selected = selected_year,
          session = session
        )

        if (!is.null(msg$values)) {
          updateCheckboxInput(
            id = "recent",
            # choices = "Most recent year",
            choices = "Most recent year",
            values = max(msg$values),
            selected = checked,
            session = session
          )
        }
      }
    }

    if (immediateMessage(msg)) {
      do_update()
    } else {
      session$onFlushed(do_update)
    }
  })

  # recent year ----
  observeEvent(input$recent, ignoreNULL = FALSE, {
    req(visible())

    Events$toggle_recent_year <- list(
      from = this,
      force = TRUE,
      value = as.numeric(input$recent),
      selected = as.numeric(input$year),
      no_years = is.null(input$year)
    )
  })

  observeEvent(Events$toggle_recent_year, {
    msg <- Events$toggle_recent_year

    do_update <- function() {
      checked <- length(msg$value) > 0

      if (checked) {
        state$year <- msg$value
      } else if (!msg$no_years) {
        setYear(msg$selected)
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

    Events$set_indicator <- list(
      from = this,
      selected = input$indicator
    )
  })

  # ├ set_indicator ----
  observeEvent(Events$set_indicator, {
    msg <- Events$set_indicator

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

    do_update <- function() {
      selected_indicator <- setIndicator(msg$selected)

      if (!ignoreMessage(msg)) {
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

    if(is.null(dimension)){
      state$dimension <- translate(c(language(), "values", "dimensions", "Subnational region"))
    }

    m_options$reset(
      horizontal_title = if (!is.null(measure) && benchmarks) {
        translate(c(language(), "options", "labels", "average"))#"Setting average" # @translate
      }
    )

})

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

      do_update <- function() {
        selected_dimension <- setDimension(msg$selected)

        if (!ignoreMessage(msg)) {
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

      Events$set_measure <- list(
        from = this,
        selected = input$measure,
        previous = state$measure
      )
    })

    # ├ set_measure ----
    observeEvent(Events$set_measure, {
      msg <- Events$set_measure

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



  # benchmarks module ----
  if (isTRUE(benchmarks)) {

    benchmark <- callModule(
      benchmarkServer, "benchmark",
      parent = this,
      Data = Data, Events = Events,
      visible = reactive(input$nav == NAV$BENCHMARK),
      language = language
    )

    r_comparison <- benchmark$comparison

    observe(state$comparison <- r_comparison())

    r_variance <- benchmark$variance
  }

  # options module ----
  m_options <- callModule(
    optionsServer, "options",
    Events = Events,
    parent = this,
    visible = reactive(input$nav == NAV$OPTIONS),
    is_log_scale = if (!is.null(measure)) reactive(is_log_scale(input$measure))
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
        selected = ''
      )
    })
  }

  # ├ observe title_main ----
  if (!is.null(options$title)) {
    observeEvent(r_data(), {

      # state$title_main <- options$title(r_data())
      #
      # m_options$set_title_main(state$title_main)
      m_options$set_title_main(options$title(r_data(), language()))

      if (!is.null(measure)) {
        m_options$set_title_vertical(measure_name(state$measure, language()))
      }
    })

    # observeEvent(m_options$title_main(), {
    #   state$title_main <- m_options$title_main()
    # })
  }

  # disclaimer ----
  output$disclaimer <- renderUI({

    tags$p(
      class = "text-muted p-no-margin-bottom",
      if (
        NROW(r_data()) == 0 |
        !state$setting %in% r_data()$setting |
        (isTRUE(options$disclaimer == "map") && !(unique(r_data()$iso3) %in% heatdata::spatial_national$iso3))
        ) {
        if (isTRUE(options$disclaimer == "map")) {
          # "There is no data for this combination of variables. Maps are not available for MICS and RHS data."
          i18n(key = "charts.disclaimers.nomap")
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
  })

  # data ----
  if (benchmarks && is.null(measure)) {
    # ├ Compare disaggregated ----
    r_data <- reactive({

      req(
        state$setting,
        state$source,
        state$year,
        state$indicator,
        state$dimension,
      )

      m_year <- max(state$year)
      v_year <- seq.int(state$year - r_variance(), state$year + r_variance())

      Data$main() %>%
        dplyr::filter(
          (setting %in% state$setting & year == state$year) |
            ((iso3 %in% state$comparison | setting %in% state$comparison) & year %in% v_year),
          #source == state$source, #I re-allowed this, but could it be an issue?
          indicator_abbr %in% state$indicator,
          dimension %in% state$dimension
        ) %>%
        dplyr::group_by(setting) %>%
        dplyr::filter(year == closest(year, m_year)) %>%
        dplyr::ungroup()


    })
  } else if (!is.null(measure)) {
    # ├ Compare summary ----
    if (benchmarks) {
      r_data <- reactive({
        req(
          state$setting,
          state$source,
          state$year,
          state$indicator,
          state$dimension
        )

        m_year <- max(state$year)
        v_year <- seq.int(state$year - r_variance(), state$year + r_variance())

        Data$measures() %>%
          dplyr::filter(
            (setting %in% state$setting & year == state$year) |
              ((iso3 %in% state$comparison | setting %in% state$comparison) & year %in% v_year),
            #source == state$source,#I re-allowed this, but could it be an issue?
            indicator_abbr %in% state$indicator,
            dimension %in% state$dimension,
            measure %in% state$measure
          ) %>%
          dplyr::group_by(setting) %>%
          dplyr::filter(year == closest(year, m_year)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(dplyr::desc(setting == state$setting))
      })
    } else {
      # ├ Explore summary ----
      r_data <- reactive({

        req(
          state$setting,
          state$source,
          state$year,
          state$indicator,
          state$dimension,
          state$measure
        )

        Data$measures() %>%
          dplyr::filter(
            setting %in% state$setting,
            source %in% state$source,
            year %in% state$year,
            indicator_abbr %in% state$indicator,
            dimension %in% state$dimension,
            measure %in% state$measure
          )
      })
    }
    # ├ Explore disaggregated bar, line, and table ----
  } else {
    r_data <- reactive({
      req(
        state$setting,
        state$source,
        state$year,
        state$indicator,
        state$dimension
      )

      lang <- isolate(language())
      dimension <- state$dimension

      if (lang != "en" && dimension == "Subnational region") {
        dimension <- translate(c(lang, "values", "dimensions", dimension))
      }

      Data$main() %>%
        dplyr::filter(
          setting %in% state$setting,
          source %in% state$source,
          year %in% state$year,
          indicator_abbr %in% state$indicator,
          dimension %in% !!dimension,
        )
    })
  }

  # visual reactive ----
  r_visual <- reactive({
    req(NROW(r_data()) > 0 & state$setting %in% r_data()$setting)

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
      data_only = FALSE,#isolate(input$nav != NAV$DOWNLOADS),
      rename = TRUE,
      language = language()
    )
  })

  # render visual ----
  output$visual <- render({
    visible()

    if (getOption("heat.debug", 0) >= 1) {
      debug_msg("Visual", "render", NULL)
    }

    r_visual()
  })
  # shiny::outputOptions(output, "visual", suspendWhenHidden = FALSE)

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
      visual(
        .data = i_data,
        focus_setting = state$setting,
        table_decimals = m_options$table_decimals(),
        columns = m_options$columns(),
        data_only = TRUE, #isolate(input$nav == NAV$DOWNLOADS),
        rename = FALSE,
        language = language()
      )
    })
  }
  callModule(
    downloadsServer, "downloads",
    chart = if (isTRUE(downloads$chart)) r_visual,
    data = r_data_download,
    language = language
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
