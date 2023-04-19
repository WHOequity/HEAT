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

optionsUI <- function(id, data_labels, confidence_intervals, reference_lines,
                      subgroup_highlight, sorting, axis_limits, label_format,
                      titles, columns_disaggregated, columns_summary,
                      decimal_places, lang) {
  ns <- NS(id)

  list(
    # disaggregated table columns ----
    if (columns_disaggregated) {

      list(
        h6(i18n("options.labels.content")), # Table content,
        hr() %>% margin(top = 0) %>% background("black"),
        formGroup(
          label = div(i18n("options.labels.variables")), # "Variables",
          chipInput(
            i18n(
              ns = "options.values",
              target = "button"
            ),
            id = ns("columns"),
            inline = TRUE,
            placeholder = "inputs.placeholders.selection",
            choices = c(
              "setting",
              "year",
              "indicatorname",
              "dimension",
              "subgroup",
              "estimate",
              "popshare",
              "source",
              "indicatorabbr",
              "lowerbound",
              "upperbound",
              "flag",
              "settingavg"
            ),
            values =  c(
              "setting",
              "year",
              "indicatorname",
              "dimension",
              "subgroup",
              "estimate",
              "popshare",
              "source",
              "indicatorabbr",
              "lowerbound",
              "upperbound",
              "flag",
              "settingavg"
            ),
            selected = c(
              "setting", "year", "indicatorname", "dimension", "subgroup",
              "estimate", "popshare"
            )
          ) %>%
            active("grey") %>%
            shadow()
        ),
        formGroup(
          label = div(i18n("options.labels.decimals")), # "Number of decimals",
          radiobarInput(
            id = ns("table_decimals"),
            choices = 0:5,
            selected = 1
          ) %>%
            shadow("small")
        )
      )
    },
    # summary table columns ----
    if (columns_summary) {

      list(
        h6(i18n("options.labels.content")), # Table content"),
        hr() %>% margin(top = 0) %>% background("black"),
        formGroup(
          label = div(i18n("options.labels.variables")), # Variables",
          chipInput(
            i18n(
              ns = "options.values",
              target = "button"
            ),
            id = ns("columns"),
            inline = TRUE,
            placeholder = "inputs.placeholders.selection",
            choices = list(
              "setting",
              "year",
              "indicatorname",
              "dimension",
              "measurename",
              "estimate",
              "source",
              "indicatorabbr",
              "measureabbr",
              "lowerbound",
              "upperbound",
              "settingavg"
            ),
            values = c(
              "setting",
              "year",
              "indicatorname",
              "dimension",
              "measurename",
              "estimate",
              "source",
              "indicatorabbr",
              "measureabbr",
              "lowerbound",
              "upperbound",
              "settingavg"
            ),
            selected = c(
              "setting", "year", "indicatorname", "dimension", "measurename",
              "estimate"
            )
          ) %>%
            active("grey") %>%
            shadow()
        ),
        formGroup(
          label = div(i18n("options.labels.decimals")), # "Number of decimals",
          radiobarInput(
            id = ns("table_decimals"),
            choices = 0:5,
            selected = 1
          ) %>%
            shadow("small")
        )
      )
    },
    # label format ----
    if (isTRUE(label_format)) {
      list(
        h6(i18n("options.labels.graphstyle")), # "Graph style"),
        hr() %>% margin(top = 0) %>% background("black"),
        formGroup(
          label = div(i18n("options.values.format")),#"Format",
          radioInput(
            id = ns("label_point"),
            # choices = c("Points", "ISO3 Labels", "Setting Labels"),
            choices = list(
              div(i18n("options.labels.points")),
              div(i18n("options.labels.iso3")),
              div(i18n("options.labels.setting"))
            ),
            values = c("points", "iso3", "setting"),
            selected = "points"
          )
        ),
        formGroup(
          label = div(i18n("options.labels.size")), # "Size",
          radiobarInput(
            id = ns("label_size"),
            # choices = c("Small", "Medium", "Large"),
            choices = list(
              div(i18n("options.values.small")),
              div(i18n("options.values.medium")),
              div(i18n("options.values.large"))
            ),
            values = c("small", "medium", "large"),
            selected = "medium"
          )
        )
      )
    },
    # data labels & decimals ----
    if (isTRUE(data_labels) || isTRUE(decimal_places)) {
      list(
        h6(i18n("options.labels.data")),  # "Data labels"),
        hr() %>% margin(top = 0) %>% background("black"),
        if (isTRUE(data_labels)) {
          formGroup(
            label = div(i18n("options.labels.size")), # "Size",
            radiobarInput(
              id = ns("data_labels"),
              # choices = c("None", "Small", "Medium", "Large"),
              choices = list(
                div(i18n("options.values.none")),
                div(i18n("options.values.small")),
                div(i18n("options.values.medium")),
                div(i18n("options.values.large"))
              ),
              values = c("none", "small", "medium", "large"),
              selected = "none"
            ) %>%
              shadow("small")
          )
        },
        if (isTRUE(decimal_places)) {
          formGroup(
            label = div(i18n("options.labels.decimals")), # "Number of decimals",
            radiobarInput(
              id = ns("decimal_places"),
              choices = 0:5,
              selected = 1
            ) %>%
              shadow("small")
          )
        }
      )
    },
    if (isTRUE(confidence_intervals)) { # confidence intervals ----
      list(
        h6(i18n("options.labels.ci")), # "Confidence intervals"),
        hr() %>% margin(top = 0) %>% background("black"),
        checkboxInput(
          id = ns("confidence_interval"),
          # choices = c("Include 95% confidence intervals"),
          choices = list(
            div(i18n("options.labels.include95"))
          ),
          values = "include"
        ) %>%
          margin(bottom = 3)
      )
    },
    if (isTRUE(reference_lines)) { # explore detail reference lines ----
      list(
        h6(i18n("options.labels.reference")), # "Reference lines"),
        hr() %>% margin(top = 0) %>% background("black"),
        switchInput(
          id = ns("reference_lines"),
          # choices = c("Setting average", "Median"),
          choices = list(
            div(i18n("options.labels.average")),
            div(i18n("options.labels.median"))
          ),
          values = c("setting", "median"),
          selected = "median"
        ) %>%
          margin(bottom = 3)
      )
    },
    if (isTRUE(sorting)) { # explore detail sorting ----
      list(
        h6(i18n("options.labels.sortby")), # "Sort data"),
        hr() %>% margin(top = 0) %>% background("black"),
        formGroup(
          label = div(i18n("options.labels.sortby")), # "Sort by",
          radioInput(
            id = ns("sort_by"),
            # choices = c("Subgroup", "Indicator"),
            choices = list(
              div(i18n("options.labels.subgroup")),
              div(i18n("options.labels.indicator"))
            ),
            values = c("subgroup", "indicator"),
            selected = "subgroup"
          )
        ) %>%
          margin(bottom = 0),
        # git 504
        # collapsePane(
        #   id = ns("pane_indicator"),
        #   animate = FALSE,
        #   selectInput(
        #     id = ns("sort_indicator")
        #   ) %>%
        #     margin(top = 2)
        # ),
        formGroup(
          label = div(i18n("options.labels.sortorder")), # "Sort order",
          radioInput(
            id = ns("sort_order"),
            # choices = c("Ascending", "Descending"),
            choices = list(
              div(i18n("options.labels.ascending")),
              div(i18n("options.labels.descending"))
            ),
            values = c("ascending", "descending"),
            selected = "ascending"
          )
        ) %>%
          margin(top = 2)
      )
    },
    if (isTRUE(subgroup_highlight)) { # explore detail subgroup highlight ----
      list(
        h6(i18n("options.labels.highlight")), # "Highlight subgroup"),
        hr() %>% margin(top = 0) %>% background("black"),
        chipInput(
          id = ns("highlight_subgroup"),
          inline = TRUE,
          placeholder = "inputs.placeholders.selection"
        ) %>%
          active("grey") %>%
          margin(bottom = 3)
      )
    },
    if (axis_limits == 2) { # axis limits (2) ----
      list(
        h6(i18n("options.labels.range")), # "Axis range"
        hr() %>% margin(top = 0) %>% background("black"),
        formRow(
          formGroup(
            width = 6,
            label = div(i18n("options.labels.min")), # "Axis minimum"
            numberInput(
              id = ns("axis_min"),
              step = "any"
            ) %>%
              width("3/5")
          ),
          formGroup(
            width = 6,
            label = div(i18n("options.labels.max")), # Axis maximum",
            numberInput(
              id = ns("axis_max"),
              step = "any"
            ) %>%
              width("3/5")
          )
        )
      )
    } else if (axis_limits == 4) { # compare summary axis limits (4) ----
      list(
        h6(i18n("options.labels.range")), # "Axis range"),
        hr() %>% margin(top = 0) %>% background("black"),
        formRow(
          formGroup(
            width = 6,
            label = div(i18n("options.labels.hmin")), # "Horizontal axis minimum",
            numberInput(
              id = ns("axis_horizontal_min")
            ) %>%
              width("3/5")
          ),
          formGroup(
            width = 6,
            label = div(i18n("options.labels.hmax")), # "Horizontal axis maximum",
            numberInput(
              id = ns("axis_horizontal_max")
            ) %>%
              width("3/5")
          )
        ),
        formRow(
          formGroup(
            width = 6,
            label = div(i18n("options.labels.vmin")), # "Vertical axis minimum",
            numberInput(
              id = ns("axis_vertical_min")
            ) %>%
              width("3/5")
          ),
          formGroup(
            width = 6,
            label = div(i18n("options.labels.vmax")), # "Vertical axis maximum",
            numberInput(
              id = ns("axis_vertical_max")
            ) %>%
              width("3/5")
          )
        )
      )
    },
    if (length(titles) > 0) {
      list(
        h6(i18n("options.labels.graph")), # "Graph titles"),
        hr() %>% margin(top = 0) %>% background("black"),
        if ("main" %in% titles) { # main title ----
          formGroup(
            label = div(i18n("options.labels.title")), # Main title",
            textInput(
              id = ns("main_title")
            )
          )
        },
        if ("horizontal" %in% titles) { # horizontal title ----
          formGroup(
            label = div(i18n("options.labels.htitle")), # "Horizontal axis title",
            textInput(
              id = ns("horizontal_title")
            )
          )
        },
        if ("vertical" %in% titles) { # vertical title ----
          formGroup(
            label = div(i18n("options.labels.vtitle")), # Vertical axis title",
            textInput(
              id = ns("vertical_title")
            )
          )
        }
      )
    }
  )
}

optionsServer <- function(input, output, session, parent,
                          Events, visible, is_log_scale = NULL, language) {
  ns <- session$ns
  this <- ns(NULL)

  add_time("enter optionsServer", this)

  # helpers ----
  ignoreMessage <- function(msg) {
    msg$from != parent && !isTRUE(msg$force)
  }

  immediateMessage <- function(msg) {
    msg$from == parent || msg$from == this ||
      msg$from == "init"
  }

  # state ----
  state <- reactiveValues(
    data_labels = NULL,
    confidence_intervals = NULL,
    main_title = NULL,
    vertical_title = NULL,
    horizontal_title = NULL,
    sort_indicator = NULL,
    sort_by = NULL,
    sort_order = NULL,
    reference_lines = NULL,
    axis_max = NULL,
    axis_min = NULL,
    axis_vertical_max = NULL,
    axis_vertical_min = NULL,
    axis_horizontal_max = NULL,
    axis_horizontal_min = NULL
  )

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

  # data labels ----
  d_data_labels <- debounce(reactive({
    input$data_labels
  }), 250)

  observe(state$data_labels <- d_data_labels())

  # conf intervals ----
  observe(state$confidence_interval <- input$confidence_interval)

  # titles ----

  # ├ main ----
  r_main_title <- reactive(input$main_title %||% "")
  d_main_title <- debounce(r_main_title, 350)

  observe(state$main_title <- d_main_title())

  set_title_main <- function(x) {
    state$main_title <- x

    updateTextInput(
      id = "main_title",
      value = x,
      session = session
    )
  }

  # ├ horizontal  ----
  r_horizontal_title <- reactive(input$horizontal_title)
  d_horizontal_title <- debounce(r_horizontal_title, 350)

  observe(state$horizontal_title <- input$horizontal_title)

  set_title_horizontal <- function(x) {
    state$horizontal_title <- x

    updateTextInput(
      id = "horizontal_title",
      value = x %||% "",
      session = session
    )
  }

  # ├ vertical ----
  r_vertical_title <- reactive(input$vertical_title)
  d_vertical_title <- debounce(r_vertical_title, 350)

  observe(state$vertical_title <- input$vertical_title)

  set_title_vertical <- function(x) {

    state$vertical_title <- x

    updateTextInput(
      id = "vertical_title",
      value = x %||% "",
      session = session
    )
  }

  # sorting ----
  observeEvent(input$sort_by, {
    if (input$sort_by == "indicator") {
      showCollapsePane(ns("pane_indicator"))
    } else {
      hideCollapsePane(ns("pane_indicator"))
    }
  })

  # ├ sort by indicator ----
  observe(state$sort_by <- input$sort_by)
  observe(state$sort_order <- input$sort_order)
  observe(state$sort_indicator <- input$sort_indicator)

  # │├ set_sort_indicator ----
  update_sort_indicator <- function(choices, values, selected) {
    state$sort_indicator <- selected

    updateSelectInput(
      id = "sort_indicator",
      choices = choices,
      values = values,
      selected = selected,
      session = session
    )
  }

  # highlight subgroup ----
  r_highlight_subgroup <- reactive(input$highlight_subgroup)
  d_highlight_subgroup <- debounce(r_highlight_subgroup, 350)

  observeEvent(d_highlight_subgroup(), {
    state$highlight_subgroup <- r_highlight_subgroup()
  }, ignoreNULL = FALSE)

  update_highlight_subgroup <- function(choices, values, selected) {
    state$highlight_subgroup <- selected


    updateChipInput(
      id = "highlight_subgroup",
      choices = choices,
      values = values,
      session = session,
      selected = selected
    )
  }

  # reference lines ----
  observe(state$reference_lines <- input$reference_lines)

  # table columns ----
  d_columns <- debounce(reactive({
    input$columns
  }), 350)

  # decimal places ----
  r_decimal_places <- reactive({
    if (is.null(input$decimal_places)) {
      1
    } else {
      as.numeric(input$decimal_places)
    }
  })

  r_table_decimals <- reactive({
    if (is.null(input$table_decimals)) {
      1
    } else {
      as.numeric(input$table_decimals)
    }
  })


  # axis min and max ----
  observe(state$axis_max <- input$axis_max)
  observe(state$axis_min <- input$axis_min)
  observe(state$axis_horizontal_min <- input$axis_horizontal_min)
  observe(state$axis_horizontal_max <- input$axis_horizontal_max)
  observe(state$axis_vertical_min <- input$axis_vertical_min)
  observe(state$axis_vertical_max <- input$axis_vertical_max)

  # # ├ axis min for log scale ----
  # observeEvent(input$axis_min, ignoreNULL = FALSE, {
  #   state$axis_min <- input$axis_min
  # })
  #
  # observeEvent(input$axis_vertical_min, ignoreNULL = FALSE, {
  #   state$axis_vertical_min <- input$axis_vertical_min
  # })

  if (!is.null(is_log_scale)) {
    observe({

      less_than_zero <- isTRUE(input$axis_min <= 0)

      updateTextInput(
        id = "axis_min",
        invalid = if (is_log_scale() && less_than_zero) {
          translate(c(isolate(language()), "charts", "warnings", "axismin")) #Axis minimum must be greater than 0 for log-scale axis"
        },
        session = session
      )

      if (less_than_zero) {
        state$axis_min <- 0.1
      }
    })

    observe({

      less_than_zero <- isTRUE(input$axis_vertical_min <= 0)

      updateTextInput(
        id = "axis_vertical_min",
        invalid = if (is_log_scale() && less_than_zero) {
          translate(c(isolate(language()), "charts", "warnings", "verticalaxismin")) #"Vertical axis minimum must be greater than 0 for log-scale axis"
        },
        session = session
      )

      if (less_than_zero) {
        state$axis_vertical_min <- 0.1
      }
    })
  }

  # reset options ----
  reset <- function(horizontal_title = NULL, vertical_title = NULL) {
    # ├ axes ----
    add_time("options reset", this)
    state$axis_min <- NULL
    state$axis_max <- NULL
    state$axis_horizontal_min <- NULL
    state$axis_horizontal_max <- NULL
    state$axis_vertical_min <- NULL
    state$axis_vertical_max <- NULL

    updateTextInput(id = "axis_min", value = "", session = session)
    updateTextInput(id = "axis_max", value = "", session = session)
    updateTextInput(id = "axis_horizontal_min", value = "", session = session)
    updateTextInput(id = "axis_horizontal_max", value = "", session = session)
    updateTextInput(id = "axis_vertical_min", value = "", session = session)
    updateTextInput(id = "axis_vertical_max", value = "", session = session)

    # ├ labels & decimals ----
    state$data_labels <- "none"
    updateRadiobarInput(id = "data_labels", selected = "none", session = session)
    updateRadiobarInput(id = "decimal_places", selected = 1, session = session)
    updateRadiobarInput(id = "table_decimals", selected = 1, session = session)

    # ├ title ----

    set_title_horizontal(horizontal_title)


    set_title_vertical(vertical_title)

    # ├ conf intervals ----
    state$confidence_interval <- NULL
    updateCheckboxInput(id = "confidence_interval", selected = FALSE, session = session)

    # reference line ----
    state$reference_lines <- "median"
    updateSwitchInput(id = "reference_lines", selected = "median", session = session)

    # sorting ----
    state$sort_by <- "subgroup"
    state$sort_order <- "ascending"

    updateRadioInput(id = "sort_by", selected = "subgroup", session = session)
    updateRadioInput(id = "sort_order", selected = "ascending", session = session)
  }
  add_time("end options reset", this)
  # return list of reactives ----
  list(
    reset = reset,

    data_labels = reactive(state$data_labels),
    confidence_interval = reactive(!is.null(state$confidence_interval)),
    reference_lines = reactive(state$reference_lines),
    highlight_subgroup = reactive(state$highlight_subgroup),
    update_highlight_subgroup = update_highlight_subgroup,
    sort_by = reactive(state$sort_by),
    sort_indicator = reactive(state$sort_indicator),
    update_sort_indicator = update_sort_indicator,
    sort_order = reactive(state$sort_order),
    axis_min = reactive(state$axis_min),
    axis_max = reactive(state$axis_max),
    axis_horizontal_min = reactive(state$axis_horizontal_min),
    axis_horizontal_max = reactive(state$axis_horizontal_max),
    axis_vertical_min = reactive(state$axis_vertical_min),
    axis_vertical_max = reactive(state$axis_vertical_max),
    label_format = reactive({
      list(style = input$label_point, size = input$label_size)
    }),
    title_main = reactive(state$main_title),
    set_title_main = set_title_main,
    title_horizontal = reactive(state$horizontal_title),
    set_title_horizontal = set_title_horizontal,
    title_vertical = reactive(state$vertical_title),
    set_title_vertical = set_title_vertical,
    columns = d_columns,
    decimal_places = r_decimal_places,
    table_decimals = r_table_decimals
  )
}
