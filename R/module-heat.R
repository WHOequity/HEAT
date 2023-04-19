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

#' HEAT Application
#'
#' The HEAT application.
#'
#' @export
heatApp <- function(launch.browser = TRUE, port = 8080) {
  options(
    shiny.launch.browser = launch.browser,
    shiny.port = port,
    shiny.host = "0.0.0.0"
  )

  if(incl_timers())
    options(digits.secs = 2)

  shinyOptions(cache = cachem::cache_mem(max_size = 2000e6))

  shinyApp(
    ui = list(
      waiter::use_waiter(),
      #htmltools::htmlDependencies(highchartOutput(NULL)),
      tags$head(
        locales(),
        assets()
      ),
      waiter::waiter_show_on_load(
        html = div(
          div(style="display: block; margin-left: auto; margin-right: auto; width: 30%",
              img("heat-assets/img/who-logo-white.png")
          ),
          d2("Health Equity Assessment Toolkit") %>%
            font(color = "white") %>%
            margin(bottom = 5, right = 2, left = 2),
          tags$h1("Initializing") %>%
            font(color = "white"),
          div(
            waiter::spin_circle()
          ) %>%
            margin(top = -4)
        ),
        color = "#008dc9"#,
          #logo = "heat-assets/img/who-logo-white.png"

      ),
      heatUI(
        id = "heat",
        home = homeUI("home")#,
        #nav_extra = dataChooseDropdown("dc")
      )
    ),
    server = function(input, output, session) {

      r_lang <- reactive({
        input$lang
      })

      m_home <- callModule(
        homeServer, "home"
      )




      session$onFlushed(function() {

        waiter::waiter_hide()
        #showModal(licenseModal())

        m_data_choose <- callModule(
          dataChooseServer, "dc",
          language = reactive("en")
        )

        showModal(
          dataManagementModal(
            session$ns("dc"),
            current_data = "rep_rmnch",
            language = "en",
            add_cancel = FALSE
          )
        )

        observeEvent(m_data_choose$dataset_name(), {

          waiter::waiter_show(
            html = div(
              tags$h1(translate(c("en", "navigation", "labels", "spinner"))) %>%
                font(color = "white"),
              div(
                waiter::spin_circle()
              ) %>%
                margin(top = -4)
            ),
            color = "#008dc9"

          )
          callModule(
            heatServer, "heat",
            open_explore = m_home$open_explore,
            open_compare = m_home$open_compare,
            language = r_lang,
            initial_dataset = m_data_choose$dataset_name()
          )

        })


      })
    },
    onStart = function() {
      if(incl_timers() && file.exists("~/junk/timings.csv")){
        file.remove("~/junk/timings.csv")
      }
    }
  )
}

#' @export
heatUI <- function(id, home, nav_extra = NULL) {
  ns <- NS(id)

  webpage(
    nav = container(
      navbar(
        collapse = "xl",
        brand = div(
          # mobile language selector ----
          div(class="mobile-lang-select",
              languageSelect(),
              if (!getOption("heat.plus", FALSE)) actionButton(inputId = ns("datachoose"), label = tags$span(i18n("navigation.labels.choose_dataset"))) |>
                margin(right = 2),
          ) %>%
            margin(right = 2),

          tags$img(
            height = "30px",
            src = "heat-assets/img/who-logo-white.png"
          ),
          span(id="heat-header-title-desktop",
               # "Health Equity Assessment Toolkit",
               if (is_heat_plus()) {
                 i18n("navigation.labels.heatplus")
               } else {
                 i18n("navigation.labels.heat")
               }
               # if (is_heat_plus()) "Plus (HEAT Plus)" else "(HEAT)"
          ) %>%
            font(color = "white", weight = "bold", align = "center") %>%
            padding(left = 0, right = 0, top = 2, bottom = 2) %>%
            display("flex") %>%
            flex(justify = "center")
        ) %>%
          display("flex") %>%
          width(1/2) %>%
          flex(justify = "between", align = "center"),
        # nav ----
        navInput(
          i18n(
            ns = "navigation.labels"
          ),
          id = ns("nav"),
          choices = list(
            "home",
            menuInput(
              align = "right",
              id = ns("explore"),
              label = "explore",
              choices = c(
                "disaggregated",
                "summary"
              ),
              values = c("disag", "summary")
            ),
            menuInput(
              align = "right",
              id = ns("compare"),
              label = "compare",
              choices = c(
                "disaggregated",
                "summary"
              ),
              values = c("disag", "summary")
            ),
            menuInput(
              align = "right",
              id = ns("about"),
              label = "about",
              choices = c(
                "manual",
                "technotes",
                if (!is_heat_plus()) "glossary",
                "training",
                "software",
                "versions",
                "license",
                "feedback",
                "acknowledgements"
              )
            )
          ),
          values = c("home", "explore", "compare", "about"),
          selected = "home"
        ) %>%
          margin(left = -4, right = -4) %>%
          active(if (getOption("heat.plus", FALSE)) "green" else "orange"),
        #if (getOption("heat.plus", TRUE)) dataChooseDropdown(ns("dc")),
        nav_extra  # extra nav components ----
        # div(class="desktop-lang-select",
        #     languageSelect()
        # )
      ) %>%
        padding(all = 0),
      div(id="heat-header-title-mobile",
          # "Health Equity Assessment Toolkit",
          if (is_heat_plus()) {
            i18n("navigation.labels.heatplus")
          } else {
            i18n("navigation.labels.heat")
          }
          # if (is_heat_plus()) "Plus (HEAT Plus)" else "(HEAT)"
      ) %>%
        font(color = "white", weight = "bold", align = "center") %>%
        padding(left = 0, right = 0, top = 2, bottom = 2) %>%
        display("flex") %>%
        flex(justify = "center")
    ) %>%
      flex(align = "center") %>%
      background("blue") %>%
      shadow("small") |>
      affix("sticky"),
    navContent(
      navPane(
        class = "active",
        id = ns("pane_home"),
        fade = FALSE,
        home # home ----
      ),
      navPane( # other ----
               fade = FALSE,
               class = "container-fluid",
               id = ns("pane_other"),
               columns(
                 column(
                   width = c(xs = 12, sm = 10),
                   htmlOutput(
                     outputId = ns("about_content")
                   )
                 ) %>%
                   margin(l = "auto", r = "auto")
               )
      ),
      navPane(
        id = ns("pane_main"),
        fade = FALSE,
        container(
          columns(
            column(
              width = 4,

              navContent(

                navPane(
                  fade = FALSE,
                  id = ns("pane_explore_disag_titles"),
                  uiOutput(ns('dataname_expdis')),
                  div(class = "grey-pane-title",
                    tags$span(i18n("navigation.labels.explore")),
                    "| ",
                    tags$span(i18n("navigation.labels.disag"))
                  )
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_explore_summary_titles"),
                  uiOutput(ns('dataname_expsum')),
                  div(class = "grey-pane-title",
                      tags$span(i18n("navigation.labels.explore")),
                      "| ",
                      tags$span(i18n("navigation.labels.summary"))
                  )
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_compare_disag_titles"),
                  uiOutput(ns('dataname_comdis')),
                  div(class = "grey-pane-title",
                      tags$span(i18n("navigation.labels.compare")),
                      "| ",
                      tags$span(i18n("navigation.labels.disag"))
                  )
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_compare_summary_titles"),
                  uiOutput(ns('dataname_comsum')),
                  div(class = "grey-pane-title",
                      tags$span(i18n("navigation.labels.compare")),
                      "| ",
                      tags$span(i18n("navigation.labels.summary"))
                  )
                )
              )
            ),
            column(
              class = "controls-margtop align-items-center justify-content-center subtract-15-padding",
              width = 8,
              navContent(
                # chart / table view nav ----
                class = "heat-view-controls",
                uiOutput(ns("dataset_title")),
                navPane(
                  fade = FALSE,
                  id = ns("pane_nav_explore_disag"),
                  radiobarInput(
                    class = "btn-group-justified",
                    id = ns("nav_explore_disag"),
                    choices = drop_nulls(list(
                      list(div(i18n("navigation.labels.hline")), icon("chart-line")),
                      list(div(i18n("navigation.labels.vbar")), span(class = "fa fa-flip-horizontal", icon(class = "fa-rotate-270", "chart-bar"))),
                      list(div(i18n("navigation.labels.hbar")),  icon("chart-bar")),
                      if (!getOption("heat.plus", FALSE)) list(div(i18n("navigation.labels.map")), icon("globe")),
                      list(div(i18n("navigation.labels.table")), icon("table"))
                    )),
                    values = c(
                      "line",
                      "bar",
                      "detail",
                      if (!getOption("heat.plus", FALSE)) "map",
                      "table"
                    ),
                    selected = "line"
                  ) %>%
                    active(if (getOption("heat.plus", FALSE)) "green" else "orange")
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_nav_explore_summary"),
                  radiobarInput(
                    id = ns("nav_explore_summary"),
                    choices = list(
                      list(div(i18n("navigation.labels.bar")), icon("chart-bar")),
                      list(div(i18n("navigation.labels.line")), icon("chart-line")),
                      list(div(i18n("navigation.labels.table")), icon("table"))
                    ),
                    values = c(
                      "bar",
                      "line",
                      "table"
                    ),
                    selected = "bar"
                  ) %>%
                    active(if (getOption("heat.plus", FALSE)) "green" else "orange")
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_nav_compare_disag"),
                  radiobarInput(
                    id = ns("nav_compare_disag"),
                    choices = list(
                      list(div(i18n("navigation.labels.graph")), icon("chart-line")),
                      list(div(i18n("navigation.labels.table")), icon("table"))
                    ),
                    values = c(
                      "graph",
                      "table"
                    ),
                    selected = "graph"
                  ) %>%
                    active(if (getOption("heat.plus", FALSE)) "green" else "orange")
                ),
                navPane(
                  fade = FALSE,
                  id = ns("pane_nav_compare_summary"),
                  radiobarInput(
                    id = ns("nav_compare_summary"),
                    choices = list(
                      list(div(i18n("navigation.labels.graph")), icon("chart-line")),
                      list(div(i18n("navigation.labels.table")), icon("table"))
                    ),
                    values = c(
                      "graph",
                      "table"
                    ),
                    selected = "graph"
                  ) %>%
                    active(if (getOption("heat.plus", FALSE)) "green" else "orange")
                )
              )
            )
          )
        ),
        navContent(
          navPane(
            fade = FALSE,
            id = ns("pane_explore_disag_line"),
            # explore disaggregated line ----
            viewUI(
              id = ns("explore_disag_line"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = 5,
              dimension = 5,
              measure = NULL,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = FALSE,
                decimal_places = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_disag_bar"),
            # explore disaggregated bar ----
            viewUI(
              id = ns("explore_disag_bar"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = 5,
              dimension = 5,
              measure = NULL,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = TRUE,
                decimal_places = TRUE,
                confidence_intervals = TRUE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_disag_detail"),
            # explore disaggregated detail ----
            viewUI(
              id = ns("explore_disag_detail"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS, NAV$SUMMARIES),
              source = Inf,
              year = 1,
              indicator = 5,
              dimension = 1, #if (isTRUE(getOption("heat.plus"))) 1,
              measure = NULL,
              summaries = TRUE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = TRUE,
                decimal_places = TRUE,
                confidence_intervals = TRUE,
                reference_lines = TRUE,
                subgroup_highlight = TRUE,
                sorting = TRUE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_disag_map"),
            # explore disaggregated map ----
            viewUI(
              id = ns("explore_disag_map"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = 1,
              indicator = 1,
              dimension = NULL,
              measure = NULL,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = FALSE,
                label_format = FALSE,
                titles = c("main"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_disag_table"),
            # explore disaggregated table ----
            viewUI(
              id = ns("explore_disag_table"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = Inf,
              dimension = Inf,
              measure = NULL,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) DT::dataTableOutput(...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = FALSE,
                label_format = FALSE,
                titles = NULL,
                columns_disaggregated = TRUE,
                columns_summary = FALSE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_summary_bar"),
            # explore summary bar ----
            viewUI(
              id = ns("explore_summary_bar"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = 5,
              dimension = 5,
              measure = 1,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = TRUE,
                confidence_intervals = TRUE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE,
                decimal_places = TRUE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_summary_line"),
            # explore summary line ----
            viewUI(
              id = ns("explore_summary_line"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = 5,
              dimension = 5,
              measure = 1,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = TRUE,
                confidence_intervals = TRUE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE,
                decimal_places = TRUE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_explore_summary_table"),
            # explore summary table ----
            viewUI(
              id = ns("explore_summary_table"),
              nav = c(NAV$SELECTION, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = Inf,
              indicator = Inf,
              dimension = Inf,
              measure = Inf,
              summaries = FALSE,
              benchmarks = FALSE,
              output = function(...) DT::dataTableOutput(...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = FALSE,
                label_format = FALSE,
                titles = NULL,
                columns_disaggregated = FALSE,
                columns_summary = TRUE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_compare_disag_graph"),
            # compare disaggregated graph ----
            viewUI(
              id = ns("compare_disag_graph"),
              nav = c(NAV$SELECTION, NAV$BENCHMARK, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = 1,
              indicator = 1,
              dimension = 1,
              measure = NULL,
              summaries = FALSE,
              benchmarks = TRUE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 2,
                label_format = FALSE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_compare_disag_table"),
            # compare disaggregated table ----
            viewUI(
              id = ns("compare_disag_table"),
              nav = c(NAV$SELECTION, NAV$BENCHMARK, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = 1,
              indicator = Inf,
              dimension = Inf,
              measure = NULL,
              summaries = FALSE,
              benchmarks = TRUE,
              output = function(...) DT::dataTableOutput(...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = FALSE,
                label_format = FALSE,
                titles = NULL,
                columns_disaggregated = TRUE,
                columns_summary = FALSE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_compare_summary_graph"),
            # compare summary graph ----
            viewUI(
              id = ns("compare_summary_graph"),
              nav = c(NAV$SELECTION, NAV$BENCHMARK, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = 1,
              indicator = 1,
              dimension = 1,
              measure = 1,
              summaries = FALSE,
              benchmarks = TRUE,
              output = function(...) uiOutput(class = "heat-plot-output container-fluid", ...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = 4,
                label_format = TRUE,
                titles = c("main", "horizontal", "vertical"),
                columns_disaggregated = FALSE,
                columns_summary = FALSE,
                decimal_places = FALSE
              )
            )
          ),
          navPane(
            fade = FALSE,
            id = ns("pane_compare_summary_table"),
            # compare summary table ----
            viewUI(
              id = ns("compare_summary_table"),
              nav = c(NAV$SELECTION, NAV$BENCHMARK, NAV$OPTIONS, NAV$DOWNLOADS),
              source = Inf,
              year = 1,
              indicator = Inf,
              dimension = Inf,
              measure = Inf,
              summaries = FALSE,
              benchmarks = TRUE,
              output = function(...) DT::dataTableOutput(...),
              options = list(
                data_labels = FALSE,
                confidence_intervals = FALSE,
                reference_lines = FALSE,
                subgroup_highlight = FALSE,
                sorting = FALSE,
                axis_limits = FALSE,
                label_format = FALSE,
                titles = NULL,
                columns_disaggregated = FALSE,
                columns_summary = TRUE,
                decimal_places = FALSE
              )
            )
          )
        )
      )
    )
  )
}

#' @export
heatServer <- function(input, output, session, Data = NULL,
                       open_explore = NULL, open_compare = NULL,
                       nullify = NULL, on_data_open = NULL, language = NULL,
                       initial_dataset = "rep_rmnch") {
  ns <- session$ns


  Events <- reactiveValues(
    set_setting = NULL,
    set_source = NULL,
    set_year = NULL,
    set_recent_year = NULL,
    set_indicator = NULL,
    set_dimension = NULL,
    set_benchmark_setting = NULL,
    set_benchmark_income = NULL,
    set_benchmark_region = NULL,
    set_benchmark_comparison = NULL,
    set_benchmark_variance = NULL,
    set_benchmark_year_beg = NULL,
    set_benchmark_year_end = NULL,
    set_title_main = NULL,
    nullify = NULL
  )

  # state ----
  state <- reactiveValues(
    data_change_explore_disag = FALSE,
    data_change_explore_summary = FALSE,
    data_change_compare_disag = FALSE,
    data_change_compare_summary = FALSE,
    data_name = initial_dataset,
    force_ui_data_refresh = FALSE # git687
  )

  # debug ----
  # debug all event changes
  debug <- getOption("heat.debug", 0)
  if (debug >= 2) {
    debug_msg <- function(event, fields, values) {
      cli::cat_rule(left = "Event", right = event, col = cli::style_bold)

      purrr::walk2(fields, values, function(field, value) {
        cli::cat_bullet(cli::style_bold(field), bullet = "play")
        cli::cat_line(
          "  ", cli::style_dim(
            strtrim(
              glue::glue_collapse(value, sep = ", "),
              cli::console_width() - 2
            )
          )
        )
      })

      cli::cat_line()
    }

    lapply(isolate(names(Events)), function(x) {
      observeEvent(Events[[x]], {
        debug_msg(x, names(Events[[x]]), Events[[x]])
      })
    })
  }


    observeEvent(input$chartexists_debug_timer, {
        add_time("#### CHART LOADED")
    })





  # data select server ----
  m_data_choose <- callModule(
    dataChooseServer, "dc",
    language = language
  )

  #m_data_choose$dataset_name()
  dataset_name <- reactive({
    if(is.null(m_data_choose$dataset_name())){
      state$data_name
    } else {
      m_data_choose$dataset_name()
    }
  })


  # Data$ object ----
  if (is.null(Data)) {
    Data <- list(
      main = reactive({
        req(language(), dataset_name())

        add_time("Begin reading main data")

        heatdata::get_heat_table(dataset_name(), "data_raw") |>
          heatdata::translate_subset(language = language())

      }),
      measures = reactive({
        req(language(), dataset_name())

        add_time("Begin reading inequality data")

        heatdata::get_heat_table(dataset_name(), "data_inequality_measures") |>
          heatdata::translate_subset(language = language())

      }),
      strata = reactive({
        req(language(), dataset_name())

        add_time("Begin reading strata data")

        heatdata::get_heat_table(dataset_name(), "data_strata") |>
          heatdata::translate_subset(language = language())

      }),
      setting_yr_src = reactive({
        req(language(), dataset_name())

        add_time("Begin reading setting_yr_src data")

        heatdata::get_heat_table(dataset_name(), "info_setting_yr_src") |>
          heatdata::translate_subset(language = language())

      }),
      country_info = reactive({
        req(language(), dataset_name())

        add_time("Begin reading country_info data")

        heatdata::get_heat_table(dataset_name(), "data_countries") |>
          heatdata::translate_subset(language = language())

      }),
      date_to_integer = reactive({
        req(language(), dataset_name())

        add_time("Begin reading date_to_integer data")

        heatdata::get_heat_table(dataset_name(), "info_date_to_integer") |>
          heatdata::translate_subset(language = language())

      })
    )
  }

  # nullify ----
  if (!is.null(nullify)) {
    observeEvent(nullify(), {
      Events$nullify <- (Events$nullify %||% 0) + 1
    })
  }

  # nav logic ----

  observeEvent(input$nav, {
    clicked <- input$nav

    if (clicked == "home") {
      showNavPane(ns("pane_home"))
    } else if (clicked == "explore" || clicked == "compare") {
      showNavPane(ns("pane_main"))
    } else {
      showNavPane(ns("pane_other"))
    }
  })

  .about_cache <- list()
  output$about_content <- renderUI({
    req(input$nav == "about")

    file <- switch(
      input$about,
      "manual" = "manual.html",
      "glossary" = "compendium.html",
      "technotes" = "technical.html",
      "software" = "software.html",
      "versions" = "versions.html",
      "license" = "license.html",
      "feedback" = "feedback.html",
      "acknowledgements" = "acknowledgements.html",
      "training" = "training.html"
    )

    path <- file.path("www", "locales", "en", file)

    if (is.null(.about_cache[[path]])) {
      lines <- readLines(system.file(path, package = get_heat_prefix()))

      metadata <- heatdata::info_databases |>
        dplyr::filter(internal_name == dataset_name()) |>
        dplyr::pull(metadata)

      metadata <- paste0("heat-assets/locales/en/", metadata)

      lines <- stringr::str_replace(lines,"heat-assets/locales/en/indicator_compendium.pdf", metadata)

      content <- HTML(paste(lines, collapse = "\n"))

      .about_cache[[path]] <- content

      content
    } else {
      .about_cache[[path]]
    }
  })
  outputOptions(output, "about_content", suspendWhenHidden = FALSE)

  observe({

    state$force_ui_data_refresh

    req(input$nav == "explore")

    if (input$explore == "disag") {
      clicked <- input$nav_explore_disag

      isolate({
        if(state$data_change_explore_disag){
          clicked <- "line"
          state$data_change_explore_disag <- FALSE
        }
      })


      if (clicked == "line") {
        showNavPane(ns("pane_explore_disag_line"))
      } else if (clicked == "bar") {
        showNavPane(ns("pane_explore_disag_bar"))
      } else if (clicked == "detail") {
        showNavPane(ns("pane_explore_disag_detail"))
      } else if (clicked == "map") {
        showNavPane(ns("pane_explore_disag_map"))
      } else if (clicked == "table") {
        showNavPane(ns("pane_explore_disag_table"))
      }

      showNavPane(ns("pane_nav_explore_disag"))
      showNavPane(ns("pane_explore_disag_titles"))
    } else {
      clicked <- input$nav_explore_summary

      isolate({
        if(state$data_change_explore_summary){
          clicked <- "bar"
          state$data_change_explore_summary <- FALSE
        }
      })



      if (clicked == "bar") {
        showNavPane(ns("pane_explore_summary_bar"))
      } else if (clicked == "line") {
        showNavPane(ns("pane_explore_summary_line"))
      } else if (clicked == "table") {
        showNavPane(ns("pane_explore_summary_table"))
      }

      showNavPane(ns("pane_nav_explore_summary"))
      showNavPane(ns("pane_explore_summary_titles"))
    }

  })

  observe({

    req(input$nav == "compare")

    if (input$compare == "disag") {
      clicked <- input$nav_compare_disag


      isolate({
        if(state$data_change_compare_disag){
          clicked <- "graph"
          state$data_change_compare_disag <- FALSE
        }
      })



      if (clicked == "graph") {
        showNavPane(ns("pane_compare_disag_graph"))
      } else if (clicked == "table") {
        showNavPane(ns("pane_compare_disag_table"))
      }

      showNavPane(ns("pane_nav_compare_disag"))
      showNavPane(ns("pane_compare_disag_titles"))
    } else {
      clicked <- input$nav_compare_summary

      isolate({
        if(state$data_change_compare_summary){
          clicked <- "graph"
          state$data_change_compare_summary <- FALSE
        }
      })



      if (clicked == "graph") {
        showNavPane(ns("pane_compare_summary_graph"))
      } else if (clicked == "table") {
        showNavPane(ns("pane_compare_summary_table"))
      }

      showNavPane(ns("pane_nav_compare_summary"))
      showNavPane(ns("pane_compare_summary_titles"))
    }
  })

  if (!is.null(open_explore)) {
    if (!is.list(open_explore)) {
      open_explore <- list(open_explore)
    }

    lapply(open_explore, function(r) {
      observeEvent(r(), {
        updateNavInput("nav", selected = "explore", session = session)
        updateMenuInput("explore", selected = "disag", session = session)
      })
    })
  }

  if (!is.null(open_compare)) {
    if (!is.list(open_compare)) {
      open_compare <- list(open_compare)
    }

    lapply(open_compare, function(r) {
      observeEvent(r(), {
        updateNavInput("nav", selected = "compare", session = session)
        updateMenuInput("compare", selected = "disag", session = session)
      })
    })
  }





  # observe: dataset_name() ----
  observeEvent(dataset_name(), {
    req(!is_heat_plus())
    waiter::waiter_show(
      html = div(
        tags$h1(translate(c(isolate(language()), "navigation", "labels", "spinner"))) %>%
          font(color = "white"),
        div(
          waiter::spin_circle()
        ) %>%
          margin(top = -4)
      ),
      color = "#008dc9"

    )
    add_time("observe dataset_name()", dataset_name())
    session$userData$app_init <- TRUE
    state$data_name <- dataset_name()

    info_database <- heatdata::info_databases |>
      dplyr::filter(internal_name == dataset_name())

    state$is_who_dataset <- unique(info_database$is_WHO_dataset)
    state$is_map_dataset <- unique(info_database$show_map)

    updateRadiobarInput("nav_explore_disag", selected = "line")
    updateRadiobarInput("nav_explore_summary", selected = "bar")

    updateRadiobarInput("nav_compare_disag", selected = "graph")
    updateRadiobarInput("nav_compare_summary", selected = "graph")

    updateNavInput("nav", selected = "explore", session = session)
    updateMenuInput("explore", selected = "disag", session = session)
    updateMenuInput("compare", selected = FALSE, session = session)
    showNavPane(ns("pane_explore_disag_line"))

    # git687
    if(input$nav == "explore" && input$explore == "disag")
      state$force_ui_data_refresh <- !state$force_ui_data_refresh

    state$data_change_explore_disag <- TRUE
    state$data_change_explore_summary <- TRUE
    state$data_change_compare_disag <- TRUE
    state$data_change_compare_summary <- TRUE

    datname <- gsub("data_", "", dataset_name()) |> unique()
    # txt <- heatdata::info_databases$dataset_name[heatdata::info_databases$internal_name == datname ] |>
    #   unique()

    # Sorry! This is terrible, module did not save code
    # and putting it above pane was not nice

    tmpui <- div(class ="dataname-title",
                 tags$span(i18n(paste0("module.dataset_name.", datname, "_dataset"))))
    output$dataname_expdis <- renderUI(tmpui)
    output$dataname_expsum <- renderUI(tmpui)
    output$dataname_comdis <- renderUI(tmpui)
    output$dataname_comsum <- renderUI(tmpui)

    Events$set_recent_year <- list(
      from = "init",
      force = TRUE,
      value = NULL,
      selected = NULL,
      no_years = FALSE,
      seed = rnorm(1)
    )

    disp <- ifelse(state$is_map_dataset, "true", "false")
    session$sendCustomMessage("hide-map-button", list(display = disp))


    session$onFlushed(waiter::waiter_hide)
    add_time("end observe dataset_name()", dataset_name())
  }, ignoreInit = FALSE, priority = 10)


  observeEvent(dataset_name(), {
    state$data_name <- dataset_name()
  }, once = TRUE)


  # VIEWS ON INITIAL LOAD ----
  # explore disaggregated line ----
  callModule(
    viewServer, "explore_disag_line",
    Events = Events, Data = Data,
    visible = reactive(
      input$nav == "explore" &&
        input$explore == "disag" &&
        input$nav_explore_disag == "line"
    ),
    source = Inf, year = Inf, indicator = 5, dimension = 5, measure = NULL,
    summaries = FALSE,
    benchmarks = FALSE, render = renderUI,
    options = list(
      title = title_explore_disaggregated
    ),
    downloads = list(
      chart = TRUE
    ),
    language = language,
    dataset_name = dataset_name,
    is_who_dataset = reactive(state$is_who_dataset),
    is_map_dataset = reactive(state$is_map_dataset),
    visual = function(.data, ...) {
      args <- list(...)

      chartExploreDisaggregatedLine(
        data = .data,
        title_main = args$title_main,
        title_horizontal = args$title_horizontal,
        title_vertical = args$title_vertical,
        axis_min = args$axis_min,
        axis_max = args$axis_max,
        decimal_places = args$decimal_places,
        language = args$language,
        recent = args$recent,
        is_who_dataset = args$is_who_dataset
      )
    }
  )



  # VIEWS ON DELAYED LOAD ----

  observeEvent(input$chartexists, {




    # compare disaggregated graph ----
    callModule(
      viewServer, "compare_disag_graph",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "compare" &&
          input$compare == "disag" &&
          input$nav_compare_disag == "graph"
      ),
      source = Inf, year = 1, indicator = 1, dimension = 1, measure = NULL,
      summaries = FALSE, benchmarks = TRUE, render = renderUI,
      options = list(
        title = title_compare_disaggregated
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartCompareDefault(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_min = args$axis_min,
          axis_max = args$axis_max,
          focus_setting = args$focus_setting,
          decimal_places = args$decimal_places,
          language = args$language,
          is_who_dataset = args$is_who_dataset
        )
      }
    )


    # compare summary graph ----
    callModule(
      viewServer, "compare_summary_graph",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "compare" &&
          input$compare == "summary" &&
          input$nav_compare_summary == "graph"
      ),
      source = Inf, year = 1, indicator = 1, dimension = 1, measure = 1,
      summaries = FALSE, benchmarks = TRUE, render = renderUI,
      options = list(
        sorting = TRUE,
        title = title_compare_summary
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartCompareSummary(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_horizontal_min = args$axis_horizontal_min,
          axis_horizontal_max = args$axis_horizontal_max,
          axis_vertical_min = args$axis_vertical_min,
          axis_vertical_max = args$axis_vertical_max,
          focus_setting = args$focus_setting,
          label_style = args$label_style,
          label_size = args$label_size,
          decimal_places = args$decimal_places,
          language = args$language,
          is_who_dataset = args$is_who_dataset
        )
      }
    )



    # explore summary bar ----
    callModule(
      viewServer, "explore_summary_bar",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "summary" &&
          input$nav_explore_summary == "bar"
      ),
      source = Inf, year = Inf, indicator = 5, dimension = 5, measure = 1,
      summaries = FALSE, benchmarks = FALSE, render = renderUI,
      options = list(
        title = title_explore_summary
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartExploreSummaryBar(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_min = args$axis_min,
          axis_max = args$axis_max,
          conf_int = args$conf_int,
          data_labels = args$data_labels,
          decimal_places = args$decimal_places,
          language = args$language,
          is_who_dataset = args$is_who_dataset
        )
      }
    )

    # explore disaggregated bar ----

    callModule(
      viewServer, "explore_disag_bar",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "disag" &&
          input$nav_explore_disag == "bar"
      ),
      source = Inf, year = Inf, indicator = 5, dimension = 5, measure = NULL,
      summaries = FALSE, benchmarks = FALSE, render = renderUI,
      options = list(
        title = title_explore_disaggregated
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartExploreDisaggregatedBar(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_min = args$axis_min,
          axis_max = args$axis_max,
          conf_int = args$conf_int,
          data_labels = args$data_labels,
          decimal_places = args$decimal_places,
          language = args$language,
          recent = args$recent,
          is_who_dataset = args$is_who_dataset
        )
      }
    )

    # explore disaggregated detail ----
    callModule(
      viewServer, "explore_disag_detail",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "disag" &&
          input$nav_explore_disag == "detail"
      ),
      source = Inf, year = 1, indicator = 5,
      dimension = 1, #if (isTRUE(getOption("heat.plus"))) 1,
      measure = NULL,
      summaries = TRUE, benchmarks = FALSE, render = renderUI,
      options = list(
        sorting = TRUE,
        subgroup_highlight = TRUE,
        title = title_explore_detail
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartExploreDetail(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_min = args$axis_min,
          axis_max = args$axis_max,
          sort_by = args$sort_by,
          sort_order = args$sort_order,
          sort_indicator = args$sort_indicator,
          highlight_subgroup = args$highlight_subgroup,
          plot_lines = args$plot_lines,
          conf_int = args$conf_int,
          data_labels = args$data_labels,
          decimal_places = args$decimal_places,
          language = args$language,
          is_who_dataset = args$is_who_dataset
        )
      }
    )

    # explore disaggregated map ----
    callModule(
      viewServer, "explore_disag_map",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "disag" &&
          input$nav_explore_disag == "map"
      ),
      source = Inf, year = 1, indicator = 1, dimension = NULL, measure = NULL,
      summaries = FALSE, benchmarks = FALSE, render = renderUI,
      options = list(
        disclaimer = "map",

        title = title_explore_map
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartExploreMap(
          data = .data,
          title_main = args$title_main,
          decimal_places = args$decimal_places,
          language = args$language,
          dataset_name = args$dataset_name,
          is_map_dataset = isolate(state$is_map_dataset),
          is_who_dataset = args$is_who_dataset
        )
      }
    )

    # explore disaggregated table ----
    callModule(
      viewServer, "explore_disag_table",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "disag" &&
          input$nav_explore_disag == "table"
      ),
      source = Inf, year = Inf, indicator = Inf, dimension = Inf, measure = NULL,
      summaries = FALSE, benchmarks = FALSE, render = DT::renderDataTable,
      downloads = list(
        chart = FALSE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)
        table_explore_disaggregated(
          .data = .data,
          columns = args$columns,
          decimal_places = args$table_decimals,
          data_only = args$data_only,
          rename = args$rename,
          language = args$language
        )
      }
    )


    # explore summary line ----
    callModule(
      viewServer, "explore_summary_line",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "summary" &&
          input$nav_explore_summary == "line"
      ),
      source = Inf, year = Inf, indicator = 5, dimension = 5, measure = 1,
      summaries = FALSE, benchmarks = FALSE, render = renderUI,
      options = list(
        title = title_explore_summary
      ),
      downloads = list(
        chart = TRUE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        chartExploreSummaryLine(
          data = .data,
          title_main = args$title_main,
          title_horizontal = args$title_horizontal,
          title_vertical = args$title_vertical,
          axis_min = args$axis_min,
          axis_max = args$axis_max,
          conf_int = args$conf_int,
          data_labels = args$data_labels,
          decimal_places = args$decimal_places,
          language = args$language,
          is_who_dataset = args$is_who_dataset
        )
      }
    )

    # explore summary table ----
    callModule(
      viewServer, "explore_summary_table",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "explore" &&
          input$explore == "summary" &&
          input$nav_explore_summary == "table"
      ),
      source = Inf, year = Inf, indicator = Inf, dimension = Inf, measure = Inf,
      summaries = FALSE, benchmarks = FALSE, render = DT::renderDataTable,
      downloads = list(
        chart = FALSE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        table_explore_summary(
          .data = .data,
          columns = args$columns,
          decimal_places = args$table_decimals,
          data_only = args$data_only,
          rename = args$rename,
          language = args$language
        )
      }
    )


    # compare disaggregated table ----
    callModule(
      viewServer, "compare_disag_table",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "compare" &&
          input$compare == "disag" &&
          input$nav_compare_disag == "table"
      ),
      source = Inf, year = 1, indicator = Inf, dimension = Inf, measure = NULL,
      summaries = FALSE, benchmarks = TRUE, render = DT::renderDataTable,
      downloads = list(
        chart = FALSE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        table_compare_disaggregated(
          .data = .data,
          columns = args$columns,
          decimal_places = args$table_decimals,
          focus_setting = args$focus_setting,
          data_only = args$data_only,
          rename = args$rename,
          language = args$language
        )
      }
    )


    # compare summary table ----
    callModule(
      viewServer, "compare_summary_table",
      Events = Events, Data = Data,
      visible = reactive(
        input$nav == "compare" &&
          input$compare == "summary" &&
          input$nav_compare_summary == "table"
      ),
      source = Inf, year = 1, indicator = Inf, dimension = Inf, measure = Inf,
      summaries = FALSE, benchmarks = TRUE, render = DT::renderDataTable,
      downloads = list(
        chart = FALSE
      ),
      language = language,
      dataset_name = dataset_name,
      is_who_dataset = reactive(state$is_who_dataset),
      is_map_dataset = reactive(state$is_map_dataset),
      visual = function(.data, ...) {
        args <- list(...)

        table_compare_summary(
          .data = .data,
          columns = args$columns,
          decimal_places = args$table_decimals,
          focus_setting = args$focus_setting,
          data_only = args$data_only,
          rename = args$rename,
          language = args$language
        )
      }
    )

  }, once = TRUE)
  # observe change in main data ----
  observeEvent(c(Data$main(), language()), {
    req(NROW(Data$main()) > 0)

    add_time("observe Data$main", dataset_name())

    lang <- language()
    dataname <- dataset_name()

    x_setting <- Data$setting_yr_src() %>%
      dplyr::distinct(setting) %>%
      dplyr::pull()

    init_setting <- initial_setting(x_setting, lang, dataname)

    data_selection <- Data$strata() %>%
      dplyr::filter(setting == !!init_setting)

    data_source_years <- data_selection %>%
      dplyr::distinct(source, year)

    x_source <- data_source_years %>%
      dplyr::distinct(source) %>%
      dplyr::pull(source) %>%
      sort()

    init_source <- initial_source(x_source)

    x_year <- data_source_years %>%
      dplyr::distinct(year) %>%
      dplyr::pull(year) %>%
      sort()

    init_year <- initial_year(x_year)

    x_indicators <- data_selection %>%
      dplyr::distinct(choices = indicator_name, values = indicator_abbr) %>%
      dplyr::arrange(choices)

    x_dimension <- data_selection %>%
      dplyr::distinct(dimension) %>%
      dplyr::arrange(dimension) %>%
      dplyr::pull()

    selected_indicator_dimension <- default_indicator_dimension(
      strata = Data$strata(),
      current_indicator = initial_indicator(x_indicators, lang, dataname),
      current_dimension = initial_dimension(x_dimension, lang, dataname),
      new_country = init_setting,
      new_source = init_source,
      new_year = init_year
    )


    Events$set_setting <- list(
      from = "init",
      prev = "pre-init", #git804
      choices = x_setting,
      values = x_setting,
      selected = init_setting,
      sample = rnorm(1)
    )


    Events$set_benchmark_setting <- list(
      from = "init",
      selected = init_setting,
      language = language(), # this is not used EXCEPT to force a change
      rand = rnorm(1) # this is not used EXCEPT to force a change
    )

    Events$set_source <- list(
      from = "init",
      choices = x_source,
      values = x_source,
      selected = initial_source(x_source)
    )


    Events$set_year <- list(
      from = "init",
      choices = x_year,
      values = x_year,
      selected = init_year,
      recent = as.numeric(input$`explore_disag_line-recent`) #git545
    )

    Events$set_indicator <- list(
      from = "init",
      choices = x_indicators$choices,
      values = x_indicators$values,
      selected = selected_indicator_dimension$indicator_selected
    )



    Events$set_dimension <- list(
      from = "init",
      choices = x_dimension,
      values = x_dimension,
      selected = selected_indicator_dimension$dimension_selected
    )

    # observe change in measures data ---

    init_measure <- pick_measures(
      possible_measures = Data$measures(),
      dimension = selected_indicator_dimension$dimension_selected,
      language = lang
    )

    Events$set_measure <- list(
      from = "init",
      choices = init_measure$choices,
      values = init_measure$values,
      selected = initial_measure(init_measure$values)
    )

    if (!is.null(on_data_open)) {
      on_data_open()
    }
    add_time("end observe Data$main", dataset_name())
  })


  observeEvent(input$datachoose, {
    req(!is_heat_plus())
    showModal(
      dataManagementModal(
        ns("dc"),
        current_data = dataset_name(),
        language = language(),
        add_cancel = TRUE
      )
    )

  })


}
