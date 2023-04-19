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

#git 56 -- another.
# FLEXIBLE_SCALE_INDICATORS <- c(
#   "Adolescent fertility rate (births per 1000 women aged 15-19 years)" = "asfr1",
#   "Total fertility rate (births per woman)" = "tfr",
#   "Stunting prevalence in children aged < 5 years (%)" = "stunt5",
#   "Underweight prevalence in children aged < 5 years (%)" = "uwgt5",
#   "Wasting prevalence in children aged < 5 years (%)" = "wast5",
#   "Infant mortality rate (deaths per 1000 live births)" = "imr",
#   "Neonatal mortality rate (deaths per 1000 live births)" = "nmr",
#   "Under-five mortality rate (deaths per 1000 live births)" = "u5mr",
#   "Obesity prevalence in non-pregnant women aged 15-49 years, BMI >= 30 (%)" = "bmi30wm",
#   "Overweight prevalence in children aged < 5 years (%)" = "overweight",
#   "Severe wasting prevalence in children aged < 5 years (%)" = "sevwasting",
#   "AIDS-related mortality (deaths per 1000 population)" = "aids_mortality"
# )

FLEXIBLE_SCALE_INDICATORS <- heatdata::info_indicators %>%
  dplyr::filter(flexible_scale == 1)

FLEXIBLE_SCALE_INDICATORS <- FLEXIBLE_SCALE_INDICATORS$indicator_abbr %>%
  setNames(FLEXIBLE_SCALE_INDICATORS$indicator_name)


hc_xAxis_multiples <- function(hc, ...) {
  args <- list(...)

  if (length(args) == 1 && is.null(names(args))) {
    hc$x$hc_opts$xAxis <- args[[1]]
  } else {
    hc$x$hc_opts$xAxis <- args
  }

  hc
}

chart_font_size <- function(x) {
  switch(
    x,
    small = "8px",
    medium = "10px",
    large = "14px"
  )
}

px_to_pt <- function(x) {
  x * 16 / 12
}

chartEmpty <- function() {
  chart <- highchart() %>%
    hc_add_series(
      type = "line",
      data = list(NULL)
    ) %>%
    hc_subtitle(
      floating = TRUE,
      text = "No data",
      verticalAlign = "middle"
    ) %>%
    hc_legend(
      enabled = FALSE
    ) %>%
    hc_xAxis(
      labels = list(
        enabled = FALSE
      ),
      lineWidth = 0,
      tickAmount = 0,
      tickLength = 0
    )

  class(chart) <- c("highchart", "emptychart", "htmlwidget")

  chart
}

is.emptychart <- function(x) {
  inherits(x, "emptychart")
}

chart_dimensions <- function(...) {
  args <- drop_nulls(list(...))

  if (length(args) == 0) {
    return(NULL)
  }

  widths <- c(550, 350, 200)

  vapply(args, function(x) {
    w <- widths[length(x)]

    if (is.na(w) || length(w) == 0) {
      tail(widths, 1)
    } else {
      w
    }
  }, numeric(1))
}

chart_config <- function(chart,
                         title = NULL,
                         subtitle = NULL,
                         height = 650,
                         width = 1000,
                         margin = NULL,
                         reflow = FALSE,
                         boost = TRUE) {
  chart %>%
    hc_chart(
      height = height,
      margin = margin,
      reflow = reflow,
      space = c(10, 10, 10, 10),
      style = list(
        cursor = "default",
        fontFamily = "Roboto"
      ),
      width = width
    ) %>%
    hc_title(
      text = title %||% JS("undefined"),
      style = list(
        fontFamily = "Roboto Condensed",
        fontWeight = "normal"
      ),
      useHTML = TRUE
    ) %>%
    hc_subtitle(
      text = subtitle %||% JS("undefined"),
      useHTML = TRUE
    ) %>%
    hc_plotOptions(
      series = list(
        marker = list(
          symbol = "circle",
          radius = 5
        )
      ),
      line = list(
        color = "rgba(0,0,0,0.5)",
        lineWidth = 1,
        marker = list(
          enabled = TRUE
        ),
        showInLegend = FALSE,
        states = list(
          hover = list(
            # enabled = FALSE
          )
        )
      )
    ) %>%
    hc_boost(
      enabled = boost
    )
}

chart_legend <- function(items,
                         symbol = "circle",
                         item_font_size = 12,
                         item_padding = 20,
                         item_min_width = 50,
                         item_width = NULL,
                         margin = 40,
                         by_dimension = TRUE) {
  char_width <- px_to_pt(10)

  if (by_dimension) {
    items_split <- split(items, items$dimension)

    purrr::map(items_split, function(items) {
      dummy_series <- purrr::pmap(items, function(dimension, label, color, type) {
        list(
          color = color,
          name = label,
          showInLegend = TRUE,
          type = type
        )
      })

      highchart() %>%
        hc_add_series_list(
          dummy_series
        ) %>%
        hc_chart(
          backgroundColor = NULL,
          className = "heat-legend-chart",
          height = NULL,
          margin = 0,
          showAxes = FALSE,
          spacing = 0,
          width = NULL
        ) %>%
        hc_plotOptions(
          series = list(
            grouping = FALSE,
            events = list(
              legendItemClick = JS("function() { return false; }")
            ),
            marker = list(
              symbol = symbol
            )
          )
        ) %>%
        hc_xAxis(
          visible = FALSE
        ) %>%
        hc_yAxis(
          visible = FALSE
        ) %>%
        hc_legend(
          align = "left",
          alignColumns = TRUE,
          enabled = TRUE,
          floating = TRUE,
          itemDistance = item_padding,
          itemMarginBottom = 5,
          itemStyle = list(
            cursor = "default",
            fontWeight = "normal",
            fontSize = glue("{ item_font_size }px"),
            textOverflow = NULL
          ),
          labelFormat = "{name}",
          margin = margin,
          navigation = list(
            enabled = FALSE
          ),
          symbolHeight = if (symbol == "square") 10,
          symbolRadius = 0,
          useHTML = TRUE,
          verticalAlign = "top"
          # width = width
        )
    })
  } else {
    dummy_series <- purrr::pmap(items, function(dimension, label, color, type) {
      list(
        color = color,
        name = label,
        showInLegend = TRUE,
        type = type
      )
    })

    highchart() %>%
      hc_add_series_list(
        dummy_series
      ) %>%
      hc_chart(
        backgroundColor = NULL,
        className = "heat-legend-chart",
        height = "5%",
        showAxes = FALSE
      ) %>%
      hc_plotOptions(
        series = list(
          grouping = FALSE,
          events = list(
            legendItemClick = JS("function() { return false; }")
          ),
          marker = list(
            symbol = symbol
          )
        )
      ) %>%
      hc_xAxis(
        visible = FALSE
      ) %>%
      hc_yAxis(
        visible = FALSE
      ) %>%
      hc_legend(
        align = "left",
        alignColumns = TRUE,
        enabled = TRUE,
        floating = TRUE,
        itemDistance = item_padding,
        itemMarginBottom = 5,
        itemStyle = list(
          cursor = "default",
          fontWeight = "normal",
          fontSize = glue("{ item_font_size }px"),
          # width = max(nchar(items$label)) * item_font_size
          textOverflow = NULL
        ),
        labelFormat = "{name}",
        margin = margin,
        navigation = list(
          enabled = FALSE
        ),
        symbolHeight = if (symbol == "square") 10,
        symbolRadius = 0,
        useHTML = TRUE,
        verticalAlign = "top"
      )
  }




}

chart_title_dimension <- function(chart, text, flipped = FALSE) {
  title <- list(
    text = text,
    useHTML = TRUE
  )

  if (flipped) {
    axis_type <- "yAxis"
    axis_fun <- hc_yAxis
    axis_fun_multiple <- hc_yAxis_multiples
  } else {
    axis_type <- "xAxis"
    axis_fun <- hc_xAxis
    axis_fun_multiple <- hc_xAxis_multiples
  }

  if (is.null(chart$x$hc_opts[[axis_type]])) {
    chart %>%
      axis_fun(
        labels = list(
          enable = FALSE
        ),
        opposite = TRUE,
        title = title
      )
  } else {
    chart %>%
      axis_fun_multiple(
        chart$x$hc_opts[[axis_type]],
        list(
          labels = list(
            enabled = FALSE
          ),
          lineWidth = 0,
          opposite = TRUE,
          title = title
        )
      )
  }
}

chart_title_indicator <- chart_title_dimension

plot_line_setting <- function(value, language) {

  setting_average <- translate(c(language, "options", "labels", "average"))

  list(
    color = "#625C56",
    label = list(
      style = list(
        color = "#625C56",
        fontWeight = "bold",
        letterSpacing = ".4px"
      ),
      text = glue("{setting_average}: { value }"),
      useHTML = TRUE,
      zIndex = 5
    ),
    value = value,
    width = 2,
    zIndex = 5
  )
}

plot_line_median <- function(value, language, zindex = 5) {

  setting_median <- translate(c(language, "options", "labels", "median"))

  list(
    color = "#E07600",
    label = list(
      style = list(
        color = "#E07600",
        fontWeight = "bold",
        letterSpacing = ".4px"
      ),
      text = glue("{setting_median}: { value }"),
      useHTML = TRUE,
      zIndex = zindex
    ),
    value = value,
    width = 2,
    zIndex = zindex
  )
}

chart_tooltip <- function(chart, heading, body, ..., decimal_places = 1) {

  body_str <- paste(
    purrr::map_chr(body, function(x) {

      if(grepl("this.point.flag", x)){
        paste("(this.point.flag !== null ? '<tr><td style = \"padding-top: 0.15rem;\">' + ", x, " + '</td></tr>' : '')")
      } else {
        paste("'<tr><td style = \"padding-top: 0.15rem;\">' + ", x, " + '</td></tr>'")
      }
    }),
    collapse = "+"
  )

  # true ? '<tr><td style = \"padding-top: 0.15rem;\">Works!</td></tr>' : null +
  # body_str <- "(false ? '<tr><td style = \"padding-top: 0.15rem;\">Works!</td></tr>' : '') + '<tr><td style = \"padding-top: 0.15rem;\">' +  this.point.subgroup + (this.point.popshare === 'NA' ? '' : ' (' + this.point.popshare + '% of affected population)')  + '</td></tr>'+'<tr><td style = \"padding-top: 0.15rem;\">' +  'Estimate: ' + this.point.estimate + (this.point.ci_lb === 'NA' ? '; 95% CI   not available ' : '; 95% CI ' + this.point.ci_lb + '-' + this.point.ci_ub)  + '</td></tr>'+'<tr><td style = \"padding-top: 0.15rem;\">' +  'Setting average: '  + this.point.setting_average  + '</td></tr>'"


  formatter_js <- glue(
    "function() {{ ",
    "if(!this.point.setting) {{return false}} else {{",
    "return '<table { chart_tooltip_attributes() }>' + ",
    "'<thead>' + ",
    "'<tr><td style = \"font-weight: 700; border-bottom: 1px solid #263238;\">' + ",
    "{heading} + ",
    "'</tr></td>' +",
    "'</thead>' + ",
    "'<tbody style=\"font-weight: 300;\">' + ",
    "{body_str} +",
    "'</tbody>' + ",
    "'</table>' ",
    "}}}"
  )

  hc_tooltip(
    chart,
    ...,
    backgroundColor = "#fafafa",
    borderColor = NULL, # "#263238",
    # followPointer = TRUE,
    # footerFormat = "</table>",
    # headerFormat = glue("<table { chart_tooltip_attributes() }>"),
    formatter = JS(
      formatter_js
    ),
    hideDelay = 100,
    outside = TRUE,
    padding = 0,
    style = list(
      opacity = 1
    ),
    useHTML = TRUE,
    zIndex = 10
  )
}

chart_tooltip_attributes <- function() {
  "style=\"margin: 1px; color: #263238; background-color: #fafafa; letter-spacing: .5px; opacity: 0.85; border-collapse: separate; border-spacing: 5px 5px; border: none;\" data-html2canvas-ignore=\"true\""
}

highcharterDependencies <- function() {
  htmltools::htmlDependencies(highchartOutput(NULL))[-2]
}

chart_map_disclaimer <- function(language) {
  list(
    tags$p(
      class = "p--disclaimer",
      translate(c(language, "charts", "disclaimer", "para1"))
      # "The designations employed and the presentation of the material in this toolkit do not imply the expression of any opinion whatsoever on the part of WHO concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on maps represent approximate border lines for which there may not yet be full agreement."
    ) %>%
      font(size = "xs", weight = "italic"),
    tags$p(
      class = "p--disclaimer",
      translate(c(language, "charts", "disclaimer", "para2"))
      # "The international borders of the map provided reflect the current political and geographic status as of the date of publication (2020). However, the technical health information is based on data accurate with respect to the year selected. The disconnect in this arrangement should be noted but no implications regarding political or terminological status should be drawn from this arrangement as it is purely a function of technical and graphical limitations."
    ) %>%
      font(size = "xs", weight = "italic")
  )
}

chart_disclaimer <- function(language = "en", is_who_dataset) {
  if (is_heat_plus()) {
    div(
      class = "heat-chart-disclaimer",
      tags$p(
        # "Health Equity Assessment Toolkit Plus (HEAT Plus): Software for exploring and comparing health inequalities in countries. Upload database edition: Version 3.0 (beta). Geneva, World Health Organization, 2020."
        translate(c(language, "downloads", "text", "heatplus1"))
      ) %>%
        font(size = "xs"),
      tags$p(
        # "WHO provides this toolkit without data, and all data added to, or resulting from, the toolkit are the sole responsibility of the user, not WHO."
        translate(c(language, "downloads", "text", "heatplus2"))
      ) %>%
        font(size = "xs")
    )
  } else {
    div(
      class = "heat-chart-disclaimer",
      tags$p(
        # "Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing
        #  health inequalities in countries. Built-in database edition.
        #  Version 4.0 (beta). Geneva, World Health Organization, 2020."
        translate(c(language, "downloads", "text", "heat1"))
      ) %>%
        font(size = "xs"),
      tags$p(
        # "The disaggregated data used in this version were drawn from the WHO Health
        #  Equity Monitor database (2019 update) which may have been revised or updated
        #  since that time. The most recent version of that database is available on the
        #  WHO website",
        translate(c(language, "downloads", "text", "heat2"))
      ) %>%
        font(size = "xs"),
      tags$p(
        # "Health Equity Assessment Toolkit (HEAT): Software for exploring and comparing
        #  health inequalities in countries. Built-in database edition.
        #  Version 4.0 (beta). Geneva, World Health Organization, 2020."
        if(!is_who_dataset)
          get_nonwho_disclaimer()
      ) %>%
        font(size = "xs")
    )
  }
}

chart_table <- function(charts, title_top, title_right,
                        title_main, title_horizontal, title_vertical, legend,
                        map_disclaimer = FALSE, language = "en",
                        is_who_dataset) {
  total_columns <- if (is.null(title_right)) 12 else 10


  if (!is.null(title_top)) {
    my_width <- floor(total_columns / length(title_top))
  } else {
    my_width <- total_columns
  }

  list(
    fluidRow(
      column(
        # class = "d-flex align-items-center justify-content-center",
        width = total_columns,
        tags$h3(title_main, class = "text-center heat-main-title")
      )
    ),
    if (map_disclaimer) {
      columns(
        column(
          width = total_columns,
          class = "heat-pane-title",
          div(class = "heat-pane-title__inner", title_top)
        ),
        column(
          width = total_columns,
          class = "heat-pane-chart",
          charts
        )
      )
    } else {
      list(
        fluidRow(
          class = "flex-nowrap flex-row",
          if (!is.null(title_vertical)) {
            column(
              class = "pr-0",
              width = "content",
              div(
                class = "heat-vertical-title__outer",
                div(
                  class = "heat-vertical-title__inner heat-axis-title",
                  title_vertical
                )
              )
            )
          },
          column(
            purrr::map(1:length(charts), function(row_number) {
              row_of_charts <- charts[[row_number]]

              # Add a big row that includes a column for each dimension
              div(
                class = "pseudo-row",
                purrr::map(seq_along(row_of_charts), function(columnnumber) {
                  column(
                    # width = my_width,
                    if (row_number == 1) {
                      list(
                        fluidRow(
                          column(
                            class = "heat-pane-title",
                            div(class = "heat-pane-title__inner", title_top[columnnumber])
                          )
                        ),
                        fluidRow(
                          column(
                            class = "heat-pane-chart",
                            row_of_charts[[columnnumber]]
                          )
                        )
                      )
                    } else{
                      fluidRow(
                        column(
                          class = "heat-pane-chart",
                          row_of_charts[[columnnumber]]
                        )
                      )
                    }
                  )
                }),
                if (!is.null(title_right[row_number])) {
                  column(
                    width = 2,
                    class = "my-auto heat-variable-title",
                    p(class = "max-char-25px", title_right[row_number])
                  )
                }
              )
            })
          )
        ),
        if (!is.null(title_horizontal)) {
          fluidRow(
            column(
              width = total_columns,
              class = "heat-axis-title",
              title_horizontal
            )
          )
        }
      )
    },
    fluidRow(
      column(
        width = total_columns,
        class = "heat-legend-container column-no-padding",
        legend
      )
    ),
    if (map_disclaimer) {
      fluidRow(
        class = "background-gray",
        chart_map_disclaimer(language)
      )
    },
    chart_disclaimer(language, is_who_dataset)
  )
}

chart_singleton <- function(chart, title_top,
                            title_main, title_right,
                            title_horizontal, title_vertical,
                            legend, disclaimer) {
  total_columns <- 12

  list(
    columns(
      column(
        width = total_columns,
        tags$h3(title_main, class = "text-center heat-main-title")
      )
    ),
    columns(
      class = "flex-nowrap flex-row",
      if (!is.null(title_vertical)) {
        column(
          class = "ml-auto pr-0",
          width = "content",
          div(
            class = "heat-vertical-title__outer",
            div(
              class = "heat-vertical-title__inner heat-axis-title",
              title_vertical
            )
          )
        )
      },
      column(
        width = if (!is.null(title_right)) 10 else "content",
        class = paste("heat-pane-chart", if (is.null(title_right)) "mr-auto"),
        chart
      ),
      if (!is.null(title_right)) {
        column(
          width = 2,
          class = "my-auto heat-variable-title",
          p(class = "max-char-25px", title_right)
        )
      }
    ),
    if (!is.null(title_horizontal)) {
      columns(
        column(
          width = total_columns,
          class = "heat-axis-title",
          title_horizontal
        )
      )
    },
    columns(
      column(
        width = total_columns,
        class = "heat-legend-container column-no-padding",
        legend
      )
    ),
    lapply(en_list(disclaimer), function(d) {
      columns(column(d))
    })
  )
}

# @param charts A list of lists of charts.
chart_layout <- function(charts,
                         title_top,
                         title_side,
                         title_main,
                         title_horizontal,
                         title_vertical,
                         legend) {


  stopifnot(length(charts) == length(title_side))

  browsable(container(
    columns(
      class = "heat-chart",
      chart_layout_title_main(title_main),
      chart_layout_title_vertical(title_vertical),
      chart_layout_chart_rows(charts, title_top, title_side),
      chart_layout_title_horizontal(title_horizontal),
      chart_layout_legend(legend)
    ),
    columns(
      column(width = 12, chart_disclaimer())
    )
  ))
}

chart_layout_body <- function(charts,
                              title_top,
                              title_side,
                              title_horizontal,
                              legend) {
  column(
    chart_layout_chart_rows(charts, title_top, title_side),
    chart_layout_title_horizontal(title_horizontal),
    chart_layout_legend(legend)
  ) %>%
    padding(left = 0)
}

chart_layout_title_main <- function(text) {
  if (!str_truthy(text)) {
    return(NULL)
  }

  column(
    width = 12,
    class = "heat-main-title",
    text
  ) %>%
    font(align = "center")
}

chart_layout_title_vertical <- function(text) {
  if (!str_truthy(text)) {
    return(NULL)
  }

  column(
    width = "auto",
    div(
      class = "heat-axis-title text-vertical",
      text
    )
  ) %>%
    padding(0) %>%
    width(2) %>%
    height("full") %>%
    display("flex") %>%
    flex(align = "center", justify = "center")
}

chart_layout_title_horizontal <- function(text) {
  if (!str_truthy(text)) {
    return(NULL)
  }

  column(
    class = "heat-axis-title",
    width = 12,
    column(width = 1),
    column(width = 11, text)
  ) %>%
    padding(0) %>%
    font(align = "center")
}

chart_layout_title_indicators <- function(indicators) {
  column(
    width = 1,
    columns(
      purrr::map(indicators, function(text) {
        column(
          width = 12,
          class = "heat-variable-title",
          text
        ) %>%
          padding(0) %>%
          font(align = "center") %>%
          display("flex") %>%
          flex(align = "center")
      })
    ) %>%
      height("full")
  )
}

# @param charts A _flat_ list of charts.
chart_layout_chart_rows <- function(chart_grid, title_top, title_indicators) {
  column(
    purrr::imap(chart_grid, function(row_of_charts, row_number) {
      columns(
        column(
          columns(
            if (row_number == 1) {
              list(
                purrr::map(title_top, column, class = "heat-pane-title"),
                div(class = "w-full")
              )
            },
            purrr::map(row_of_charts, column, class = "d-flex")
          )
        ),
        column(
          width = 2,
          class = "heat-variable-title",
          title_indicators[row_number]
        ) %>%
          display("flex") %>%
          flex(align = "center", justify = "center")
      )
    })
  )
  #     purrr::imap(chart_grid, function(chart_row, i) {
  #     column(
  #       width = 12,
  #       purrr::imap(chart_row, function(chart, j) {
  #         column(
  #           width = "auto",
  #           if (i == 1) {
  #             div(class = "heat-pane-title", title_top[j])
  #           },
  #           chart %>%
  #             div() %>%
  #             margin(top = "auto", bottom = "auto")
  #         ) %>%
  #           display("flex") %>%
  #           flex(direction = "column") %>%
  #           padding(0)
  #       }),
  #       column(
  #         class = "heat-variable-title",
  #         width = 1,
  #         title_indicators[i]
  #       ) %>%
  #         height("full") %>%
  #         display("flex") %>%
  #         flex(align = "center") %>%
  #         font(align = "center")
  #     ) %>%
  #       display("flex") %>%
  #       flex(justify = "center")
  #   })
  # )
}

chart_layout_legend <- function(legend) {
  if (!is.null(legend)) {
    column(
      width = 12,
      legend
    )
  }
}


pad_axis_min_max <- function(.data, pad_proportion){

  .data %>%
    dplyr::mutate(diff = max-min,
                  max_pad = max + diff * pad_proportion,
                  min_pad = min - diff * pad_proportion
    )


}

get_axis_min_max <- function(.data,
                             manual_axis_min = NULL,
                             manual_axis_max = NULL,
                             include_conf = FALSE,
                             disaggregated = TRUE,
                             bar = FALSE,
                             plot_type = "none") {
  # Note coverage indicators are mostly percentages 0-100
  # outcome indicators can be anything
  isHEATPlus <- is_heat_plus()
  indicators <- .data$indicator_name %>% unique()
  only_coverage_indicator <- !any(.data$indicator_abbr%in%FLEXIBLE_SCALE_INDICATORS)
  padval <- 0.05
  est_var <- ifelse(disaggregated, "estimate", "inequal")
  lb_var <- ifelse(disaggregated, "ci_lb", "se.lowerci")
  ub_var <- ifelse(disaggregated, "ci_ub", "se.upperci")
  max_trigger_pad <- 98
  min_trigger_pad <- 2
  is_inequal_data <- "measure" %in% names(.data)
  ci_lb_all_NA <- all(is.na(.data[[lb_var]]))
  ci_ub_all_NA <- all(is.na(.data[[ub_var]]))

  if (is_inequal_data) {
    log_sum_measure <- is_log_scale(.data$measure[1])
  }

  # if manual_axis_min and manual axis_max are both not null
  # just return the manual values
  if ((!is.null(manual_axis_min) & !is.null(manual_axis_max))) {
    if (plot_type != "explore-summary-line") {
      maxmin <- .data %>%
        dplyr::distinct(indicator_abbr, indicator_name) %>%
        dplyr::mutate(
          min = manual_axis_min,
          max = manual_axis_max
        )
    } else {
      maxmin <- .data %>%
        dplyr::summarize(
          min = manual_axis_min,
          max = manual_axis_max
        )
    }

    return(maxmin)
  }

  # There were some reasons why using tidyverse would have
  # made this even more complex and hard to read. Here we
  # split the data by indicator in order to compute max and
  # min. But with the summary-line the indicators are in
  # the same plot so we will simply "split" by setting so
  # there is only one split
  if (plot_type != "explore-summary-line") {
    .data_split <- split(.data, .data$indicator_abbr)
  } else{
    .data_split <- split(.data, .data$setting)
  }

  # For each of the indicators and the associated data
  maxmin <- purrr::map_dfr(.data_split, function(x) {
    # The reason for the complexity here is, for example,
    # you compute a min using the estimates themselves
    # if (A) the confidence intervals are turned off OR
    # (B) if all the lower bound confidence interval
    # is all NA (and confidence is on) etc

    if(!include_conf | all(is.na(x[[lb_var]]))){
      min <- min(x[[est_var]], na.rm = TRUE)
      all_pos <- all(x[[est_var]] >= 0, na.rm = TRUE)
      all_above1 <- all(x[[est_var]] >= 1, na.rm = TRUE)
      close_to_0 <- any(x[[est_var]] < min_trigger_pad, na.rm = TRUE)

    } else {
      min <- min(x[[lb_var]], na.rm = TRUE)
      all_pos <- all(x[[lb_var]] >= 0, na.rm = TRUE)
      all_above1 <- all(x[[lb_var]] >= 1, na.rm = TRUE)
      close_to_0 <- any(x[[lb_var]] < min_trigger_pad, na.rm = TRUE)
    }

    if(!include_conf | all(is.na(x[[ub_var]]))){
      max <- max(x[[est_var]], na.rm = TRUE)
      all_neg <- all(x[[est_var]] < 0, na.rm = TRUE)
      all_below1 <- all(x[[est_var]] < 1, na.rm = TRUE)
      close_to_100 <- any(x[[est_var]] > max_trigger_pad, na.rm = TRUE)
    } else {
      max <- max(x[[ub_var]], na.rm = TRUE)
      all_neg <- all(x[[ub_var]] < 0, na.rm = TRUE)
      all_below1 <- all(x[[ub_var]] < 1, na.rm = TRUE)
      close_to_100 <- any(x[[ub_var]] > max_trigger_pad, na.rm = TRUE)
    }

    res <- dplyr::tibble(
      min = min,
      max = max,
      all_pos = all_pos,
      all_neg = all_neg,
      all_above1 = all_above1,
      all_below1 = all_below1,
      close_to_100 = close_to_100,
      close_to_0 = close_to_0
    )

    if (plot_type != "explore-summary-line") {
      res$indicator_abbr <- x$indicator_abbr[1]
      res$indicator_name <- x$indicator_name[1]
    }

    res
  })

  if (plot_type != "explore-summary-line") {
    maxmin <- maxmin %>%
      dplyr::mutate(coverage_indicator = !indicator_name %in% names(FLEXIBLE_SCALE_INDICATORS))
  }

  if (plot_type %in% c("explore-disaggregated-line", "compare-disaggregated-line")) {
    # For coverage indicators
    maxmin$min[maxmin$coverage_indicator  & !isHEATPlus] <- 0
    maxmin$max[maxmin$coverage_indicator  & !isHEATPlus] <- 100

    maxmin <- pad_axis_min_max(maxmin, padval)

    maxmin$max[maxmin$coverage_indicator & maxmin$close_to_100  & !isHEATPlus] <-
      maxmin$max_pad[maxmin$coverage_indicator & maxmin$close_to_100  & !isHEATPlus]

    # For non-coverage indicators or HEAT Plus
    maxmin$min[!maxmin$coverage_indicator  | isHEATPlus] <-
      maxmin$min_pad[!maxmin$coverage_indicator  | isHEATPlus]

    maxmin$max[!maxmin$coverage_indicator  | isHEATPlus] <-
      maxmin$max_pad[!maxmin$coverage_indicator  | isHEATPlus]
  }

  if (plot_type %in% c("explore-disaggregated-bar", "explore-detail")) {
    maxmin$min <- 0

    # For coverage indicators
    maxmin$max[maxmin$coverage_indicator  & !isHEATPlus] <- 100

    maxmin <- pad_axis_min_max(maxmin, padval)

    maxmin$max[maxmin$coverage_indicator & maxmin$close_to_100  & !isHEATPlus] <-
      maxmin$max_pad[maxmin$coverage_indicator & maxmin$close_to_100  & !isHEATPlus]

    # For non-coverage indicators or HEAT Plus
    maxmin$max[!maxmin$coverage_indicator | isHEATPlus] <-
      maxmin$max_pad[!maxmin$coverage_indicator | isHEATPlus]
  }

  if (plot_type == "explore-summary-line") {
    maxmin <- pad_axis_min_max(maxmin, padval)
    maxmin$min <- maxmin$min_pad
    maxmin$max <- maxmin$max_pad
  }

  if (plot_type == "explore-summary-bar") {
    if (!log_sum_measure) {
      pos_and_neg <- !maxmin$all_neg & !maxmin$all_pos

      maxmin$min[maxmin$all_pos] <- 0
      maxmin$max[maxmin$all_neg] <- 0

      maxmin <- pad_axis_min_max(maxmin, padval)

      maxmin$min[maxmin$all_neg] <- maxmin$min_pad[maxmin$all_neg]
      maxmin$min[pos_and_neg] <- maxmin$min_pad[pos_and_neg]

      maxmin$max[maxmin$all_pos] <- maxmin$max_pad[maxmin$all_pos]
      maxmin$max[pos_and_neg] <- maxmin$max_pad[pos_and_neg]
    } else {
      above_and_below_1 <- !maxmin$all_below1 & !maxmin$all_above1

      maxmin$min[maxmin$all_above1] <- 1
      maxmin$max[maxmin$all_below1] <- 1

      maxmin <- pad_axis_min_max(maxmin, padval)

      maxmin$min[maxmin$all_below1] <- maxmin$min_pad[maxmin$all_below1]
      maxmin$min[above_and_below_1] <- maxmin$min_pad[above_and_below_1]

      maxmin$max[maxmin$all_above1] <- maxmin$max_pad[maxmin$all_above1]
      maxmin$max[above_and_below_1] <- maxmin$max_pad[above_and_below_1]
    }
  }

  if (plot_type == "compare-summary") {
    # No special rules
  }

  if (is_inequal_data && log_sum_measure) {
    if (!include_conf) {
      maxmin$min[maxmin$min <= 0] <- min(.data$inequal, na.rm = TRUE) - 0.01
    } else {
      maxmin$min[maxmin$min <= 0] <- min(.data$se.lowerci, na.rm = TRUE) - 0.01
    }

    maxmin$min[maxmin$min <= 0 | is.na(maxmin$min)] <- 0.0001
  }

  # If either of the manual axes are set
  if (!is.null(manual_axis_min)) maxmin$min <- manual_axis_min
  if (!is.null(manual_axis_max)) maxmin$max <- manual_axis_max

  maxmin
}

color_adjustment <- function(.data, lang) {
  if (is_heat_plus()) {
    #sub_nat <- translate(c(lang, "values", "dimensions", "Subnational region"))

    # .data <- .data %>%
    #   dplyr::mutate(
    #     color = ifelse(
    #       dimension == !!sub_nat,
    #       highcharter::hex_to_rgba(color, 0.5),
    #       color
    #     )
    #   )
  # } else {
    dims <- .data %>%
      dplyr::group_by(setting, source, year, dimension) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::filter(n > 7) %>%
      dplyr::distinct(dimension) %>%
      dplyr::pull(dimension)

    .data <- .data %>%
      dplyr::mutate(
        color = ifelse(
          dimension %in% dims,
          highcharter::hex_to_rgba(color, 0.5),
          color
        )
      )
  }

  .data
}

round_vars <- function(.data, vars_to_round, decimal_places) {
  .data %>%
    dplyr::mutate_at(
      vars_to_round,
      ~ trimws(format(round(., decimal_places), nsmall = decimal_places))
    )
}

standard_tooltip_header <- function(summary_measure = FALSE) {
  "this.point.setting + ', ' + this.point.source + ' ' + this.point.year"
}

standard_tooltip_body <- function(include_conf = TRUE, from_map = FALSE,
                                  summary_measure = FALSE, language = "en") {
  label_estimate <- translate(c(language, "charts", "tooltips", "estimate"))
  label_setting_avg <- paste0(
    "'", translate(c(language, "charts", "tooltips", "settingavg")), ": '"
  )
  label_of_affected_pop <- paste0("% ", translate(c(language, "charts", "tooltips", "affectedpop")))
  text_notavailable <- translate(c(language, "charts","legend","notavailable"))
  text_ci <- translate(c(language, "charts","tooltips","95ci"))#"95% CI"
  text_flag <- translate(c(language, "options", "values", "flag"))

  text_conf <- paste(
    " + (this.point.ci_lb === 'NA' ? ';", text_ci, " ", tolower(text_notavailable), "' : ';", text_ci, "' + this.point.ci_lb + '-' + this.point.ci_ub)"
  )

  if (!from_map && !summary_measure) {
    tooltip_body <- c(
      glue::glue("this.point.subgroup + (this.point.popshare === 'NA' ? '' : ' (' + this.point.popshare + '{label_of_affected_pop})')"),
      paste0(
        "'", label_estimate, ": ' + this.point.estimate",
        if (include_conf) text_conf
      ),
      #paste("'Flag: '", "+ this.point.flag"),
      paste0("'", text_flag, ": ' + this.point.flag"),
      paste(label_setting_avg, " + this.point.setting_average")
    )

  }

  if (from_map) {

    tooltip_body <- c(
      glue::glue("this.point.subgroup + (this.point.popshare === null ? '' : ' (' + this.point.popshare + '{label_of_affected_pop})')"),
      glue::glue("'{label_estimate}: ' + (this.point.subgroup === '14 jammu and kashmir' ? 'Not applicable' :  this.point.estimate_for_tooltip + (this.point.ci_lb === 'NA' || this.point.ci_lb === null ? '; {text_ci} {tolower(text_notavailable)}' : '; {text_ci} ' + this.point.ci_lb + '-' + this.point.ci_ub))"),
      paste0("'", text_flag, ": ' + this.point.flag"),
      glue::glue("{label_setting_avg} + (this.point.subgroup === '14 jammu and kashmir' ? 'Not applicable' :  this.point.setting_average)")
    )
  }

  if (!from_map && summary_measure) {

    tooltip_body <- "this.point.measure_name + ': ' + this.point.inequal"
    if (include_conf) tooltip_body <- glue::glue("{tooltip_body} + (this.point.ci_lb === 'NA' ? '; {text_ci} {tolower(text_notavailable)}' : '; {text_ci} ' + this.point.ci_lb + '-' + this.point.ci_ub)")
    tooltip_body <- c(tooltip_body , glue::glue("{label_setting_avg} + this.point.r_national"))

# tooltip_body <-  c(
#       paste(
#         "this.point.measure_name + ': ' + this.point.inequal",
#         if (include_conf) " + (this.point.ci_lb === 'NA' ? '; 95% CI not available' : '; 95% CI ' + this.point.ci_lb + '-' + this.point.ci_ub)"
#       ),
#       glue::glue("{label_setting_avg} + this.point.r_national")
#     )
  }

  tooltip_body
}


get_summary_vtitle <- function(data, language){

  txt <- "Something went wrong"

  if(nrow(data) > 0){

    dims <- unique(data$dimension)
    inds <- unique(data$indicator_abbr)
    measure <- unique(data$measure)
    base_txt <- "between two (extreme) subgroups"
    measure_vals <- list("D" = "Difference", "R" = "Ratio")
    separator <- ifelse(measure == "D", "-", "/")

    measure_txt <- measure_vals[[measure]]


    if(length(dims) > 1 || length(inds) > 1){
      txt <- paste(measure_txt, base_txt)
    } else {

      favourable_indicator <- heatdata::info_indicators |>
        dplyr::filter(indicator_abbr == data$indicator_abbr[1]) |>
        dplyr::pull(favourable_indicator) |>
        unique()
      favourable_indicator <- favourable_indicator == 1

      dim_info <- heatdata::info_dimension_colors |>
        dplyr::filter(dimension == data$dimension[1]) |>
        dplyr::select(subgroup, ordered_dimension, subgroup_order, reference_subgroup) |>
        dplyr::distinct()

      ordered <- unique(dim_info$ordered_dimension) == 1
      n_subgroups <- nrow(dim_info)
      has_reference <- any(dim_info$reference_subgroup == 1)

      if(ordered){
        if(favourable_indicator){
          txt <- glue::glue("Most-advantaged {separator} Most-disadvantaged")
        } else {
          txt <- glue::glue("Most-disadvantaged {separator} Most-advantaged")
        }
      }

      if(!ordered){
        if(n_subgroups == 2){
          if(has_reference){
            if(favourable_indicator){
              txt <- glue::glue("Reference group {separator} Other group")
            } else {
              txt <- glue::glue("Other group {separator} Reference group")
            }
          } else {
            if(favourable_indicator){
              txt <- glue::glue("Highest {separator} Lowest")
            } else {
              txt <- glue::glue("Highest {separator} Lowest")
            }
          }

        }
        if(n_subgroups > 2){
          if(has_reference){
            if(favourable_indicator){
              txt <- glue::glue("Reference group {separator} Other group (that maximizes the {tolower(measure_txt)})")
            } else {
              txt <- glue::glue("Other group (that maximizes the {tolower(measure_txt)}) {separator} Reference group")
            }
          } else {
            if(favourable_indicator){
              txt <- glue::glue("Highest {separator} Lowest")
            } else {
              txt <- glue::glue("Highest {separator} Lowest")
            }
          }

        }
      }

    }
  }


  txt

}
