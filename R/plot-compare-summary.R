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

chartCompareSummary <- function(data,
                                focus_setting,
                                label_style,
                                label_size,
                                title_main = NULL,
                                title_vertical = NULL,
                                title_horizontal = NULL,
                                axis_horizontal_min = NULL,
                                axis_horizontal_max = NULL,
                                axis_vertical_min = NULL,
                                axis_vertical_max = NULL,
                                decimal_places = 1,
                                language = "en",
                                is_who_dataset) {
  include_conf <- all(!is.na(data$se.lowerci)) && all(!is.na(data$se.upperci))
  color_setting <- if (is_heat_plus()) "#b5e61d" else "#e56620"
  color_benchmark <- "#2a5783"
  use_labels <- label_style %in% c("iso3", "setting")

  # data <- dplyr::bind_rows(
  #   dplyr::filter(data, setting != focus_setting),
  #   dplyr::filter(data, setting == focus_setting),
  # )

  data <- data %>%
    dplyr::arrange(setting == !!focus_setting)

  data_dimension <- data$dimension[1]
  data_measure <- data$measure[1]
  data_log <- is_log_scale(data_measure)

  data_series <- data %>%
    dplyr::transmute(
      setting,
      year,
      source,
      inequal,
      iso3,
      r_national,

      x = r_national,
      y = inequal,
      ci_lb = se.lowerci,
      ci_ub = se.upperci,
      name = if (label_style == "iso3") iso3 else setting,
      color = ifelse(setting == focus_setting, color_setting, color_benchmark),
      title = glue::glue("{ setting } ({ source } { year })"),
      measure_name = measure_name(measure, language)
    )

  data_series <-   data_series %>%
    round_vars(
      c("inequal", "ci_lb", "ci_ub", "r_national"),
      decimal_places
    )

  x_avg <- median(data_series$x, na.rm = TRUE)
  y_avg <- median(data_series$y, na.rm = TRUE)

  dec_x <- get_median_decimals(x_avg)
  dec_y <- get_median_decimals(y_avg)

  x_avg <- round(x_avg, dec_x)
  y_avg <- round(y_avg, dec_y)

  data_series <- data_series %>%
    dplyr::group_by(title) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::pull() %>%
    purrr::map(function(d) {
      list(
        data = purrr::transpose(d),
        lineColor = "rgba(0,0,0,0)",
        type = "line"
      )
    })

  if (NROW(data_series) == 1) {
    if (is.null(axis_horizontal_min) && is.null(axis_horizontal_max)) {
      h_label_format <- "{value:.1f}"
    } else {
      h_label_format <- "{value}"
    }

    if (is.null(axis_vertical_min) && is.null(axis_vertical_max)) {
      v_label_format <- "{value:.1f}"
    } else {
      v_label_format <- "{value}"
    }
  } else {
    h_label_format <- v_label_format <- "{value}"
  }

  legend_items <- dplyr::tibble(
    dimension = c(data_dimension, data_dimension),
    label = c(translate(c(language, "charts", "legend", "benchmark")), focus_setting), #"Benchmark settings"
    color = c(color_benchmark, color_setting),
    type = c("scatter", "scatter")
  )

  legend <- chart_legend(legend_items)


  chart <- highchart() %>%
    hc_add_series_list(data_series) %>%
    hc_chart(
      height = 650,
      marginTop = 15,
      plotBorderWidth = 1,
      #reflow = TRUE,
      width = NULL # 650
    ) %>%
    hc_plotOptions(
      line = list(
        animation = list(
          duration = 0
        ),
        dataLabels = list(
          allowOverlap = TRUE,
          defer = FALSE,
          enabled = use_labels,
          format = "<span style='color:{point.color};'>{point.name}</span>",
          useHTML = TRUE,
          style = list(
            fontSize = chart_font_size(label_size),
            textOutline = "none"
          ),
          verticalAlign = "middle"
        ),
        marker = list(
          radius = if (use_labels) 0 else 6,
          states = list(
            hover = list(
              enabled = !use_labels
            )
          ),
          symbol = "circle"
        ),
        showInLegend = FALSE
      )
    ) %>%
    hc_xAxis(
      endOnTick = is.null(axis_horizontal_max),
      gridLineWidth = 1,
      labels = list(
        format = h_label_format
      ),
      plotLines = list(plot_line_median(x_avg, language, 0)),
      min = axis_horizontal_min,
      max = axis_horizontal_max,
      startOnTick = is.null(axis_horizontal_min),
      tickLength = 0,
      title = list(
        text = FALSE
      )
    ) %>%
    hc_boost(
      enabled = FALSE
    ) %>%
    hc_yAxis(
      endOnTick = is.null(axis_vertical_max),
      labels = list(
        format = v_label_format
      ),
      min = axis_vertical_min,
      max = axis_vertical_max,
      startOnTick = is.null(axis_vertical_min),
      plotLines = list(plot_line_median(y_avg, language, 0)),
      title = list(
        text = FALSE
      ),
      type = if (data_log) "logarithmic"
    ) %>%
    chart_tooltip(
      heading = standard_tooltip_header(),
      body = standard_tooltip_body(include_conf = TRUE, summary_measure = TRUE, language = language),
      decimal_places = decimal_places
    )


  chart_singleton(
    chart = chart,
    title_top = NULL,
    title_right = NULL,
    title_main = title_main,
    title_horizontal = title_horizontal,
    title_vertical = title_vertical,
    legend = legend,
    disclaimer = chart_disclaimer(language, is_who_dataset)
  )
}
