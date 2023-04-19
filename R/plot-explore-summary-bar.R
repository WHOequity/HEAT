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

chartExploreSummaryBar <- function(data,
                                   title_main = NULL,
                                   title_vertical = NULL,
                                   title_horizontal = NULL,
                                   axis_min = NULL,
                                   axis_max = NULL,
                                   conf_int = FALSE,
                                   data_labels = "none",
                                   decimal_places = 1,
                                   language = "en",
                                   is_who_dataset) {

  # print(paste('explore-summary-bar ', language))

  include_conf <- conf_int
  indicator_palette <- c("#4f7cba", "#2cb5c0", "#21b087", "#a2b627", "#f8b620")
  data_indicators <- sort(unique(data$indicator_name))
  data_years <- sort(unique(data$year))
  data_measure <- data$measure[1]
  data_log <- is_log_scale(data_measure)
  data_dimensions <- unique(data$dimension)

  chart_dims <- chart_dimensions(data_indicators, data_dimensions)

  axis_minmax <- get_axis_min_max(
    .data = data,
    manual_axis_min = axis_min,
    manual_axis_max = axis_max,
    include_conf = include_conf,
    disaggregated = FALSE,
    bar = TRUE,
    plot_type = "explore-summary-bar"
  )

  data_grouped <- data %>%
    dplyr::transmute(
      setting,
      year,
      source,
      dimension,
      measure,
      indicator_name,
      inequal,
      r_national,

      x = match(year, !!data_years) - 1,
      y = inequal,
      ci_lb = se.lowerci,
      ci_ub = se.upperci,
      low = ci_lb,
      high = ci_ub,
      measure_name = measure_name(measure, language)
    ) %>%
    dplyr::group_by(dimension, indicator_name) %>%
    dplyr::arrange(dimension, x) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    tidyr::complete(dimension, indicator_name)

  if (any(purrr::map_lgl(data_grouped$data, is.null))) {
    data_repaired <- data_grouped %>%
      dplyr::group_by(dimension) %>%
      dplyr::mutate(
        data = purrr::map(data, function(d) {
          if (!is.null(d)) {
            return(d)
          }

          data[!purrr::map_lgl(data, is.null)][[1]] %>%
            dplyr::mutate(inequal = NA, y = NA, ci_lb = NA, ci_ub = NA, low = NA, high = NA)
        })
      ) %>%
      dplyr::ungroup()
  } else {
    data_repaired <- dplyr::ungroup(data_grouped)
  }

  data_repaired$data <- purrr::map(data_repaired$data, function(i){
    round_vars(
      i,
      c("inequal", "ci_lb", "ci_ub", "r_national"),
      decimal_places
    )
  })

  data_charts <- data_repaired %>%
    dplyr::mutate(
      chart = purrr::pmap(., function(dimension, indicator_name, data_group) {
        chart_color <- indicator_palette[match(indicator_name, data_indicators)]

        highchart() %>%
          hc_add_series(
            color = chart_color,
            data = dplyr::select(data_group, -low, -high) %>%
              purrr::transpose(),
            type = "column",
            minPointLength = 2
          ) %>% {
            if (conf_int) {
              hc_add_series(
                .,
                data = data_group %>%
                  dplyr::select(x, low, high) %>%
                  purrr::transpose(),
                type = "errorbar",
                color = "#606060",
                enableMouseTracking = FALSE
              )
            } else {
              .
            }
          } %>%
          hc_chart(
            height = chart_dims[1],
            marginTop = 15,
            plotBorderWidth = 1,
            reflow = TRUE,
            spacingRight = 1,
            width = NULL
          ) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                allowOverlap = TRUE,
                crop = FALSE,
                enabled = data_labels != "none",
                format = glue("{{y:.{ decimal_places }f}}"),
                overflow = "allow",
                style = list(
                  fontSize = switch(
                    data_labels,
                    small = "8px",
                    medium = "10px",
                    large = "14px"
                  ),
                  fontWeight = 300,
                  textOutline = "none"
                ),
                useHTML = TRUE
              ),
              grouping = FALSE,
              maxPointWidth = 50,
              minPointWidth = 25,
              threshold = if (data_log) 1 else 0,
              showInLegend = FALSE
            )
          ) %>%
          hc_xAxis(
            categories = as.list(data_years),
            # labels = list( #commented out git 286
            #   step = 1,
            #   style = list(
            #     textOverflow = "none",
            #     whiteSpace = "normal"
            #   )
            # ),
            min = 0,
            max = length(data_years) - 1,
            tickLength = 0,
            tickmarkPlacement = "on",
            title = list(
              text = FALSE
            ),
            type = "category"
          ) %>%
          hc_yAxis(
            endOnTick = FALSE,
            # labels = list(
            #   x = -5
            # ),
            # labels = list(
            #   format = glue::glue("{{value:.{decimal_places}f}")
            # ),
            min = axis_minmax$min[axis_minmax$indicator_name == indicator_name],
            max = axis_minmax$max[axis_minmax$indicator_name == indicator_name],
            startOnTick = FALSE,
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
      })
    )

  if (!str_truthy(title_vertical)) {
    title_vertical <- measure_name(data$measure[[1]], language)
  }

  charts <- data_charts %>%
    dplyr::group_by(indicator_name) %>%
    dplyr::summarise(charts = list(chart)) %>%
    dplyr::pull()



  chart_table(
    charts = charts,
    title_top = unique(data_charts$dimension),
    title_right = unique(data_charts$indicator_name),
    title_main = title_main,
    title_vertical = title_vertical,
    title_horizontal = title_horizontal,
    legend = NULL,
    language = language,
    is_who_dataset = is_who_dataset
  )
}

