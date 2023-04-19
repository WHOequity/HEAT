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

chartExploreDetail <- function(data,
                               sort_by,
                               sort_order,
                               sort_indicator,
                               highlight_subgroup,
                               plot_lines = NULL,
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
  include_conf <- conf_int


  if (sort_by == "subgroup") {
    if (sort_order == "ascending") {
      if(unique(data$ordered_dimension) == 1){
        data_sorted <- dplyr::arrange(data, xtfrm(subgroup_order))
      } else {
        data_sorted <- dplyr::arrange(data, xtfrm(subgroup))
      }


    } else if (sort_order == "descending") {
      if(unique(data$ordered_dimension) == 1){
        data_sorted <- dplyr::arrange(data, -xtfrm(subgroup_order))
      } else {
        data_sorted <- dplyr::arrange(data, -xtfrm(subgroup))
      }
    }
  }

  if (sort_by == "indicator") {
    order_scalar <- if (sort_order == "ascending") -1 else 1

    # when certain indicators have no data for the given combination of
    # variable we cannot sort
    if (any(sort_indicator %in% data$indicator_abbr)) {
      data_sorted <- data %>%
        dplyr::group_by(subgroup) %>%
        # git 504
        # dplyr::mutate(
        #   group_estimate = estimate[indicator_abbr == !!sort_indicator]
        # ) %>%
        dplyr::arrange(!!order_scalar * dplyr::desc(estimate)) %>%
        # dplyr::select(-group_estimate) %>%
        dplyr::ungroup()
    } else {
      data_sorted <- dplyr::ungroup(data)
    }
  }

  n_indicator <- dplyr::n_distinct(data_sorted$indicator_name)
  color_highlight <- if (is_heat_plus()) "#b5e61d" else "#e56620"
  # data_subgroups <- unique(data_sorted$subgroup)
  data_indicators <- unique(data_sorted$indicator_name)

  chart_width <- chart_dimensions(data_indicators)

  axis_minmax <- get_axis_min_max(
    .data = data,
    manual_axis_min = axis_min,
    manual_axis_max = axis_max,
    include_conf = include_conf,
    plot_type = "explore-detail"
  )

  start_on_tick <- FALSE
  end_on_tick <- FALSE

  data_charts <- data_sorted %>%
    dplyr::mutate(color = gsub(",0.5)", ",1.0)", color)) %>% #git735
    dplyr::transmute(
      dimension,
      indicator_name,
      estimate,
      ci_lb,
      ci_ub,
      setting_average,
      setting,  # tooltip
      source,   # tooltip
      year,     # tooltip
      subgroup, # tooltip
      popshare, # tooltip

      y = estimate,
      name = subgroup,
      id = name,
      linkedTo = name,
      low = ci_lb,
      high = ci_ub,
      color = ifelse(subgroup %in% highlight_subgroup, color_highlight, color),
      popshare = popshare * 100,
      flag
    ) %>%
    dplyr::group_by(dimension, indicator_name) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      chart = purrr::pmap(., function(dimension, indicator_name, data_group) {
        line_setting <- if ("setting" %in% plot_lines) {
          plot_line_setting(
            value = round(data_group$setting_average[1], 1),
            language = language
          )
        }

        line_median <- if ("median" %in% plot_lines) {
          plot_line_median(
            value = round(median(data_group$estimate, na.rm = TRUE), 1),
            language = language
          )
        }

        data_group <- data_group %>%
          round_vars(
            c("popshare", "estimate", "ci_lb", "ci_ub", "setting_average"),
            decimal_places
          )

        series_bar <- data_group %>%
          dplyr::select(-linkedTo, -low, -high) %>%
          purrr::transpose()

        series_conf <- data_group %>%
          dplyr::select(linkedTo, y, low, high) %>%
          purrr::transpose()

        highchart() %>%
          hc_add_series(
            type = "bar",
            data = series_bar
          ) %>% {
            if (conf_int) {
              hc_add_series(
                .,
                color = "#606060",
                data = series_conf,
                enableMouseTracking = FALSE,
                type = "errorbar"
              )
            } else {
              .
            }
          } %>%
          hc_chart(
            height = max(650, NROW(series_bar) * 25),
            marginTop = 15,
            plotBorderWidth = 1,
            spacingRight = 1,
            reflow = TRUE,
            width = NULL
          ) %>%
          chart_tooltip(
            heading = standard_tooltip_header(),
            body = standard_tooltip_body(include_conf = TRUE, language = language),
            decimal_places = decimal_places
          ) %>%
          hc_plotOptions(
            bar = list(
              dataLabels = list(
                allowOverlap = TRUE,
                crop = FALSE,
                enabled = data_labels != "none",
                filter = list(
                  property = "y",
                  operator = ">",
                  value = -9999
                ),
                format = glue("{{point.y:.{ decimal_places }f}}"),
                overflow = FALSE,
                style = list(
                  color = "#000000",
                  fontSize = chart_font_size(data_labels),
                  fontWeight = "normal",
                  textOutline = "none"
                ),
                useHTML = TRUE,
                zIndex = 4
              ),
              maxPointWidth = 50,
              minPointWidth = 25,
              showInLegend = FALSE,
              stickyTracking = TRUE
            )
          ) %>%
          hc_xAxis(
            categories = as.list(data_group$subgroup),
            labels = list(
              syle = list(
                whiteSpace = "normal"
              ),
              x = -5
            ),
            min = 0,
            max = length(data_group$subgroup) - 1,
            tickLength = 0,
            tickmarkPlacement = "on",
            title = list(
              text = FALSE
            ),
            type = "category"
          ) %>%
          hc_yAxis(
            endOnTick = end_on_tick, # TRUE,
            min = axis_minmax$min[axis_minmax$indicator_name == indicator_name],
            max = axis_minmax$max[axis_minmax$indicator_name == indicator_name],
            plotLines = purrr::compact(list(line_setting, line_median)),
            startOnTick = start_on_tick, # TRUE,
            tickLength = 0,
            title = list(
              text = FALSE
            )
          )
      })
    ) %>%
    tidyr::complete(
      dimension, indicator_name,
      fill = list(chart = list(chartEmpty()))
    )

  data_years <- sapply(data_charts$data, function(x) unique(x$year))

  chart_table(
    charts = list(data_charts$chart),
    title_top = paste0(unique(data_charts$indicator_name), " (", data_years, ")"),
    title_right = data_sorted$dimension[1],
    title_main = title_main,
    title_horizontal = title_horizontal,
    title_vertical = title_vertical,
    legend = NULL,
    language = language,
    is_who_dataset = is_who_dataset
  )
}

