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

chartCompareDefault <- function(data,
                                focus_setting,
                                title_main = NULL,
                                title_vertical = NULL,
                                title_horizontal = NULL,
                                axis_min = NULL,
                                axis_max = NULL,
                                decimal_places = 1,
                                language = "en",
                                is_who_dataset) {


  data <- color_adjustment(data, language)

  include_conf <- FALSE
  axis_minmax <- get_axis_min_max(
    .data = data,
    manual_axis_min = axis_min,
    manual_axis_max = axis_max,
    include_conf = include_conf,
    plot_type = "compare-disaggregated-line"
  )

  data_sorted <- data %>%
    dplyr::mutate(
      title = glue::glue("{ setting } ({ source } { year })")
    ) %>%
    dplyr::arrange(setting == !!focus_setting, dplyr::desc(setting))

  data_titles <- unique(data_sorted$title)

  data_series <- data_sorted %>%
    dplyr::transmute(
      title,
      setting,
      source,
      year,
      dimension,
      subgroup,
      popshare,
      estimate,
      ci_lb,
      ci_ub,
      setting_average,
      indicator_name,
      color,

      y = match(title, !!data_titles) - 1,
      x = estimate,
      popshare = popshare * 100,
      flag
    )

  data_series <- data_series %>%
    round_vars(
      c("popshare", "estimate", "ci_lb", "ci_ub", "setting_average"),
      decimal_places
    )

  data_series <- data_series %>%
    dplyr::group_by(title) %>%
    dplyr::arrange(x) %>%
    tidyr::nest() %>%
    dplyr::pull() %>%
    purrr::map(function(d) {
      list(
        data = purrr::transpose(d),
        type = "line"
      )
    })

  chart_height <- max(650, NROW(data_series) * 25)

  chart <- highchart() %>%
    hc_add_series_list(data_series) %>%
    hc_chart(
      height = chart_height,
      marginTop = 15,
      plotBorderWidth = 1,
      reflow = TRUE,
      width = NULL
    ) %>%
    hc_boost(
      enabled = FALSE
    ) %>%
    hc_plotOptions(
      line = list(
        color = "rgba(0,0,0,0.5)",
        states = list(
          hover = list(
            lineWidthPlus = 0
          )
        ),
        lineWidth = 1,
        marker = list(
          enabled = TRUE,
          radius = 6,
          symbol = "circle"
        ),
        showInLegend = FALSE
      )
    ) %>%
    hc_xAxis(
      endOnTick = FALSE,
      gridLineWidth = 1,
      min = axis_minmax$min[axis_minmax$indicator_name == data$indicator_name[1]],
      max = axis_minmax$max[axis_minmax$indicator_name == data$indicator_name[1]],
      startOnTick = FALSE, # TRUE,
      tickLength = 0,
      title = list(
        text = FALSE
      )
    ) %>%
    hc_yAxis(
      categories = as.list(data_titles),
      min = 0,
      max = length(data_titles) - 1,
      tickmarkPlacement = "on",
      title = list(
        text = FALSE
      ),
      type = "category"
    ) %>%
    chart_tooltip(
      heading = standard_tooltip_header(),
      body = standard_tooltip_body(include_conf = TRUE, language = language),
      decimal_places = decimal_places
    )

  legend <- data %>%
    dplyr::group_by(dimension) %>%
    dplyr::mutate(
      subgroup = if (dplyr::n_distinct(color) == 1) dimension else subgroup,
    ) %>%
    dplyr::arrange(dimension, subgroup_order) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dimension, subgroup, color) %>%
    dplyr::transmute(
      dimension = dimension,
      label = subgroup,
      color = color,
      type = "scatter"
    ) %>%
    chart_legend()

  chart_singleton(
    chart = chart,
    title_right = data$indicator_name[1],
    title_main = title_main,
    title_horizontal = title_horizontal,
    title_vertical = title_vertical,
    legend = legend,
    disclaimer = chart_disclaimer(language, is_who_dataset)
  )
}
