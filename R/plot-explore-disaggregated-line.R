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

chartExploreDisaggregatedLine <- function(data,
                                          title_main = NULL,
                                          title_horizontal = NULL,
                                          title_vertical = NULL,
                                          axis_min = NULL,
                                          axis_max = NULL,
                                          decimal_places = 1,
                                          language = "en",
                                          recent,
                                          is_who_dataset) {






  data_years <- sort(unique(data$year))
  data_indicators <- sort(unique(data$indicator_name))
  data_dimensions <- unique(data$dimension)

  data <- color_adjustment(data, language)

  chart_dims <- chart_dimensions(data_indicators, data_dimensions)

  include_conf <- FALSE #all(!is.na(data$ci_lb)) && all(!is.na(data$ci_ub))
  first_indicator <- data_indicators[1]

  axis_minmax <- get_axis_min_max(
    .data = data,
    manual_axis_min = axis_min,
    manual_axis_max = axis_max,
    include_conf = include_conf,
    disaggregated = TRUE,
    plot_type = "explore-disaggregated-line"
  )

  start_on_tick <- FALSE
  end_on_tick <- FALSE


  data_grouped <- data %>%
    dplyr::select(
      setting, dimension, indicator_name, indicator_abbr, year,
      source, subgroup, estimate, se, ci_lb, ci_ub, population, popshare,
      setting_average, color, flag
    ) %>%
    dplyr::mutate(
      setting,
      dimension,
      x = estimate,
      y = match(year, !!data_years) - 1,
      year0 = y,
      name = subgroup,
      popshare = popshare * 100,
      y = if(recent) 0 else y,
      year0 = if(recent) 0 else year0
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
            dplyr::mutate(estimate = NA, x = NA, ci_lb = NA, ci_ub = NA)
        })
      ) %>%
      dplyr::ungroup()
  } else {
    data_repaired <- dplyr::ungroup(data_grouped)
  }


  data_repaired$data <- purrr::map(data_repaired$data, function(i){
    round_vars(
      i,
      c("popshare", "estimate", "ci_lb", "ci_ub", "setting_average"),
      decimal_places
    )
  })

  data_charts <- data_repaired %>%
    dplyr::mutate(
      chart = purrr::pmap(., function(dimension, indicator_name, data) {
        is_top_row <- indicator_name == first_indicator


        data_series <- data %>%
          dplyr::mutate(year_copy = year) %>%
          dplyr::group_by(year_copy) %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::pull() %>%
          purrr::map(function(d) {
            list(
              type = "line",
              data = purrr::transpose(d)
            )
          })

        if(recent) data_years <- data$year[1]
        generateChartExploreDisaggregatedLine(
          data = data_series,
          categories = data_years,
          width = chart_dims[2],
          height = chart_dims[1],
          axis_min = axis_minmax$min[axis_minmax$indicator_name == indicator_name],
          axis_max = axis_minmax$max[axis_minmax$indicator_name == indicator_name],
          start_on_tick = start_on_tick,
          end_on_tick = FALSE,
          include_conf = include_conf,
          decimal_places = decimal_places,
          language = language
        )
      })
    )


  charts <- data_charts %>%
    dplyr::group_by(indicator_name) %>%
    dplyr::summarise(charts = list(chart)) %>%
    dplyr::pull()

  title_indicators <- unique(data_charts$indicator_name)


  legend_info <- data %>%
    dplyr::group_by(dimension) %>%
    dplyr::mutate(
      subgroup = if (dplyr::n_distinct(color) == 1) dimension else subgroup
    ) %>%
    dplyr::arrange(dimension, subgroup_order) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dimension, subgroup, color) %>%
    dplyr::transmute(
      dimension = dimension,
      label = subgroup,
      color = color,
      type = "scatter"
    )

  # legend_info$color[legend_info$dimension == "Subnational region"] <-
  #   highcharter::hex_to_rgba(legend_info$color[legend_info$dimension == "Subnational region"], alpha = 0.5)

  legend <- chart_legend(legend_info)

  chart_table(
    charts = charts,
    title_top = unique(data_charts$dimension),
    title_right = title_indicators,
    title_main = title_main,
    title_horizontal = title_horizontal,
    title_vertical = title_vertical,
    legend = legend,
    language = language,
    is_who_dataset = is_who_dataset
  )
}

generateChartExploreDisaggregatedLine <- function(data,
                                                  categories,
                                                  height,
                                                  width,
                                                  axis_min,
                                                  axis_max,
                                                  start_on_tick,
                                                  end_on_tick,
                                                  include_conf,
                                                  decimal_places,
                                                  language = "en") {


  highchart() %>%
    hc_add_series_list(data) %>%
    hc_chart(
      marginTop = 15,
      height = height,
      width = NULL,
      plotBorderWidth = 1,
      reflow = TRUE,
      spacingRight = 1
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
    hc_tooltip(
      hideDelay = 50
    ) %>%
    hc_yAxis(
      categories = as.list(categories),
      endOnTick = end_on_tick,
      labels = list(
        x = -5
        #,formatter = JS("function(){return this.value.toString().length !== 4 ? '' : this.value;}")
      ),
      min = 0,
      max = length(categories) - 1,
      tickLength = 0,
      tickmarkPlacement = "on",
      title = list(
        text = FALSE
      ),
      type = "category"
    ) %>%
    hc_xAxis(
      endOnTick = end_on_tick, # TRUE,
      gridLineWidth = 1,
      min = axis_min,
      max = axis_max,
      startOnTick = start_on_tick, # TRUE,
      tickLength = 0,
      title = list(
        text = FALSE
      )
    ) %>%
    chart_tooltip(
      heading = standard_tooltip_header(),
      body = standard_tooltip_body(
        include_conf = TRUE,
        language = language
      ),
      decimal_places = decimal_places
    )
}

