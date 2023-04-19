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

chartExploreDisaggregatedBar <- function(data,
                                         title_main = NULL,
                                         title_horizontal = NULL,
                                         title_vertical = NULL,
                                         axis_min = NULL,
                                         axis_max = NULL,
                                         conf_int = FALSE,
                                         data_labels = "none",
                                         decimal_places = 1,
                                         language = "en",
                                         recent,
                                         is_who_dataset) {
  data_years <- sort(unique(data$year))
  data_indicators <- sort(unique(data$indicator_name))
  data_dimensions <- unique(data$dimension)

  chart_dims <- chart_dimensions(data_indicators, data_dimensions)

  include_conf <- conf_int
  first_indicator <- data_indicators[1]

  axis_minmax <- get_axis_min_max(
    .data = data,
    manual_axis_min = axis_min,
    manual_axis_max = axis_max,
    include_conf = include_conf,
    plot_type = "explore-disaggregated-bar"
  )

  start_on_tick <- FALSE
  end_on_tick <- FALSE

  data_grouped <- data %>%
    dplyr::mutate(color = gsub(",0.5)", ",1.0)", color)) %>% #git735
    dplyr::select(
      setting, dimension, indicator_name, indicator_abbr, year,
      source, subgroup, estimate, se, ci_lb, ci_ub, population, popshare,
      setting_average, color, ordered_dimension, subgroup_order, flag
    ) %>%
    dplyr::mutate(
      y = estimate,
      x = match(year, !!data_years) - 1,
      low = ci_lb,
      high = ci_ub,
      year0 = x,
      popshare = popshare * 100,
      x = if(recent) 0 else x,
      year0 = if(recent) 0 else year0
    ) %>%
    dplyr::group_by(dimension, indicator_name) %>%
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
            dplyr::mutate(estimate = NA, y = NA, ci_lb = NA, ci_ub = NA, low = NA, high = NA)
        })
      ) %>%
      dplyr::ungroup()
  } else {
    data_repaired <- data_grouped
  }

  data_repaired$data <- purrr::map(data_repaired$data, function(i) {
    round_vars(
      i,
      c("popshare", "estimate", "ci_lb", "ci_ub", "setting_average"),
      decimal_places
    )
  })


  data_charts <- data_repaired %>%
    dplyr::mutate(
      chart = purrr::pmap(., function(dimension, indicator_name, data_group) {

        ordered_dim <- all(data_group$ordered_dimension == 1)
        if (is_heat_plus()) {
            if (ordered_dim) {
            data_arranged <- data_group %>%
              dplyr::arrange(subgroup_order, as.numeric(estimate))
          } else {
            if (dplyr::n_distinct(data_group$subgroup, na.rm = TRUE) > 7) {
              data_arranged <- data_group %>%
                dplyr::arrange(year, as.numeric(estimate))
            } else {
              data_arranged <- data_group %>%
                dplyr::arrange(subgroup)
            }
          }
        } else if (dimension == translate(c(language, "values", "dimensions", "Subnational region"))) {
          data_arranged <- data_group %>%
            dplyr::arrange(as.numeric(estimate))
        } else if(ordered_dim) {
          data_arranged <- data_group %>%
            dplyr::arrange(subgroup_order, as.numeric(estimate))
        } else {
          data_arranged <- data_group %>%
            dplyr::arrange(subgroup)
        }

        # who-heat/1073 align bars
        data_arranged <- data_arranged %>%
          dplyr::group_by(year) %>%
          dplyr::mutate(
            rank = dplyr::row_number(),
            max_rank = dplyr::n()
          ) %>%
          dplyr::ungroup()

        rank_diffs <- (max(data_arranged$max_rank) - min(data_arranged$max_rank)) / max(data_arranged$max_rank)

        if (rank_diffs > 0.2) {
          total_max <- max(data_arranged$max_rank)
          data_arranged$max_diff <- round((total_max - data_arranged$max_rank) / 2)
          data_arranged$rank <- data_arranged$rank + data_arranged$max_diff

          temp_data <- data_arranged %>%
            dplyr::select(year, estimate, rank) %>%
            tidyr::spread(year, estimate, fill = 0) %>%
            tidyr::gather(year, estimate, -rank)

          data_arranged <- dplyr::left_join(temp_data, data_arranged, by = c("rank", "year"), suffix = c(".x", ""))
          data_arranged$x <- as.numeric(as.character(factor(data_arranged$year, levels = sort(unique(data_arranged$year)), labels = sort(unique(data_arranged$year0)))))
        }

        data_series <- data_arranged %>%
          dplyr::select(-ordered_dimension, -subgroup_order) %>%
          dplyr::group_by(year) %>%
          dplyr::mutate(
            name = paste0("group", seq_len(dplyr::n()))
          ) %>%
          dplyr::group_by(name) %>%
          tidyr::nest() %>%
          dplyr::ungroup()

        series_column <- purrr::pmap(data_series, function(name, data) {
          data <- dplyr::select(data, -low, -high)
          list(
            data = purrr::transpose(data),
            name = name,
            id = name,
            type = "column"
          )
        })

        series_errorbars <- purrr::pmap(data_series, function(name, data) {
          list(
            color = "#606060",
            data = data %>%
              dplyr::select(x, low, high) %>%
              purrr::transpose(),
            enableMouseTracking = FALSE,
            linkedTo = name,
            name = name,
            type = "errorbar"
          )
        })

        if(recent) data_years <- data_group$year[1]
        generateChartExploreDisaggregatedBar(
          series_column = series_column,
          series_errorbars = series_errorbars,
          categories = data_years,
          height = chart_dims[1],
          width = chart_dims[2],
          axis_min = axis_minmax$min[axis_minmax$indicator_name == indicator_name],
          axis_max = axis_minmax$max[axis_minmax$indicator_name == indicator_name],
          conf_int = conf_int,
          data_labels = data_labels,
          n_subgroups = length(data_series),
          include_conf = include_conf,
          decimal_places = decimal_places,
          start_on_tick = start_on_tick,
          end_on_tick = end_on_tick,
          language=language
        )
      })
    )

  legend <- data %>%
    dplyr::group_by(dimension) %>%
    dplyr::mutate(
      subgroup = if (dplyr::n_distinct(color) == 1) dimension else subgroup # @hardcoded
    ) %>%
    dplyr::arrange(dimension, subgroup_order) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dimension, subgroup, color) %>%
    dplyr::mutate(color = gsub(",0.5)", ",1.0)", color)) %>% #git735
    dplyr::transmute(
      dimension = dimension,
      label = subgroup,
      color = color,
      type = "column"
    ) %>%
    chart_legend(symbol = "square")

  charts <- data_charts %>%
    dplyr::group_by(indicator_name) %>%
    dplyr::summarise(charts = list(chart)) %>%
    dplyr::pull()



  chart_table(
    charts = charts,
    title_top = unique(data_charts$dimension),
    title_right = data_indicators, # unique(data_charts$indicator_name),
    title_main = title_main,
    title_vertical = title_vertical,
    title_horizontal = title_horizontal,
    legend = legend,
    language = language,
    is_who_dataset = is_who_dataset
  )
}

generateChartExploreDisaggregatedBar <- function(series_column,
                                                 series_errorbars,
                                                 categories,
                                                 height,
                                                 width,
                                                 axis_min,
                                                 axis_max,
                                                 conf_int,
                                                 data_labels,
                                                 n_subgroups,
                                                 include_conf,
                                                 decimal_places,
                                                 start_on_tick,
                                                 end_on_tick,
                                                 language = "en") {

  chart <- highchart() %>%
    hc_add_series_list(series_column)

  if (conf_int) {
    chart <- hc_add_series_list(chart, series_errorbars)
  }


  chart %>%
    hc_chart(
      height = height,
      marginTop = 15,
      plotBorderWidth = 1,
      spacingRight = 1,
      reflow = TRUE,
      width = NULL
    ) %>%
    hc_plotOptions(
      column = list(
        colorByPoint = TRUE,
        dataLabels = list(
          allowOverlap = TRUE,
          crop = FALSE,
          enabled = data_labels != "none",
          filter = list(
            property = "y",
            operator = ">",
            value = -9999
          ),
          format = glue("{{point.y:.{ decimal_places }.f}}"),
          overflow = FALSE,
          style = list(
            color = "#000000",
            fontSize = switch(
              data_labels,
              small = "8px",
              medium = "10px",
              large = "14px"
            ),
            fontWeight = "normal",
            textOutline = "none"
          ),
          useHTML = TRUE,
          zIndex = 4
        ),
        maxPointWidth = 50,
        minPointWidth = 25,
        minPointLength = 2,
        borderWidth = 0.5,
        showInLegend = FALSE,
        stacking = FALSE
      )
    ) %>%
    hc_tooltip(
      hideDelay = 50
    ) %>%
    hc_xAxis(
      categories = as.list(categories),
      min = 0,
      max = length(categories) - 1,
      tickLength = 0,
      tickmarkPlacement = "on",
      title = list(
        text = FALSE
      ),
      type = "category"
    ) %>%
    hc_yAxis(
      endOnTick = end_on_tick,
      gridLineWidth = 1,
      min = axis_min,
      max = axis_max,
      startOnTick = start_on_tick,
      tickLength = 0,
      title = list(
        text = FALSE
      )
    ) %>%
    hc_boost(
      enabled = FALSE
    ) %>%
    chart_tooltip(
      heading = "this.point.setting + ', ' + this.point.source + ' ' + this.point.year",
      body= standard_tooltip_body(include_conf = TRUE, language = language),
      decimal_places = decimal_places
    )
}
