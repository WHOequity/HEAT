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

chartDeterminant <- function(data,
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
                                regression_line = NULL,
                                decimal_places = 1,
                                language = "en",
                                is_who_dataset
                             ) {
  color_setting <- if (is_heat_plus()) "#b5e61d" else "#e56620"
  color_benchmark <- "#2a5783"
  use_labels <- label_style %in% c("iso3", "setting")
  lm_dtl <- NULL


  data <- data %>%
    dplyr::arrange(setting == !!focus_setting)

  data_series <- data %>%
    # dplyr::rename(
    #   year_orig = year,
    #   year = year_alt
    # ) |>
    dplyr::transmute(
      setting,
      year,
      sdh_year,
      sdh_name,
      sdh_source,
      source,
      indicator_name,
      iso3,
      setting_average,
      sdh_estimate,
      x = sdh_estimate,
      y = setting_average,
      name = if (label_style == "iso3") iso3 else setting,
      color = ifelse(setting == focus_setting, color_setting, color_benchmark),
      title = glue::glue("{ setting } ({ source } { year })")
    )

  data_series <-   data_series %>%
    round_vars(
      c("sdh_estimate", "setting_average"),
      decimal_places
    )

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
    dimension = c("data_dimension", "data_dimension"),
    label = c(translate(c(language, "charts", "legend", "benchmark")), focus_setting), #"Benchmark settings"
    color = c(color_benchmark, color_setting),
    type = c("scatter", "scatter")
  )

  if(nrow(data) > 1 && regression_line){
    lm1 <- lm(setting_average~sdh_estimate, data = data)


    lm_dtl <- list(
      intercept = lm1$coefficients[1],
      slope = lm1$coefficients[2],
      r2 = summary(lm1)$r.squared
    )

    lm_dtl <- lapply(lm_dtl, function(x){format(round(x,3), nsmall = 3)})

    p_val <- lm_pval(lm1)

    if(is.na(p_val) | is.null(p_val)){
      p_val <- "N/A"
    } else {
      if(p_val < 0.001){
        p_val <- "< 0.001"
      } else {
        p_val <- format(round(p_val, 3), nsmall = 3)
      }
    }

    lm_dtl$p_val <- p_val

    pct <- 0.1
    sdval <- sd(data$sdh_estimate)
    minval <- min(data$sdh_estimate) - pct * sdval
    maxval <- max(data$sdh_estimate) + pct * sdval
    pred_data <- dplyr::tibble(sdh_estimate = seq(minval, maxval, length = 30))
    pred_data$preds <- predict(lm1, pred_data)

    legend_items <- dplyr::bind_rows(
      legend_items,
      dplyr::tibble(
        dimension = c("data_dimension"),
        label = translate(c(language, "options", "labels", "regression")), #"Regression line"
        color = c("grey"),
        type = c("line")
      )
    )
  }

  legend <- chart_legend(legend_items)

  chart <- highchart()

  # git962
  start_on_tick <- is.null(axis_horizontal_min)
  if(is.null(axis_horizontal_min) &&
     !data$sdh_abbr[1] %in% c("NY.GDP.PCAP.KD.ZG","SI_HEI_TOTL_B40", "SI_HEI_TOTL_T" ) &&
     min(data$sdh_estimate, na.rm = TRUE) < 1
  ){
    start_on_tick <- FALSE
  }


  if(nrow(data) > 1 && regression_line){
    chart <- chart |>
      hc_add_series(
        name = "determinant_line",
        pred_data,
        type = "line",
        hcaes(x = sdh_estimate, y = preds),
        showInLegend = FALSE,
        color = "grey",
        showAxes = FALSE,
        animation = FALSE,
        marker = list(
          enabled = FALSE,
          radius = 0
        )
      )
}

  chart <- chart |>
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
      min = axis_horizontal_min,
      max = axis_horizontal_max,
      startOnTick = start_on_tick,
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
      title = list(
        text = FALSE
      )
    ) %>%
    chart_tooltip(
      heading = standard_tooltip_header(determinant = TRUE),
      body = standard_tooltip_body(
        include_conf = FALSE,
        summary_measure = FALSE,
        from_determinant = TRUE,
        language = language
        ),
      decimal_places = decimal_places,
      lm_dtl = lm_dtl,
      language = language
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
