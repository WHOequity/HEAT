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

# ├─ constants ----
CONSTANT_DISPUTED <- 99999999

# ├─ data ----
# spatial_disputed <- structure(
#   list(
#     setting = "India",
#     subgroups = list(
#       list("[jm] jammu and kashmir", "jammu")
#     )
#   ),
#   row.names = c(NA, -1L),
#   class = c("tbl_df", "tbl", "data.frame")
# )


# ├─ geographies ----
geography_subnational <- function(iso3, year) {
  geo_filtered <- dplyr::filter(
    heatdata::spatial_subnational_simplified,
    iso3 == !!iso3,
    year == !!year
  )

  if (NROW(geo_filtered) == 0) {
    return(NULL)
  }

  dplyr::pull(geo_filtered, geojson)[[1]]
}

geography_national <- function(iso3) {
  heatdata::spatial_national %>%
    dplyr::filter(
      iso3 == !!iso3
    ) %>%
    dplyr::pull(geojson) %>%
    .[[1]]
}


geography_disputed <- function(iso3) {
  heatdata::spatial_disputed %>%
    dplyr::filter(iso3 == !!iso3) %>%
    dplyr::pull(geojson) %>%
    .[[1]]
}

# ├─ disputed utilities ----
# disputed_setting <- function(setting) {
disputed_iso3 <- function(iso3) {
  isTRUE(iso3 %in% heatdata::spatial_disputed$iso3)
}

# disputed_adjust <- function(.data, setting) {
#   subgroups <- disputed_subgroups(setting)
#
#   dplyr::mutate(
#     .data,
#     estimate = ifelse(
#       (setting == !!setting) & (subgroup %in% !!subgroups),
#       CONSTANT_DISPUTED,
#       estimate
#     )
#   )
# }

# disputed_subgroups <- function(setting) {
#   if (!(setting %in% heatdata::spatial_disputed$setting)) {
#     return(NULL)
#   }
#
#   return(NULL)
#
#   # there's no subgroup column?
#   heatdata::spatial_disputed %>%
#     dplyr::filter(setting == !!setting) %>%
#     dplyr::pull(subgroups) %>%
#     unlist()
# }

# dummyExploreMap <- function() {
#   .global$main %>%
#     dplyr::filter(
#       source == "DHS",
#       setting == "Egypt"
#     ) %>%
#     dplyr::filter(
#       year == max(year),
#       dimension == "Subnational region",
#       indicator_abbr == "sba"
#     )
# }

# ├─ generate chart ----
chartExploreMap <- function(data,
                            title_horizontal = NULL,
                            title_vertical = NULL,
                            title_main = NULL,
                            decimal_places = 1,
                            language = "en",
                            dataset_name = TRUE,
                            is_map_dataset,
                            is_who_dataset) {

  if(!is_map_dataset) return()
  # ├─ config ----
  data_setting <- unique(data$setting)
  data_iso3 <- unique(data$iso3)
  data_year <- unique(data$year)
  data_indicator <- data$indicator_name[1]
  null_fill_color <- "rgb(100,100,100)"
  flex_indicator <- data_indicator %in% names(FLEXIBLE_SCALE_INDICATORS)


  chart_palette <- color_stops(5, c(viridisLite::viridis(5, begin = 0.2, direction = -1)))
  if (!str_truthy(title_main) && isTRUE(title_main != "")) {
    title_main <- data %>%
      dplyr::distinct(setting, source, year) %>%
      dplyr::group_by(setting, source) %>%
      dplyr::summarise(year = glue::glue_collapse(year, sep = ", ")) %>%
      dplyr::mutate(source = paste(source, year)) %>%
      dplyr::summarise(
        source = glue::glue_collapse(source, sep = ", ", last = ", and ")
      ) %>%
      glue::glue_data("{ setting } ({ source })")
  }

  # ├─ adjust for disputed ----
  # if (disputed_setting(data_setting)) {
  #   data_adjusted <- disputed_adjust(data, data_setting)
  #
  #   # git1063 add extreme final category
  #   chart_palette[[length(chart_palette)]][[1]] <- 0.9999999
  #   chart_palette[[(length(chart_palette) + 1)]] <- list(1, "rgb(120, 120, 120)")
  # } else {
  #   data_adjusted <- data
  # }

  data_adjusted <- data
  data_chart <- data_adjusted %>%
    dplyr::select(
      setting, year, subgroup, indicator_name, estimate, ci_lb, ci_ub,
      setting_average, dimension, popshare, flag
    ) %>%
    dplyr::mutate(
      #subgroup =  tolower(sub("^\\s*\\d+\\s*", "", subgroup)),
      subgroup_title = totitle(subgroup),
      popshare = popshare * 100,
      estimate_for_tooltip = estimate
    )

  data_chart <- data_chart %>%
    round_vars(
      c("popshare", "estimate_for_tooltip", "ci_lb", "ci_ub", "setting_average"),
      decimal_places
    )

  # Miserable hack to deal with disputed subregion in India. I'm setting the
  # name of the region to "JUNK" so there is no match
  if (data_setting == "India") {
    data_chart[grepl("(?=.*jammu)(?=.*kashmir)", data_chart$subgroup, perl = TRUE),
               c("estimate", "ci_lb", "ci_ub",  "setting_average", "popshare")] <- NA
  }

  if (!(data_iso3 %in% heatdata::spatial_national$iso3) ||
      !(data_iso3 %in% heatdata::spatial_subnational_simplified$iso3)) {
    return(NULL)
  }

  geo_subnational <- geography_subnational(data_iso3, data_year)
  geo_national <- geography_national(data_iso3)

  if (is.null(geo_subnational) || is.null(geo_national)) {
    return(NULL)
  }

  chart <- highchart(type = "map") %>%
    hc_chart(
      reflow= TRUE,
      width = NULL,
      height = 650
    ) %>%
    hc_add_series_map(
      map = geo_subnational,
      df = data_chart,
      joinBy = "subgroup",
      value = "estimate",
      nullInteraction = TRUE, #ifelse(data_setting!="India", TRUE, FALSE),
      nullColor = ifelse(data_setting!="India", "#f7f7f7", "rgb(120, 120, 120)")
    ) %>%
    hc_add_series(
      mapData = geo_national,
      type = "mapline",
      nullColor = "rgb(100,100,100)",
      lineWidth = 2
    ) %>%
    hc_colorAxis(
      labels = list(
        useHTML = TRUE
      ),
      stops = chart_palette,
      max = if(flex_indicator) NULL else 100
    ) %>%
    chart_tooltip(
      heading = "this.point.setting + ', ' + 'DHS' + ' ' + this.point.year",
      body = standard_tooltip_body(from_map = TRUE, language = language),
      decimal_places = decimal_places
    )

  chart <- chart_map_legend(chart, data_setting, language)

  if (disputed_iso3(data_iso3)) {
    chart <- hc_add_series(
      chart,
      mapData = geography_disputed(data_iso3),
      type = "mapline",
      nullColor = "rgb(50,50,50)",
      dashStyle = "Dash",
      lineWidth = 2
    )
  }

  chart_table(
    charts = list(list(chart)),
    title_top = data_indicator,
    title_right = NULL,
    title_main = title_main,
    title_horizontal = NULL,
    title_vertical = NULL,
    legend = NULL,
    map_disclaimer = TRUE,
    language = language,
    is_who_dataset = is_who_dataset
  )
}

chart_map_legend <- function(chart, setting, language) {
  # @hardcoded
  if (setting == "India") {
    chart <- hc_add_series(
      chart,
      data = NULL,
      name = "Not applicable",
      showInLegend = TRUE,
      type = "scatter",
      marker = list(
        symbol = "square",
        fillColor = "rgb(120, 120, 120)",
        lineColor = "rgb(100, 100, 100)",
        lineWidth = 2,
        radius = 6
      )
    )
  }

  chart %>%
    hc_add_series(
      data = NULL,
      name = translate(c(language, "charts", "legend", "notavailable" )), # Not available"
      showInLegend = TRUE,
      type = "scatter",
      marker = list(
        symbol = "square",
        fillColor = "#f7f7f7",
        lineColor = "rgb(100, 100, 100)",
        lineWidth = 1,
        radius = 6
      )
    ) %>%
    hc_add_series(
      data = NULL,
      name = translate(c(language, "charts", "legend", "subnational" )),#"Subnational boundaries (DHS)",
      showInLegend = TRUE,
      type = "line",
      color = "#cccccc"
      #marker = list()
    ) %>%
    hc_add_series(
      data = NULL,
      name = translate(c(language, "charts", "legend", "national" )),
      showInLegend = TRUE,
      type = "line",
      color = "rgb(100,100,100)"
      #marker = list()
    ) %>%
    hc_plotOptions(

      scatter = list(
        events = list(legendItemClick = JS("function() { return false; }"))
      ),
      line = list(
        events = list(legendItemClick = JS("function() { return false; }"))
      )
    ) %>%
    hc_xAxis(visible = FALSE) |>
    hc_legend(
      useHTML = TRUE,
      enabled = TRUE,
      itemDistance = 18,
      itemMarginBottom = 2,
      lineHeight = 24,
      reversed = TRUE
    )
}
