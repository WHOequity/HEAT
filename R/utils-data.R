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

appendColors <- function(.data, dimension_colors) {

  dimension_colors <- dimension_colors %>%
    dplyr::select(-dimension, -subgroup)

  one_col <- dimension_colors %>%
    dplyr::filter(subgroup_id == "ALL_SUBGROUPS")

  multi_col <- dimension_colors %>%
    dplyr::filter(subgroup_id != "ALL_SUBGROUPS")


  .data <-  dplyr::left_join(.data, multi_col, by = c("dimension_id", "subgroup_id"))

  for(i in 1:nrow(one_col)){
    dimen_id <- one_col$dimension_id[i]
    onecol <- one_col$color[i]
    oneopacity <- one_col$opacity[i]
    .data$color[.data$dimension_id == dimen_id] <- onecol
    .data$opacity[.data$dimension_id == dimen_id] <- oneopacity
  }

  .data %>%
    dplyr::mutate(
      opacity = ifelse(is.na(opacity), 1, opacity),
      color = highcharter::hex_to_rgba(color, opacity)
    )
}

table_column_alignment <- function(.data) {

  centered <- c(
    "Year", "Estimate", "Population share",
    "Source", "Indicator abbreviation",
     "95% CI lower bound","95% CI upper bound", "Setting average"
  )

  left <- c(
    "Setting", "Indicator name", "Dimension", "Subgroup",
    "Summary measure name",
    "Source", "Indicator abbreviation", "Summary measure abbreviation",
     "95% CI lower bound","95% CI upper bound", "Flag"
  )


  cols <- names(.data)

  list(
    list(className = "table-text-center", targets = which(cols %in% centered) - 1),
    list(className = "table-text-nocenter", targets = which(cols %in% left) - 1)
  )
}

str_round <- function(x, digits) {
  sprintf(glue::glue("%.{ digits }f"), x)
}

measure_name <- function(measures, language) {
  # nm <- names(
  #   heatmeasures::HEAT_summary_measures_names[
  #     match(measures, heatmeasures::HEAT_summary_measures_names)
  #   ]
  # )
  sapply(tolower(measures), function(x)
    translate(c(language, "values", "indicators", x)), USE.NAMES = FALSE)
}

is_log_scale <- function(measure) {
  isTRUE(measure %in% c("R", "RII"))
}

r_d_html <- function(data, language) {
  lang <- language()
  r_d_data <- r_d_percentiles(data, lang)
  r_d_warning <- p(
    class = "text-muted",
    translate(c(lang, "sidepanel", "text", "calculations" ))#"Difference and ratio measures are calculated for dimensions with 30 subgroups or more. If estimates are not available, then summary measures cannot be calculated"
  ) %>%
    font(size = "xs")

  if (all(lengths(r_d_data$r_d_percents) == 0)) {
    return(HTML(as.character(r_d_warning)))
  }

  r_d_tags <- purrr::pmap(r_d_data, function(indicator_name, r_d_percents) {
    columns(
      class = "flex-row",
      column(
        width = 12,
        tags$h6(
          class = "heat-summaries-indicator",
          indicator_name
        ) %>%
          padding(bottom = 1) %>%
          margin(bottom = 2) %>%
          border(sides = "bottom", color = "black")
      ),
      column(
        width = 9,
        tags$p(class = "heat-summaries-subtitle", translate(c(lang, "sidepanel", "table", "measure" )))#"Summary measure"
      ),
      column(
        width = 3,
        tags$p(class = "heat-summaries-subtitle", translate(c(lang, "sidepanel", "table", "estimate" ))) #"Estimate"
      ),
      lapply(seq_along(r_d_percents), function(i) {
        list(
          column(
            width = 9,
            names(r_d_percents)[i] %>%
              HTML() %>% tags$p() %>% font(size = "sm")
          ),
          column(
            class = "d-flex align-items-center",
            width = 3,
            r_d_percents[i] %>%
              round(2) %>% tags$p() %>% font(size = "sm")
          )
        )
      })
    )
  })

  r_d_body <- paste(purrr::map_chr(r_d_tags, as.character), collapse = "\n")

  HTML(paste0(
    as.character(p(r_d_warning)),
    r_d_body,
    sep = "\n"
  ))
}

r_d_percentiles <- function(data, lang) {
  #dat <- plotData
  n_subgroup <- dplyr::n_distinct(data$subgroup)

  data_grouped <- data %>%
    dplyr::group_by(setting, year, source, indicator_name) %>%
    dplyr::mutate(n_valid = sum(!is.na(estimate))) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  r_d_percents <- purrr::pmap(
    data_grouped,
    function(setting, year, source, indicator_name, data) {
      subset <- data
      ratio30 <- ratio60 <- ratio90 <- NULL


      if (subset$n_valid[1] >= 30) {
        ratio30 <- r_d_diff_ratio(c(0.2, 0.8), subset)
        names(ratio30) <- c(
          translate(c(lang, "sidepanel", "difference", "percentile8020")),#"Difference (percentile&nbsp;80&nbsp;-&nbsp;percentile&nbsp;20)"),
          translate(c(lang, "sidepanel", "ratio", "ratio8020")),#"Ratio (percentile&nbsp;80&nbsp;/&nbsp;percentile&nbsp;20)",
          translate(c(lang, "sidepanel", "difference", "quintile51")),#"Difference (mean&nbsp;quintile&nbsp;5&nbsp;-&nbsp;mean&nbsp;quintile&nbsp;1)",
          translate(c(lang, "sidepanel", "ratio", "quintile51"))#"Ratio (mean&nbsp;quintile&nbsp;5&nbsp;/&nbsp;mean&nbsp;quintile&nbsp;1)"
        )
      }

      if (subset$n_valid[1] >= 60) {
        ratio60 <- r_d_diff_ratio(c(0.1, 0.9), subset)
        names(ratio60) <- c(
          translate(c(lang, "sidepanel", "difference", "percentile9010")),#"Difference (percentile&nbsp;90&nbsp;-&nbsp;percentile&nbsp;10)",
          translate(c(lang, "sidepanel", "ratio", "ratio9010")),#"Ratio (percentile&nbsp;90&nbsp;/&nbsp;percentile&nbsp;10)",
          translate(c(lang, "sidepanel", "difference", "decile9010")),#"Difference (mean&nbsp;decile&nbsp;90&nbsp;-&nbsp;mean&nbsp;decile&nbsp;10)",
          translate(c(lang, "sidepanel", "ratio", "decile9010"))#"Ratio (mean&nbsp;decile&nbsp;90&nbsp;/&nbsp;mean&nbsp;decile&nbsp;10)"
        )
      }

      if (subset$n_valid[1] >= 100) {
        ratio90 <- r_d_diff_ratio(c(0.05, 0.95), subset)
        names(ratio90) <- c(
          translate(c(lang, "sidepanel", "difference", "percentile955")), #"Difference (percentile&nbsp;95&nbsp;-&nbsp;percentile&nbsp;5)",
          translate(c(lang, "sidepanel", "ratio", "ratio955")),#"Ratio (percentile&nbsp;95&nbsp;/&nbsp;percentile&nbsp;5)",
          translate(c(lang, "sidepanel", "difference", "topbottom")),#"Difference (mean&nbsp;top&nbsp;5%&nbsp;-&nbsp;mean&nbsp;bottom&nbsp;5%)",
          translate(c(lang, "sidepanel", "ratio", "topbottom"))#"Ratio (mean&nbsp;top&nbsp;5%&nbsp;/&nbsp;mean&nbsp;bottom&nbsp;5%)"
        )
      }

      ratios <- c(ratio30, ratio60, ratio90)

      # names(vals) <- vapply(
      #   names(vals),
      #   translate,
      #   character(1),
      #   isolate(getDefaultReactiveDomain()$input$lang)
      # )

      ratios
    }
  )

  data_grouped %>%
    dplyr::mutate(r_d_percents = r_d_percents) %>%
    dplyr::select(indicator_name, r_d_percents)
}

r_d_diff_ratio <- function(probs, data, roundval = 2) {
  rd_percentiles <- quantile(data$estimate, probs = probs, na.rm = TRUE)
  d <- unname(rd_percentiles[2] - rd_percentiles[1])
  r <- unname(rd_percentiles[2] / rd_percentiles[1])

  highval <- data %>%
    dplyr::filter(estimate > rd_percentiles[2]) %>%
    dplyr::pull(estimate) %>%
    mean(na.rm = TRUE)

  lowval <- data %>%
    dplyr::filter(estimate <= rd_percentiles[1]) %>%
    dplyr::pull(estimate) %>%
    mean(na.rm = TRUE)

  dM <- highval - lowval
  rM <- highval / lowval

  c(
    d_percentile = round(d, roundval),
    r_percentile = round(r, roundval),
    d_mean_percentile = round(dM, roundval),
    r_mean_percentile = round(rM, roundval)
  )
}

# comparison tools ----
benchmark_income_groups <- function(country_info, language) {


  found_groups <- country_info %>%
    dplyr::filter(!is.na(wbincome_name), !is.na(wbincome)) %>%
    dplyr::distinct(wbincome_name, wbincome)

  found_groups_en <- found_groups %>%
    dplyr::pull(wbincome)

  found_groups_lang <- found_groups %>%
    dplyr::pull(wbincome_name)


  possible_groups <- c(
    "Low income",
    "Lower middle income",
    "Upper middle income",
    "High income",
    "No income group defined"
  )

  if (!any(possible_groups %in% found_groups_en)) {
    return(NULL)
  }

  # possible_groups[possible_groups %in% found_groups]
  found_groups_lang[na.exclude(match(possible_groups, found_groups_en))]
}

benchmark_regions <- function(country_info) {

  country_info %>%
    dplyr::distinct(whoreg6_name, whoreg6) %>%
    dplyr::select(choices = whoreg6_name, values = whoreg6)
}

benchmark_comparisons <- function(setting_yr_src, country_info, income_group, region, sources) {

  if(!is.null(sources) && !any(sources == " ")){
    setting_yr_src <- setting_yr_src |>
      dplyr::filter(source %in% sources)
  }

  settings <- setting_yr_src |>
    dplyr::pull(setting) |>
    unique()

  country_info <- country_info |>
    dplyr::filter(
      setting %in% settings
    )


  filter_incomes <- country_info %>%
    dplyr::filter(
      wbincome_name %in% !!income_group
    )


  if (is.null(region)) {
    filter_regions <- filter_incomes
  } else {
    filter_regions <- filter_incomes %>%
      dplyr::filter(
        whoreg6 %in% !!region
      )
  }

  filter_regions %>%
    dplyr::select(setting, iso3) %>%
    dplyr::mutate(
      choices = setting,
      values = ifelse(is.na(iso3) | is.null(iso3), setting, iso3)
    ) %>%
    dplyr::distinct(choices, values)
}

get_benchmark_years <- function(setting_yr_src, setting, comparisons){

  filter_incomes <- setting_yr_src %>%
    dplyr::filter(
      setting %in% c(setting, comparisons)
    ) %>%
    dplyr::select(year) %>%
    dplyr::distinct(year) %>%
    dplyr::pull() %>%
    sort()

}

get_median_decimals <- function(val){
  if(!isTruthy(val)) return(10)
  if(val >= 1) return(1)
  if(val >= 0.5 & val < 1) return(2)
  if(val < 0.5) return (3)
  10
}

