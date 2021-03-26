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

appendColors <- function(.data, language) {

  # age ----
  dim_age <- translate(c(language, "values", "dimensions", "Age"))
  # age_0_2 <- translate(c(language, "values", "subgroups", "0-2 years"))
  # age_2_5 <-
  # age_0_2 <- "0-2 years"
  # age_2_5 <- "2-5 years"
  age_0_1 <- translate(c(language, "values", "subgroups", "0-1 years"))
  age_2_5 <- translate(c(language, "values", "subgroups", "2-5 years"))
  age_15_19 <- translate(c(language, "values", "subgroups", "15-19 years"))
  age_20_49 <- translate(c(language, "values", "subgroups", "20-49 years"))

  # economic status (wealth quintile) ----
  dim_econ_quin <- translate(c(language, "values", "dimensions", "Economic status (wealth quintile)"))
  econ_quin_1 <- translate(c(language, "values", "subgroups", "Quintile 1 (poorest)"))
  econ_quin_2 <- translate(c(language, "values", "subgroups", "Quintile 2"))
  econ_quin_3 <- translate(c(language, "values", "subgroups", "Quintile 3"))
  econ_quin_4 <- translate(c(language, "values", "subgroups", "Quintile 4"))
  econ_quin_5 <- translate(c(language, "values", "subgroups", "Quintile 5 (richest)"))

  # education ----
  dim_edu <- translate(c(language, "values", "dimensions", "Education"))
  edu_none <- translate(c(language, "values", "subgroups", "No education"))
  edu_primary <- translate(c(language, "values", "subgroups", "Primary school"))
  edu_secondary  <- translate(c(language, "values", "subgroups", "Secondary school +"))

  # place of residence ----
  dim_reside <- translate(c(language, "values", "dimensions", "Place of residence"))
  reside_urban <- translate(c(language, "values", "subgroups", "Urban"))
  reside_rural <- translate(c(language, "values", "subgroups", "Rural"))

  # sex ----
  dim_sex <- translate(c(language, "values", "dimensions", "Sex"))
  sex_female <- translate(c(language, "values", "subgroups", "Female"))
  sex_male <- translate(c(language, "values", "subgroups", "Male"))

  # subnational region ----
  dim_subnat <- translate(c(language, "values", "dimensions", "Subnational region"))

  # economic status (wealth decile) ----
  dim_econ_dec <- translate(c(language, "values", "dimensions", "Economic status (wealth decile)"))
  econ_dec_1 <- translate(c(language, "values", "subgroups", "Decile 1 (poorest)"))
  econ_dec_2 <- translate(c(language, "values", "subgroups", "Decile 2"))
  econ_dec_3 <- translate(c(language, "values", "subgroups", "Decile 3"))
  econ_dec_4 <- translate(c(language, "values", "subgroups", "Decile 4"))
  econ_dec_5 <- translate(c(language, "values", "subgroups", "Decile 5"))
  econ_dec_6 <- translate(c(language, "values", "subgroups", "Decile 6"))
  econ_dec_7 <- translate(c(language, "values", "subgroups", "Decile 7"))
  econ_dec_8 <- translate(c(language, "values", "subgroups", "Decile 8"))
  econ_dec_9 <- translate(c(language, "values", "subgroups", "Decile 9"))
  econ_dec_10 <- translate(c(language, "values", "subgroups", "Decile 10 (richest)"))

  dplyr::mutate(
    .data,
    color = dplyr::case_when(
      dimension == !!dim_age & subgroup == !!age_0_1 ~ "#e6c1f0",
      dimension == !!dim_age & subgroup == !!age_2_5 ~ "#a874a8",
      dimension == !!dim_age & subgroup == !!age_15_19 ~ "#c6c1f0",
      dimension == !!dim_age & subgroup ==!!age_20_49 ~ "#8074a8",
      dimension == !!dim_econ_quin & subgroup == !!econ_quin_1 ~ "#8ed07f",
      dimension == !!dim_econ_quin & subgroup == !!econ_quin_2 ~ "#6eb663",
      dimension == !!dim_econ_quin & subgroup == !!econ_quin_3 ~ "#519c51",
      dimension == !!dim_econ_quin & subgroup == !!econ_quin_4 ~ "#308344",
      dimension == !!dim_econ_quin & subgroup == !!econ_quin_5 ~ "#24693d",
      dimension == !!dim_edu & subgroup == !!edu_none ~ "#fa8f79",
      dimension == !!dim_edu & subgroup == !!edu_primary ~ "#e74545",
      dimension == !!dim_edu & subgroup == !!edu_secondary ~ "#ae123a",
      dimension == !!dim_reside & subgroup == !!reside_urban ~ "#98d9e4",
      dimension == !!dim_reside & subgroup == !!reside_rural ~ "#3ca8bc",
      dimension == !!dim_sex & subgroup == !!sex_female ~ "#fcc66d",
      dimension == !!dim_sex & subgroup == !!sex_male ~ "#ef8a0c",
      dimension == !!dim_subnat ~ "#2a5783",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_1 ~ "#b9e1af",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_2 ~ "#a3d896",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_3 ~ "#8ed07f",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_4 ~ "#77c664",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_5 ~ "#65b058",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_6 ~ "#3ca856",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_7 ~ "#36964d",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_8 ~ "#308344",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_9 ~ "#24693d",
      dimension == !!dim_econ_dec & subgroup == !!econ_dec_10 ~ "#154b2b",
      TRUE ~ "#007300"
    )
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

benchmark_comparisons <- function(country_info, income_group, region) {
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
