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

table_compare_disaggregated <- function(.data, columns, decimal_places,
                                        focus_setting, data_only = FALSE,
                                        rename = TRUE, language = "en") {


  columns_order <- c(
    Setting = "setting",
    Year = "year",
    Source = "source",
    `Indicator abbreviation` = "indicator_abbr",
    `Indicator name` = "indicator_name",
    Dimension = "dimension",
    Subgroup = "subgroup",
    Estimate = "estimate",
    `95% CI lower bound` = "ci_lb",
    `95% CI upper bound` = "ci_ub",
    `Population share` = "popshare",
    Flag = "flag",
    `Setting average` = "setting_average"
  )

  columns_keys <- c(
    setting = "setting",
    year = "year",
    source = "source",
    indicatorabbr = "indicator_abbr",
    indicatorname = "indicator_name",
    dimension = "dimension",
    subgroup = "subgroup",
    estimate = "estimate",
    lowerbound = "ci_lb",
    upperbound = "ci_ub",
    popshare = "popshare",
    flag = "flag",
    settingavg = "setting_average"
  )

  columns <- columns_keys[columns]
  columns <- names(columns_order)[columns_order%in%columns]

  .data <- .data %>%
    dplyr::arrange(
      dplyr::desc(setting == !!focus_setting),
      setting,
      dplyr::desc(year),
      source,
      indicator_name,
      dimension,
      subgroup_order,
      subgroup
    )

  names(columns_keys) <- vapply(names(columns_keys), function(nm) {
    translate(c(language, "options", "values", nm))
  }, character(1), USE.NAMES = FALSE)

  if (!data_only) {
    columns_selection <- columns_order[names(columns_order) %in% columns]
  } else {
    columns_selection <- c(
      "setting", "year", "source", "indicator_abbr", "indicator_name",
      "dimension", "subgroup", "estimate", "se", "ci_lb", "ci_ub", "population",
      "flag", "setting_average", "iso3", "favourable_indicator",
      "indicator_scale", "ordered_dimension", "subgroup_order",
      "reference_subgroup"
    )
  }

  names(columns_selection) <- vapply(columns_selection, function(x) {
    nm <- names(columns_keys[columns_keys == x])
    if(length(nm) == 0) nm <- x
    return(nm)
  }, character(1), USE.NAMES = FALSE)

  if (data_only && !rename) {
    names(columns_selection) <- NULL
  }

  data_formatted <- .data %>%
    dplyr::mutate(popshare = popshare * 100) %>%
    dplyr::mutate_if(~ is.numeric(.), ~ round_for_tables(., !!decimal_places)) %>%
    #dplyr::mutate(year = round(as.numeric(year))) %>%
    dplyr::select(!!!columns_selection)

  if (isTRUE(data_only)) {
    return(data_formatted)
  }

  DT::datatable(
    data = data_formatted,
    rownames = FALSE,
    container = tags$table(
      class = "table table-striped table-hover table-header-vertalign",
      DT::tableHeader(data_formatted)
    ),
    options = list(
      columnDefs = table_column_alignment(data_formatted),
      pageLength = 100,
      # autoWidth = TRUE,
      language = list(
        search = translate(c(language, "tables", "labels", "search")),
        paginate = list(previous = translate(
          c(language, "tables", "labels", "previous")
        ),
        `next` = translate(c(
          language, "tables", "labels", "next"
        )))
      ),
      dom = "frtp",
      scrollX = TRUE,
      scrollY = "600px",
      processing = FALSE,
      scrollCollapse = TRUE
    )
  )
}
