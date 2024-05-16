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

table_determinant <- function(.data, columns, decimal_places, focus_setting,
                                  data_only = FALSE, rename = TRUE,
                                  language = "en") {


  columns_order <- c(
    Setting = "setting",
    Year = "year",
    Source = "source",
    `Indicator abbreviation` = "indicator_abbr",
    `Indicator name` = "indicator_name",
    `Setting average` = "setting_average",
    `Determinant date` = "sdh_year",
    `Determinant source` = "sdh_source",
    `Determinant abbreviation` = "sdh_abbr",
    `Determinant name` = "sdh_name",
    `Determinant setting average` = "sdh_estimate",
    `Determinant note` = "sdh_flag"

  )

  columns_keys <- c(
    setting = "setting",
    year = "year",
    source = "source",
    indicatorabbr = "indicator_abbr",
    indicatorname = "indicator_name",
    settingavg = "setting_average",
    sdhyear = "sdh_year",
    sdhsource = "sdh_source",
    sdhabbr = "sdh_abbr",
    sdhname = "sdh_name",
    sdhestimate = "sdh_estimate",
    sdhnote = "sdh_flag"
  )


  columns <- columns_keys[columns]
  columns <- names(columns_order)[columns_order%in%columns]

  .data <- .data %>%
    dplyr::arrange(
      dplyr::desc(setting == !!focus_setting),
      setting,
      dplyr::desc(year),
      source,
      indicator_name
    )



  names(columns_keys) <- vapply(names(columns_keys), function(nm) {
    val <- translate(c(language, "options", "values", nm))
    if(is.null(val))
      val <- nm
    val
  }, character(1), USE.NAMES = FALSE)

  if (!data_only) {
    columns_selection <- columns_order[names(columns_order) %in% columns]
  } else {
    columns_selection <- c(
      "setting", "year", "source", "indicator_abbr", "indicator_name", "setting_average", "iso3",
      "sdh_year", "sdh_source", "sdh_abbr", "sdh_name", "sdh_estimate", "sdh_flag"
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

  data_selection <- .data %>%
    dplyr::mutate(sdh_year = as.character(sdh_year)) %>%
    dplyr::mutate_if(~ is.numeric(.), ~ round_for_tables(., !!decimal_places)) %>%
    dplyr::select(!!!columns_selection)

  if (isTRUE(data_only)) {
    return(data_selection)
  }

  DT::datatable(
    data = data_selection,
    rownames = FALSE,
    container = tags$table(
      class = "table table-striped table-hover table-header-vertalign",
      DT::tableHeader(data_selection)
    ),
    options = list(
      columnDefs = table_column_alignment(data_selection),
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
