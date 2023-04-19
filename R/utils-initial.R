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

initial_setting <- function(x, lang, dataname) {
  if (!getOption("heat.plus", FALSE)) {

    default_setting <- heatdata::info_databases |>
      dplyr::filter(internal_name %in% dataname) |>
      dplyr::pull(setting) |>
      unique()

    transl <- translate(c(lang, "values", "settings", default_setting))
    ifelse(is.null(transl), x[1], transl)

  } else {
    x[1]
  }
}

initial_source <- function(x) {
  x
}

initial_year <- function(x) {
  sort(x)
}

initial_indicator <- function(x, lang, dataname) {
  if (!getOption("heat.plus", FALSE)) {
    indic <- heatdata::info_databases |>
      dplyr::filter(internal_name %in% dataname) |>
      dplyr::pull(indicator_abbr) |>
      unique()

    ifelse(is.null(indic), x[1], indic)

  } else {
    x[1]
  }
}

initial_dimension <- function(x, lang, dataname) {
  if (!getOption("heat.plus", FALSE)) {

    default_dimension <- heatdata::info_databases |>
      dplyr::filter(internal_name %in% dataname) |>
      dplyr::pull(dimension) |>
      unique()

    dimension <- translate(c(lang, "values", "dimensions", default_dimension))
    ifelse(is.null(dimension), x[1], dimension)

  } else {
    x[1]
  }
}

initial_measure <- function(x) {
  "D"
}
