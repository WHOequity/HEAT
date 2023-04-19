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

default_setting <- function(values) {
  values[1]
}

default_source <- function(values, max) {
  head(values, max)
}

default_year <- function(values, max) {
  if (max == 1) {
    val <- suppressWarnings(max(values))
    if(is.infinite(val))
      val <- NULL
  } else {
    val <- sort(head(sort(values, decreasing = TRUE), max))
  }
  val
}

default_indicator <- function(values, max) {
  head(values, max)
}

default_dimension <- function(values, max) {
  head(values, max)
}

default_measure <- function(values, max) {
  head(values, max)
}

default_benchmark_year <- function(values){
  list(
    beg = 2001,
    end = 2003
  )
}

default_indicator_dimension <- function(strata,
                                        current_indicator, current_dimension,
                                        new_country, new_source, new_year) {


  # All indicators and all dimensions in country
  all_indicators_dimensions <- strata %>%
    dplyr::filter(
      setting == !!new_country,
      year %in% !!new_year,
      source %in% !!new_source
    ) %>%
    dplyr::distinct(indicator_abbr, indicator_name, dimension) # %>%
    # dplyr::arrange(indicator_name, dimension)

  # All unique indicators in country
  all_indicators <- all_indicators_dimensions %>%
    dplyr::distinct(indicator_abbr, indicator_name)

  # All unique dimensions in country
  all_dimensions <- all_indicators_dimensions %>%
    dplyr::distinct(dimension) # %>%
    # dplyr::arrange(dimension)

  sorted_indicators <- all_indicators %>%
    dplyr::arrange(indicator_name)

  sorted_dimensions <- all_dimensions %>%
    dplyr::arrange(dimension)

  indicator_dimension_dtls <- list(
    indicator_values = sorted_indicators$indicator_abbr,
    indicator_choices = sorted_indicators$indicator_name,
    indicator_selected = all_indicators_dimensions$indicator_abbr[1],
    dimension = sorted_dimensions$dimension,
    dimension_selected = all_indicators_dimensions$dimension[1]
  )

  # Test if any current indicators exist in country
  indicator_match <- all_indicators_dimensions %>%
    dplyr::filter(indicator_abbr %in% !!current_indicator)

  # New setting does have at least one current indicators
  if (NROW(indicator_match) > 0) {

    possible_dimensions <- indicator_match %>%
      dplyr::filter(dimension %in% !!current_dimension) %>%
      dplyr::distinct(dimension) %>%
      dplyr::pull(dimension)

    matched_dimensions <- current_dimension[current_dimension %in% possible_dimensions]

    # Among records with matching indicators is there
    # also a matching dimension?
    possible_ind_abbrs <- indicator_match %>%
      dplyr::distinct(indicator_abbr, indicator_name) %>%
      dplyr::pull(indicator_abbr)

    matched_ind_current <- current_indicator[current_indicator %in% possible_ind_abbrs]

    indicator_dimension_dtls[["indicator_selected"]] <- matched_ind_current

    if (length(matched_dimensions) > 0) {
      # New setting has both an indicator and a dimension
      # return all the indicators and just the dimensions
      # that exist with the indicators

      indicator_dimension_dtls[["dimension_selected"]] <- matched_dimensions
    } else {
      # New setting has an indicator but not a dimension so
      # return all the indicators and the first dimension
      # associated with one of these indicators

      dimension_new <- indicator_match %>%
        dplyr::distinct(dimension) %>%
        dplyr::arrange(dimension) %>%
        dplyr::pull(dimension)

      indicator_dimension_dtls[["dimension_selected"]] <- dimension_new[1]
    }
  }


  indicator_dimension_dtls
}

pick_measures <- function(possible_measures, dimension, language) {
  possible_measures %>%
    dplyr::distinct(dimension, measure) %>%
    dplyr::filter(dimension %in% !!dimension) %>%
    dplyr::distinct(values = measure) %>%
    dplyr::mutate(choices = measure_name(values, language)) %>%
    dplyr::arrange(choices)
}
