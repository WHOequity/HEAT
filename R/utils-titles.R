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

title_explore_disaggregated <- function(data, language = "en") {
  # data %>%
  #   dplyr::distinct(setting, source, year) %>%
  #   dplyr::group_by(setting, source) %>%
  #   dplyr::summarise(year = glue::glue_collapse(year, sep = ", ")) %>%
  #   dplyr::mutate(srcyr = paste(source, year)) %>%
  #   dplyr::summarise(
  #     srcyr = glue::glue_collapse(srcyr, sep = ", ", last = ", and ")
  #   ) %>%
  #   glue::glue_data("{ setting } ({ srcyr })") %>%
  #   as.character()

  unique(data$setting)
}

title_explore_detail <- function(data, language = "en") {

  data %>%
    dplyr::distinct(setting, source, year) %>%
    dplyr::group_by(setting, source) %>%
    dplyr::summarise(year = glue::glue_collapse(year, sep = ", ")) %>%
    dplyr::mutate(srcyr = paste(source, year)) %>%
    dplyr::summarise(
      srcyr = glue::glue_collapse(srcyr, sep = ", ", last = ", and ")
    ) %>%
    glue::glue_data("{ setting }") %>%
    as.character()
}

title_explore_map <- function(data, language = "en") {
  data %>%
    dplyr::distinct(setting, source, year) %>%
    dplyr::group_by(setting, source) %>%
    dplyr::summarise(year = glue::glue_collapse(year, sep = ", ")) %>%
    dplyr::mutate(srcyr = paste(source, year)) %>%
    dplyr::summarise(
      srcyr = glue::glue_collapse(srcyr, sep = ", ", last = ", and ")
    ) %>%
    glue::glue_data("{ setting } ({ srcyr })") %>%
    as.character()
}

title_explore_summary <- function(data, language = "en") {
  # data %>%
  #   dplyr::distinct(setting, source, year) %>%
  #   dplyr::group_by(setting, source) %>%
  #   dplyr::summarise(year = glue::glue_collapse(year, sep = ", ")) %>%
  #   dplyr::mutate(srcyr = paste(source, year)) %>%
  #   dplyr::summarise(
  #     srcyr = glue::glue_collapse(srcyr, sep = ", ", last = ", and ")
  #   ) %>%
  #   glue::glue_data("{ setting } ({ srcyr })") %>%
  #   as.character()

  unique(data$setting)
}

title_compare_disaggregated <- function(data, language = "en") {

  n_settings <- data %>%
    dplyr::group_by(setting) %>%
    dplyr::n_groups()

  tmp_title <- translate(c(language, "charts", "titles", "comparedisag"))
  tmp_title <- gsub(pattern = "\\[indicator_name\\]", data$indicator_name[1], tmp_title)
  tmp_title <- gsub(pattern = "\\[dimension\\]", tolower(data$dimension[1]), tmp_title)
  tmp_title <- gsub(pattern = "\\[number_settings\\]", n_settings, tmp_title)

  as.character(tmp_title)
}

title_compare_summary <- function(data, language = "en") {
  n_settings <- data %>%
    dplyr::group_by(setting) %>%
    dplyr::n_groups()

  tmp_title <- translate(c(language, "charts", "titles", "comparesumm"))
  tmp_title <- gsub(pattern = "\\[indicator_name\\]", data$indicator_name[1], tmp_title)
  tmp_title <- gsub(pattern = "\\[dimension\\]", tolower(data$dimension[1]), tmp_title)
  tmp_title <- gsub(pattern = "\\[number_settings\\]", n_settings, tmp_title)

  as.character(tmp_title)
}
