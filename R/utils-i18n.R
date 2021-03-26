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

#' @export
i18n <- function(key = NULL, ns = NULL, plural = NULL, target = NULL,
                 assign = NULL) {
  rlang::splice(
    list(
      `data-i18n` = key,
      `data-i18n-ns` = ns,
      `data-i18n-plural` = if (isTRUE(plural) || (!is.null(plural) && plural != 1)) "true",
      `data-i18n-target` = target,
      `data-i18n-assign` = assign
    )
  )
}

i18n_languages <- function() {
  locales <- fs::path_package("heat", "www/locales")
  langs <- fs::path_file(fs::dir_ls(locales, type = "directory"))

  langs <- as.list(langs)
  names(langs) <- langs

  lapply(langs, function(l) {
    jsonlite::read_json(fs::path(locales, l, "default.json"))
  })
}

.LANGS <- i18n_languages()

keys_error_msg <- function(keys) {
  paste0("could not translate c(", paste0('"', keys, '"', collapse = ", "), ")")
}

#' @export
translate <- function(keys) {
  tryCatch(.LANGS[[keys]], error = function(e) {
    warning(keys_error_msg(keys))
  })
}
