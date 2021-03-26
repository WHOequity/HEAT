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
assets <- function() {
  list(
    tags$script(src = "heat-assets/jspdf/promise-polyfill.js", type = "text/javascript"),
    tags$script(src = "heat-assets/html2canvas/html2canvas.min.js", type = "text/javascript"),
    tags$script(src = "heat-assets/jspdf/jspdf.min.js", type = "text/javascript"),
    tags$script(src = "heat-assets/js/i18next.min.js", type = "text/javascript"),
    tags$script(src = "heat-assets/js/i18nextXHRBackend.min.js", type = "text/javascript"),
    tags$script(src = "heat-assets/js/loc-i18next.min.js", type = "text/javascript"),
    tags$link(rel = "stylesheet", href = "heat-assets/css/landing.css"),
    tags$link(rel = "stylesheet", href = "heat-assets/css/main.css"),
    tags$script(src = "heat-assets/js/main.js", type = "text/javascript")
  )
}

#' @export
locales <- function() {
  base <- fs::path_package("heat")
  dirs <- fs::dir_ls(fs::path_package("heat", "www/locales"), type = "directory")

  paths <- fs::path("heat-assets", fs::path_rel(dirs, base), "default.json")

  lapply(paths, function(src) {
    tags$script(src = src, type = "text/json")
  })
}
