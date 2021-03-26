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

#' Launch the HEAT application
#'
#' Use `run()` to launch the application.
#'
#' @param launch_browser One of `TRUE` or `FALSE` specifying if the application
#'   launches in the browser or rstudio viewer, defaults to `TRUE`.
#'
#' @export
app <- function(launch_browser = TRUE) {
  invisible(shinyApp(
    ui = appTemplate("app"),
    server = function(input, output, session) {
      callModule(
        module = appServer,
        id = "app"
      )
    },
    options = list(
      launch.browser = launch_browser
    )
  ))
}

#' @rdname app
#' @export
run <- function() {
  (app())
}
