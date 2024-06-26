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

homeUI <- function(id) {
  ns <- NS(id)

  shiny::includeHTML(
    system.file("landing-page", "heat_landing.html", package = "heat")
  )
}

homeServer <- function(input, output, session) {

  lst <- list(
    open_explore = reactive({
      input$open_explore
    }),
    open_compare = reactive({
      input$open_compare
    })
  )

  if(!is_heat_plus()){
    lst$open_determinant <-  reactive({
      input$open_determinant
    })
  }

  lst

}
