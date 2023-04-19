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

round_for_tables <- function(x, decimal_places) {

  if(is.infinite(decimal_places)){
    decimal_places <- max(nchar(x), na.rm = TRUE)
    vals <- format(round(x, decimal_places), nsmall = decimal_places)
  } else {
    vals <- format(round(x, decimal_places), nsmall = decimal_places)
    vals[trimws(vals) == "NA"] <- ""
  }

  vals
}


table_disclaimer <- function(language = "en", is_who_dataset) {
  if (!is_heat_plus()) {

    nonwho_disclaimer <- ""

    if(!is_who_dataset()){
      nonwho_disclaimer <-  c(
        paste('"', get_nonwho_disclaimer(), '"'),
        "\n"
      )
    }

    c(
      paste('"', translate(c(language, "downloads", "text", "heat1")), '"'),
      "\n",
      paste('"', translate(c(language, "downloads", "text", "heat2")), '"'),
      "\n",
      nonwho_disclaimer
    )
  } else {
    c(
      paste('"', translate(c(language, "downloads", "text", "heatplus1")), '"'),
      "\n",
      paste('"', translate(c(language, "downloads", "text", "heatplus2")), '"'),
      "\n"
    )
  }
}


