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

licenseModal <- function(ns = NULL) {

  buttonid <- "thismodal"
  if(!is.null(ns))
    buttonid <- ns(buttonid)
  m <- modal(
    id = NULL,
    size = "lg",
    header = "Terms of use and software license agreement",
    htmltools::includeHTML(
      system.file("www/locales/en/license-popup.html", package = get_heat_prefix())
    ),
    footer = div(
      buttonInput(
        id = buttonid,
        label = "Close",
        `data-dismiss` = "modal"
      )
    )
  )

  m$children[[1]] <- htmltools::tagAppendAttributes(
    m$children[[1]],
    class = "modal-dialog-scrollable" # h-1/2"
  )

  m <- htmltools::tagAppendAttributes(
    m,
    `data-backdrop` = "static"
  )

  m
}

languageSelect <- function() {
  possible_langs <- list.dirs(
    system.file("www/locales", package = "heat"),
    full.names = FALSE
  )

  possible_langs <- possible_langs[possible_langs != ""]

  tags$select(
    class = "custom-select d-inline-block w-auto",
    id = "lang",
    autocomplete = "off",
    lapply(possible_langs, function(lang) {
      tags$option(value = lang, toupper(lang))
    })
    # tags$option(value = "en", "EN"),
    # tags$option(value = "es", "ES")
  )
}

get_nonwho_disclaimer <- function(){
  "Please note that these data are from external published sources and are therefore not WHO official estimates. Please refer to the Indicator Metadata in the About menu for more information."
}
