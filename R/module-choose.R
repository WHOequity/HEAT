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


dataManagementModal <- function(id, current_data, language = NULL, add_cancel = TRUE) {
  ns <- NS(id)

  lang <- language


  radio_list <- heatdata::info_databases %>%
    dplyr::arrange(category_order, dataset_order) |>
    dplyr::select(category, dataset_name, internal_name, blurb, internal_category_name)

  cats <- unique(radio_list$category)
  internal_cats <- unique(radio_list$internal_category_name)
  radio_list <- split(radio_list, radio_list$category)
  radio_list <- radio_list[cats]


  f <- function(radio_list, current_data){

    radio_ui <- purrr::map(1:length(radio_list), function(i) {
      nm <- names(radio_list)[i]
      dat <- radio_list[[i]]

      category <- make_input_name(dat$category) |>
        unique()

      internal_cat_name <- dat$internal_category_name |>
        unique()
      accordion(ns, nm, category, internal_cat_name, dat$dataset_name, dat$internal_name, current_data, dat$blurb)
    })

    item <- list(
      div(
        class = 'selected-dataset',
        h5(i18n("module.text.selected_dataset")), #"Currently selected dataset"
        uiOutput(ns("selected_dataset"))
      )
    )
    radio_ui <- c(item, radio_ui)
  }


  m <- modal(
    size = "lg",
    id = ns("choosedatamodal"),
    uiOutput(ns("accordions")),
    header = div(class = "modal-header-subdiv",
      h4(i18n("navigation.labels.choose_dataset")),#"Choose dataset"
      a(tags$span(i18n("module.text.tutorial")), href="https://youtu.be/_MMbZe5a1_o", target = "_blank")
      ),
    f(radio_list, current_data),
    # header = h5(i18n("manager.labels.managedata")) %>% margin(bottom = 0),
    # h5(i18n("manager.labels.uploadnew")), #"Upload new database"


    footer = tagList(
      div(
        class = "terms-span terms-warn",
        tags$span(i18n("module.text.disclaimer"))
        # "Please note that some datasets listed here are from external
        #           published sources and are therefore not WHO official estimates.
        #           Please refer to the Indicator Metadata in the About menu for more information."
      ),
      div(class = "terms-button",
        if(!add_cancel) div(
          class = "terms-span",
          tags$span(i18n("module.text.terms1")), #"By clicking OK, you are agreeing to the "
          actionLink(ns("terms_of_use"), tags$span(i18n("module.text.terms2"))) #"Terms of Use and Software License Agreement"
        ),
        if(add_cancel) actionButton(ns("cancel"), label = tags$span(i18n("module.text.cancel"))), # Cancel
        actionButton(ns("ok"), label = tags$span(i18n("module.text.ok")))

      )

    )
  )

  m <- tagAppendAttributes(
    m,
    class = "heat-management-modal",
    `data-backdrop` = "static",
    `data-bs-keyboard` = "false",
    `data-keyboard` = "false",
    )

  m
}




dataChooseServer <- function(input, output, session,  language = NULL) {
  ns <- session$ns
  this <- ns(NULL)

  State <- reactiveValues(
    radio_dataset = "rep_rmnch",
    dataset = NULL,
    is_who_dataset = NULL,
    is_map_datset = NULL
  )

  radios <- heatdata::info_databases |>
    dplyr::mutate(
      cat_radio_name = paste0(make_input_name(category), "_", internal_name, "_radio")
    ) |>
    dplyr::arrange(category_order, dataset_order)


  # Apologies, this is setting up observers and updates for
  # the category-specific observers
  lapply(
    unique(radios$cat_radio_name),
    function(x) {
      observeEvent(input[[x]], {

        sel_name <- radios |>
          dplyr::filter(
            internal_name == input[[x]]
          )

        nonsel_categories <- radios |>
          dplyr::filter(
            cat_radio_name != x
          ) |>
          dplyr::pull(cat_radio_name)


        State$radio_dataset <- unique(sel_name$internal_name)

        output$selected_dataset <- renderUI({
          translate(c(isolate(language()), "module", "dataset_name", paste0(State$radio_dataset, "_dataset")))
        })

        lapply(unique(nonsel_categories), function(radio_id) {
          updateRadioInput(radio_id, selected = "NOTHING")
        })
      })
    }
  )

  observeEvent(input$ok, {

    add_time("### OK ON NEW DATASET", State$radio_dataset)
    if(incl_timers()){
      session$sendCustomMessage("chart-timer", rnorm(1))
    }
    State$dataset <- State$radio_dataset

    info_database <- heatdata::info_databases |>
      dplyr::filter(internal_name == State$radio_dataset)

    State$is_who_dataset <- unique(info_database$is_WHO_dataset)
    State$is_map_dataset <- unique(info_database$show_map)


    closeModal(ns("choosedatamodal"))
  })

  observeEvent(input$cancel, {
    closeModal(ns("choosedatamodal"))
  })

  observeEvent(input$terms_of_use, {
    showModal(licenseModal(ns))
  })

  observeEvent(input$thismodal, {
    Sys.sleep(0.5)
    session$sendCustomMessage("modal-stay-open", rnorm(1))
  }, ignoreInit = TRUE)

  list(
    dataset_name = reactive(State$dataset)
  )

}
