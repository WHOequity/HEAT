accordion <- function(ns, catname, category, internal_category, dataset_names, internal_names, selected, txt){

  sel <- NULL
  if(selected %in% internal_names && !is.null(selected))
    sel <- selected



  radios <- lapply(1:length(internal_names), function(i){
    #data-i18n="module.dataset_name.{internal_names[i]}_dataset"
    nm <- paste0(category, "_", internal_names[i], "_radio")
    htmlinfo <- glue::glue('<div class = "tooltip-radio"><i class="fas fa-circle-info"></i>
      <span class = "tooltip-radiotext" data-i18n="module.blurb.{internal_names[i]}_blurb"></span>
      </div>')

    div(
      class = 'radio-line',
      yonder::radioInput(
        ns(nm),
        choices = list(div(i18n(paste0("module.dataset_name.", internal_names[i], "_dataset")))),#dataset_names[i],
        values = internal_names[i],
        selected = sel
      ),
      HTML(htmlinfo)
    )

  })


  tagList(
    tags$button(class = 'accordion', catname) |>
      tagAppendAttributes(onclick = "addAccordionToggle(this)",
                          `data-i18n` = paste0("module.category.", internal_category)),
    div(
      class = "accpanel",

      radios

    )

  )
}




