do_indicator_indent <- function(.indicators, .dataset){

  ind <- heatdata::info_indicators |>
    dplyr::filter(
      internal_name == gsub("_", "-", .dataset)
    )

  ind <- ind[match(.indicators, ind$indicator_name),]

  ind$spaces <- ""

  # Sorry about this piece of code!
  ind$spaces[ind$indentation > 0] <- sapply(ind$indentation[ind$indentation > 0], function(x){
    create_big_space(x*2)
    })

  ind |>
    dplyr::mutate(
      indicator_indent = paste0(spaces, indicator_name)
    ) |>
    dplyr::pull(indicator_indent)


}
