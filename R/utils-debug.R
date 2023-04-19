message_state <- function(state){
  message(
    "****** STATE SETTINGS ******\n",
    "Setting: ", state$setting, "\n",
    "Source: ", paste(state$source, collapse = ", "), "\n",
    "Year: ", paste(state$year, collapse = ", "), "\n",
    "Recent: ", state$recent, "\n",
    "Indicator: ", paste(state$indicator, collapse = ", "), "\n",
    "Dimension: ", paste(state$dimension, collapse = ", "), "\n",
    "Measure: ", paste(state$measure, collapse = ", "), "\n",
    "Comparison: ", paste(state$comparison, collapse = ", "), "\n",
    "Benchmark source: ", paste(state$benchmark_source, collapse = ", "), "\n",
    Sys.time(), "\n"
  )
}
