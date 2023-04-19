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

is_heat_plus <- function() {
  isTRUE(getOption("heat.plus"))
}

get_app_color <- function() {
  if (is_heat_plus()) "green" else "orange"
}

get_heat_prefix <- function() {
  if (is_heat_plus()) "heatplus" else "heat"
}

str_args <- function(...) {
  purrr::map_chr(eval(substitute(alist(...))), function(sym) {
    gsub("\\s+", "", deparse(sym))
  })
}

str_truthy <- function(x) {
  isTRUE(nzchar(x))
}

str_multiline <- function(x) {
  gsub(" ", "<br>", x, fixed = TRUE)
}

drop_nulls <- function(x) {
  if (length(x) == 0) {
    return(x)
  } else {
    x[!vapply(x, is.null, logical(1))]
  }
}

totitle <- function(x, first = FALSE) {
  (if (first) sub else gsub)("(^|\\s)(.)", "\\1\\U\\2", x, perl = TRUE)
}

`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

`%&&%` <- function(a, b) {
  if (is.null(a) || is.na(a) || isFALSE(a)) {
    a
  } else {
    b
  }
}

not_null <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }

  which(!vapply(x, is.null, logical(1)))
}

all_equal <- function(a, b) {
  if (!(is.unsorted(a) || is.unsorted(b))) {
    isTRUE(all.equal(a, b))
  } else {
    isTRUE(all.equal(sort(a), sort(b)))
  }
}

closest <- function(possible, x) {
  diffs <- abs(possible - x)
  max(possible[diffs == min(diffs, na.rm = TRUE)])
}

en_list <- function(x) {
  if (inherits(x, "shiny.tag")) {
    list(x)
  } else if (is.list(x) && !isTRUE(class(x) == "list")) {
    list(x)
  } else {
    as.list(x)
  }
}

is_value <- function(val) {
  !is.null(val) &&
    !is.na(val) &&
    !is.infinite(val) &&
    length(val) != 0
}


# obsolete ----

only <- function(.data, var) {
  var <- deparse(substitute(var))
  sort(unique(.data[[var]]))
}

tabTitle <- function(text, icon) {
  div(
    text,
    icon %>%
      margin(left = c(lg = 1, xl = 2)) %>%
      display(c(default = "none", lg = "inline-block"))
  ) %>%
    margin(left = c(lg = -2), right = c(lg = -2)) %>%
    font(size = "sm") %>%
    display("flex") %>%
    flex(
      direction = c(lg = "column", xl = "row"),
      align = "center",
      justify = "center"
    )
}

# borrowed from {shiny}
customDebounce <- function(r, millis, priority = 100,
                           domain = getDefaultReactiveDomain()) {
  force(r)
  force(millis)

  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }

  v <- reactiveValues(trigger = NULL, when = NULL)
  firstRun <- TRUE

  observe(
    {
      r()
      # needed to remove this in order to get data to load on start up
      # if (firstRun) {
      #   firstRun <<- FALSE
      #   return()
      # }
      v$when <- Sys.time() + millis() / 1000
    },
    label = "debounce tracker",
    domain = domain,
    priority = priority
  )

  observe(
    {
      if (is.null(v$when)) {
        return()
      }

      now <- Sys.time()

      if (now >= v$when) {
        v$trigger <- isolate(v$trigger %||% 0) %% 999999999 + 1
        v$when <- NULL
      } else {
        invalidateLater((v$when - now) * 1000)
      }
    },
    label = "debounce timer",
    domain = domain,
    priority = priority
  )

  er <- eventReactive(v$trigger,
    {
      r()
    },
    label = "debounce result",
    ignoreNULL = FALSE,
    domain = domain
  )

  primer <- observe(
    {
      primer$destroy()
      er()
    },
    label = "debounce primer",
    domain = domain,
    priority = priority
  )

  er
}


is_mapping_dataset <- function(dataset_name) {
  dataset_name %in% c("rep_rmnch")
}


# https://stackoverflow.com/questions/40513153/shiny-extra-white-space-in-selectinput-choice-display-label
create_big_space <- function(n1){
  if(!is.na(n1))
    n1 <- paste(rep(intToUtf8(160), n1), collapse = "")

  n1
}

get_date_integer <- function(val, .data) {
  .data |>
    dplyr::filter(year == val) |>
    dplyr::pull(year_int) |>
    unique()
}
is_chip_input <- function(obj) {
  grepl("chips", deparse1(obj))
}

make_input_name <- function(val){
  tolower(gsub(" ", "_", gsub(",", "", val)))
}


clean_time <- function(){

  stringr::str_sub(Sys.time(), 12)
}

elapsed_time <- function(comparetime){
  round(Sys.time() - comparetime, 2)
}

incl_timers <- function(){
  opt <- getOption("heat.debug.timers", "none")
  opt != "none"
}

add_timing <- function(session, txt){


}

show_timings <- function(module = "heat-explore_disag_line", return_data = FALSE){
  suppressMessages(tbl <- readr::read_csv("~/junk/timings.csv"))

  names(tbl) <- c("what", "time")
  tbl$diff <- tbl$time - c(NA, tbl$time[1:(length(tbl$time)-1)])

  tbl <- tbl |>
    dplyr::mutate(
      diff = round(as.numeric(diff), 2)
    ) |>
    dplyr::relocate(diff) |>
    dplyr::select(-time) |>
    data.frame()

  tbl$diff[1] <- 0

  tbl$elapsed <- cumsum(tbl$diff)

  if(!is.null(module)){
    tbl <- tbl |>
      dplyr::filter(
        grepl(module, what) | !grepl("heat-", what)
      ) |>
      data.frame()


  }
  print(tbl, max = 1000)

  dplyr::count(tbl, what, sort = TRUE) |>
    dplyr::filter(n > 1) |>
    print(max = 1000)

  if(return_data){
    return(tbl)
  } else {
    invisible()
  }

}

add_time <- function(..., file = "~/junk/timings.csv"){
  if(incl_timers()){
    opt <- getOption("heat.debug.timers", "none")
    fe <- file.exists(file)
    txt <- paste0(unlist(list(...)), collapse=" -- ")

    if(grepl(opt, "show")){
      if(opt == "showall"){
        message(Sys.time(), " ", txt)
      } else{
        if(!grepl(
          "summary_bar|disag_graph|summary_graph|disag_map|disag_table|summary_line|summary_table|disag_detail|disag_bar",
          txt
        )){
          message(Sys.time(), " ", txt)
        }
      }
    }


    cat(
      txt,
      ",",
      as.character(Sys.time()),
      "\n",
      file = file,
      append = fe
    )
  }

  invisible()
}


is_compare <- function(current){
  grepl("compare_disag", current) || grepl("compare_summary", current)
}

# options(heat.special.message = NULL) # all modules
# options(heat.special.message = "heat-explore_disag_line") # just one
#options(heat.special.message = "heat-explore_disag_line heat-compare_summary_graph heat-compare_summary_graph-benchmark no this") # a few
special_message <- function(txt, this, ...){

  module <- getOption("heat.special.message")
  if(is.null(module) || grepl(this, module)){
    time_info <- format(Sys.time(), "%M:%S")
    message(time_info, " ", txt, " ", gsub("heat-", "", this), " ", paste(..., collapse = " | "))
  }

}

# message(
#   "Data rows: ", nrow(Data$main()), "\n",
#   paste(state$setting, collapse = ", "), "\n",
#   paste(state$source, collapse = ", "), "\n",
#   paste(state$year, collapse = ", "), "\n",
#   paste(state$recent, collapse = ", "), "\n",
#   paste(state$indicator, collapse = ", "), "\n",
#   paste(state$dimension, collapse = ", "), "\n",
#   paste(state$comparison, collapse = ", "), "\n"
# )
#
