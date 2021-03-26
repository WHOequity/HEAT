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

  observe({
    r()
    # needed to remove this in order to get data to load on start up
    # if (firstRun) {
    #   firstRun <<- FALSE
    #   return()
    # }
    v$when <- Sys.time() + millis()/1000
  }, label = "debounce tracker", domain = domain, priority = priority)

  observe({
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
  }, label = "debounce timer", domain = domain, priority = priority)

  er <- eventReactive(v$trigger, {
    r()
  }, label = "debounce result", ignoreNULL = FALSE, domain = domain)

  primer <- observe({
    primer$destroy()
    er()
  }, label = "debounce primer", domain = domain, priority = priority)

  er
}

