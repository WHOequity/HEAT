#!/usr/local/bin/Rscript

#
# Convert the langs.csv file into json files
#

#
# Helper functions
#
first_complete_row <- function(df) {
  for (i in seq_len(NROW(df))) {
    df_row <- df[i, ]
    is_complete <- all(!is.na(df_row)[1, ])

    if (is_complete) {
      return(i)
    }
  }

  stop("No complete row found")
}

clean_data_frame <- function(df) {
  n <- first_complete(df)

  trimmed <- df[-seq_len(n), ]

}


#************************************************
# Create langs.csv ----
#************************************************

# You need to move the file from Google Drive
src_folder <- "/Users/zevross/Library/CloudStorage/GoogleDrive-zev@zevross.com/My Drive/projects/proj-who-heat/data-sharing-tmp/HEAT-language-dictionary/"
src_possible <- fs::dir_ls(src_folder, glob = "*-langs-dictionary.xlsx")
src_latest <- rev(src_possible)[1]

if (length(src_latest) != 1 || !file.exists(src_latest)) {
  stop(
    "Please make sure you've done the following:\n",
    "  1. Download the latest translations spreadsheet,\n",
    "     https://drive.google.com/drive/folders/1Ob-V3pttvV-ZBEe-r_bUSl6-3CaynOga\n",
    "  2. Move this file to the inst/www/locales/ folder\n",
    "  3. Make sure the file follows the naming conention <DATE>-langs-dictionary.xlsx\n"
  )
}

src_sheets <- readxl::excel_sheets(src_latest)

if (!all(c("langs", "data") %in% src_sheets)) {
  stop(
    "Missing expected spreadsheet sheets 'langs' and 'data'"
  )
}

#
# The "langs" sheet is used by the front-end
src_langs <- readxl::read_xlsx(src_latest, "langs")
src_langs[["NEW\r\nTRANSLATION \r\n(1=yes, 0=no)"]] <- NULL
src_langs[["REVISION"]] <- NULL
src_langs[["Link/URL"]] <- NULL

# The "data" sheet (combined with the "langs" sheet) is used by the back-end
src_data <- readxl::read_xlsx(src_latest, "data")
src_data[["NEW\r\nTRANSLATION \r\n(1=yes, 0=no)"]] <- NULL
src_data[["REVISION"]] <- NULL
src_data[["Link/URL"]] <- NULL

raw <- dplyr::bind_rows(
  src_langs,
  src_data
)


#filter(raw, key == "heat3a") %>% View()

library(magrittr)

# message("Reading raw language data")
# raw <- readr::read_csv(
#   file = "inst/www/locales/langs.csv",
#   col_types = readr::cols(.default = "c")
# )

if (!all(c("namespace", "subject", "key") %in% colnames(raw))) {
  stop("Missing required columns: namespace, subject, key")
}

valid <- dplyr::filter(raw, !is.na(key))
valid$`english (en)`[valid$key == "heat2" & valid$subject=="citations"] <- " "
valid$`english (en)`[valid$key == "heatplus3" & valid$subject=="citations"] <- " "

readr::write_csv(valid, "inst/www/locales/langs.csv")

# This also needs to go to the HEAT-Data location
# AND YOU NEED TO REBUILD/Commit the changes

readr::write_csv(valid, "~/git-repos/HEAT-Data-development-5.0/inst/langs.csv")

message("Summarising data by language")
json <- valid %>%
  tidyr::gather("language", "value", -namespace, -subject, -key) %>%
  dplyr::group_by(language, namespace, subject) %>%
  dplyr::summarise(keys = list(as.list(setNames(value, key)))) %>%
  dplyr::summarise(subjects = list(setNames(keys, subject))) %>%
  dplyr::summarise(namespaces = list(setNames(subjects, namespace))) %>%
  dplyr::rename(namespace = namespaces) %>%
  tidyr::extract(language, c("language", "iso"), "^([a-z]+)\\s+\\(([a-z]+)\\)$")

if (anyNA(json$iso)) {
  no_iso <- json %>%
    dplyr::filter(is.na(iso)) %>%
    dplyr::pull(language)

  warning(
    "Dropping languages missing iso codes: ", paste(no_iso, collapse = ", ")
  )

  json <- dplyr::filter(json, !is.na(iso))
}

message("Creating language json files")
purrr::pwalk(json, function(language, iso, namespace) {
  fs::dir_create(glue::glue("inst/www/locales/{ iso }"))

  jsonlite::write_json(
    namespace, glue::glue("inst/www/locales/{ iso }/default.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  message(glue::glue("* inst/www/locales/{ iso }/default.json"))
})
