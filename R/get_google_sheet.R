library(readr)
library(stringr)
library(data.table)

#' @title  **Import a Google Sheet**
#' @description Import a Google Sheet
#'
#' @param url Google Sheet URL
#' @param format Google Sheet export format
#' @sheet_id Index of the desired Sheet in the Google Sheet file
#' @return data.frame read from the Google Sheet
#' @import readr
#' @import stringr
#' @import data.table
#' @export

get_google_sheet <- function(
  url = "https://docs.google.com/spreadsheets/d/1sywjZ6quCmbLBq_wEEXZEDzcW0NeBpCtRaT3W1hDkhI",
  format = "csv",
  sheet_id = NULL
) {
  # Taken from Max Conway: https://github.com/maxconway/gsheet/tree/master
  key <- stringr::str_extract(
    url,
    "[[:alnum:]_-]{30,}"
  )

  if (
    is.null(sheet_id) &&
      stringr::str_detect(
        url,
        "gid=[[:digit:]]+"
      )
  ) {
    sheet_id <- as.numeric(
      str_extract(
        stringr::str_extract(
          url,
          "gid=[[:digit:]]+"
        ),
        "[[:digit:]]+"
      )
    )
  }

  address <- paste0(
    "https://docs.google.com/spreadsheets/export?id=",
    key,
    "&format=",
    format
  )

  if (!is.null(sheet_id)) {
    address <- paste0(
      address,
      "&gid=",
      sheet_id
    )
  }

  df <- read_csv(
    address,
    col_names = c(""),
    col_types = cols(
      .default = "c"
    )
  ) |>
    transpose(
      make.names = 1
    )

  return(df)
}
