library(tidyverse)
library(gt)
library(bslib)
library(shiny)
library(bsicons)
library(ggimage)
library(rsvg)

invisible(
  lapply(
    list.files(
      "./R",
      full.names = TRUE
    ),
    source
  )
)

current_date = today(
  tzone = "EST"
)

current_date = as.Date("2026-04-25")

season_schedules_by_id <- get_season_schedules_by_id()