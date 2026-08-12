library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

#%% Exploration and validation of method

draft_order_2025 <- list(
  "1" = list(
    "1" = "Kristýna Kaltounková",
    "2" = "Haley Winn",
    "3" = "Casey O'Brien",
    "4" = "Nicole Gosling",
    "5" = "Rory Guilday",
    "6" = "Kendall Cooper",
    "7" = "Michelle Karvinen",
    "8" = "Jenna Buglioni"
  ),
  "2" = list(
    "1" = "Anne Cherkowski",
    "2" = "Ella Huber",
    "3" = "Emma Gentry",
    "4" = "Natálie Mlýnková",
    "5" = "Anna Shokhina",
    "6" = "Abby Hustler",
    "7" = "Hannah Murphy",
    "8" = "Kiara Zanon"
  ),
  "3" = list(
    "1" = "Makenna Webster",
    "2" = "Olivia Mobley",
    "3" = "Nina Jobst-Smith",
    "4" = "Skylar Irving",
    "5" = "Sarah Wozniewicz",
    "6" = "Anna Segedi",
    "7" = "Clara Van Wieren",
    "8" = "Lily Delianedis"
  ),
  "4" = list(
    "1" = "Dayle Ross",
    "2" = "Riley Brengman",
    "3" = "Maddi Wheeler",
    "4" = "Callie Shanahan",
    "5" = "Peyton Hemp",
    "6" = "Ava Rinker",
    "7" = "Jada Habisch",
    "8" = "Brianna Brooks"
  ),
  "5" = list(
    "1" = "Anna Bargman",
    "2" = "Abby Newhook",
    "3" = "Sara Hjalmarsson",
    "4" = "Maya Labad",
    "5" = "Sanni Ahola",
    "6" = "Vanessa Upson",
    "7" = "Madison Samoskevich",
    "8" = "Lyndie Lobdell"
  ),
  "6" = list(
    "1" = "Haley Doyle",
    "2" = "Amanda Thiele",
    "3" = "Hanna Baskin",
    "4" = "Tamara Giaquinto",
    "5" = "Fanuza Kadirova",
    "6" = "Brooke Becker",
    "7" = "Olivia Wallin",
    "8" = "Chanreet Bassi"
  )
) |>
  bind_rows() |>
  rownames_to_column(
    var = "draft_round"
  ) |>
  pivot_longer(
    cols = -draft_round,
    names_to = "draft_position",
    values_to = "name"
  ) |>
  mutate(
    draft_round = as.numeric(draft_round),
    draft_position = as.numeric(draft_position),
    overall_draft_position = max(draft_position)*(
      draft_round - 1
    ) + draft_position
  )

team_stats <- readRDS(
  "team_stats.rds"
)

all_skaters <- bind_rows(
  lapply(
    team_stats,
    `[[`,
    1
  )
) |>
  mutate(
    fantasy_points = 2*(
      goals - short_handed_goals
    ) +
    3*short_handed_goals +
    1*(
      assists - short_handed_assists
    ) +
    2*short_handed_assists +
    0.1*shots + 
    0.1*shots_blocked_by_player,
    # 0.05*faceoff_wins,
    fantasy_points_per_game = fantasy_points / games_played,
    projected_fantasy_points = 30*fantasy_points_per_game
  ) |>
  select(
    name,
    team_code,
    rookie,
    position,
    goals,
    assists,
    shots,
    fantasy_points,
    games_played,
    fantasy_points_per_game,
    projected_fantasy_points
  )

all_goalies <- bind_rows(
  lapply(
    team_stats,
    `[[`,
    2
  )
) |> 
  mutate(
    fantasy_points = 1*shutouts +
    0.05*(
      shots - goals_against
    ),
    fantasy_points_per_game = fantasy_points / games_played,
    projected_fantasy_points = 30*fantasy_points_per_game
  ) |>
  select(
    name,
    team_code,
    rookie,
    position,
    shots,
    goals_against,
    fantasy_points,
    games_played,
    fantasy_points_per_game,
    projected_fantasy_points
  )

all_fantasy_points <- bind_rows(
  all_skaters |>
    select(
      name,
      team_code,
      rookie,
      position,
      fantasy_points,
      fantasy_points_per_game,
      projected_fantasy_points
    ),
  all_goalies |>
    select(
      name,
      team_code,
      rookie,
      position,
      fantasy_points,
      fantasy_points_per_game,
      projected_fantasy_points
    )
) %>%
  mutate(
    position = replace_values(
      .$position,
      "C" ~ "F",
      "LW" ~ "F",
      "RW" ~ "F",
      "F" ~ "F",
      "LD" ~ "D",
      "RD" ~ "D",
      "D" ~ "D",
      "G" ~ "G"
    )
  ) |>
  left_join(
    draft_order_2025,
    by = "name"
  ) |>
  replace_na(
    list(
      draft_round = 0,
      draft_position = 0,
      overall_draft_position = 0
    )
  ) |>
  mutate(
    fantasy_draft_value = case_when(
      rookie == 0 ~ round(
        projected_fantasy_points
      ),
      rookie == 1 & overall_draft_position != 0 ~ round(
        all_fantasy_points |>
          filter(
            rookie == 1
          ) |> summarise(
            max = max(projected_fantasy_points)
          ) |>
          pull() * (
            max(overall_draft_position) - overall_draft_position + 1
          ) / max(overall_draft_position)
      ),
      rookie == 1 & overall_draft_position != 0 ~ 1,
      .default = round(
        projected_fantasy_points
      )
    )
  )

ggplot(
  all_fantasy_points |>
    filter(
      rookie == 1 & overall_draft_position != 0
    ),
  aes(
    x = fantasy_draft_value,
    y = projected_fantasy_points
  )
) +
  geom_point() +
  geom_smooth(
    method="lm",
    se=FALSE,
    col="red"
  )

bind_rows(
  all_skaters |>
    filter(
      rookie == 0
    ) |>
    summarise(
      name = "non_rookie_skaters",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
  all_skaters |>
    filter(
      rookie == 1
    ) |>
    summarise(
      name = "rookie_skaters",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
  all_skaters |>
    summarise(
      name = "all_skaters",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    filter(
      rookie == 0
    ) |>
    summarise(
      name = "non_rookie_goalies",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    filter(
      rookie == 1
    ) |>
    summarise(
      name = "rookie_goalies",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
  all_goalies |>
    summarise(
      name = "all_goalies",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    ),
    all_fantasy_points |>
      filter(
        rookie == 1
      ) |>
      summarise(
        name = "all_rookies",
        min = min(
          projected_fantasy_points,
          na.rm = TRUE
        ),
        median = median(
          projected_fantasy_points,
          na.rm = TRUE
        ),
        mean = mean(
          projected_fantasy_points,
          na.rm = TRUE
        ),
        max = max(
          projected_fantasy_points,
          na.rm = TRUE
        )
      ),
  all_fantasy_points |>
    summarise(
      name = "all_players",
      min = min(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      median = median(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      mean = mean(
        projected_fantasy_points,
        na.rm = TRUE
      ),
      max = max(
        projected_fantasy_points,
        na.rm = TRUE
      )
    )
)

#%% 2026 draft
library(tidyverse)
library(gt)
library(gtExtras)
library(bslib)
library(shiny)
library(bsicons)
library(ggimage)
library(rsvg)
library(here)
library(htmltools)

invisible(
  lapply(
    list.files(
      "./R",
      full.names = TRUE
    ),
    source
  )
)

environment(custom_gt_split) <- asNamespace('gt')
assignInNamespace("gt_split", custom_gt_split, ns = "gt")

current_date <- today(
  tzone = "EST"
)

current_date <- as.Date("2026-08-11")
# current_date <- as.Date("2025-11-15")
# current_date <- as.Date("2026-05-20")

season_schedules_by_id <- get_season_schedules_by_id()

season_id <- get_season_id_of_current_date(
  current_date,
  season_schedules_by_id
)

current_schedule <- season_schedules_by_id[[
  season_id
]]$schedule

team_info <- get_team_info(
  season_id
)

player_boxes_per_game <- get_player_boxes_per_game(
  current_schedule
)

team_stats <- get_team_stats(
  season_id,
  team_info,
  player_boxes_per_game
)

draft_order_2026 <- list(
  "1" = list(
    "1"  = "Caroline Harvey"
    "2"  = "Abbey Murphy"
    "3"  = "Tessa Janecke"
    "4"  = "Laila Edwards"
    "5"  = "Lacey Eden"
    "6"  = "Nelli Laitinen"
    "7"  = "Emma Peschel"
    "8"  = "Kirsten Simms"
    "9"  = "Sara Swiderski"
    "10" = "Grace Dwyer"
    "11" = "Vivian Jungels"
    "12" = "Petra Nieminen"
  ),
  "2" = list(
    "1"  = "Issy Wunder"
    "2"  = "Sydney Morrow"
    "3"  = "Andrea Brändli"
    "4"  = "Sloane Matthews"
    "5"  = "Thea Johansson"
    "6"  = "Jade Iginla"
    "7"  = "Elisa Holopainen"
    "8"  = "Jamie Nelson"
    "9"  = "Viivi Vainikka"
    "10" = "Casey Borgiel"
    "11" = "Jordan Ray"
    "12" = "Avi Adam"
  ),
  "3" = list(
    "1"  = "Jules Constantinople"
    "2"  = "Emerson Jarvis"
    "3"  = "Leah Stecker"
    "4"  = "Tia Chan"
    "5"  = "Josefin Bouveng"
    "6"  = "Elyssa Biederman"
    "7"  = "Carina DiAntonio"
    "8"  = "Brooke Disher"
    "9"  = "Madelyn Christian"
    "10" = "MK O'Brien"
    "11" = "Tereza Pištěková"
    "12" = "Zoe Uens"
  ),
  "4" = list(
    "1"  = "Katie DeSa"
    "2"  = "Grace Elliott"
    "3"  = "Kyla Josifovic"
    "4"  = "Lily Shannon"
    "5"  = "Saskia Maurer"
    "6"  = "Megan Woodworth"
    "7"  = "Katelyn Roberts"
    "8"  = "Jane Kuehl"
    "9"  = "Tova Henderson"
    "10" = "Jaden Bogden"
    "11" = "Tory Mariano"
    "12" = "Hailey MacLeod"
  ),
  "5" = list(
    "1"  = "Kendall Butze"
    "2"  = "Gracie Gilkyson"
    "3"  = "Sena Catterall"
    "4"  = "McKenna Van Gelder"
    "5"  = "Alexis Petford"
    "6"  = "Emma-Sofie Nordstrøm"
    "7"  = "Grace Wolfe"
    "8"  = "Emerson O'Leary"
    "9"  = "Darya Gredzen"
    "10" = "Jenna Goodwin"
    "11" = "Neena Brick"
    "12" = "Erica Rieder"
  ),
  "6" = list(
    "1"  = "Ashley Messier"
    "2"  = "Gabriella Durante"
    "3"  = "Georgia Schiff"
    "4"  = "Reichen Kirchmair"
    "5"  = "Sydney Healey"
    "6"  = "Mya Vaslet"
    "7"  = "Naomi Boucher"
    "8"  = "Alyssa Regalado"
    "9"  = "Lara Beecher"
    "10" = "Maeve Kelly"
    "11" = "Taylor Otremba"
    "12" = "Émilie Lavoie"
  )
)  |>
  bind_rows() |>
  rownames_to_column(
    var = "draft_round"
  ) |>
  pivot_longer(
    cols = -draft_round,
    names_to = "draft_position",
    values_to = "name"
  ) |>
  mutate(
    draft_round = as.numeric(draft_round),
    draft_position = as.numeric(draft_position),
    overall_draft_position = max(draft_position)*(
      draft_round - 1
    ) + draft_position
  )
