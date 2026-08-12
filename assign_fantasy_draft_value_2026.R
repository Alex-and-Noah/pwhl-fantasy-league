#%%
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

current_date <- as.Date("2026-04-25")

draft_order <- list(
  "1" = list(
    "1"  = "Caroline Harvey (D)",
    "2"  = "Abbey Murphy (F)",
    "3"  = "Tessa Janecke (F)",
    "4"  = "Laila Edwards (D)",
    "5"  = "Lacey Eden (F)",
    "6"  = "Nelli Laitinen (D)",
    "7"  = "Emma Peschel (D)",
    "8"  = "Kirsten Simms (F)",
    "9"  = "Sara Swiderski (D)",
    "10" = "Grace Dwyer (D)",
    "11" = "Vivian Jungels (D)",
    "12" = "Petra Nieminen (F)"
  ),
  "2" = list(
    "1"  = "Issy Wunder (F)",
    "2"  = "Sydney Morrow (D)",
    "3"  = "Andrea Brändli (G)",
    "4"  = "Sloane Matthews (F)",
    "5"  = "Thea Johansson (F)",
    "6"  = "Jade Iginla (F)",
    "7"  = "Elisa Holopainen (F)",
    "8"  = "Jamie Nelson (F)",
    "9"  = "Viivi Vainikka (F)",
    "10" = "Casey Borgiel (D)",
    "11" = "Jordan Ray (F)",
    "12" = "Avi Adam (F)"
  ),
  "3" = list(
    "1"  = "Jules Constantinople (F)",
    "2"  = "Emerson Jarvis (F)",
    "3"  = "Leah Stecker (D)",
    "4"  = "Tia Chan (G)",
    "5"  = "Josefin Bouveng (F)",
    "6"  = "Elyssa Biederman (F)",
    "7"  = "Carina DiAntonio (F)",
    "8"  = "Brooke Disher (D)",
    "9"  = "Madelyn Christian (F)",
    "10" = "MK O'Brien (F)",
    "11" = "Tereza Pištěková (F)",
    "12" = "Zoe Uens (F)"
  ),
  "4" = list(
    "1"  = "Katie DeSa (G)",
    "2"  = "Grace Elliott (F)",
    "3"  = "Kyla Josifovic (F)",
    "4"  = "Lily Shannon (F)",
    "5"  = "Saskia Maurer (G)",
    "6"  = "Megan Woodworth (F)",
    "7"  = "Katelyn Roberts (F)",
    "8"  = "Jane Kuehl (F)",
    "9"  = "Tova Henderson (D)",
    "10" = "Jaden Bogden (F)",
    "11" = "Tory Mariano (D)",
    "12" = "Hailey MacLeod (G)"
  ),
  "5" = list(
    "1"  = "Kendall Butze (D)",
    "2"  = "Gracie Gilkyson (D)",
    "3"  = "Sena Catterall (F)",
    "4"  = "McKenna Van Gelder (F)",
    "5"  = "Alexis Petford (F)",
    "6"  = "Emma-Sofie Nordstrøm (G)",
    "7"  = "Grace Wolfe (D)",
    "8"  = "Emerson O'Leary (F)",
    "9"  = "Darya Gredzen (G)",
    "10" = "Jenna Goodwin (F)",
    "11" = "Neena Brick (F)",
    "12" = "Erica Rieder (D)"
  ),
  "6" = list(
    "1"  = "Ashley Messier (D)",
    "2"  = "Gabriella Durante (G)",
    "3"  = "Georgia Schiff (F)",
    "4"  = "Reichen Kirchmair (F)",
    "5"  = "Sydney Healey (F)",
    "6"  = "Mya Vaslet (F)",
    "7"  = "Naomi Boucher (F)",
    "8"  = "Alyssa Regalado (D)",
    "9"  = "Lara Beecher (F)",
    "10" = "Maeve Kelly (D)",
    "11" = "Taylor Otremba (F)",
    "12" = "Émilie Lavoie (F)"
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
  separate(
    col = name, 
    into = c("name", "position"), 
    sep = "[\\(\\)]",
    extra = "drop",
    fill = "right"
  ) |>
  mutate(
    name = trimws(name),
    draft_round = as.numeric(draft_round),
    draft_position = as.numeric(draft_position),
    overall_draft_position = max(draft_position)*(
      draft_round - 1
    ) + draft_position
  )

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
    projected_fantasy_points = fantasy_points
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

fantasy_draft_values_2026 <- bind_rows(
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
    ),
  draft_order
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
  replace_na(
    list(
      draft_round = 0,
      draft_position = 0,
      overall_draft_position = 0,
      rookie = -1 ,
      fantasy_points = 0,
      fantasy_points_per_game = 0,
      projected_fantasy_points = 0
    )
  ) %>%
  mutate(
    fantasy_draft_value = case_when(
      rookie == 0 | rookie == 1 ~ round(
        projected_fantasy_points
      ),
      rookie == -1 & overall_draft_position != 0 ~ round(
        . |>
           filter(
            rookie == 1
          ) |> summarise(
            max = max(projected_fantasy_points)
          ) |>
          pull() * 0.6 *  (
            max(overall_draft_position) - overall_draft_position + 1
          ) / max(overall_draft_position)
      ),
      rookie == -1 & overall_draft_position != 0 ~ 1,
      .default = round(
        projected_fantasy_points
      )
    )
  ) |>
  select(
    name,
    team_code,
    rookie,
    position,
    fantasy_draft_value
  )

saveRDS(
  fantasy_draft_values_2026,
  file = "fantasy_draft_values_2026.rds"
)

# #%% Some analysis

# fantasy_draft_values_2026 |>
#   group_by(
#     position
#   ) |>
#   summarise(
#     q725= quantile(
#       fantasy_draft_value,
#       probs = 0.25
#     ),
#     median = median(fantasy_draft_value),
#     mean = mean(fantasy_draft_value),
#     q75 = quantile(
#       fantasy_draft_value,
#       probs = 0.75
#     )
#   )

# fantasy_draft_values_2026 |>
# arrange(
#   position,
#   desc(fantasy_draft_value)
# )|>View()

# fantasy_teams_2025 <- readRDS(
#   "fantasy_teams.rds"
# )

# data <- lapply(
#   names(fantasy_teams_2025),
#   function(fantasy_team_name) {
    
#     bind_rows(
#       fantasy_teams_2025[[fantasy_team_name]]$roster$skaters |>
#         select(
#           name,
#           team_code,
#           rookie,
#           fantasy_points
#         ) |>
#         left_join(
#           fantasy_draft_values_2026
#         ),
#       fantasy_teams_2025[[fantasy_team_name]]$roster$goalies |>
#         select(
#           name,
#           team_code,
#           rookie,
#           fantasy_points
#         ) |>
#         left_join(
#           fantasy_draft_values_2026
#         )
#     ) |>
#       mutate(
#         fantasy_team_name = fantasy_team_name
#       )
#   }
# ) |>
# bind_rows()


# data |>
#   group_by(
#     fantasy_team_name
#   ) |>
#   summarise(
#     fantasy_draft_value = sum(fantasy_draft_value),
#     fantasy_points = sum(fantasy_points)
#   ) |>
#   mutate(
#     point_gain = fantasy_points - fantasy_draft_value
#   ) |>
#   arrange(
#     desc(point_gain)
#   )
