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
library(glue)

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

current_date <- as.Date("2025-03-3")

draft_order <- list(
  "1" = list(
    "1" = "Kristýna Kaltounková (F)",
    "2" = "Haley Winn (D)",
    "3" = "Casey O'Brien (F)",
    "4" = "Nicole Gosling (D)",
    "5" = "Rory Guilday (D)",
    "6" = "Kendall Cooper (D)",
    "7" = "Michelle Karvinen (F)",
    "8" = "Jenna Buglioni (F)"
  ),
  "2" = list(
    "1" = "Anne Cherkowski (F)",
    "2" = "Ella Huber (F)",
    "3" = "Emma Gentry (F)",
    "4" = "Natálie Mlýnková (F)",
    "5" = "Anna Shokhina (F)",
    "6" = "Abby Hustler (F)",
    "7" = "Hannah Murphy (G)",
    "8" = "Kiara Zanon (F)"
  ),
  "3" = list(
    "1" = "Makenna Webster (F)",
    "2" = "Olivia Mobley (F)",
    "3" = "Nina Jobst-Smith (F)",
    "4" = "Skylar Irving (F)",
    "5" = "Sarah Wozniewicz (F)",
    "6" = "Anna Segedi (F)",
    "7" = "Clara Van Wieren (F)",
    "8" = "Lily Delianedis (F)"
  ),
  "4" = list(
    "1" = "Dayle Ross (D)",
    "2" = "Riley Brengman (D)",
    "3" = "Maddi Wheeler (F)",
    "4" = "Callie Shanahan (G)",
    "5" = "Peyton Hemp (F)",
    "6" = "Ava Rinker (D)",
    "7" = "Jada Habisch (F)",
    "8" = "Brianna Brooks (F)"
  ),
  "5" = list(
    "1" = "Anna Bargman (F)",
    "2" = "Abby Newhook (F)",
    "3" = "Sara Hjalmarsson (F)",
    "4" = "Maya Labad (F)",
    "5" = "Sanni Ahola (G)",
    "6" = "Vanessa Upson (F)",
    "7" = "Madison Samoskevich (D)",
    "8" = "Lyndie Lobdell (D)"
  ),
  "6" = list(
    "1" = "Haley Doyle (G)",
    "2" = "Amanda Thiele (G)",
    "3" = "Hanna Baskin (D)",
    "4" = "Tamara Giaquinto (D)",
    "5" = "Fanuza Kadirova (F)",
    "6" = "Brooke Becker (D)",
    "7" = "Olivia Wallin (F)",
    "8" = "Chanreet Bassi (F)"
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

team_stats_season_5 <- get_team_stats(
  season_id,
  team_info,
  player_boxes_per_game
)

saveRDS(
  team_stats_season_5,
  file = glue("team_stats_season_{season_id}.rds")
)

all_skaters <- bind_rows(
  lapply(
    team_stats_season_5,
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
    fantasy_points_per_game = fantasy_points / max(5,games_played),
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
    team_stats_season_5,
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
    fantasy_points_per_game = fantasy_points / max(5,games_played),
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

fantasy_draft_values_2025 <- bind_rows(
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
    rookie,
    position,
    fantasy_draft_value
  )

saveRDS(
  fantasy_draft_values_2025,
  file = glue("fantasy_draft_values_season_{season_id}.rds")
)

team_stats_season_8 <- readRDS(
  "team_stats_season_8.rds"
)

all_skaters <- bind_rows(
  lapply(
    team_stats_season_8,
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
    team_stats_season_8,
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

fantasy_points_season_8 <- bind_rows(
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
  select(
    name,
    rookie,
    position,
    projected_fantasy_points,
    fantasy_points
  )

saveRDS(
  fantasy_points_season_8,
  file = "fantasy_points_season_8.rds"
)

# data <- fantasy_draft_values_2025 |>
#   inner_join(
#     fantasy_points_2025,
#     by = c(
#       "name",
#       "position"
#     )
#   ) |>
#   mutate(
#     rookie = rookie.y
#   ) |>
#   select(
#     name,
#     rookie,
#     position,
#     fantasy_draft_value,
#     projected_fantasy_points
#   )

# ggplot(
#   data,
#   aes(
#     x = fantasy_draft_value
#   )
# ) +
#   geom_point(
#     aes(
#       y = projected_fantasy_points,
#       colour = factor(rookie)
#     ),
#     size = 5
#   ) +
#   geom_smooth(
#     data = \(df) filter(df,rookie==1),
#     aes(
#       y = projected_fantasy_points
#     ),
#     method="lm",
#     se=TRUE,
#     col="blue"
#   ) +
#   geom_smooth(
#     data = \(df) filter(df,rookie==0),
#     aes(
#       y = projected_fantasy_points
#     ),
#     method="lm",
#     se=TRUE,
#     col="red"
#   ) +
#   geom_line(
#     aes(
#       y = fantasy_draft_value
#     )
#   )

# ggplot(
#   data,
#   aes(
#     x = fantasy_draft_value
#   )
# ) +
#   geom_point(
#     aes(
#       y = projected_fantasy_points,
#       colour = factor(position)
#     ),
#     size = 5
#   ) +
#   geom_smooth(
#     data = \(df) filter(df,position == "F"),
#     aes(
#       y = projected_fantasy_points
#     ),
#     method="lm",
#     se=TRUE,
#     col="green"
#   ) +
#   geom_smooth(
#     data = \(df) filter(df,position == "D"),
#     aes(
#       y = projected_fantasy_points
#     ),
#     method="lm",
#     se=TRUE,
#     col="red"
#   ) +
#   geom_smooth(
#     data = \(df) filter(df,position == "G"),
#     aes(
#       y = projected_fantasy_points
#     ),
#     method="lm",
#     se=TRUE,
#     col="blue"
#   ) +
#   geom_line(
#     aes(
#       y = fantasy_draft_value
#     )
#   )
