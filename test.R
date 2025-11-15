team_data_frames <- get_all_teams(
  season_id,
  season,
  game_type
)


all_cancers <- team_data_frames["all_teams"][[1]] |>
  mutate(sign = DescTools::Zodiac(dob)) |>
  filter(sign == "Cancer")

all_stats <- data.frame()

teams <- pwhl_teams(
  season_id = season_id
)

for (team_label in teams$team_label) {
  # here we use our modified functions
  df_stats <- pwhl_stats_fix(
    position = "skater",
    team_label_arg = team_label,
    teams = teams,
    season = 2025,
    game_type = 'regular'
  )

  all_stats <- rbind(all_stats, df_stats)
}

all_stats <- all_stats |> select(player_id, plus_minus)

all_cancer_stats <- all_cancers |>
  merge(all_stats, by = "player_id") |>
  arrange(plus_minus)


#repeat for pre season

all_stats <- data.frame()

teams <- pwhl_teams(
  season_id = season_id
)

for (team_label in teams$team_label) {
  # here we use our modified functions
  df_stats <- pwhl_stats_fix(
    position = "skater",
    team_label_arg = team_label,
    teams = teams,
    season = 2026,
    game_type = 'preseason'
  )

  all_stats <- rbind(all_stats, df_stats)
}

all_stats <- all_stats |> select(player_id, plus_minus)

all_cancer_stats_preseason <- all_cancers |>
  merge(all_stats, by = "player_id") |>
  arrange(plus_minus)
