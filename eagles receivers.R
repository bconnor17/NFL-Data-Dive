# Eagles receivers

eagles15_19 <- pbp_db %>%
  filter(posteam == "PHI",
         season %in% c(2015:2019),
         play_type == "pass")

passing_overall <- eagles15_19 %>%
  group_by(season) %>%
  summarize(team_dropbacks = n(),
            team_attempts = sum(pass_attempt),
            team_completions = sum(complete_pass),
            team_passyards = sum(yards_gained[complete_pass == T]),
            team_air_yards = sum(air_yards),
            team_yac = sum(yards_after_catch)) %>%
  mutate(team_ypa = team_passyards/team_attempts,
         team_ypd = team_passyards/team_dropbacks,
         team_comp_pct = team_completions/team_attempts)

receivers_overall <- eagles15_19 %>%
  group_by(season, receiver_id, receiver_player_name) %>%
  summarize(targets = n(),
            receptions = sum(complete_pass),
            rec_yards = sum(yards_gained[complete_pass == T]),
            rec_airyards = sum(air_yards),
            rec_yac = sum(yards_after_catch)) %>%
  mutate(receiver_ypt = rec_yards/targets,
         receiver_ypc = rec_yards/receptions,
         receiver_comp_pct = receptions/targets) %>%
  left_join(passing_overall, by = "season") %>%
  mutate(relative_comp = receiver_comp_pct/team_comp_pct,
         relative_ypa = receiver_ypt/team_ypa)

receivers_overall <- as.data.frame(receivers_overall)
