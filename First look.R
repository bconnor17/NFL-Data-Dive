# Package load
library(DBI)
library(nflfastR)
library(RSQLite)
library(tidyverse)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Connect to database
update_db()

connection <- dbConnect(SQLite(), "./pbp_db")
connection

pbp_db <- tbl(connection, "nflfastR_pbp")

# Let's look at Jets quarterbacks on third and long
jets_offensive <- pbp_db %>%
  filter(posteam == "NYJ")

jets_offensive_df <- as.data.frame(jets_offensive)

jets_third_long <- jets_offensive_df %>%
  filter(down == 3,
         ydstogo >= 7) %>%
  mutate(year = as.numeric(substr(game_id, 1, 4)),
         complete = ifelse(incomplete_pass == 0, 1, 0))

jets_third_long_qbs <- jets_third_long %>%
  group_by(year, passer, season_type) %>%
  summarise(attempts = n(),
            completions = sum(complete),
            conversions = sum(third_down_converted),
            avg_distance = mean(ydstogo),
            avg_airyards = mean(air_yards, na.rm = T)) %>%
  mutate(completion_pct = completions/attempts,
         conversion_pct = conversions/attempts)

# The soonest that the Jets' win probability dropped under 10%
jets <- pbp_db %>%
  filter(home_team == "NYJ" | away_team == "NYJ")

jets_df <- as.data.frame(jets)
  
jets_wp_below10 <- jets_df %>%
  mutate(jets_wp = ifelse(posteam == "NYJ", wp, (1-wp))) %>%
  filter(season >= 2010,
         wp <= .10,
         game_seconds_remaining < 3600,
         game_half == "Half1") %>%
  distinct(game_id, .keep_all = T)

jets_offensive_df <- jets_offensive_df %>%
  arrange(desc(epa))


#Jets QBs EPA
jets_qb_EPA <- jets_offensive_df %>%
  filter(!is.na(passer)) %>%
  group_by(passer, season) %>%
  summarize(attempts = n(),
           avgEPA = mean(epa)) %>%
  filter(attempts >= 50)

# all EPA all time
passing_plays <- pbp_db %>%
  filter(!is.na(passer))

passing_plays_df <- as.data.frame(passing_plays)

epa_all_time <- passing_plays_df %>%
  group_by(passer, posteam, season) %>%
  summarize(attempts = n(),
            avgEPA = mean(epa, na.rm = T)) %>%
  filter(attempts >= 100)


epa2020 <- epa_all_time %>%
  filter(season == "2020")
