# NFL offensive line metrics
# Offensive line rushing points added

# Package load
library(DBI)
library(lme4)
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

# Data cleaning

## All 2020 plays
plays_2020 <- pbp_db %>%
  filter(season == 2020)

plays_2020.df <- as.data.frame(plays_2020)

plays_2020.df <- plays_2020.df %>%
  mutate(posteam_type = as.factor(posteam_type),
         shotgun = as.factor(shotgun),
         no_huddle = as.factor(no_huddle),
         pass_location = as.factor(pass_location)) %>%
  select(posteam, defteam, season, play_id, desc, passer, passer_id, receiver,
         receiver_id, rusher, rusher_id, posteam_type, shotgun, no_huddle,
         pass_location, air_yards, play_type, run_location, run_gap, side_of_field,
         yardline_100, half_seconds_remaining, game_seconds_remaining, game_half,
         goal_to_go, qb_scramble, yards_after_catch,down, ydstogo, interception,
         qb_hit, sack, touchdown, fumble, roof, surface, temp, wind, epa)

plays_2020.df <- decode_player_ids(plays_2020.df)

## 2020 rosters
rosters_2020 <- fast_scraper_roster(seasons = "2020")

rosters_2020 <- rosters_2020 %>%
  select(position, gsis_id)

rosters_2020_pass <- rosters_2020 %>%
  rename(passer_position = position)

rosters_2020_rec <- rosters_2020 %>%
  rename(rec_position = position)

rosters_2020_rush <- rosters_2020 %>%
  rename(rush_position = position)

# Merge

## Passing plays
plays_2020_pass <- plays_2020.df %>%
  filter(!is.na(receiver_id)) %>%
  left_join(rosters_2020_pass, by = c('passer_id' = 'gsis_id')) %>%
  left_join(rosters_2020_rec, by = c('receiver_id' = 'gsis_id')) %>%
  mutate(rush_position = NA)

## Rushing plays
plays_2020_rush <- plays_2020.df %>%
  filter(!is.na(rusher_id)) %>%
  mutate(passer_position = NA,
         rec_position = NA) %>%
  left_join(rosters_2020_rush, by = c('rusher_id' = 'gsis_id'))

## Combine
plays_2020_all <- rbind(plays_2020_pass,plays_2020_rush) %>%
  left_join(team_epa, by = "posteam") %>%
  mutate(rec_position = as.factor(rec_position),
         rush_position = as.factor(rush_position),
         passer_id = as.factor(passer_id),
         rusher_id = as.factor(rusher_id),
         receiver_id = as.factor(receiver_id),
         location_gap = ifelse(play_type == "run",
                               paste(run_location, run_gap), NA),
         location_gap = ifelse(run_location == "middle", "middle",
                               location_gap),
         team_location_gap = ifelse(play_type == "run",
                                    paste(posteam, location_gap), NA))

plays_2020_all$rec_position <- relevel(plays_2020_all$rec_position, ref = "WR")
plays_2020_all$rush_position <- relevel(plays_2020_all$rush_position, ref = "RB")
plays_2020_all$location_gap <- as.factor(plays_2020_all$location_gap)
plays_2020_all$team_location_gap <- as.factor(plays_2020_all$team_location_gap)

# Model sets
plays_2020_pass_model <- plays_2020_all %>%
  filter(play_type == "pass")

plays_2020_run_model <- plays_2020_all %>%
  filter(play_type == "run")

# Offensive line model set
