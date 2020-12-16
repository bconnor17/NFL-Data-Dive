# nflWAR attempt

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
         pass_location, air_yards, play_type, run_location, run_gap, epa)

plays_2020.df <- decode_player_ids(plays_2020.df)

## Passing and rushing EPA by team
team_epa <- plays_2020.df %>%
  group_by(posteam, play_type) %>%
  summarize(avg_EPA = mean(epa, na.rm = T)) %>%
  filter(play_type %in% c("pass", "run")) %>%
  spread(key = play_type, value = avg_EPA) %>%
  rename(rush_strength = run,
         pass_strength = pass)

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

# Modeling
## Initial EPA multilevel model per Yurko, Ventura, Horowitz (2018)
passing_lm1 <- lmer(epa ~ posteam_type + shotgun + no_huddle + pass_location +
                      air_yards  + rush_strength +
                      (1| passer_id) + (1|receiver_id),
                    data = plays_2020_pass_model)

### Note - had to remove passstrength and receiver position from model (singularity)
summary(passing_lm1)

passer_iPA <- as.data.frame(coef(passing_lm1)[2]) %>%
  rownames_to_column(var = "passer_id") %>%
  select(passer_id, passer_id..Intercept.)

receiver_iPA <- as.data.frame(coef(passing_lm1)[1]) %>%
  rownames_to_column(var = "receiver_id") %>%
  select(receiver_id, receiver_id..Intercept.)  

## Running and OL model!
rushing_lm1 <- lmer(epa ~ posteam_type + shotgun + no_huddle + pass_strength +
                      (1| rusher_id) + (1|team_location_gap),
                    data = plays_2020_run_model)

summary(rushing_lm1)

rusher_iPA <- as.data.frame(coef(rushing_lm1)[1]) %>%
  rownames_to_column(var = "rusher_id") %>%
  select(rusher_id, rusher_id..Intercept.)

OL_iPA <- as.data.frame(coef(rushing_lm1)[2]) %>%
  rownames_to_column(var = "team_location_gap") %>%
  select(team_location_gap, team_location_gap..Intercept.) %>%
  arrange(desc(team_location_gap..Intercept.))

# Adding some names in there
pass_attempts <- plays_2020_pass_model %>%
  group_by(passer_id) %>%
  summarize(attempts = n())

targets <- plays_2020_pass_model %>%
  group_by(receiver_id) %>%
  summarize(targets = n())

rushes <- plays_2020_run_model %>%
  group_by(rusher_id) %>%
  summarize(carries = n())

rushes_OL <- plays_2020_run_model %>%
  group_by(team_location_gap) %>%
  summarize(rushes = n())

rosters_2020_names <- fast_scraper_roster(seasons = "2020") %>%
  select(team, position, full_name, gsis_id)

passer_iPA <- merge(passer_iPA, rosters_2020_names, 
                    by.x = "passer_id", by.y = "gsis_id",
                    all.x = T, all.y = F) %>%
  left_join(pass_attempts, by = "passer_id") %>%
  arrange(desc(passer_id..Intercept.)) %>%
  filter(attempts > 150)

receiver_iPA <- merge(receiver_iPA, rosters_2020_names,
                      by.x = "receiver_id", by.y = "gsis_id",
                      all.x = T, all.y = F) %>%
  left_join(targets, by = "receiver_id") %>%
  arrange(desc(receiver_id..Intercept.)) %>%
  filter(targets > 35)

rusher_iPA <- merge(rusher_iPA, rosters_2020_names,
                    by.x = "rusher_id", by.y = "gsis_id",
                    all.x = T, all.y = F) %>%
  left_join(rushes, by = "rusher_id") %>%
  arrange(desc(rusher_id..Intercept.)) %>%
  filter(carries > 20)

OL_iPA <- OL_iPA %>%
  left_join(rushes_OL, by = "team_location_gap") %>%
  arrange(desc(team_location_gap..Intercept.)) %>%
  filter(rushes > 10)
