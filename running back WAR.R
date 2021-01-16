# WAR for running backs

# Package load
library(caret)
library(data.table)
library(DBI)
library(DMwR)
library(ggrepel)
library(leaps)
library(lme4)
library(MASS)
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

# rushing data cleaned
rushing_all <- pbp_db %>%
  filter(play_type == "run") %>%
  dplyr::select(rusher_id, posteam_type, posteam, defteam, yardline_100, half_seconds_remaining,
                game_half, down, ydstogo, shotgun, no_huddle, run_gap, run_location,
                score_differential, season, roof, yards_gained, wpa, epa)

rushing_all.df <- as.data.frame(rushing_all) %>%
  unite(col = "team_run_gap", c("posteam", "run_location", "run_gap"), remove = F) %>%
  unite(col = "def_season", c("defteam", "season"), remove = F) %>%
  mutate(team_run_gap = as.factor(team_run_gap),
         def_season = as.factor(def_season))

rosters_all <- fast_scraper_roster(seasons = c(1999:2020))%>%
  select(season, gsis_id, full_name, position, height, weight) %>%
  arrange(season) %>%
  distinct(gsis_id, .keep_all = T) %>%
  rename(rookie_year = season)

rushing_all.df <- decode_player_ids(rushing_all.df) %>%
  left_join(rosters_all, by = c("rusher_id" = "gsis_id")) 

# Non QB
rushing_all.rb <- rushing_all.df %>%
  filter(position == "RB",
         is.na(rusher_id) == F) %>%
  mutate(position = as.factor(position))

# Pass defense strength
defensive_strength_pass <- pbp_db %>%
  filter(play_type == "pass") %>%
  group_by(defteam, season) %>%
  summarize(def_strength_pass = -mean(epa))

defensive_strength <- as.data.frame(defensive_strength_pass) %>%
  unite(col = def_season, c("defteam", "season")) %>%
  left_join(defensive_strength_run, by = "def_season")

rushing_all.rb <- rushing_all.rb %>%
  left_join(defensive_strength, by = "def_season") %>%
  mutate(def_season = as.factor(def_season))

# Ranking by season, everything under 96 is considered replacement level
carries_by_season <- rushing_all.rb %>%
  group_by(rusher_id, season) %>%
  summarize(carries = n()) %>%
  group_by(season) %>%
  mutate(rank_carries = dense_rank(desc(carries)))

carries_by_season <- carries_by_season %>%
  unite(col= "rusher_season", c("rusher_id", "season"), remove = F)

# Designate replacement

## Method here - calculate replacement level by season, but only take players ranked 
## under 96 instead of 64 - assumes two running backs per team
rushing_all.rb <- rushing_all.rb %>%
  unite(col = "rusher_season", c("rusher_id", "season"), remove = F) %>%
  left_join(carries_by_season, by = "rusher_season") %>%
  select(-c(season.y, rusher_id.y)) %>%
  rename(season = season.x,
         rusher_id = rusher_id.x)%>%
  mutate(rusher_season = ifelse(rank_carries <= 64, rusher_season, paste("replacement-level",season, sep = "_"))) %>%
  mutate(rusher_season = as.factor(rusher_season))


# Linear model for WPA
## using basis from Yurko, Ventura, Horowitz
wpa_lm1_rushing_rb <- lmerTest::lmer(wpa ~ posteam_type + shotgun + no_huddle + def_strength_pass +
                                      (1|rusher_season) + (1|def_season) + (1|team_run_gap),
                                     data = rushing_all.rb)

## Coefficients
coef_wpa_lm1_rushing_rb <- as.data.frame(coef(wpa_lm1_rushing_rb)[1]) %>%
  select(1) %>%
  rownames_to_column(var = "rusher_season") %>%
  separate(col = "rusher_season", into = c("rusher_id", "season"), sep = "_", remove = F) %>%
  mutate(season_n = as.numeric(season))

## Replacement level
replacement_level <- coef_wpa_lm1_rushing_rb %>%
  filter(rusher_id %like% "replacement") %>%
  rename(replacement_level = rusher_season..Intercept.) %>%
  mutate(season_n = as.numeric(season)) %>%
  select(season_n, replacement_level)

## Carries and names
carries <- carries_by_season %>%
  select(season, rusher_season, rusher_id, carries) %>%
  left_join(rosters_all, by = c("rusher_id" = "gsis_id")) %>%
  mutate(season_n = as.numeric(season))


## WAR
runningback_WAR <- coef_wpa_lm1_rushing_rb %>%
  select(rusher_season, rusher_season..Intercept.) %>%
  rename(WPA_average = rusher_season..Intercept.)%>%
  left_join(carries, by = "rusher_season") %>%
  left_join(replacement_level, by = "season_n") %>%
  mutate(replacement_wins = replacement_level*carries,
         wins = WPA_average*carries,
         rushingWAR = wins - replacement_wins)

## last ten years
runningback_WAR_11_20 <- runningback_WAR %>%
  filter(season %in% c(2011:2020)) %>%
  select(season, full_name, rushingWAR, carries) %>%
  arrange(desc(rushingWAR))

### this year
runningback_WAR_20 <- runningback_WAR %>%
  filter(season == 2020)

## Adding seasons in league
runningback_WAR.2 <- runningback_WAR %>%
  select(rusher_id, season, full_name, rushingWAR, carries, WPA_average, replacement_level,
         rookie_year) %>%
  mutate(season_number = season - rookie_year,
         above_zero = ifelse(rushingWAR > 0, 1, 0))

## By RB
runningback_WAR_by_rb <- runningback_WAR.2 %>%
  group_by(rusher_id, full_name) %>%
  summarize(total_carries = sum(carries),
            total_WAR = sum(rushingWAR),
            WPA_per_carry = total_WAR/total_carries,
            seasons_above_zero = sum(above_zero)) %>%
  arrange(desc(total_WAR))
