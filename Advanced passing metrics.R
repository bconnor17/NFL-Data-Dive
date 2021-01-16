# Advanced passing metrics
# AKA" Which wideout should the Jets sign?

# Package load
library(caret)
library(DBI)
library(DMwR)
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
  dplyr::select(posteam, defteam, season, play_id, desc, passer, passer_id, receiver,
         receiver_id, rusher, rusher_id, posteam_type, shotgun, no_huddle,
         pass_location, air_yards, play_type, run_location, run_gap, side_of_field,
         yardline_100, half_seconds_remaining, game_seconds_remaining, game_half,
         goal_to_go, qb_scramble, yards_after_catch,down, ydstogo, interception,
         qb_hit, sack, touchdown, fumble, roof, surface, temp, wind, complete_pass, epa)

plays_2020.df <- decode_player_ids(plays_2020.df)

## 2020 rosters
rosters_2020 <- fast_scraper_roster(seasons = "2020")

rosters_2020 <- rosters_2020 %>%
  dplyr::select(position, gsis_id)

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



# Run and pass strength
run_strength <- plays_2020_run_model %>%
  group_by(posteam) %>%
  summarize(run_strength = mean(epa, na.rm = T))

pass_strength <- plays_2020_pass_model %>%
  group_by(posteam) %>%
  summarize(pass_strength = mean(epa, na.rm = T))

plays_2020_pass_model <- plays_2020_pass_model %>%
  left_join(run_strength, by = "posteam") %>%
  left_join(pass_strength, by = "posteam")


# Completions vs. incompletions
plays_2020_pass_model.complete <- plays_2020_pass_model %>%
  filter(complete_pass == T)

plays_2020_pass_model.incomplete <- plays_2020_pass_model %>%
  filter(complete_pass == F)

 # Passing model
plays_2020_pass_model.final.complete <- plays_2020_pass_model.complete %>%
  dplyr::select(posteam_type, passer_id, receiver_id,shotgun, yardline_100,
                game_seconds_remaining, goal_to_go, ydstogo,
                touchdown, no_huddle, side_of_field, half_seconds_remaining,
                game_half, fumble, rec_position, air_yards, yards_after_catch, down, 
                pass_location, run_strength, pass_strength, defteam, qb_hit, epa) %>%
  filter(!is.na(epa),
         !is.na(down),
         !is.na(rec_position),
         !is.na(air_yards),
         !is.na(pass_location)) %>%
  mutate(yards_after_catch = ifelse(is.na(yards_after_catch) == T, 0, yards_after_catch))

plays_2020_pass_model.final.incomplete <- plays_2020_pass_model.incomplete %>%
  dplyr::select(posteam_type, passer_id, receiver_id,shotgun, yardline_100,
                game_seconds_remaining, goal_to_go, ydstogo,
                touchdown, no_huddle, side_of_field, half_seconds_remaining,
                game_half, fumble, rec_position, air_yards, yards_after_catch, down, 
                pass_location, run_strength, pass_strength, defteam, qb_hit, epa) %>%
  filter(!is.na(epa),
         !is.na(down),
         !is.na(rec_position),
         !is.na(air_yards),
         !is.na(pass_location)) %>%
  mutate(yards_after_catch = ifelse(is.na(yards_after_catch) == T, 0, yards_after_catch))

plays_2020_pass_model.final.all <- plays_2020_pass_model %>%
  dplyr::select(posteam_type, passer_id, receiver_id,shotgun, yardline_100,
                game_seconds_remaining, goal_to_go, ydstogo,
                touchdown, no_huddle, side_of_field, half_seconds_remaining,
                game_half, fumble, rec_position, air_yards, yards_after_catch, down, 
                pass_location, run_strength, pass_strength, defteam, qb_hit, epa) %>%
  filter(!is.na(epa),
         !is.na(down),
         !is.na(rec_position),
         !is.na(air_yards),
         !is.na(pass_location)) %>%
  mutate(yards_after_catch = ifelse(is.na(yards_after_catch) == T, 0, yards_after_catch))

# Modeling

### With wideouts

epa_all.2020 <- lmerTest::lmer(epa ~ posteam_type + shotgun + no_huddle + qb_hit +
                                pass_location  + run_strength +
                                (1|passer_id) + (1|receiver_id) + (1|defteam),
                              data = plays_2020_pass_model.final.all)

### Removing rec position elimiates singularity - but now the model is different
epa_no_rec <- lmerTest::lmer(epa ~ posteam_type + shotgun + no_huddle + qb_hit +
                                pass_location  + run_strength +  (1|passer_id) + 
                              + (1|defteam),
                              data = plays_2020_pass_model.final.all)

### Passers
passer_iPA.all <- as.data.frame(coef(epa_all.2020)[2]) %>%
  rownames_to_column(var = "passer_id") %>%
  dplyr::select(passer_id, passer_id..Intercept.) %>%
  rename(EPA_with_rec = passer_id..Intercept.)

passer_iPA.norec <- as.data.frame(coef(epa_no_rec)[1]) %>%
  rownames_to_column(var = "passer_id") %>%
  dplyr::select(passer_id, passer_id..Intercept.) %>%
  rename(EPA_no_rec = passer_id..Intercept.)

passer_iPA <- left_join(passer_iPA.all, passer_iPA.norec) %>%
  mutate(split = EPA_with_rec - EPA_no_rec)

### Receivers

# Adding some names in there
pass_attempts <- plays_2020_pass_model %>%
  group_by(passer_id) %>%
  summarize(attempts = n())

targets <- plays_2020_pass_model %>%
  group_by(receiver_id) %>%
  summarize(targets = n())

rosters_2020_names <- fast_scraper_roster(seasons = "2020") %>%
  dplyr::select(team, position, full_name, gsis_id)

## Passers
passer_iPA.2 <- merge(passer_iPA.all, rosters_2020_names, 
                    by.x = "passer_id", by.y = "gsis_id",
                    all.x = T, all.y = F) %>%
  left_join(pass_attempts, by = "passer_id") %>%
  filter(attempts > 100)

## Receivers
receiver_iPA <- as.data.frame(coef(epa_all)[1]) %>%
  rownames_to_column(var = "receiver_id") %>%
  dplyr::select(receiver_id, receiver_id..Intercept.) %>%
  rename(EPA_rec = receiver_id..Intercept.)

receiver_iPA_2 <- merge(receiver_iPA, rosters_2020_names,
                        by.x = "receiver_id", by.y = "gsis_id",
                        all.x = T, all.y = F) %>%
  left_join(targets, by = "receiver_id") %>%
  arrange(desc(EPA_rec))


## Passers by team
passer_byteam <- passer_iPA.2 %>%
  mutate(total_iPA_w_rec = EPA_with_rec * attempts) %>%
  group_by(team) %>%
  summarize(total_iPA_w_rec = sum(total_iPA_w_rec),
            total_attempts = sum(attempts)) %>%
  mutate(team_iPA_avg = total_iPA_w_rec/total_attempts)

receivers_IPA_2020 <- receiver_iPA_2 %>%
  left_join(passer_byteam, by = "team") %>%
  dplyr::select(receiver_id, position, EPA_rec, team, full_name, targets, total_attempts,
         team_iPA_avg) %>%
  mutate(year = "2020",
         relative_IPA = EPA_rec/team_iPA_avg,
         total_IPA = EPA_rec*targets) %>%
  arrange(desc(relative_IPA))

## Tim Patrick - wtf. Sanity check
plays2020_qb_epa <- plays_2020_pass_model %>%
  group_by(posteam) %>%
  summarize(avg_EPA_passer = mean(epa),
            attempts = n()) 

plays2020_wr_epa <- plays_2020_pass_model %>%
  group_by(receiver, posteam, rec_position) %>%
  summarize(avg_EPA_receiver = mean(epa),
            targets = n()) %>%
  left_join(plays2020_qb_epa, by = "posteam") %>%
  mutate(relative_EPA = sqrt(avg_EPA_receiver^2/avg_EPA_passer^2)) %>%
  filter(targets >= 50) %>%
  arrange(desc(relative_EPA))

# Let's do some receiver clustering and train the QB model that way.