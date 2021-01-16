# Package load
library(caret)
library(DBI)
library(DMwR)
library(ggrepel)
library(leaps)
library(lmerTest)
library(lmtest)
library(MASS)
library(Metrics)
library(merTools)
library(nflfastR)
library(RSQLite)
library(sandwich)
library(tidyverse)
library(rstudioapi)
library(xgboost)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Connect to database
update_db()

connection <- dbConnect(SQLite(), "./pbp_db")
connection

pbp_db <- tbl(connection, "nflfastR_pbp")

# 2016 through 2020 rushing data
rushing_all <- pbp_db %>%
  filter(play_type == "run") %>%
  mutate(era = ifelse(season %in% c(1999:2001), 1,
                      ifelse(season %in% c(2002:2005), 2,
                      ifelse(season %in% c(2006:2013), 3,
                      ifelse(season %in% c(2014:2017), 4, 5))))) %>%
  dplyr::select(rusher_id, posteam_type, posteam, defteam, yardline_100, half_seconds_remaining,
         game_half, down, ydstogo, era, shotgun, no_huddle, run_gap,
         score_differential, season, roof, yards_gained)

rushing_all.df <- as.data.frame(rushing_all)

rushing_all.df$run_gap <- ifelse(is.na(rushing_all.df$run_gap) == T, "Other",
                                 rushing_all.df$run_gap)

rushing_all.df <- rushing_all.df %>%
  mutate(posteam_type = as.factor(posteam_type),
         posteam = as.factor(posteam),
         defteam = as.factor(defteam),
         game_half = as.factor(game_half),
         down = as.factor(down),
         era = as.factor(era),
         shotgun = as.factor(shotgun),
         no_huddle = as.factor(no_huddle),
         run_gap = as.factor(run_gap),
         season = as.factor(season),
         roof = as.factor(roof)) %>%
  filter(!is.na(yardline_100),
         !is.na(down),
         !is.na(roof)) %>%
  unite(col = def_season, c("defteam", "season"), remove = F)

defensive_strength_run <- pbp_db %>%
  filter(play_type == "run") %>%
  group_by(defteam, season) %>%
  summarize(def_strength_run = -mean(epa))

defensive_strength_run <- as.data.frame(defensive_strength_run) %>%
  unite(col = def_season, c("defteam", "season"))

defensive_strength_pass <- pbp_db %>%
  filter(play_type == "pass") %>%
  group_by(defteam, season) %>%
  summarize(def_strength_pass = -mean(epa))

defensive_strength <- as.data.frame(defensive_strength_pass) %>%
  unite(col = def_season, c("defteam", "season")) %>%
  left_join(defensive_strength_run, by = "def_season")

rushing_all.df <- rushing_all.df %>%
  left_join(defensive_strength, by = "def_season")

# Training and test
set.seed(123)
trainIndex <- createDataPartition(rushing_all.df$yards_gained,
                                  p = 0.7,
                                  list = FALSE,
                                  times = 1)

rushing_train <- rushing_all.df[trainIndex,]
rushing_valid <- rushing_all.df[-trainIndex,]

# Initial modeling (xRY, expected rushing yards)
xRY_lm1 <- lm(asinh(yards_gained) ~ posteam_type + asinh(yardline_100) +
               down + ydstogo + shotgun + run_gap +
              asinh(score_differential) + roof + era + def_strength_pass +
                def_strength_run,
              data = rushing_train)

summary(xRY_lm1)                

# Checking out the predictions
rushing_valid <- rushing_valid %>%
  mutate(pred_1 = predict(xRY_lm1, rushing_valid),
         exp_pred_1 = exp(pred_1),
         over_pred = yards_gained - pred_1,
         asinh_yards_gained = asinh(yards_gained))

prediction_plot.1 <- ggplot(data = rushing_valid, 
                          aes(x = exp_pred_1, y = yards_gained)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

prediction_plot.1

# Metrics
pred_1_rmse <- RMSE(rushing_valid$exp_pred_1, rushing_valid$yards_gained)

# Heteroskedasticity check
bptest(xRY_lm1)

## We have heteroskedasticity.
## Lets get a HC covariance matrix
HC_coef <- coeftest(xRY_lm1, vcov. = vcovHC(xRY_lm1))

 # Model looks bad - but let's just see who the top performers were
rosters_all <- fast_scraper_roster(seasons = c(1999:2020))%>%
  select(season, gsis_id, full_name, position, height, weight) %>%
  arrange(season) %>%
  distinct(gsis_id, .keep_all = T) %>%
  rename(rookie_year = season)

rushing_all.df <- decode_player_ids(rushing_all.df) %>%
  mutate(pred1 = predict(xRY_lm1, rushing_all.df),
         exp_pred_1 = exp(pred1))

#rushing_all.df <- merge(rushing_all.df, rosters_all, 
          #              by.x = "rusher_id",
          #              by.y ="gsis_id",
          #              all.x = T)

rushers_rank<- rushing_all.df %>%
  group_by(rusher_id, season, posteam) %>%
  summarize(carries = n(),
            expected_YPA = mean(exp_pred_1),
            actual_YPA = mean(yards_gained),
            avg_diff = actual_YPA - expected_YPA) %>%
  mutate(expected_total_yards = carries * expected_YPA,
         actual_total_yards = carries * actual_YPA,
         total_diff = actual_total_yards - expected_total_yards) %>%
  filter(is.na(rusher_id) == F,
         carries >= 20) %>%
  left_join(rosters_all, by = c("rusher_id" = "gsis_id")) %>%
  mutate(years_in_league = as.numeric(season) - rookie_year)
