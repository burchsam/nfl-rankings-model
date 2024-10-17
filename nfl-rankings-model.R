library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(gt)
library(lmtest)
library(car)
library(glmnet)
library(kknn)
library(randomForestSRC)
library(xgboost)



# 2024 Ranking Model Projection -------------------------------------------

## Uses regular season data
## Add in postseason later
pbp = read_csv("coding-projects/nfl-fast-r/pbp-df-thru23")[, -1]


## Updating with Weekly data


pbp_raw = load_pbp(1999:2024)

### CHANGE
current_week = 6


off_wk = pbp_raw |>
  ### CHANGE!!!
  filter(!is.na(yards_gained), (pass == 1 | rush == 1), week <= current_week) |>
  mutate(season = substr(game_id, 1, 4)) |>
  group_by(season, posteam) |>
  summarise(mean_epa_wk = mean(epa, na.rm = TRUE),
            success_rate_wk = mean(success, na.rm = TRUE),
            ypa_wk = mean(yards_gained, na.rm = TRUE),
            pass_epa_wk = mean(epa[pass == 1], na.rm = TRUE),
            pass_sr_wk = mean(success[pass == 1], na.rm = TRUE),
            rush_epa_wk = mean(epa[rush == 1], na.rm = TRUE),
            rush_sr_wk = mean(success[rush == 1], na.rm = TRUE),
            ed_epa_wk = mean(epa[down == 1 | down == 2], na.rm = TRUE),
            ed_sr_wk = mean(success[down == 1 | down == 2], na.rm = TRUE),
            ld_epa_wk = mean(epa[down == 3 | down == 4], na.rm = TRUE),
            ld_sr_wk = mean(success[down == 3 | down == 4], na.rm = TRUE),
            ypa_wk = mean(yards_gained),
            exp_play_wk = sum((pass == 1 & yards_gained >= 20) | (rush == 1 & yards_gained >= 10)) / n(),
            # Drive Killers
            to_rate_wk = sum(interception == 1 | fumble_forced == 1 | sack == 1) / n(),
            # proe_wk = mean(pass_oe, na.rm = TRUE),
            
            plays = n(),
            .groups = "drop") |>
  rename(team = posteam) |>
  arrange(-mean_epa_wk) |>
  select(-plays)

def_wk = pbp_raw |>
  ### CHANGE!!!
  filter(!is.na(yards_gained), (pass == 1 | rush == 1), week <= current_week) |>
  mutate(season = substr(game_id, 1, 4)) |>
  group_by(season, defteam) |>
  summarise(mean_epa_allowed_wk = mean(epa, na.rm = TRUE),
            success_rate_allowed_wk = mean(success, na.rm = TRUE),
            ypa_allowed_wk = mean(yards_gained, na.rm = TRUE),
            pass_epa_allowed_wk = mean(epa[pass == 1], na.rm = TRUE),
            pass_sr_allowed_wk = mean(success[pass == 1], na.rm = TRUE),
            rush_epa_allowed_wk = mean(epa[rush == 1], na.rm = TRUE),
            rush_sr_allowed_wk = mean(success[rush == 1], na.rm = TRUE),
            ed_epa_allowed_wk = mean(epa[down == 1 | down == 2], na.rm = TRUE),
            ed_sr_allowed_wk = mean(success[down == 1 | down == 2], na.rm = TRUE),
            ld_epa_allowed_wk = mean(epa[down == 3 | down == 4], na.rm = TRUE),
            ld_sr_allowed_wk = mean(success[down == 3 | down == 4], na.rm = TRUE),
            ypa_allowed_wk = mean(yards_gained),
            exp_play_allowed_wk = sum((pass == 1 & yards_gained >= 20) | (rush == 1 & yards_gained >= 10)) / n(),
            # DKs
            to_forced_rate_wk = sum(interception == 1 | fumble_forced == 1 | sack == 1) / n(),
            # proe_forced_wk = mean(pass_oe, na.rm = TRUE),
            
            plays = n(),
            .groups = "drop") |>
  rename(team = defteam) |>
  arrange(mean_epa_allowed_wk) |>
  select(-plays)

# Change Name to wk_df
wk_df = off_wk |>
  left_join(def_wk, by = c("season", "team")) |>
  mutate(season = as.double(season)) |>
  group_by(team) |>
  arrange(season) |>
  mutate(lead(across(mean_epa_wk: to_forced_rate_wk))) |>
  ungroup() |>
  arrange(season, team)


## SOS-adjustment
games = load_schedules()



avg_def_epa_wk = wk_df |> filter(season != 2024) |> group_by(season) |> 
  summarise(mean_epa_allowed_season_wk = mean(mean_epa_allowed_wk))
avg_off_epa_wk = wk_df |> filter(season != 2024) |> group_by(season) |> 
  summarise(mean_epa_season_wk = mean(mean_epa_wk))


off_epa_wk = wk_df |> filter(season != 2024) |> 
  select(season, team, mean_epa_wk) |> arrange(season, team)
def_epa_wk = wk_df |> filter(season != 2024) |> 
  select(season, team, mean_epa_allowed_wk) |> arrange(season, team)

seasons_raw_wk = wk_df |> filter(season != 2024) |> 
  select(season) |> pull()
teams_raw_wk = wk_df |> filter(season != 2024) |> select(team) |> pull()



off_adj_raw_wk = numeric(797)

for (i in 1:797) { 
  off_adj_raw_wk[i] = games |> 
    mutate(season = season - 1) |> 
    ### CHANGE!!!
    filter(season == seasons_raw_wk[i], !is.na(result), week <= current_week) |> 
    select(season, home_team, away_team) |> 
    mutate(home_team = clean_team_abbrs(home_team),
           away_team = clean_team_abbrs(away_team)) |> 
    filter(home_team == teams_raw_wk[i] | away_team == teams_raw_wk[i]) |> 
    mutate(opp_team = if_else(home_team == teams_raw_wk[i], away_team, home_team)) |> 
    left_join(def_epa_wk, by = c("opp_team" = "team", "season")) |> 
    # Fix issue with 0'2 HOU
    summarise(mean(mean_epa_allowed_wk, na.rm = TRUE))
}

off_adj_raw_wk |> as_vector()




def_adj_raw_wk = numeric(797)

for (i in 1:797) { 
  def_adj_raw_wk[i] = games |> 
    mutate(season = season - 1) |> 
    ### CHANGE!!!
    filter(season == seasons_raw_wk[i], !is.na(result), week <= current_week) |> 
    select(season, home_team, away_team) |> 
    mutate(home_team = clean_team_abbrs(home_team),
           away_team = clean_team_abbrs(away_team)) |> 
    filter(home_team == teams_raw_wk[i] | away_team == teams_raw_wk[i]) |> 
    mutate(opp_team = if_else(home_team == teams_raw_wk[i], away_team, home_team)) |> 
    left_join(off_epa_wk, by = c("opp_team" = "team", "season")) |> 
    # Fix issue with 0'2 HOU
    summarise(mean(mean_epa_wk, na.rm = TRUE))
}


def_adj_raw_wk |> as_vector()




## Full Season Data

# pbp = load_pbp(1999:2023)
# offs = pbp |>
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1), !is.na(posteam)) |>
#   group_by(season, posteam) |>
#   summarise(mean_epa = mean(epa, na.rm = TRUE),
#             success_rate = mean(success, na.rm = TRUE),
#             pass_epa = mean(epa[pass == 1], na.rm = TRUE),
#             pass_sr = mean(success[pass == 1], na.rm = TRUE),
#             rush_epa = mean(epa[rush == 1], na.rm = TRUE),
#             rush_sr = mean(success[rush == 1], na.rm = TRUE),
#             ed_epa = mean(epa[down == 1 | down == 2], na.rm = TRUE),
#             ed_sr = mean(success[down == 1 | down == 2], na.rm = TRUE),
#             ld_epa = mean(epa[down == 3 | down == 4], na.rm = TRUE),
#             ld_sr = mean(success[down == 3 | down == 4], na.rm = TRUE),
#             ypa = mean(yards_gained),
#             plays_off = n(),
#             exp_play = sum((pass == 1 & yards_gained >= 20) | (rush == 1 & yards_gained >= 10)) / plays_off,
#             to_rate = sum(interception == 1 | fumble == 1 | sack == 1) / plays_off,
#             .groups = "drop") |>
#   arrange(-mean_epa) |>
#   mutate(mean_epa_pct = pnorm(scale(mean_epa))*100,
#          success_rate_pct = pnorm(scale(success_rate))*100,
#          pass_epa_pct = pnorm(scale(pass_epa))*100,
#          pass_sr_pct = pnorm(scale(pass_sr))*100,
#          rush_epa_pct = pnorm(scale(rush_epa))*100,
#          rush_sr_pct = pnorm(scale(rush_sr))*100,
#          ed_epa_pct = pnorm(scale(ed_epa))*100,
#          ed_sr_pct = pnorm(scale(ed_sr))*100,
#          ld_epa_pct = pnorm(scale(ld_epa))*100,
#          ld_sr_pct = pnorm(scale(ld_sr))*100,
#          exp_play_pct = pnorm(scale(exp_play))*100,
#          to_rate_pct = pnorm(scale(to_rate))*100,
#          ypa_pct = pnorm(scale(ypa))*100,
#          plays_off_pct = pnorm(scale(plays_off))*100) |>
#   rename(team = posteam)
# 
# 
# 
# defs = pbp |>
#   filter(!is.na(yards_gained), (pass == 1 | rush == 1), !is.na(defteam)) |>
#   group_by(season, defteam) |>
#   summarise(mean_epa_allowed = mean(epa, na.rm = TRUE),
#             success_rate_allowed = mean(success, na.rm = TRUE),
#             pass_epa_allowed = mean(epa[pass == 1], na.rm = TRUE),
#             pass_sr_allowed = mean(success[pass == 1], na.rm = TRUE),
#             rush_epa_allowed = mean(epa[rush == 1], na.rm = TRUE),
#             rush_sr_allowed = mean(success[rush == 1], na.rm = TRUE),
#             ed_epa_allowed = mean(epa[down == 1 | down == 2], na.rm = TRUE),
#             ed_sr_allowed = mean(success[down == 1 | down == 2], na.rm = TRUE),
#             ld_epa_allowed = mean(epa[down == 3 | down == 4], na.rm = TRUE),
#             ld_sr_allowed = mean(success[down == 3 | down == 4], na.rm = TRUE),
#             ypa_allowed = mean(yards_gained),
#             plays_def = n(),
#             exp_play_allowed = sum((pass == 1 & yards_gained >= 20) | (rush == 1 & yards_gained >= 10)) / plays_def,
#             to_forced_rate = sum(interception == 1 | fumble == 1 | sack == 1) / plays_def,
#             .groups = "drop") |>
#   arrange(-mean_epa_allowed) |>
#   mutate(mean_epa_allowed_pct = pnorm(scale(mean_epa_allowed))*100,
#          success_rate_allowed_pct = pnorm(scale(success_rate_allowed))*100,
#          pass_epa_allowed_pct = pnorm(scale(pass_epa_allowed))*100,
#          pass_sr_allowed_pct = pnorm(scale(pass_sr_allowed))*100,
#          rush_epa_allowed_pct = pnorm(scale(rush_epa_allowed))*100,
#          rush_sr_allowed_pct = pnorm(scale(rush_sr_allowed))*100,
#          ed_epa_allowed_pct = pnorm(scale(ed_epa_allowed))*100,
#          ed_sr_allowed_pct = pnorm(scale(ed_sr_allowed))*100,
#          ld_epa_allowed_pct = pnorm(scale(ld_epa_allowed))*100,
#          ld_sr_allowed_pct = pnorm(scale(ld_sr_allowed))*100,
#          exp_play_allowed_pct = pnorm(scale(exp_play_allowed))*100,
#          to_forced_rate_pct = pnorm(scale(to_forced_rate))*100,
#          ypa_allowed_pct = pnorm(scale(ypa_allowed))*100,
#          plays_def_pct = pnorm(scale(plays_def))*100) |>
#   rename(team = defteam)
# 
# 
# final_df = offs |>
#   left_join(defs, by = c("season", "team")) |>
#   mutate(tot_plays = plays_off + plays_def)
# 
# write.csv(final_df, file = "pbp-df-thru23")


## Draft Imput

draft_df = read_csv("coding-projects/nfl-fast-r/draft_value_99")[, -1]


## FA Input

fa_df = read_csv("coding-projects/nfl-fast-r/offeseason-player-value-changes.csv")[, -1]


## Injury Input
### Update!!!
injury_df = read_csv("coding-projects/nfl-fast-r/injury-value-lost.csv")[, -1]


## TOs

to_df = read_csv("coding-projects/nfl-fast-r/turnover-value.csv")[, -1]






# Full Dataset
full_df = pbp |> 
  # SOS
  ## Season
  mutate(off_sos = off_adj_raw |> as_vector() |> as.double(),
         def_sos = def_adj_raw |> as_vector()) |> 
  left_join(avg_off_epa, by = "season") |> 
  left_join(avg_def_epa, by = "season") |> 
  ## Weekly
  mutate(off_sos_wk = off_adj_raw_wk |> as_vector() |> as.double(),
         def_sos_wk = def_adj_raw_wk |> as_vector()) |> 
  left_join(avg_off_epa_wk, by = "season") |> 
  left_join(avg_def_epa_wk, by = "season") |> 
  
  
  mutate(adj_mean_epa = mean_epa - (off_sos - mean_epa_allowed_season),
         adj_mean_epa_allowed = mean_epa_allowed - (def_sos - mean_epa_season)) |> 
  group_by(season) |> 
  mutate(almost = .6*scale(adj_mean_epa) - .4*scale(adj_mean_epa_allowed)) |>
  #Maybe leave as z-score in future
  mutate(ranking = scale(almost) *
           (20 / (max(scale(almost)) - min(scale(almost))))) |>
  ungroup() |> 
  group_by(team) |> 
  arrange(season) |> 
  mutate(next_ranking = case_when(
    (lead(team) == team) ~ lead(ranking),
    .default = NA
  )) |> 
  ungroup() |> 
  dplyr::select(-almost, 
                -c(mean_epa_pct:plays_off_pct), 
                -c(mean_epa_allowed_pct:plays_def_pct)) |> 
  
  ## Adding in weekly data
  left_join(wk_df, by = c("season", "team")) |>
  # filter(!is.na(ypa_allowed_wk), !is.na(off_sos_wk)) |>
  mutate(adj_mean_epa_wk = mean_epa_wk - (off_sos_wk - mean_epa_allowed_season_wk),
         adj_mean_epa_allowed_wk = mean_epa_allowed_wk - (def_sos_wk - mean_epa_season_wk)) |>
  mutate(raw_ranking_wk = .6*scale(mean_epa_wk) - .4*scale(mean_epa_allowed_wk)) |>
  mutate(ranking_wk = .6*scale(adj_mean_epa_wk) - .4*scale(adj_mean_epa_allowed_wk)) |>
  
  
  ## DRAFT
  left_join(draft_df, by = c("team", "season")) |>
  
  ## TOs
  left_join(to_df, by = c("team", "season")) |>
  
  ## FA MOVES
  left_join(fa_df, by = c("team", "season")) |>
  mutate(value_added = if_else(is.na(value_added), 0, value_added),
         value_lost = if_else(is.na(value_lost), 0, value_lost)) |>
  mutate(net_value = value_added - value_lost) |>
  
  ## Injuries
  left_join(injury_df, by = c("team", "season")) |>
  mutate(tot_injured_value_lost =
           if_else(is.na(tot_injured_value_lost), 0, tot_injured_value_lost)) |>
  
  arrange(season, team)




# For building the model
full_df2 = full_df |>
  filter(!is.na(next_ranking)) |> 
  dplyr::select(-season, - team)


# Correlations
cor_tbl = cor(full_df2) |>
  as_tibble() |> 
  mutate(names = colnames(full_df2)) |>
  filter(next_ranking != 1) |>
  dplyr::select(names, next_ranking) |>
  arrange(-abs(next_ranking)) |>
  print(n = 100) 

ggplot(cor_tbl |> filter(next_ranking >= .3), aes(x = abs(next_ranking), y = reorder(names, abs(next_ranking)))) +
  geom_col() +
  theme_minimal()


## ALL Correlations

cors_all = full_df |> 
  select(-next_ranking) |> 
  pivot_longer(
    cols = mean_epa:tot_injured_value_lost, 
    names_to = "metric", 
    values_to = "value") |> 
  group_by(team, metric) |> 
  arrange(season) |> 
  mutate(lead_value = lead(value), next_season = lead(season)) %>%
  ungroup() |> 
  filter(!is.na(lead_value)) |> 
  group_by(metric)  |> 
  summarise(correlation = cor(value, lead_value, use = "complete.obs"))  |> 
  ungroup() |> 
  arrange(-correlation)



cors_all |> View()

off_metrics = pbp |> select(mean_epa:to_rate) |> colnames()
def_metrics = pbp |> select(mean_epa_allowed:to_forced_rate) |> colnames()
sr_metrics = pbp |> select(ends_with("sr"), ends_with("sr_allowed"), success_rate, success_rate_allowed) |> colnames()
mean_epa_metrics = pbp |> select(ends_with("epa"), ends_with("epa_allowed")) |> colnames()

cors_all |> filter(metric %in% def_metrics) |> summarise(mean(correlation))
cors_all |> filter(metric %in% off_metrics) |> summarise(mean(correlation))
cors_all |> filter(metric %in% sr_metrics) |> summarise(mean(correlation))
cors_all |> filter(metric %in% mean_epa_metrics) |> summarise(mean(correlation))

cors_all |> filter(metric == "mean_epa" | metric == "mean_epa_allowed") |> summarise(mean(correlation))
cors_all |> filter(metric == "success_rate" | metric == "success_rate_allowed") |> summarise(mean(correlation))
cors_all |> filter(metric == "ypa" | metric == "ypa_allowed") |> summarise(mean(correlation))



# Modeling ----------------------------------------------------------------


# Train/Test




n = nrow(full_df2)


set.seed(123)


train_ind = sample(1:n, .7*n)

train_data = full_df2[train_ind, ]
test_data = full_df2[-train_ind, ]


# PCA



pca_result = prcomp(train_data |> select(-next_ranking), scale. = TRUE)


plot(pca_result)


pca_data = data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2], 
  Ranking = train_data |> select(next_ranking) |> pull())

# Plot the data on the first two principal components, colored by the digit label
ggplot(pca_data, aes(x = PC1, y = PC2, color = Ranking > 5)) +
  geom_point() +
  labs(
    # title = "PCA of MNIST Data (Digits 1 and 2)",
    x = "First Principal Component",
    y = "Second Principal Component",
    color = "Ranking"
  ) +
  theme_minimal()
## Not good enough explanation


# KMeans

kmeans_result = kmeans(train_data |> select(-next_ranking), centers = 5)

kmeans_result$withinss


# Average

(avg_rmse = sqrt(mean(((train_data |> select(next_ranking) |> pull() |> mean()) - (test_data |> select(next_ranking) |> pull()))^2)))
## Off by 4.75 using average


# Linear Regression


## Model 1

m1a = lm(next_ranking ~ 1, data = train_data)

m1 = lm(next_ranking ~ ., data = train_data)
summary(m1)
### CHANGE!!!
m1_r2 = 0.64


pred = predict(m1, newdata = test_data)

(m1_rmse = sqrt(mean((pred - test_data |> select(next_ranking) |> pull())^2)))
# Off by 3.62




## Model 2
m2 = step(m1a, direction = "both", scope = formula(m1), trace = 0)
summary(m2)
###CHANGE!!!
m2_r2 = 0.65

pred = predict(m2, newdata = test_data)
(m2_rmse = sqrt(mean((pred - test_data |> select(next_ranking) |> pull())^2)))
# Off by 3.32

sqrt(vif(m2))
# Yes

par(mfrow = c(2, 2))
plot(m2)

## HIPs
sort(cooks.distance(m2), decreasing = TRUE)[1:10]
### All good, since less than 1.

## Constant Variance
bptest(m2)
### With the p-value = 0.06 > .05 = $\alpha$, we fail reject the null and conclude the constant variance assumption is satisfied.

## Normaility, n < 50
shapiro.test(m2$residuals)
### With the p-value = 0.92 > .05 = $\alpha$, we fail to reject the null and conclude the constant variance assumption is satisfied.

### CHANGE!!!
m2_diagnostics = "Yes"




m3a = lm(next_ranking ~ 1
         , data = train_data |> select(ranking, next_ranking, ends_with("wk")))

# Model 3
m3b = lm(next_ranking ~ .
        , data = train_data |> select(ranking, next_ranking, ends_with("wk")))
summary(m3b)
## r^2 = 0.62

m3 = step(m3a, direction = "both", scope = formula(m3b))
summary(m3)
### CHANGE!!!
m3_r2 = 0.62



pred = predict(m3, newdata = test_data)
(m3_rmse = sqrt(mean((pred - test_data |> select(next_ranking) |> pull())^2)))
# Off by 3.04

par(mfrow = c(2, 2))
plot(m3)

sqrt(vif(m3))
## OK!

## HIPs
sort(cooks.distance(m3), decreasing = TRUE)[1:10]
### All good, since less than 1.

## Constant Variance
bptest(m3)
### With the p-value = 0.86 > .05 = $\alpha$, we fail to reject the null and conclude the constant variance assumption is satisfied.

## Normaility, n < 50
shapiro.test(m3$residuals)
### With the p-value = 0.87 > .05 = $\alpha$, we fail to reject the null and conclude the constant variance assumption is satisfied.

### CHANGE!!!
m3_diagnostics = "Yes"




# Model 4
m4 = lm(next_ranking ~ ranking + tot_injured_value_lost +
          net_to_epa_tot + draft_value_added + net_value
        , data = train_data)
summary(m4)
# r^2 = 0.24


# Model 5
m5 = step(m4, trace = 0)
summary(m5)
m5_r2 = 0.24

pred = predict(m5, newdata = test_data)
(m5_rmse = sqrt(mean((pred - test_data |> select(next_ranking) |> pull())^2)))
## RMSE = 4.07


par(mfrow = c(2, 2))
plot(m5)

sqrt(vif(m5))






## LASSO

set.seed(123)

x_train = as.matrix(train_data |> select(-next_ranking))
y_train = as.matrix(train_data$next_ranking)

x_test = as.matrix(test_data |> select(-next_ranking))
y_test = as.matrix(test_data$next_ranking)


lasso_model = glmnet(x_train, y_train, alpha = 1)

# Perform cross-validation to select lambda
cv_lasso = cv.glmnet(x_train, y_train, alpha = 1)  # alpha = 1 for Lasso regression
par(mfrow = c(1,1))
plot(cv_lasso)

# Print optimal lambda value
print(cv_lasso$lambda.min)

coefficients = coef(lasso_model, s = cv_lasso$lambda.min)
coefficients


# Example predictions
pred = predict(lasso_model, newx = x_test, s = cv_lasso$lambda.min)

(lasso_rmse = sqrt(mean((pred - y_test)^2)))
# Off by 3.12


# RIDGE
set.seed(123)


ridge_model = cv.glmnet(x_train, y_train, alpha = 0)

plot(ridge_model)

best_lambda = ridge_model$lambda.min

final_model = glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)

final_model$beta[, 1] |> abs() |> sort(decreasing = TRUE)

pred = predict(final_model, newx = x_test)

(ridge_rmse = sqrt(mean((pred - y_test)^2)))
# Off by 3.23



# KNN

error = numeric(100)

set.seed(123)

for (i in 1:100) {
  
  knn.fit = kknn(next_ranking ~ ., train = train_data, 
                 test = test_data |> dplyr::select(-next_ranking),
                 k = i, kernel = "rectangular")
  
  test.pred = knn.fit$fitted.values
  
  error[i] = sqrt(mean((test.pred - (test_data |> dplyr::select(next_ranking) |> pull()))^2))
  
  
}

(knn_rmse = min(error))
# 3.32

which.min(error)



# Random Forrest


set.seed(123)


# train_data$qbr_nfl
m1 = rfsrc(next_ranking ~ ., data = as.data.frame(train_data))
# OOB Error Rate
tail(m1$err.rate, 1)


tuning_grid = expand.grid(mtry = c(1, 5, 10, 15, 20), nodesize = c(1, 5, 10, 15, 20))
tuned_models = vector(mode = "list", length = 25)
oob_error_rates = numeric(25)
set.seed(123)
for (i in 1:nrow(tuning_grid)) {
  rf_model = rfsrc(next_ranking ~ ., data = as.data.frame(train_data),
                   mtry = tuning_grid[i, 1], nodesize = tuning_grid[i, 2])
  tuned_models[[i]] = rf_model
  oob_error_rates[i] = tail(rf_model$err.rate, 1)
}
# OOB ER for each model
oob_error_rates


# Find the index of the minimum OOB error rate
best_index = which.min(oob_error_rates)
# Best tuning parameters
best_tuning = tuning_grid[best_index, ]
best_tuning

# Extract the random forest model with the best tuning parameters
best_rf_model = tuned_models[[best_index]]
best_rf_model
###CHANGE!!!
rf_r2 = 0.60


# Calculate the variable importance for the best model
variable_importance = vimp(best_rf_model)
sort(abs(variable_importance$importance), decreasing = TRUE)


# rf_weights = variable_importance$importance |> 
#   as_tibble() |> 
#   rename(weight = value) |> 
#   mutate(names = variable_importance$xvar.names) |> 
#   select(names, weight) |> 
#   arrange(-weight)
# 
# ggplot(rf_weights |> filter(weight >= 1), aes(x = weight, y = reorder(names, weight))) +
#   geom_col() +
#   theme_minimal()


pred_rf = predict(best_rf_model, newdata = as.data.frame(test_data))

(rf_rmse = sqrt(mean((pred_rf$predicted - as.vector(y_test))^2)))
# 3.16




### XGBoost

x_train = as.matrix(train_data |> select(-next_ranking))
y_train = as.matrix(train_data$next_ranking)
x_test = as.matrix(test_data |> select(-next_ranking))
y_test = as.matrix(test_data$next_ranking)



set.seed(123)

train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-next_ranking)), label = train_data$next_ranking)
test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-next_ranking)), label = test_data$next_ranking)

params = list(
  objective = "reg:squarederror",
  # num_class = 10, # Number of classes
  eta = 0.5, # Learning rate
  max_depth = 2 # Maximum depth of trees
)

num_round = 50


xgb.fit = xgb.train(params, train_data_xgb, num_round)


pred_xgb = predict(xgb.fit, newdata = test_data_xgb)

sqrt(mean((pred_xgb - as.vector(y_test))^2))
# Ok RMSE

tuning_grid2 = expand.grid(eta = c(0.1, 0.5, 1.0), max_depth = c(2, 5, 10))


best_eta = c(numeric(9))
best_max_depth = c(numeric(9))
best_ntrees = c(numeric(9))
best_error = c(rep(Inf, 9))

set.seed(123)




for (i in 1:nrow(tuning_grid2)) {
  # Set the xgboost parameters
  params = list(
    objective = "reg:squarederror",
    # num_class = 10,
    eta = tuning_grid2[i, 1],
    max_depth = tuning_grid2[i, 2]
  )
  
  train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-next_ranking)), label = train_data$next_ranking)
  test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-next_ranking)), label = test_data$next_ranking)
  
  num_round = 50
  
  bst = xgb.train(params, train_data_xgb, num_round)
  
  for (j in 1:50) {
    pred_xgb = predict(bst, test_data_xgb, iterationrange = c(1, j))
    
    # sqrt(mean((pred_xgb - as.vector(y_test))^2))
    # Calculate the testing error
    error = sqrt(mean((pred_xgb - as.vector(y_test))^2))
    
    # Update the best parameters and the corresponding testing error
    if (error < best_error[i]) {
      best_eta[i] = tuning_grid2[i, 1]
      best_max_depth[i] = tuning_grid2[i, 2]
      best_ntrees[i] = j
      best_error[i] = error
    }
    
  }
}

(results = data.frame(best_eta, best_max_depth, best_ntrees, best_error))

(xgb_rmse = results[which.min(best_error), 4])
# Off by 3.10

# set.seed(123)
# 
# train_data_xgb = xgb.DMatrix(data = data.matrix(train_data |> select(-next_ranking)), label = train_data$next_ranking)
# test_data_xgb = xgb.DMatrix(data = data.matrix(test_data |> select(-next_ranking)), label = test_data$next_ranking)
# 
# params = list(
#   objective = "reg:squarederror",
#   # num_class = 10, # Number of classes
#   eta = results[which.min(best_error), 1], # Learning rate
#   max_depth = results[which.min(best_error), 2] # Maximum depth of trees
# )
# 
# 
# xgb.fit = xgb.train(params, train_data_xgb, results[which.min(best_error), 3])
# 
# 
# pred_xgb = predict(xgb.fit, newdata = test_data_xgb)
# 
# sqrt(mean((pred_xgb - as.vector(y_test))^2))
# 
# 
# (xgb_var_importance = xgb.importance(colnames(train_data_xgb), model = xgb.fit))
# 
# xgb.plot.importance(xgb_var_importance)



# Model Selection



all_models = tibble(model = c("Average", "Full Linear Regression", 
                              "Reduced Linear Regression", "Rankings + 24' Stats", 
                              "Rankings + Key Traits", "Lasso", "Ridge", "KNN", 
                              "Random Forest", "XGBoost"),
                    rmse = c(avg_rmse, m1_rmse, m2_rmse, m3_rmse, m5_rmse, 
                             lasso_rmse, ridge_rmse, knn_rmse, rf_rmse, xgb_rmse),
                    r_2= c(NA, m1_r2, m2_r2, m3_r2, m5_r2, NA, NA, NA, rf_r2, NA),
                    ### CHANGE!!!
                    diagnostics = c(NA, "No", m2_diagnostics, m3_diagnostics, 
                                    "Yes*", "Yes", "Yes", "Yes", "Yes", "Yes")) |> 
  arrange(rmse)

all_models


# 2024 Predictions --------------------------------------------------------


# Already played teams...
## Uses ranking-24 file
# played_teams = current_week_off |> select(team) |> pull()


teams_23 = full_df |> 
  filter(season == 2023
         # , team != "BUF", team != "NYJ"
         # , team %in% played_teams
         )



# Selected model
pred_24 = predict(m3, newdata = teams_23)

(pred_24_vec = as.vector(pred_24))


teams_24 = teams_23 |>
  mutate(next_ranking = pred_24_vec) |> 
  select(season, team, next_ranking, everything()) |> 
  arrange(-next_ranking)

teams_24 |>
  select(team, next_ranking) |>
  mutate(next_ranking = round(next_ranking, 1)) |>
  print(n = 32)

# write.csv(teams_24, file = "team-strength-projections-24.csv")

summary(m3)

ggplot(teams_24, aes(y = reorder(team, next_ranking), x = next_ranking)) +
  labs(
    ### CHANGE!!! (2)
    title = 'NFL Team Strength Projections (Week 7)',
    subtitle = "team strength defined by 60-40 split of off and def adjusted efficiency  |  linear regression based on 23' & 24' efficiency",
    caption = 'By: Sam Burch  |  Data @nflfastR',
    x = "Projected Spread against an Average Team"
  ) +
  theme(
    plot.subtitle = element_text(size = 6, hjust = .5),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
    panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
    panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
    panel.grid.minor.x = element_blank(),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
  scale_x_continuous(breaks = seq(-20, 20, 1)) +
  geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
  nflplotR::scale_fill_nfl(type = "primary") +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)

# ggsave("nfl-power-rankings-24.png", width = 16, height = 12, units = "cm")




# ggplot(teams_24, aes(y = reorder(team, raw_ranking_wk), x = raw_ranking_wk)) +
#   labs(
#     ### CHANGE!!! (2)
#     title = 'Raw NFL Team Strength Rankings',
#     subtitle = "thru wk6  |  team strength defined by 60-40 split of off and def adjusted efficiency",
#     caption = 'By: Sam Burch  |  Data @nflfastR',
#     x = "Projected Spread against an Average Team"
#   ) +
#   theme(
#     plot.subtitle = element_text(size = 6, hjust = .5),
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
#     panel.grid.minor.y = element_blank(),  # Remove horizontal minor grid lines
#     panel.grid.major.x = element_line(color = "lightgray", size = 0.5, linetype = 2),  # Customize vertical major grid lines
#     panel.grid.minor.x = element_blank(),
#     panel.background = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank()) +
#   scale_x_continuous(breaks = seq(-2, 2, .25)) +
#   geom_col(aes(color = team, fill = team), alpha = .8, width = 1) +
#   nflplotR::scale_fill_nfl(type = "primary") +
#   nflplotR::scale_color_nfl(type = "secondary") +
#   nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03, alpha = .8)







# SOS ---------------------------------------------------------------------




ggplot(teams_24, aes(x = off_sos_wk, y = def_sos_wk)) +
  labs(x = "Offensive SOS",
       y = "Defensive SOS",
       title = "NFL Strength of Schedule",
       ### CHANGE!!!
       subtitle = "thru wk6  |  strength of schedule = average opponents' 24' efficiency  |  top right = hard  |  bottom left = easy",
       caption = "By: Sam Burch  |  Data @nflfastR") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 6),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_reverse() +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = off_sos_wk, y0 = def_sos_wk))

# ggsave("team-sos.png", width = 16, height = 9, units = "cm")


# Offensive Performance

ggplot(teams_24, aes(x = adj_mean_epa_wk, y = success_rate_wk)) +
  labs(x = 'EPA / Play',
       y = 'Success Rate',
       title = "NFL Offensive Performances",
       ### CHANGE!!!
       subtitle = "thru wk6  |  efficiency adjusted for SOS",
       caption = 'By: Sam Burch  |  Data @nflfastR') +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = adj_mean_epa_wk, y0 = success_rate_wk))

# ggsave("off-adj-performance.png", width = 16, height = 9, units = "cm")



# Defensive Performance

ggplot(teams_24, aes(x = adj_mean_epa_allowed_wk, y = success_rate_allowed_wk)) +
  labs(x = 'EPA / Play Allowed',
       y = 'Success Rate Allowed',
       title = "NFL Defensive Performances",
       ### CHANGE!!!
       subtitle = "thru wk6  |  efficiency adjusted for SOS",
       caption = 'By: Sam Burch  |  Data @nflfastR') +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    axis.line = element_line(color = "black", size = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .07, alpha = .8) +
  stat_smooth(formula = y ~ x, method = 'lm', geom = 'line', se=FALSE, color='gray') +
  nflplotR::geom_mean_lines(aes(x0 = adj_mean_epa_allowed_wk, y0 = success_rate_allowed_wk))

# ggsave("def-adj-performance.png", width = 16, height = 9, units = "cm")





