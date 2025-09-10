
library(survival)
library(survminer)
library(lubridate)
library(dplyr)
library(flexsurv)
library(splines)
library(ggplot2)
library(cmprsk) 
library(gridExtra)
install.packages("readr")  
library(readr)

rm(list=ls())
goal_data = read_csv("england-premier-league-matches-2018-to-2019-stats.csv")

str(goal_data)

# Creating event column
goal_data$event = ifelse(is.na(goal_data$first_goal_time), 0, 1)
# For right censoring
goal_data$first_goal_time[is.na(goal_data$first_goal_time)] = 90
hist(goal_data$first_goal_time)


# Overall survival
fit=survfit(Surv(goal_data$first_goal_time, goal_data$event) ~ 1)
ggsurvplot(fit, data=goal_data)

# Comparing Home and Away opening goals
fit=survfit(Surv(first_goal_time, event) ~ first_goal_Home., data=goal_data)
ggsurvplot(fit, data=goal_data)


# Adding time of day column
goal_data$time_of_day = format(mdy_hm(goal_data$date_GMT), "%H:%M")
head(goal_data$time_of_day)     

time_fit = survfit(Surv(first_goal_time, event) ~ time_of_day, data = goal_data)
ggsurvplot(time_fit, data=goal_data)
logrank_time = survdiff(Surv(first_goal_time, event) ~ time_of_day, data = goal_data)
print(logrank_time)

# Continuous time of day
goal_data$time_cont = as.numeric(sub(":", "", goal_data$time_of_day))
cox_time= coxph(Surv(first_goal_time, event)~ time_cont, data=goal_data)
summary(cox_time)

# Time as group

goal_data = goal_data %>%
  mutate(time_group = case_when( time_of_day %in% c("11:00", "11:30", "12:00", "12:30", "13:00", "13:05", "13:15", "13:30","14:00", "14:05", "14:15", "15:00", "15:30", "16:00", "16:15", "18:45", "19:00", "19:30", "19:45") ~ "Day Time",
                                 time_of_day %in% c("16:30","17:15", "17:30", "20:00", "20:30") ~ "Prime Time"))
time_fit = survfit(Surv(first_goal_time, event) ~ time_group, data = goal_data)
ggsurvplot(time_fit, data=goal_data)
logrank_time = survdiff(Surv(first_goal_time, event) ~ time_group, data = goal_data)
print(logrank_time)
# Adding day of week
goal_data$day_of_week = wday(mdy_hm(goal_data$date_GMT), label=TRUE, abbr=FALSE)                               
head(goal_data$day_of_week)
goal_data = goal_data %>%
  mutate(day_group = case_when( day_of_week %in% c("Saturday", "Sunday", "Monday") ~ "Weekend",
                                day_of_week == "Friday" ~ "Friday",
                                day_of_week %in% c("Tuesday", "Wednesday", "Thursday") ~ "Midweek"))

week_fit = survfit(Surv(first_goal_time, event) ~ day_of_week, data = goal_data)
ggsurvplot(week_fit, data=goal_data)
logrank_week = survdiff(Surv(first_goal_time, event) ~ day_of_week, data = goal_data)
print(logrank_week)

week_fit = survfit(Surv(first_goal_time, event) ~ day_group, data = goal_data)
ggsurvplot(week_fit, data=goal_data)
logrank_week = survdiff(Surv(first_goal_time, event) ~ day_group, data = goal_data)
print(logrank_week)

# Graph shows that matches played on Friday tend to have earlier goals

# Attendance
goal_data$attendance_percentage = goal_data$attendance/goal_data$stadium_capacity
head(goal_data$attendance_percentage)
cox_att= coxph(Surv(first_goal_time, event)~ attendance_percentage, data=goal_data)
summary(cox_att)

# Stadium Capacity - related to experience of home team
# Small stadiums = less risk of early goals
range(goal_data$stadium_capacity)
goal_data$stad_cap_group = ifelse(goal_data$stadium_capacity <25000, "Small", ifelse(goal_data$stadium_capacity <55000, "Mid","Large"))
stad_fit = survfit(Surv(first_goal_time, event) ~ stad_cap_group, data = goal_data)
ggsurvplot(stad_fit, data=goal_data)
logrank_stad = survdiff(Surv(first_goal_time, event) ~ stad_cap_group, data = goal_data)
print(logrank_stad)
# REF
ref_fit = survfit(Surv(first_goal_time, event) ~ referee, data = goal_data)
ggsurvplot(ref_fit, data=goal_data)
logrank_ref = survdiff(Surv(first_goal_time, event) ~ referee, data = goal_data)
print(logrank_ref)

goal_data$ref_exp = ave(goal_data$referee, goal_data$referee, FUN=length)
head(goal_data$ref_exp)
goal_data$ref_exp = as.numeric(goal_data$ref_exp)
table(goal_data$ref_exp)
cox_ref = coxph(Surv(first_goal_time, event) ~ ref_exp, data = goal_data)
summary(cox_ref)
# Group refs
goal_data$ref_group = ifelse(goal_data$ref_exp <=20, "Junior", "Experienced")
ref_fit = survfit(Surv(first_goal_time, event) ~ ref_group, data = goal_data)
ggsurvplot(ref_fit, data=goal_data)
logrank_ref = survdiff(Surv(first_goal_time, event) ~ ref_group, data = goal_data)
print(logrank_ref)

# Game Week
cox_week= coxph(Surv(first_goal_time, event)~ Game.Week, data=goal_data)
summary(cox_week)
# By Month
goal_data$date_GMT = strptime(goal_data$date_GMT, format="%b %d %Y - %I:%M%p")
goal_data$month = format(goal_data$date_GMT, "%b")
month_fit = survfit(Surv(first_goal_time, event) ~ month, data = goal_data)
ggsurvplot(month_fit, data=goal_data)
logrank_month = survdiff(Surv(first_goal_time, event) ~ month, data = goal_data)
print(logrank_month)

# Try grouping game weeks
goal_data$match_period = ifelse(goal_data$Game.Week > 33, "not", "general")
period_fit = survfit(Surv(first_goal_time, event) ~ match_period, data = goal_data)
ggsurvplot(period_fit, data=goal_data)
logrank_period = survdiff(Surv(first_goal_time, event) ~ match_period, data = goal_data)
print(logrank_period)
# COX
cox= coxph(Surv(first_goal_time, event)~ day_of_week + time_of_day, data=goal_data)
summary(cox)

cox_ppg= coxph(Surv(first_goal_time, event)~ Pre.Match.PPG..Home., data=goal_data)
summary(cox_ppg)

cox_stad= coxph(Surv(first_goal_time, event)~ stadium_capacity, data=goal_data)
summary(cox_stad)

# PPG diff - exponential works better
goal_data$ppg_diff = exp(goal_data$home_ppg - goal_data$away_ppg)

cox_ppg_diff= coxph(Surv(first_goal_time, event)~ ppg_diff, data=goal_data)
summary(cox_ppg_diff)

# Group PPG
quartiles_h = quantile(goal_data$home_ppg, probs= c(0.25, 0.5, 0.75))
goal_data = goal_data %>% mutate (h_ppg_group = case_when(
  home_ppg <= quartiles_h[3] ~ "Mid",
  home_ppg > quartiles_h[3] ~ "High"
))

quartiles_a = quantile(goal_data$away_ppg, probs= c(0.25, 0.5, 0.75))
goal_data = goal_data %>% mutate (a_pgg_group = case_when(
  away_ppg  <= quartiles_a[3] ~ "Mid",
  away_ppg > quartiles_a[3] ~ "High"
))
head(goal_data$h_pgg_group)
ppg_fit = survfit(Surv(first_goal_time, event) ~  a_pgg_group + h_ppg_group, data = goal_data)
ggsurvplot(ppg_fit, data=goal_data)
logrank_p = survdiff(Surv(first_goal_time, event) ~ h_ppg_group, data = goal_data)
print(logrank_p)


team_data = read.csv("/Users/SanaeDariouche/Downloads/england-premier-league-teams-2018-to-2019-stats.csv")

goal_data = goal_data %>%
  left_join(select(team_data, common_name, points_per_game_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, points_per_game_away), 
            by = c("away_team_name" = "common_name"))
goal_data$h_a_ppg_diff = exp(goal_data$points_per_game_home - goal_data$points_per_game_away)
cox= coxph(Surv(first_goal_time, event)~ h_a_ppg_diff, data=goal_data)
summary(cox)

goal_data = goal_data %>%
  left_join(select(team_data, common_name, league_position), 
            by = c("home_team_name" = "common_name"))
cox= coxph(Surv(first_goal_time, event)~ league_position, data=goal_data)
summary(cox)

goal_data = goal_data %>%
  left_join(select(team_data, common_name, average_total_goals_per_match_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, average_total_goals_per_match_away), 
            by = c("away_team_name" = "common_name"))



cox= coxph(Surv(first_goal_time, event)~ average_total_goals_per_match_home, data=goal_data)
summary(cox)
cox= coxph(Surv(first_goal_time, event)~ average_total_goals_per_match_away, data=goal_data)
summary(cox)
cox= coxph(Surv(first_goal_time, event)~ average_total_goals_per_match_home + average_total_goals_per_match_away, data=goal_data)
summary(cox)

cox_ppg= coxph(Surv(first_goal_time, event)~ home_ppg, data=goal_data)
summary(cox_ppg)
# Home Advantage
goal_data$big_6 = ifelse(goal_data$home_team_name %in% c("Arsenal", "Liverpool", "Chelsea", "Tottenham Hotspur", "Manchester City", "Manchester United")
                            & goal_data$away_team_name %in% c("Arsenal", "Liverpool", "Chelsea", "Tottenham Hotspur", "Manchester City", "Manchester United"), 1, 0)
big_fit = survfit(Surv(first_goal_time, event) ~  big_6, data = goal_data)
ggsurvplot(big_fit, data=goal_data)
logrank_p = survdiff(Surv(first_goal_time, event) ~ big_6, data = goal_data)
print(logrank_p)
# BTTS
cox_btts= coxph(Surv(first_goal_time, event)~ btts_percentage_pre_match, data=goal_data)
summary(cox_btts)

cox_btts= coxph(Surv(first_goal_time, event)~ odds_btts_yes, data=goal_data)
summary(cox_btts)

# Odds - interesting pre match draw odds is correlated
cox_odds= coxph(Surv(first_goal_time, event)~ odds_ft_away_team_win, data=goal_data)
summary(cox_odds)
cox_odds= coxph(Surv(first_goal_time, event)~ odds_ft_draw, data=goal_data)
summary(cox_odds)

# xG
cox_xG= coxph(Surv(first_goal_time, event)~ Away.Team.Pre.Match.xG, data=goal_data)
summary(cox_xG)

goal_data$overall_xG = goal_data$Away.Team.Pre.Match.xG + goal_data$Home.Team.Pre.Match.xG

cox_xG= coxph(Surv(first_goal_time, event)~ overall_xG, data=goal_data)
summary(cox_xG)

# average_goals_per_match_pre_match
cox_gpg= coxph(Surv(first_goal_time, event)~ average_goals_per_match_pre_match  + first_goal_Home., data=goal_data)
summary(cox_gpg)

# Home or Away Favorite
goal_data$home_fave = ifelse(goal_data$odds_ft_home_team_win < 1.4 | goal_data$odds_ft_away_team_win < 1.5, 1, 0)
fave_fit = survfit(Surv(first_goal_time, event) ~  home_fave, data = goal_data)
ggsurvplot(fave_fit, data=goal_data)
logrank_fave = survdiff(Surv(first_goal_time, event) ~ home_fave, data = goal_data)
print(logrank_fave)
# Best Cox model
cox= coxph(Surv(first_goal_time, event)~ day_group + time_group + average_total_goals_per_match_home + average_total_goals_per_match_away, data=goal_data)
summary(cox)
new_data = tribble(
  ~day_group,     ~time_group,  ~average_total_goals_per_match_home,     ~average_total_goals_per_match_away,
  "Friday",   "Day Time",   2.8,      3.47,
  "Weekend",    "Prime Time",  1.2,      2.7
)
new_data$score <- predict(cox, newdata = new_data, type = "lp")
sfit <- survfit(cox, newdata = new_data)

plot(sfit, col = 1:2, fun = "F",
     xlab = "time",
     ylab = "Cumulated Probability of Scoring")

summary(cox, time = 45)



goal_data = goal_data %>%
  left_join(select(team_data, common_name, corners_per_match_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, corners_per_match_away), 
            by = c("away_team_name" = "common_name"))

goal_data = goal_data %>%
  left_join(select(team_data, common_name, clean_sheet_percentage_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, clean_sheet_percentage_away), 
            by = c("away_team_name" = "common_name"))

cox= coxph(Surv(first_goal_time, event)~ clean_sheet_percentage_home, data=goal_data)
summary(cox)

#goals_conceded_per_match_away

goal_data = goal_data %>%
  left_join(select(team_data, common_name, goals_conceded_per_match_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, goals_conceded_per_match_away), 
            by = c("away_team_name" = "common_name"))

cox= coxph(Surv(first_goal_time, event)~ goals_conceded_per_match_away, data=goal_data)
summary(cox)

goal_data = goal_data %>%
  left_join(select(team_data, common_name, fts_percentage_home), 
            by = c("home_team_name" = "common_name"))
goal_data = goal_data %>%
  left_join(select(team_data, common_name, fts_percentage_away), 
            by = c("away_team_name" = "common_name"))

cox= coxph(Surv(first_goal_time, event)~ fts_percentage_home + average_total_goals_per_match_home , data=goal_data)
summary(cox)


# Create cause-specific event indicator
# 0 = no goal (censored), 1 = home team goal, 2 = away team goal

goal_data$event_type = 0  # Initialize all as censored

# Set events where home team scored first
goal_data$event_type[goal_data$event == 1 & 
                       !is.na(goal_data$first_goal_Home.) & 
                       goal_data$first_goal_Home. == 1] = 1

# Set events where away team scored first
goal_data$event_type[goal_data$event == 1 & 
                       (is.na(goal_data$first_goal_Home.) | 
                          goal_data$first_goal_Home. != 1)] = 2

# Check the results
table(goal_data$event_type)

# Create cause-specific event indicator for competing risks
goal_data$event_type = 0  # Initialize all as censored

# Set events where home team scored first
goal_data$event_type[goal_data$event == 1 & 
                       !is.na(goal_data$first_goal_Home.) & 
                       goal_data$first_goal_Home. == 1] = 1

# Set events where away team scored first
goal_data$event_type[goal_data$event == 1 & 
                       (is.na(goal_data$first_goal_Home.) | 
                          goal_data$first_goal_Home. != 1)] = 2

# Check event distribution
event_table <- table(goal_data$event_type)
print("Event Type Distribution:")
print(event_table)

# Create time intervals for empirical hazard analysis
goal_data$time_interval <- cut(goal_data$first_goal_time, 
                               breaks = c(0, 15, 30, 45, 60, 75, 90),
                               labels = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90"))

# Create linear time variable for time-dependent effects
goal_data$linear_time <- goal_data$first_goal_time

cat("\n\n================ CIF ANALYSIS ================\n")

# Fit cumulative incidence function
cif_fit = cuminc(goal_data$first_goal_time, goal_data$event_type)
print(cif_fit)

# Plot cumulative incidence
plot(cif_fit, col=c("blue", "red"), lwd=2,
     xlab="Match time (minutes)", ylab="Cumulative incidence",
     main="Competing risks: Home vs Away team scoring first")
legend("topleft", legend=c("Home team scores first", "Away team scores first"), 
       col=c("blue", "red"), lwd=2)


cat("\n================COMPETING RISKS REGRESSION ================\n")


# Analyze team effects on competing risks
# For home team scoring first
home_crr = crr(ftime = goal_data$first_goal_time, 
               fstatus = goal_data$event_type, 
               cov1 = goal_data[, c("points_per_game_home", "points_per_game_away")],
               failcode = 1)  # Code 1 is for home team scoring

# For away team scoring first
away_crr = crr(ftime = goal_data$first_goal_time, 
               fstatus = goal_data$event_type, 
               cov1 = goal_data[, c("points_per_game_home", "points_per_game_away")],
               failcode = 2)  # Code 2 is for away team scoring

summary(home_crr)
summary(away_crr)

# Using team metrics to predict competing risks
team_metrics = c("average_total_goals_per_match_home", "average_total_goals_per_match_away",
                 "clean_sheet_percentage_home", "clean_sheet_percentage_away")

# Create a covariate matrix
cov_matrix = as.matrix(goal_data[, team_metrics])

# Fit models with team metrics
home_model = crr(ftime = goal_data$first_goal_time, 
                 fstatus = goal_data$event_type,
                 cov1 = cov_matrix,
                 failcode = 1)

away_model = crr(ftime = goal_data$first_goal_time, 
                 fstatus = goal_data$event_type,
                 cov1 = cov_matrix,
                 failcode = 2)

summary(home_model)
summary(away_model)


# Create prediction data
new_teams = data.frame(
  points_per_game_home = c(2.5, 1.5, 1.0),
  points_per_game_away = c(1.0, 1.5, 2.0)
)

# Predict cumulative incidence
pred_time_points = seq(0, 90, by=5)
pred_home = predict(home_crr, new_teams, times=pred_time_points)
pred_away = predict(away_crr, new_teams, times=pred_time_points)

# Plot prediction
plot(pred_home[,1], pred_home[,2], type="l", col="blue", lwd=2,
     xlab="Match time (minutes)", 
     ylab="Cumulative incidence", 
     main="Predicted risk of scoring first goal",
     ylim=c(0, 1))

grid()
lines(pred_away[,1], pred_away[,2], col="red", lwd=2)
legend("topleft", 
       legend=c("Home team scores first", "Away team scores first"),
       col=c("blue", "red"), 
       lty=1, 
       lwd=2)
abline(v=c(15, 45, 60, 75), lty=2, col="gray")

cat("\n\n================ HAZARD ANALYSIS ================\n")

# Fit a flexible parametric model (Royston-Parmar)
flex_model <- flexsurvspline(Surv(first_goal_time, event) ~ 1, data = goal_data, k = 3)

# Print a summary of the hazard over time
cat("\nHazard Rate Summary from Flexible Parametric Model:\n")
print(summary(flex_model))

# Plot the hazard function
plot(flex_model, type = "hazard", main = "Hazard Rate for First Goal", 
     xlab = "Match time (minutes)", ylab = "Hazard rate")

# Calculate empirical hazard rates for each interval
hazard_by_interval <- goal_data %>%
  group_by(time_interval) %>%
  summarize(
    events = sum(event),
    total_time = sum(pmin(first_goal_time, 90) - case_when(
      time_interval == "0-15" ~ 0,
      time_interval == "15-30" ~ 15,
      time_interval == "30-45" ~ 30,
      time_interval == "45-60" ~ 45,
      time_interval == "60-75" ~ 60,
      time_interval == "75-90" ~ 75
    )),
    hazard_rate = events / total_time,
    .groups = 'drop'
  )

# Display empirical hazard results
cat("\nEmpirical Hazard Rates by Time Interval:\n")
print(hazard_by_interval)

# Plot empirical hazard rates
hazard_plot <- ggplot(hazard_by_interval, aes(x = time_interval, y = hazard_rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Empirical Hazard Rate by Match Time Interval",
       x = "Time Interval (minutes)",
       y = "Hazard Rate (goals per minute)") +
  theme_minimal()
print(hazard_plot)

cat("\n\n================ EXTENDED COX MODEL ================\n")

# Fit comprehensive model with multiple predictors
extended_cox <- coxph(Surv(first_goal_time, event) ~ 
                        average_total_goals_per_match_home + 
                        average_total_goals_per_match_away + 
                        day_group + 
                        time_group + 
                        clean_sheet_percentage_home + 
                        clean_sheet_percentage_away + 
                        h_a_ppg_diff + 
                        ref_group + 
                        stad_cap_group, 
                      data = goal_data)

cat("\nExtended Cox Model Results:\n")
ext_summary <- summary(extended_cox)
print(ext_summary)

# Calculate hazard ratios with confidence intervals
cat("\nHazard Ratios and 95% Confidence Intervals:\n")
hr_table <- exp(cbind(coef = coef(extended_cox), 
                      confint(extended_cox), 
                      "p-value" = ext_summary$coefficients[,5]))
print(hr_table)

# Plot forest plot of hazard ratios for key variables
ggforest(extended_cox, data = goal_data, 
         main = "Effect of Variables on First Goal Timing")

cat("\n\n================ MODEL SELECTION ================\n")

# Use stepwise selection to identify most important variables
step_model <- step(extended_cox, direction = "both", trace = 0)

# Display results of model selection
cat("\nResults after Stepwise Selection:\n")
step_summary <- summary(step_model)
print(step_summary)

cat("\nVariables retained in the final model:\n")
print(names(coef(step_model)))

cat("\nHazard Ratios for Selected Model:\n")
step_hr <- exp(cbind(coef = coef(step_model), 
                     confint(step_model),
                     "p-value" = step_summary$coefficients[,5]))
print(step_hr)

cat("\n\n================ NON-LINEAR EFFECTS ================\n")

# Check for non-linear effects with splines
spline_cox <- coxph(Surv(first_goal_time, event) ~ 
                      ns(average_total_goals_per_match_home, df = 3) + 
                      ns(average_total_goals_per_match_away, df = 3) + 
                      day_group + 
                      time_group,
                    data = goal_data)

# Display spline model results
cat("\nCox Model with Non-linear Effects (Splines):\n")
print(summary(spline_cox))

# Compare AIC between linear and spline models
linear_model <- coxph(Surv(first_goal_time, event) ~ 
                        average_total_goals_per_match_home + 
                        average_total_goals_per_match_away + 
                        day_group + 
                        time_group,
                      data = goal_data)

cat("\nModel Comparison (AIC):\n")
aic_comparison <- data.frame(
  Model = c("Linear", "Spline"),
  AIC = c(AIC(linear_model), AIC(spline_cox))
)
print(aic_comparison)

cat("\n\n================ PROPORTIONAL HAZARDS TEST ================\n")

# Test the proportional hazards assumption
ph_test <- cox.zph(extended_cox)

cat("\nProportional Hazards Test:\n")
print(ph_test)

# Determine which variables violate the PH assumption
cat("\nVariables violating proportional hazards (p < 0.05):\n")
ph_violations <- which(ph_test$table[,"p"] < 0.05)
if(length(ph_violations) > 0) {
  print(ph_test$table[ph_violations,])
} else {
  cat("No significant violations detected\n")
}

# Plot Schoenfeld residuals for variables with PH violations
if(length(ph_violations) > 0) {
  par(mfrow=c(2,2))
  for(i in ph_violations) {
    plot(ph_test[i], main=names(ph_violations)[i])
  }
  par(mfrow=c(1,1))
}

cat("\n\n================ STRATIFIED MODEL ================\n")

# Fit stratified model for variables that violate PH
stratified_cox <- coxph(Surv(first_goal_time, event) ~ 
                          average_total_goals_per_match_home + 
                          average_total_goals_per_match_away + 
                          strata(day_group) +  # Stratify by day_group 
                          time_group,
                        data = goal_data)

cat("\nStratified Cox Model Results:\n")
strat_summary <- summary(stratified_cox)
print(strat_summary)

# Calculate hazard ratios for stratified model
cat("\nHazard Ratios for Stratified Model:\n")
strat_hr <- exp(cbind(coef = coef(stratified_cox), 
                      confint(stratified_cox),
                      "p-value" = strat_summary$coefficients[,5]))
print(strat_hr)

# Test proportional hazards for the stratified model
ph_test_strat <- cox.zph(stratified_cox)
cat("\nProportional Hazards Test for Stratified Model:\n")
print(ph_test_strat)

cat("\n\n================ TIME-DEPENDENT COEFFICIENTS ================\n")

# Fit model with time-dependent effects
td_cox <- coxph(Surv(first_goal_time, event) ~ 
                  average_total_goals_per_match_home + 
                  average_total_goals_per_match_away +
                  time_group + 
                  day_group + day_group:linear_time,  # Interaction with time 
                data = goal_data)

cat("\nCox Model with Time-Dependent Coefficients:\n")
td_summary <- summary(td_cox)
print(td_summary)

cat("\nInterpretation of Time-Varying Effects:\n")
time_vars <- grep(":", names(coef(td_cox)), value = TRUE)
if(length(time_vars) > 0) {
  time_coefs <- coef(td_cox)[time_vars]
  for(i in seq_along(time_coefs)) {
    cat(paste0("- ", names(time_coefs)[i], ": ", 
               ifelse(time_coefs[i] > 0, 
                      "Effect increases over time (more influential later in match)", 
                      "Effect decreases over time (less influential later in match)"),
               "\n"))
  }
} else {
  cat("No time-varying effects in the model\n")
}

cat("\n\n================ MODEL COMPARISON ================\n")

# Compare AIC between models
cat("\nModel Comparison (AIC):\n")
final_comparison <- data.frame(
  Model = c("Extended Cox", "Step-Selected", "Spline", "Stratified", 
            "Time-Dependent"),
  AIC = c(AIC(extended_cox), AIC(step_model), AIC(spline_cox), 
          AIC(stratified_cox), AIC(td_cox))
)
final_comparison <- final_comparison[order(final_comparison$AIC),]
print(final_comparison)

# Identify the best model (lowest AIC)
best_model_name <- final_comparison$Model[1]
cat("\nBest model based on AIC:", best_model_name, "\n")

