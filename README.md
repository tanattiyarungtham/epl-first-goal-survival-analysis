# epl-first-goal-survival-analysis
Survival analysis of opening goal times in the English Premier League (2018–2019) with R (Cox PH, log-rank), EDA, and report.

# EPL First-Goal Survival Analysis

Survival analysis of opening goal times in the English Premier League (2018–2019). We model time-to-first-goal with Kaplan–Meier, log-rank tests, and Cox proportional hazards, and analyze effects such as time-of-day, day-of-week, stadium factors, and team strength.

## Highlights
- Dataset: EPL 2018–2019 (380 matches, 69 variables; plus team stats).  
- Methods: `Surv()`, `survdiff()`, `coxph()`; model comparison & diagnostics.  
- Findings: Prime-time & Friday games skew earlier first goals; average goals per match (home/away) are significant in Cox models; several “intuitive” factors are weaker than expected.

## Repo Structure
See the folder tree in this README.

## Data
- Place raw CSVs 
  - `england-premier-league-matches-2018-to-2019-stats.csv`
  - `england-premier-league-teams-2018-to-2019-stats.csv`

## Reproducibility
- R version: (R4.5.0)
- Recommended: `{renv}` for dependency lock.
```r
renv::restore()
source("R/00_setup.R")
