library(dplyr)
library(tidyr)
library(caret)
library(readr)
library(devtools)
library("nbastatR")
library(hoopR)
library(ggplot2)
library(ggrepel)

GAME2025=game_logs(
  seasons = 2025,
  league = "NBA",
  result_types = "team",
  season_types = "Regular Season",
  nest_data = F,
  assign_to_environment = TRUE,
  return_message = TRUE
)

home_games2025 <- GAME2025 |> filter(locationGame == "H")
away_games2025 <- GAME2025 |> filter(locationGame == "A")
GameStats2025 <- inner_join(home_games2025, away_games2025, by = c("idGame"), suffix = c("H", "A"))

#Dropping selected columns
GameStats12024 <- GameStats2025[, -c(2,3,4,7,10:12,14,16:21,22,30,47)]
GameStats22024 <- GameStats12024[, -c(6,32:36,39:43,45,46,48:50)]
GameStats32024 <- GameStats22024[, -c(29,30,34,35,43,59,60)]

#renaming columns
GameStatsClean2025 <- GameStats32024 |>
  rename(Year = yearSeasonH, Date= dateGameH, GameID = idGame, HomeTeam = nameTeamH,
         HomeTeamID = idTeamH, Home  = slugTeamH, AwayTeam = nameTeamA,
         AwayTeamID = idTeamA, Away  = slugTeamA, Minutes = minutesTeamH)

GameStatsClean2025 <- GameStatsClean2025[, -c(4,29,42)]

#creating advanced metrics
GameStatsClean2025 <- GameStatsClean2025 |>
  mutate(
    OERHome= ptsTeamH / (fgaTeamH + ((ftaTeamH * 0.9)/2)+ tovTeamH),
    OERAway= ptsTeamA / (fgaTeamA + ((ftaTeamA * 0.9)/2)+ tovTeamA),
    DERHome = ptsTeamA / (fgaTeamA + ((ftaTeamA * 0.9)/2)+ tovTeamA),
    DERAway = ptsTeamH / (fgaTeamH + ((ftaTeamH * 0.9)/2)+ tovTeamH),
    GameTotal = ptsTeamH + ptsTeamA,
    Spread = ptsTeamH - ptsTeamA,
    HPossessions = fgaTeamH + 0.4 * ftaTeamH - 1.07 * (orebTeamH / (orebTeamH + drebTeamA)) * (fgaTeamH - fgmTeamH) + tovTeamH,
    APossessions = fgaTeamA + 0.4 * ftaTeamA - 1.07 * (orebTeamA / (orebTeamA + drebTeamH)) * (fgaTeamA - fgmTeamA) + tovTeamA,
    HPace =  (HPossessions / Minutes) * 48,
    APace = (APossessions/Minutes)*48,
    HPaceAdj = HPace * (OERHome * 100 / 97.7),
    APaceAdj = APace * (OERAway * 100 / 97.7),
    EffDiffHome = OERHome - DERHome,
    EffDiffAway = OERAway - DERAway,
    TotalEffDiff = EffDiffHome - EffDiffAway,
    PPPHome = ptsTeamH / HPossessions,
    PPPAway = ptsTeamA/ APossessions,
    ORRHome = orebTeamH / (orebTeamH + drebTeamA),
    ORRAway = orebTeamA/ (orebTeamA + drebTeamH),
    TRHome = tovTeamH / HPossessions,
    TRAway = tovTeamA / APossessions,
    SOEHome = PPPHome * (1 + ORRHome) / (1 + TRHome),
    SOEAway = PPPAway * (1 + ORRAway) / (1 + TRAway)
  )

GameStatsClean2025

#creating averages for teams home 
team_averages_home <- GameStatsClean2025 |>  
  group_by(Home) |>  
  summarise(
    fgmH = mean(fgmTeamH, na.rm = TRUE),
    fgaH = mean(fgaTeamH, na.rm = TRUE),
    ftaH = mean(ftaTeamH, na.rm = TRUE),
    ptsH = mean(ptsTeamH, na.rm = TRUE),
    pctFGH = mean(pctFGTeamH, na.rm = TRUE),
    tovH = mean(tovTeamH, na.rm = TRUE),
    orebH = mean(orebTeamH, na.rm = TRUE),
    drebH = mean(drebTeamH, na.rm = TRUE),
    OERHome= mean(OERHome, na.rm = TRUE),
    DERHome = mean(DERHome, na.rm = TRUE),
    GameTotal = mean(GameTotal, na.rm = TRUE),
    Spread = mean(Spread, na.rm = TRUE),
    HPossessions = mean(HPossessions, na.rm = TRUE),
    HPace = mean(HPace, na.rm = TRUE),
    HPaceAdj = mean(HPaceAdj, na.rm = TRUE),
    EffDiffHome = mean(EffDiffHome, na.rm = TRUE),
    TotalEffDiff = mean(TotalEffDiff, na.rm = TRUE),
    PPPHome = mean(PPPHome, na.rm = TRUE),
    SOEHome = mean(SOEHome, na.rm = TRUE)
  )

#creating averages for teams away
team_averages_away <- GameStatsClean2025 |>  
  group_by(Away) |>  
  summarise(
    fgmA = mean(fgmTeamA, na.rm = TRUE),
    fgaA = mean(fgaTeamA, na.rm = TRUE),
    ftaA = mean(ftaTeamA, na.rm = TRUE),
    ptsA = mean(ptsTeamA, na.rm = TRUE),
    pctFGA = mean(pctFGTeamA, na.rm = TRUE),
    tovA = mean(tovTeamA, na.rm = TRUE),
    orebA = mean(orebTeamA, na.rm = TRUE),
    drebA = mean(drebTeamA, na.rm = TRUE),
    OERAway = mean(OERAway, na.rm = TRUE),
    DERAway = mean(DERAway, na.rm = TRUE),
    GameTotal = mean(GameTotal, na.rm = TRUE),
    Spread = mean(Spread, na.rm = TRUE),
    APossessions = mean(APossessions, na.rm = TRUE),
    APace = mean(APace, na.rm = TRUE),
    APaceAdj = mean(APaceAdj, na.rm = TRUE),
    EffDiffAway = mean(EffDiffAway, na.rm = TRUE),
    TotalEffDiff = mean(TotalEffDiff, na.rm = TRUE),
    PPPAway = mean(PPPAway, na.rm = TRUE),
    SOEAway = mean(SOEAway, na.rm = TRUE)
  )
# View results
team_averages_home
team_averages_away

# Merge home and away team averages into one dataset
team_averages_combined <- full_join(team_averages_home, team_averages_away, 
                                    by = c("Home" = "Away")) |> 
  rename(Team = Home) # Renaming the column for consistency

# View merged dataset
team_averages_combined

team_averages_combined <- team_averages_combined |>  
  mutate(
    Wins = case_when(
      Team == "ATL" ~ 30,  
      Team == "BKN" ~ 21,
      Team == "BOS" ~ 46,
      Team == "CHA" ~ 15,  
      Team == "CHI" ~ 26,
      Team == "CLE" ~ 54,
      Team == "DAL" ~ 32,  
      Team == "DEN" ~ 41,
      Team == "DET" ~ 35,
      Team == "GSW" ~ 36,  
      Team == "HOU" ~ 39,
      Team == "IND" ~ 35,
      Team == "LAC" ~ 34,  
      Team == "LAL" ~ 40,
      Team == "MEM" ~ 40,
      Team == "MIA" ~ 29,  
      Team == "MIL" ~ 36,
      Team == "MIN" ~ 37,
      Team == "NOP" ~ 17,  
      Team == "NYK" ~ 40,
      Team == "OKC" ~ 53,
      Team == "ORL" ~ 30,  
      Team == "PHI" ~ 22,
      Team == "PHX" ~ 30,
      Team == "POR" ~ 28,  
      Team == "SAC" ~ 33,
      Team == "SAS" ~ 26,
      Team == "TOR" ~ 21,  
      Team == "UTA" ~ 15,
      Team == "WAS" ~ 13,
      TRUE ~ NA_real_
    ),
    Losses = case_when(
      Team == "ATL" ~ 34,  
      Team == "BKN" ~ 42,
      Team == "BOS" ~ 18,
      Team == "CHA" ~ 48,  
      Team == "CHI" ~ 38,
      Team == "CLE" ~ 10,
      Team == "DAL" ~ 33,  
      Team == "DEN" ~ 23,
      Team == "DET" ~ 29,
      Team == "GSW" ~ 28,  
      Team == "HOU" ~ 25,
      Team == "IND" ~ 27,
      Team == "LAC" ~ 29,  
      Team == "LAL" ~ 22,
      Team == "MEM" ~ 24,
      Team == "MIA" ~ 34,  
      Team == "MIL" ~ 27,
      Team == "MIN" ~ 29,
      Team == "NOP" ~ 48,  
      Team == "NYK" ~ 23,
      Team == "OKC" ~ 11,
      Team == "ORL" ~ 35,  
      Team == "PHI" ~ 41,
      Team == "PHX" ~ 34,
      Team == "POR" ~ 37,  
      Team == "SAC" ~ 30,
      Team == "SAS" ~ 36,
      Team == "TOR" ~ 43,  
      Team == "UTA" ~ 49,
      Team == "WAS" ~ 49,
      TRUE ~ NA_real_
    ),
    WinPct = Wins / (Wins + Losses)
  )

team_averages_combined

ggplot(team_averages_combined, aes(x = OERHome, y = WinPct)) +
  geom_point(aes(color = Team), size = 3) +  
  geom_text_repel(aes(label = Team), size = 4) + 
  labs(title = "Offensive Efficiency (Home) vs. Win Percentage",
       x = "Home Offensive Efficiency Rating (OER)",
       y = "Win Percentage") +
  theme_minimal()

ggplot(team_averages_combined, aes(x = OERAway, y = WinPct)) +
  geom_point(aes(color = Team), size = 3) +  
  geom_text_repel(aes(label = Team), size = 4) + 
  labs(title = "Offensive Efficiency (Away) vs. Win Percentage",
       x = "Away Offensive Efficiency Rating (OER)",
       y = "Win Percentage") +
  theme_minimal()

team_averages_combined <- team_averages_combined |> 
  arrange(desc(Wins)) |> 
  mutate(WinGroup = case_when(
    row_number() <= 10 ~ "High-Win Teams", 
    row_number() > 10 & row_number() <= (n() - 10) ~ "Mid-Win Teams", 
    row_number() > (n() - 10) ~ "Low-Win Teams"
  ))


ggplot(team_averages_combined, aes(x = WinGroup, y = SOEHome, fill = WinGroup)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "SOE Distribution Across Win Groups",
       x = "Win Group",
       y = "SOE (Shot Opportunity Efficiency)") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Top 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 2, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Bottom 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 3, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Middle 10 Teams", fontface = "bold", size = 5)



