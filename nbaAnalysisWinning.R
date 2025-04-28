library(dplyr)
library(tidyr)
library(caret)
library(readr)
library(devtools)
library("nbastatR")
library(hoopR)
<<<<<<< HEAD

=======
library(ggplot2)
library(ggrepel)
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415

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

<<<<<<< HEAD
=======
#Dropping selected columns
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
GameStats12024 <- GameStats2025[, -c(2,3,4,7,10:12,14,16:21,22,30,47)]
GameStats22024 <- GameStats12024[, -c(6,32:36,39:43,45,46,48:50)]
GameStats32024 <- GameStats22024[, -c(29,30,34,35,43,59,60)]

<<<<<<< HEAD
=======
#renaming columns
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
GameStatsClean2025 <- GameStats32024 |>
  rename(Year = yearSeasonH, Date= dateGameH, GameID = idGame, HomeTeam = nameTeamH,
         HomeTeamID = idTeamH, Home  = slugTeamH, AwayTeam = nameTeamA,
         AwayTeamID = idTeamA, Away  = slugTeamA, Minutes = minutesTeamH)

GameStatsClean2025 <- GameStatsClean2025[, -c(4,29,42)]

<<<<<<< HEAD
=======
#creating advanced metrics
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
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

<<<<<<< HEAD


# Calculate averages for team stats, including advanced metrics
=======
#creating averages for teams home 
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
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
<<<<<<< HEAD
    SOEHome = mean(SOEHome, na.rm = TRUE),
    assistH = mean(astTeamH, na.rm = TRUE)
  )

=======
    SOEHome = mean(SOEHome, na.rm = TRUE)
  )

#creating averages for teams away
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
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
<<<<<<< HEAD
    SOEAway = mean(SOEAway, na.rm = TRUE),
    assistA = mean(astTeamA, na.rm = TRUE)
  )

team_averages_home
team_averages_away




# Merge home and away team averages into one dataset
team_averages_combined <- full_join(team_averages_home, team_averages_away, 
                                    by = c("Home" = "Away")) |> 
  rename(Team = Home) 


team_averages_combined


#Games are up until March 15th due to accessibility of data. Only got one game from the 16th can't assess anything else 
team_averages_combined <- team_averages_combined |>  
  mutate(
    Wins = case_when(
      Team == "ATL" ~ 32,  
      Team == "BKN" ~ 22,
      Team == "BOS" ~ 49,
      Team == "CHA" ~ 17,  
      Team == "CHI" ~ 28,
      Team == "CLE" ~ 56,
      Team == "DAL" ~ 33,  
      Team == "DEN" ~ 43,
      Team == "DET" ~ 37,
      Team == "GSW" ~ 39,  
      Team == "HOU" ~ 43,
      Team == "IND" ~ 37,
      Team == "LAC" ~ 37,  
      Team == "LAL" ~ 40,
      Team == "MEM" ~ 43,
      Team == "MIA" ~ 29,  
      Team == "MIL" ~ 38,
      Team == "MIN" ~ 39,
      Team == "NOP" ~ 18,  
      Team == "NYK" ~ 42,
      Team == "OKC" ~ 55,
      Team == "ORL" ~ 31,  
      Team == "PHI" ~ 22,
      Team == "PHX" ~ 31,
      Team == "POR" ~ 28,  
      Team == "SAC" ~ 33,
      Team == "SAS" ~ 28,
      Team == "TOR" ~ 24,  
      Team == "UTA" ~ 15,
      Team == "WAS" ~ 15,
      TRUE ~ NA_real_
    ),
    Losses = case_when(
      Team == "ATL" ~ 35,  
      Team == "BKN" ~ 45,
      Team == "BOS" ~ 19,
      Team == "CHA" ~ 49,  
      Team == "CHI" ~ 39,
      Team == "CLE" ~ 10,
      Team == "DAL" ~ 35,  
      Team == "DEN" ~ 25,
      Team == "DET" ~ 31,
      Team == "GSW" ~ 28,  
      Team == "HOU" ~ 25,
      Team == "IND" ~ 29,
      Team == "LAC" ~ 30,  
      Team == "LAL" ~ 25,
      Team == "MEM" ~ 25,
      Team == "MIA" ~ 38,  
      Team == "MIL" ~ 28,
      Team == "MIN" ~ 29,
      Team == "NOP" ~ 50,  
      Team == "NYK" ~ 24,
      Team == "OKC" ~ 12,
      Team == "ORL" ~ 37,  
      Team == "PHI" ~ 44,
      Team == "PHX" ~ 36,
      Team == "POR" ~ 39,  
      Team == "SAC" ~ 33,
      Team == "SAS" ~ 38,
      Team == "TOR" ~ 43,  
      Team == "UTA" ~ 52,
      Team == "WAS" ~ 51,
=======
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
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
      TRUE ~ NA_real_
    ),
    WinPct = Wins / (Wins + Losses)
  )

team_averages_combined

<<<<<<< HEAD
team_averages_combined <- team_averages_combined |>
  mutate(totalrebH = orebH + drebH,
         totalrebA = orebA + drebA)

team_averages_combined



team_averages_combined <- team_averages_combined |>
  mutate(
    TotalPoints = ptsH + ptsA,
    AvgOER = (OERHome + OERAway) / 2,
    AvgDER = (DERHome + DERAway) / 2,
    TotalRebounds = totalrebH + totalrebA,
    AvgSOE = (SOEHome + SOEAway) / 2
  )




team_averages_combined <- team_averages_combined |>
  mutate(Conference = case_when(
    Team %in% c("CLE", "BOS", "PHI", "NYK", "MIA", "MIL", "ATL", "TOR", "IND", "CHI", "BRK", "WAS", "CHA", "ORL", "DET", "BKN") ~ "East",
    Team %in% c("DEN", "LAL", "GSW", "PHX", "OKC", "MIN", "SAC", "LAC", "NOP", "DAL", "HOU", "MEM", "UTA", "POR", "SAS") ~ "West",
  ))


top_teams <- team_averages_combined |>
  arrange(desc(Wins)) |>
  mutate(Rank = row_number()) |>
  select(Rank, Team, Conference, Wins)

top_teams




# Create Rank and Group columns
ranked_teams <- team_averages_combined |>
  arrange(desc(Wins)) |>
  mutate(Rank = row_number(),
         RankGroup = case_when(
           Rank <= 10 ~ "Top 10 (Ranks 1–10)",
           Rank <= 20 ~ "Middle 10 (Ranks 11–20)",
           TRUE ~ "Bottom 10 (Ranks 21–30)"
         ))

ggplot(filter(ranked_teams, RankGroup == "Top 10 (Ranks 1–10)"),
       aes(x = reorder(Team, Rank), y = Wins, fill = Conference)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Wins), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("East" = "#1f77b4", "West" = "#ff7f0e")) +
  labs(title = "Top 10 NBA Teams",
       x = "Team",
       y = "Wins",
       fill = "Conference") +
  theme_minimal(base_size = 13)

ggplot(filter(ranked_teams, RankGroup == "Middle 10 (Ranks 11–20)"),
       aes(x = reorder(Team, Rank), y = Wins, fill = Conference)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Wins), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("East" = "#1f77b4", "West" = "#ff7f0e")) +
  labs(title = "Middle 10 NBA Teams",
       x = "Team",
       y = "Wins",
       fill = "Conference") +
  theme_minimal(base_size = 13)

ggplot(filter(ranked_teams, RankGroup == "Bottom 10 (Ranks 21–30)"),
       aes(x = reorder(Team, Rank), y = Wins, fill = Conference)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Wins), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("East" = "#1f77b4", "West" = "#ff7f0e")) +
  labs(title = "Bottom 10 NBA Teams",
       x = "Team",
       y = "Wins",
       fill = "Conference") +
  theme_minimal(base_size = 13)


team_averages_combined <- team_averages_combined |> 
  arrange(desc(Wins)) |>  
=======
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
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
  mutate(WinGroup = case_when(
    row_number() <= 10 ~ "High-Win Teams", 
    row_number() > 10 & row_number() <= (n() - 10) ~ "Mid-Win Teams", 
    row_number() > (n() - 10) ~ "Low-Win Teams"
<<<<<<< HEAD
  ),
  WinGroup = factor(WinGroup, levels = c("High-Win Teams", "Mid-Win Teams", "Low-Win Teams")))



ggplot(team_averages_combined, aes(x = WinGroup, y = AvgSOE, fill = WinGroup)) +
=======
  ))


ggplot(team_averages_combined, aes(x = WinGroup, y = SOEHome, fill = WinGroup)) +
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415
  geom_boxplot(alpha = 0.7) +
  labs(title = "SOE Distribution Across Win Groups",
       x = "Win Group",
       y = "SOE (Shot Opportunity Efficiency)") +
  theme_minimal() +
  theme(legend.position = "none") +
<<<<<<< HEAD
  annotate("text", x = 1, y = max(team_averages_combined$AvgSOE, na.rm = TRUE) + 0.02, 
           label = "Top 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 2, y = max(team_averages_combined$AvgSOE, na.rm = TRUE) + 0.02, 
           label = "Middle 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 3, y = max(team_averages_combined$AvgSOE, na.rm = TRUE) + 0.02, 
           label = "Bottom 10 Teams", fontface = "bold", size = 5)

ggplot(team_averages_combined, aes(x = WinGroup, y = AvgOER, fill = WinGroup)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Offensive Efficiency Rating Distribution Across Win Groups",
       x = "Win Group",
       y = "OER") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = max(team_averages_combined$AvgOER, na.rm = TRUE) + 0.02, 
           label = "Top 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 2, y = max(team_averages_combined$AvgOER, na.rm = TRUE) + 0.02, 
           label = "Middle 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 3, y = max(team_averages_combined$AvgOER, na.rm = TRUE) + 0.02, 
           label = "Bottom 10 Teams", fontface = "bold", size = 5)

ggplot(team_averages_combined, aes(x = WinGroup, y = AvgDER, fill = WinGroup)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Defensive Efficiency Rating Distribution Across Win Groups",
       x = "Win Group",
       y = "DER") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = max(team_averages_combined$AvgDER, na.rm = TRUE) + 0.02, 
           label = "Top 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 2, y = max(team_averages_combined$AvgDER, na.rm = TRUE) + 0.02, 
           label = "Middle 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 3, y = max(team_averages_combined$AvgDER, na.rm = TRUE) + 0.02, 
           label = "Bottom 10 Teams", fontface = "bold", size = 5)


soe_summary <- team_averages_combined |>
  group_by(WinGroup) |>
  summarize(
    Count = n(),
    Mean_SOE = round(mean(AvgSOE, na.rm = TRUE), 3),
    Median_SOE = round(median(AvgSOE, na.rm = TRUE), 3),
    SD_SOE = round(sd(AvgSOE, na.rm = TRUE), 3),
    Min_SOE = round(min(AvgSOE, na.rm = TRUE), 3),
    Max_SOE = round(max(AvgSOE, na.rm = TRUE), 3)
  )

soe_summary

oer_summary <- team_averages_combined |>
  group_by(WinGroup) |>
  summarize(
    Count = n(),
    Mean_oer = round(mean(AvgOER, na.rm = TRUE), 3),
    Median_oer = round(median(AvgOER, na.rm = TRUE), 3),
    SD_oer = round(sd(AvgOER, na.rm = TRUE), 3),
    Min_oer = round(min(AvgOER, na.rm = TRUE), 3),
    Max_oer = round(max(AvgOER, na.rm = TRUE), 3)
  )

oer_summary

der_summary <- team_averages_combined |>
  group_by(WinGroup) |>
  summarize(
    Count = n(),
    Mean_der = round(mean(AvgDER, na.rm = TRUE), 3),
    Median_der = round(median(AvgDER, na.rm = TRUE), 3),
    SD_der = round(sd(AvgDER, na.rm = TRUE), 3),
    Min_der = round(min(AvgDER, na.rm = TRUE), 3),
    Max_der = round(max(AvgDER, na.rm = TRUE), 3)
  )

der_summary


ranked_metrics <- team_averages_combined |>
  select(Team, Wins, WinGroup, OERHome, OERAway, DERHome, DERAway, ptsH, ptsA, totalrebH, totalrebA, SOEHome, SOEAway) %>%
  mutate(
    OERH_Rank = rank(-OERHome),   
    OERA_Rank = rank(-OERAway),
    DERH_Rank = rank(DERHome),    
    DERA_Rank = rank(DERAway),
    PointsH_Rank = rank(-ptsH),     
    PointsA_Rank = rank(-ptsA),
    ReboundsH_Rank = rank(-totalrebH),   
    ReboundsA_Rank = rank(-totalrebA),
    SOEH_Rank = rank(-SOEHome),
    SOEA_Rank = rank(-SOEAway)
    
  ) |>
  arrange(OERH_Rank)

ranked_metrics

top4 <- c("BOS", "OKC", "DEN", "CLE") 

bottom4 <- c("NOP", "CHA", "UTA", "WAS")  


team_averages_combined <- team_averages_combined |>
  mutate(HighlightTeam = case_when(
    Team %in% top4 ~ "Top 4",
    Team %in% bottom4 ~ "Bottom 4",
    TRUE ~ "Other"
  ))


ggplot(team_averages_combined, aes(x = reorder(Team, AvgOER), y = AvgOER, fill = HighlightTeam)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Wins), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Top 4" = "#0072B2", "Bottom 4" = "red", "Other" = "gray80"))+
  coord_flip() +
  labs(title = "Average Offensive Efficiency(OER)",
       x = "Team",
       y = "Average OER",
       fill = NULL) +
  theme_minimal(base_size = 12)



team_averages_combined <- team_averages_combined |>
  mutate(HighlightTeam = case_when(
    Team %in% top4 ~ "Top 4",
    Team %in% bottom4 ~ "Bottom 4",
    TRUE ~ "Other"
  ))

ggplot(team_averages_combined, aes(x = reorder(Team, AvgDER), y = AvgDER, fill = HighlightTeam)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Wins), hjust = -0.1, size = 3.5) +
  scale_fill_manual(values = c("Top 4" = "#0072B2", "Bottom 4" = "red", "Other" = "gray80"))+
  coord_flip() +
  labs(title = "Avergae Defensive Efficiency Rating (DER) ",
       x = "Team",
       y = "Average DER",
       fill = NULL) +
  theme_minimal(base_size = 12)



summary_by_team <- team_averages_combined |>
  select(Team, Wins, ptsH, ptsA, OERHome, OERAway)

summary_by_team <- summary_by_team |>
  arrange(desc(TotalPoints)) |>
  mutate(TotalPointsRank = row_number()) |>
  arrange(desc(AvgOER)) |>
  mutate(AvgOERRank = row_number())
summary_by_team

team_averages_combined <- team_averages_combined |>
  arrange(desc(AvgOER)) |>
  mutate(PointsRank = row_number(),
         PointsTier = case_when(
           PointsRank <= 10 ~ "Top 10",
           PointsRank > 20 ~ "Bottom 10",
           TRUE ~ "Middle 10"
         ))

ggplot(team_averages_combined, aes(x = AvgOER, y = WinPct)) +
  geom_point(aes(color = PointsTier), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Top 10" = "green", "Middle 10" = "blue", "Bottom 10" = "red")) +
  geom_text_repel(data = subset(team_averages_combined, Team %in% c("CHI", "ATL", "LAL")),
                  aes(label = Team), size = 4) +
  labs(title = "Average Offensive Efficiency vs. Win Percentage",
       x = "Average OER ",
       y = "Win Percentage",
       color = "Scoring Tier") +
  theme_minimal()

team_averages_combined <- team_averages_combined |>
  arrange(desc(TotalPoints)) %>%
  mutate(PointsRank = row_number(),
         PointsTier = case_when(
           PointsRank <= 10 ~ "Top 10",
           PointsRank > 20 ~ "Bottom 10",
           TRUE ~ "Middle 10"
         ))

ggplot(team_averages_combined, aes(x = TotalPoints, y = WinPct)) +
  geom_point(aes(color = PointsTier), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Top 10" = "green", "Middle 10" = "blue", "Bottom 10" = "red")) +
  geom_text_repel(data = subset(team_averages_combined, Team %in% c("CHI", "ATL", "LAL")),
                  aes(label = Team), size = 4) +
  labs(title = "Total Points vs. Win Percentage",
       x = "Average Total Points",
       y = "Win Percentage",
       color = "Scoring Tier") +
  theme_minimal()


summary_by_team2 <- team_averages_combined |>
  select(Team, Wins, totalrebH, totalrebA, SOEHome, SOEAway)
)
summary_by_team2 <- summary_by_team2 |>
  arrange(desc(TotalRebounds)) |>
  mutate(TotalReboundsRank = row_number()) |>
  arrange(desc(AvgSOE)) |>
  mutate(AvgSOERank = row_number())
summary_by_team2


team_averages_combined <- team_averages_combined |>
  arrange(desc(AvgSOE)) |>
  mutate(PointsRank = row_number(),
         PointsTier = case_when(
           PointsRank <= 10 ~ "Top 10",
           PointsRank > 20 ~ "Bottom 10",
           TRUE ~ "Middle 10"
         ))

ggplot(team_averages_combined, aes(x = AvgSOE, y = WinPct)) +
  geom_point(aes(color = PointsTier), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Top 10" = "green", "Middle 10" = "blue", "Bottom 10" = "red")) +
  geom_text_repel(data = subset(team_averages_combined, Team %in% c("CHI", "UTA", "TOR", "OKC", "NYK", "MIN")),
                  aes(label = Team), size = 4) +
  labs(title = "Average Shot Opportunity Efficiency vs. Win Percentage",
       x = "Average SOE ",
       y = "Win Percentage",
       color = "Scoring Tier") +
  theme_minimal()

team_averages_combined <- team_averages_combined |>
  arrange(desc(TotalRebounds)) |>
  mutate(PointsRank = row_number(),
         PointsTier = case_when(
           PointsRank <= 10 ~ "Top 10",
           PointsRank > 20 ~ "Bottom 10",
           TRUE ~ "Middle 10"
         ))

ggplot(team_averages_combined, aes(x = TotalRebounds, y = WinPct)) +
  geom_point(aes(color = PointsTier), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Top 10" = "green", "Middle 10" = "blue", "Bottom 10" = "red")) +
  geom_text_repel(data = subset(team_averages_combined, Team %in% c("CHI", "UTA", "TOR", "OKC", "NYK", "MIN")),
                  aes(label = Team), size = 4) +
  labs(title = "Total Rebounds vs. Win Percentage",
       x = "Average Rebounds Per Game",
       y = "Win Percentage",
       color = "Scoring Tier") +
  theme_minimal()




=======
  annotate("text", x = 1, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Top 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 2, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Bottom 10 Teams", fontface = "bold", size = 5) +
  annotate("text", x = 3, y = max(team_averages_combined$SOEHome, na.rm = TRUE) + 0.02, 
           label = "Middle 10 Teams", fontface = "bold", size = 5)
>>>>>>> d888dd368eb6a80806c63231039ca682ca041415



