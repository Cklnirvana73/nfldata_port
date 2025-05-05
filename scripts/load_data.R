install.packages("nflfastR")

# Load libraries
library(nflfastR)
library(dplyr)

# Load NFL data for a given season (e.g., 2020 season)
player_data <- nflfastR::load_pbp(2020)  # Play-by-play data for the 2020 season

# View the first few rows of the data
head(player_data)
