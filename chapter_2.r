# Install and load necessary libraries
#install.packages("ggthemes")
library("tidyverse")
library("nflfastR")
library("ggthemes")

# Load play-by-play data from 2016 to 2022
pbp_r <- load_pbp(2016:2022)

# Filter for pass plays with non-NA air yards
pbp_r_p <-
    pbp_r |>
    filter(play_type == "pass" & !is.na(air_yards))

# Classify passes as long or short based on air yards and handle NA in passing yards
pbp_r_p <-
    pbp_r_p |>
    mutate(
        pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
        passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)
    )

# Print summary of all passing yards
print(pbp_r_p |> 
      pull(passing_yards) |> 
      summary())

# Print summary of passing yards for short passes
print(pbp_r_p |>
      filter(pass_length_air_yards == "short") |>
      pull(passing_yards) |>
      summary())

print(pbp_r_p |>
      filter(pass_length_air_yards == "long") |>
      pull(passing_yards) |>
      summary())
