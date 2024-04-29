install.packages("nflfastR")
install.packages("tidyverse")

library("tidyverse")
library("nflfastR")

# Load play-by-play data for 2021
pbp_r <- load_pbp(2021)

# Filter for passing plays with defined air yards
pbp_r_filtered <- pbp_r |>
    filter(play_type == 'pass' & !is.na(air_yards))

# Group, summarize, filter for minimum number of plays, and calculate average depth of target
pbp_r_p <- pbp_r_filtered |>
    group_by(passer_id, passer) |>
    summarize(n = n(), adot = mean(air_yards, na.rm = TRUE)) |>
    filter(n >= 100 & !is.na(passer)) |>
    arrange(desc(adot)) |>
    print(n = Inf)
