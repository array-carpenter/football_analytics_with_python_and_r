# Load necessary libraries
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("nflfastR")) install.packages("nflfastR")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggthemes")) install.packages("ggthemes")

#install.packages("languageserver")

library("tidyverse")
library("nflfastR")
library("ggplot2")
library("ggthemes")

# Load play-by-play data from 2016 to 2022
pbp_r <- load_pbp(2016:2022)

# Filter for pass plays with non-NA air yards
pbp_r_p <- pbp_r %>%
    filter(play_type == "pass" & !is.na(air_yards)) %>%
    mutate(
        pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
        passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)
    )

# Print summary of all passing yards
print(summary(pbp_r_p$passing_yards))

# Print summary of passing yards for short and long passes
print(summary(pbp_r_p %>% filter(pass_length_air_yards == "short") %>% pull(passing_yards)))
print(summary(pbp_r_p %>% filter(pass_length_air_yards == "long") %>% pull(passing_yards)))

# Print summary of epa for short and long passes
print(summary(pbp_r_p %>% filter(pass_length_air_yards == "short") %>% pull(epa)))
print(summary(pbp_r_p %>% filter(pass_length_air_yards == "long") %>% pull(epa)))


# Create the plot object
plot <- ggplot(pbp_r_p, aes(x = passing_yards)) +
    geom_histogram(bins = 30)

# Save the plot to a file
ggsave("my_histogram_plot.png", plot, width = 10, height = 8)

plot2 <- ggplot(pbp_r_p, aes(x= pass_length_air_yards, y=passing_yards)) +
    geom_boxplot() +
    theme_bw()+
    xlab("Pass length in yards (long >= 20 yards, short < 20 yards)") +
    ylab("Yards gained (or lost) during a passing play")

ggsave("plot2.png", plot2, width=10, height=8)

pbp_r_p_s <-
    pbp_r_p |> 
    group_by(passer_player_name, passer_player_id, season) |>
    summarize(
        ypa = mean(passing_yards, na.rm = TRUE),
        n =n(),
        .groups = "drop"
    )

pbp_r_p_s |> 
    arrange(-ypa) |>
    print()

pbp_r_p_100 <-
    pbp_r_p |>
    group_by(passer_id, passer, season) |>
    summarize(
        n = n(), ypa = mean(passing_yards),
        .groups = "drop"
    ) |>
    filter(n>=100) |>
    arrange(-ypa)

pbp_r_p_100 |>
    print(n = 20)

air_yards_r <-
    pbp_r_p |>
    select(passer_id, passer, season, pass_length_air_yards, passing_yards) |>
    arrange(passer_id, season, pass_length_air_yards) |> 
    group_by(passer_id, passer, pass_length_air_yards, season) |>
    summarize(n = n(),
        ypa = mean(passing_yards),
        .groups = "drop") |>
    filter((n >= 100 & pass_length_air_yards == "short") |
        (n >= 30 & pass_length_air_yards == "long")) |>
    select(-n)

# lag dataframe including a mutate to the seasons and add 1
air_yards_lag_r <-
    air_yards_r |>
    mutate(season = season + 1) |>
    rename(ypa_last = ypa)

# join dfs to create pbp_r_p_s_pl
pbp_r_p_s_pl <- 
    air_yards_r |>
    inner_join(air_yards_lag_r,
    by = c("passer_id","pass_length_air_yards","season","passer"
    ))

pbp_r_p_s_pl |>
    filter(passer %in% c("T.Brady", "A.Rodgers")) |>
    print(n = Inf)

pbp_r_p_s_pl |>
    glimpse()

# use distinct function w/ passer_id and then see how many rows exist
pbp_r_p_s_pl |>
    distinct(passer_id) |>
    nrow()

# scaterplots
scatter_ypa_r <-
    ggplot(pbp_r_p_s_pl, aes(x = ypa_last, y = ypa)) +
    geom_point() +
    facet_grid(cols = vars(pass_length_air_yards)) + 
    labs(
        x = "Yards per Attempt, Year n",
        y = "Yards per Attempt, Year n + 1"
    ) +
    theme_bw() + 
    theme(strip.background = element_blank())
print(scatter_ypa_r)

# encourging for short passes. Include line of best fit 
# add geom_smooth() to previously saved plot
scatter_ypa_r +
    geom_smooth(method="lm")
ggsave(filename="scatter_ypa_r.png", plot=scatter_ypa_r)

# estimate using the correlations
pbp_r_p_s_pl |>
    filter(!is.na(ypa) & !is.na(ypa_last)) |>
    group_by(pass_length_air_yards) |>
    summarize(correlation = cor(ypa, ypa_last))

# Pearson's correlation coefficient can vary from -1 to 1. Number closer to +1 imples strong positive correlations and more stablity
# Number closer to 0 implies weak correlations 
# Number closer to -1 implies decreating correlation and does not exist for stability
