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
