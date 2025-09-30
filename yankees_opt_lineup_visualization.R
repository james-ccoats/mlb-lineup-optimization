library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Prepare top 5 lineups
top_lineups <- head(best, 5) |>
  mutate(avg = round(avg, 2),
         lineup_id = paste0("Lineup ", row_number(), " (", avg, ")")) |>
  separate(order, into = paste0("B", 1:9), sep = "-")

# Pivot longer for plotting
lineup_long <- top_lineups |>
  pivot_longer(cols = starts_with("B"), names_to = "position", values_to = "batter") %>%
  mutate(
    batter = str_remove(word(batter, 1), ",")
  )

# Heatmap
ggplot(lineup_long, aes(x = position, y = fct_rev(factor(lineup_id)), fill = batter)) + 
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = batter), size = 3, color = "white") +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Top 5 Yankees Lineups vs LHP (Simulated Avg Runs)",
       x = "Batting Position",
       y = "Lineup (Avg Runs per Inning)",
       fill = "Batter") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        axis.text.y = element_text(face = "bold"),
        panel.grid = element_blank())
