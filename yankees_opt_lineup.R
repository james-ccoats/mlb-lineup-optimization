library(sabRmetrics)
library(parallelly)
library(dplyr)
library(httr)
library(tictoc)

#Download Savant Data
availableWorkers()
cluster <- parallelly::makeClusterPSOCK(4)

savant_data <- sabRmetrics::download_baseballsavant(
  start_date = "2025-06-24",
  end_date = "2025-09-24",
  cl = cluster
)

#Filter for yankees stats against LHP, create totals for events
pa_events <- savant_data |>
  filter(home_team == "NYY" | away_team == "NYY") |>
  filter(pitch_hand == "L") |>
  filter(!is.na(events)) |>
  mutate(
    result = case_when(
      events %in% c("walk", "hit_by_pitch") ~ "bb",
      events == "home_run" ~ "hr",
      events == "triple" ~ "3b",
      events == "double" ~ "2b",
      events == "single" ~ "1b",
      events %in% c("strikeout", "ground_out", "fly_out", "line_out", "field_out") ~ "out",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(result))

# 3. Compute event probabilities for each batter, filter PA to ensure only yankees players listed.
batter_probs <- pa_events |>
  group_by(batter_name) |>
  summarise(
    PA = n(),
    out = sum(result == "out"),
    bb = sum(result == "bb"),
    `1b` = sum(result == "1b"),
    `2b` = sum(result == "2b"),
    `3b` = sum(result == "3b"),
    hr = sum(result == "hr")
  ) |>
  mutate(
    p_out = out / PA,
    p_bb = bb / PA,
    p1b = `1b` / PA,
    p2b = `2b` / PA,
    p3b = `3b` / PA,
    p_hr = hr / PA
  ) |>
  filter(PA >= 21) |>
  select(batter_name, PA, p_out, p_bb, p1b, p2b, p3b, p_hr)

View(batter_probs)


#Inning Simulation Function
simulate_inning <- function(order_idx, probs) {
  outs <- 0
  bases <- c(0,0,0) # 1B, 2B, 3B
  runs <- 0
  batter_pos <- 1
  while (outs < 3) {
    b_idx <- order_idx[batter_pos]
    p <- probs[b_idx,]
    ev <- sample.int(6, size=1, prob=p) # 1=out,2=bb,3=1b,4=2b,5=3b,6=hr
    
    if (ev == 1) {
      outs <- outs + 1
    } else if (ev == 2) { # walk
      if (all(bases==1)) {
        runs <- runs + 1
      }
      if (bases[1]==1 && bases[2]==1) bases[3] <- 1
      if (bases[1]==1) bases[2] <- 1
      bases[1] <- 1
    } else if (ev == 3) { # single
      if (bases[3]==1) { runs <- runs+1; bases[3]<-0 }
      if (bases[2]==1) { bases[3]<-1; bases[2]<-0 }
      if (bases[1]==1) bases[2]<-1
      bases[1] <- 1
    } else if (ev == 4) { # double
      if (bases[3]==1) { runs<-runs+1; bases[3]<-0 }
      if (bases[2]==1) { runs<-runs+1; bases[2]<-0 }
      if (bases[1]==1) { bases[3]<-1; bases[1]<-0 }
      bases[2] <- 1
    } else if (ev == 5) { # triple
      runs <- runs + sum(bases)
      bases <- c(0,0,1)
    } else if (ev == 6) { # HR
      runs <- runs + 1 + sum(bases)
      bases <- c(0,0,0)
    }
    batter_pos <- ifelse(batter_pos==9, 1, batter_pos+1)
  }
  runs
}

#lineup evalutation function
evaluate_lineup <- function(order_idx, probs, n_innings=10000) {
  total_runs <- 0
  for (i in 1:n_innings) {
    total_runs <- total_runs + simulate_inning(order_idx, probs)
  }
  total_runs / n_innings
}

# Convert player list into matrix
names <- batter_probs$batter_name
batter_probs |>
  select(3:8) -> probs

#Example
order_idx <- 1:9
avg_runs <- evaluate_lineup(order_idx, probs, n_innings=5000)
cat("Lineup:", paste(names[order_idx], collapse="-"), "Avg runs per inning:", avg_runs, "\n")


#sample random permutations
set.seed(42)
best <- data.frame(avg=numeric(), order=character())
for (i in 1:2000) {
  perm <- sample(1:9, 9)
  avg <- evaluate_lineup(perm, probs, n_innings=2000)
  best <- rbind(best, data.frame(avg=avg, order=paste(names[perm], collapse="-")))
}
best <- best[order(-best$avg),]
head(best, 10)

