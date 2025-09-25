library(ggplot2)
library(gganimate)
library(dplyr)

# Pyramid parameters (Khufu, Khafre, Menkaure)
pyramids <- data.frame(
  name = c("Khufu", "Khafre", "Menkaure"),
  layers = c(210, 200, 180),
  base_side = c(230.4, 215.5, 103.4),  # meters
  total_stones = c(2300000, 1700000, 235000),
  max_crew = c(18000, 12000, 5000)    # Increased for Khufu, Khafre
)
stone_weight_avg <- 1.5  # tons
djed_ropes <- 3         # 2 pull groups (8 Ankh sub-ropes), 1 retractor
pull_groups <- 5        # per layer
days_per_stone <- 0.005 # ~7.2 mins per stone
ramp_time_factor <- 0.1 # 10% extra for ramps
fatigue_rate <- 0.02    # Reduced from 0.05

# Simulate construction for each pyramid
results <- lapply(1:nrow(pyramids), function(i) {
  n_layers <- pyramids$layers[i]
  base_side <- pyramids$base_side[i]
  total_stones <- pyramids$total_stones[i]
  max_crew <- pyramids$max_crew[i]
  
  # Layer data
  layer_data <- data.frame(
    layer = 1:n_layers,
    pyramid = pyramids$name[i],
    side_length = seq(base_side, base_side / n_layers, length.out = n_layers),
    stones = round(seq(total_stones / n_layers, 100, length.out = n_layers)),
    crew = round(seq(max_crew, 500, length.out = n_layers)),
    pull_groups = pull_groups,
    days = NA,
    lift_ok = TRUE
  )
  
  # Adjust for stone weight and fatigue
  stone_weights <- runif(n_layers, 1, 2)  # Tighter range: 1–2 tons
  layer_data$days <- layer_data$stones * days_per_stone * (stone_weights / stone_weight_avg)
  for (j in 1:n_layers) {
    years_so_far <- sum(layer_data$days[1:j]) / 365
    if (years_so_far > 5) {
      layer_data$days[j] <- layer_data$days[j] * (1 + fatigue_rate * (years_so_far - 5))
    }
  }
  layer_data$days <- layer_data$days * (1 + ramp_time_factor)
  layer_data$lift_ok <- layer_data$stones * djed_ropes <= layer_data$crew * 0.8
  
  layer_data
})

# Combine results
all_data <- bind_rows(results)
total_years <- all_data %>% group_by(pyramid) %>% summarise(total_years = sum(days) / 365)

# Console output
for (p in pyramids$name) {
  cat(sprintf("--- %s PYRAMID ---\n", toupper(p)))
  p_data <- all_data %>% filter(pyramid == p)
  cat(sprintf("Layers: %d, Total Stones: %d\n", max(p_data$layer), sum(p_data$stones)))
  for (i in 1:nrow(p_data)) {
    cat(sprintf("LAYER %d: %d stones | Days: %.1f | Crew: %d | Pull Groups: %d | Lift OK: %s\n",
                p_data$layer[i], p_data$stones[i], p_data$days[i],
                p_data$crew[i], p_data$pull_groups[i], p_data$lift_ok[i]))
  }
  cat(sprintf("Total Time: %.1f years\n", total_years$total_years[total_years$pyramid == p]))
  cat(sprintf("Feasibility: %s\n", ifelse(total_years$total_years[total_years$pyramid == p] <= 20, "FEASIBLE!", "CHECK CONSTRAINTS")))
}

# Animation data
anim_data <- all_data %>%
  group_by(pyramid) %>%
  mutate(progress = cumsum(stones) / sum(stones) * 100, years = cumsum(days) / 365)

# Create animation
anim <- ggplot(anim_data, aes(x = years, y = progress, size = pull_groups, color = pyramid)) +
  geom_point() +
  transition_states(layer, transition_length = 2, state_length = 1) +
  labs(title = "Giza Pyramids Construction (Tasker Djed, Optimized)",
       x = "Years", y = "% Complete",
       subtitle = "Layer: {closest_state}") +
  scale_color_manual(values = c("Khufu" = "darkgoldenrod", "Khafre" = "darkblue", "Menkaure" = "darkred")) +
  theme_minimal()

# Render and save GIF
anim_save("giza_pyramids_v4.gif", animate(anim, nframes = max(all_data$layer), fps = 5))