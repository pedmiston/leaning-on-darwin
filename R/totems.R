source("R/setup.R")

# ---- totems-setup
library(totems)
data("TotemsTrials")
data("TotemsPlayers")
data("TotemsSampled")
data("TotemsTeams")

TotemsTrials %<>%
  totems::recode_strategy() %>%
  recode_groups_by_generation()

TotemsPlayers %<>%
  totems::recode_strategy()

TotemsTeams %<>%
  totems::recode_strategy()

TotemsSampledMeans <- TotemsSampled %>%
  group_by(Strategy, SampledTime) %>%
  summarize(NumInnovations = mean(NumInnovations)) %>%
  ungroup() %>%
  totems::recode_strategy() %>%
  guess_generation("SampledTime") %>%
  recode_groups_by_generation()

trial0 <- data_frame(
  Strategy = c("Diachronic", "Diachronic", "Synchronic", "Isolated"),
  Generation = c(1, 2, 1, 1),
  SampledTime = c(0, 60*25, 0, 0),
  NumInnovations = 0
) %>%
  recode_strategy() %>%
  recode_groups_by_generation()

TotemsSampledMeans %<>%
  bind_rows(trial0)

# Summarize guesses at each stage (each inventory)
TeamInventoryGuesses <- TotemsTrials %>%
  filter(Result == 0) %>%
  group_by(TeamID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(TeamUniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TotemsTrials, TeamID, TeamInventory, Strategy, NumTeamInnovations)) %>%
  totems::recode_strategy()

IndividualInventoryGuesses <- TotemsTrials %>%
  filter(Result == 0) %>%
  group_by(PlayerID, TeamInventory) %>%
  summarize(
    Guesses = n(),
    Redundancy = 1 - (sum(UniqueGuess)/n())
  ) %>%
  ungroup() %>%
  left_join(select(TotemsTrials, PlayerID, TeamID, TeamInventory, Strategy, NumInnovations)) %>%
  totems::recode_strategy()

totems_theme <- load_totems_theme()
totems_theme["base_theme"] <- list(base_theme)

scale_x_inventory_size <- scale_x_continuous("Inventory size",
                                             breaks = 6:15)

# ---- types-of-time
diachronic <- data_frame(
  Strategy = "Diachronic",
  CalendarHours = 1:100,
  LaborHours = CalendarHours,
  Person = rep(c(1, 2), each = 50)
)

isolated <- diachronic %>%
  mutate(
    Strategy = "Isolated",
    Person = 1
  )

synchronic <- data_frame(
  Strategy = "Synchronic",
  CalendarHours = 1:50,
  LaborHours = CalendarHours * 2,
  Person = 1
)

time <- rbind(diachronic, synchronic, isolated) %>%
  group_by(Strategy, Person) %>%
  mutate(PersonHours = 1:n()) %>%
  ungroup() %>%
  recode_strategy() %>%
  mutate(
    # Separate Diachronic and Isolated lines
    LaborHours = ifelse(Strategy == "Synchronic", LaborHours,
                        ifelse(Strategy == "Diachronic", LaborHours + 0.8, LaborHours - 0.8))
  )

axis_breaks <- c(0, 50, 100)
axis_labels <- c(0, expression(1/2), 1)

gg_time <- ggplot(time, aes(CalendarHours, LaborHours)) +
  geom_line(aes(color = StrategyLabel), size = 1.2) +
  scale_x_continuous("Calendar hours", breaks = axis_breaks, labels = axis_labels) +
  scale_y_continuous("Labor hours", breaks = axis_breaks, labels = axis_labels) +
  totems_theme["scale_color_strategy"] +
  scale_linetype_manual(values = c(1, 1, 2)) +
  guides(color = guide_legend("", reverse = TRUE), linetype = "none") +
  totems_theme["base_theme"] +
  theme(legend.position = c(0.75, 0.2))

time %<>%
  mutate(StrategyIsolated = factor(Strategy, levels = c("Synchronic", "Diachronic", "Isolated")))

gg_person <- ggplot(time) +
  aes(StrategyIsolated, PersonHours) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "max",
           alpha = 0.8) +
  scale_x_discrete("") +
  scale_y_continuous("Learning hours", breaks = axis_breaks, labels = axis_labels) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank())

gridExtra::grid.arrange(
  read_graphviz("team-structures", package = "totems"),
  gg_time,
  gg_person,
  nrow = 1,
  widths = c(0.2, 0.4, 0.4)
)

# ---- sample-landscape
# Trajectories plot
data("BotsExplore")
sample_trajectories <- BotsExplore %>%
  filter(inventory_size <= 15) %>%
  select(sim_id, inventory, inventory_size, trajectory, n_adjacent) %>%
  group_by(inventory_size) %>%
  summarize(n_unique_trajectories = length(unique(trajectory)))

unique_trajectories_plot <- ggplot(sample_trajectories) +
  aes(inventory_size, n_unique_trajectories, group = 1) +
  geom_line(size = 1.2, color = "gray") +
  scale_x_inventory_size +
  scale_y_continuous("Unique trajectories", breaks = c(1, seq(10, 80, by = 10))) +
  totems_theme["base_theme"] +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Combinatorial explosion plot
calculate_num_guesses <- function(step_info) {
  n <- step_info$inventory_size
  r <- step_info$guess_size
  factorial(n)/(factorial(r) * factorial(n-r))
}

possible_guesses <- expand.grid(inventory_size = 6:15,
                                guess_size = 1:4) %>%
  by_row(calculate_num_guesses, .to = "n_combinations",
         .collate = "rows") %>%
  group_by(inventory_size) %>%
  summarize(n_combinations = sum(n_combinations)) %>%
  mutate(n_new_combinations = n_combinations - lag(n_combinations, 1, default = 0)) %>%
  gather(combination_type, combination_value, -inventory_size)

correct_combinations <- BotsExplore %>%
  filter(inventory_size <= 15) %>%
  select(inventory_size, n_adjacent) %>%
  unique() %>%
  group_by(inventory_size) %>%
  summarize(combination_value = median(n_adjacent)) %>%
  mutate(combination_type = "n_correct_combinations")
possible_guesses %<>% bind_rows(correct_combinations)

combination_type_labels <- data_frame(
  combination_type = c("n_combinations", "n_new_combinations", "n_correct_combinations"),
  label = c("Total", "New", "Correct"),
  inventory_size = 13,
  combination_value = c(750, 450, 100)
)

possible_guesses_plot <- ggplot(possible_guesses) +
  aes(inventory_size, combination_value, color = combination_type) +
  geom_line(size = 1.2) +
  geom_text(aes(label = label), data = combination_type_labels) +
  # Uncomment to add text labels showing the number of correct combinations
  # geom_label(aes(label = combination_value),
  #            data = filter(possible_guesses, combination_type == "n_correct_combinations")) +
  scale_x_inventory_size +
  scale_y_continuous("Combinations") +
  scale_color_manual(
    # alphabetical: n_combinations, n_correct_combinations, n_new_combinations
    values = c(totems_theme$synchronic_color, totems_theme$isolated_color, totems_theme$diachronic_color)
  ) +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.minor.x = element_blank()
  )

grid.arrange(
  read_graphviz_with_images("landscape-sample", "totems"),
  arrangeGrob(possible_guesses_plot, unique_trajectories_plot, ncol = 1),
  nrow = 1,
  widths = c(0.6, 0.4)
)

# ---- totems-inventory
inventory_mod <- lm(
  NumInnovations ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TotemsTeams
)

inventory_preds <- get_lm_mod_preds(inventory_mod) %>%
  rename(NumInnovations = fit, SE = se.fit) %>%
  totems::recode_strategy()

performance_plot <- ggplot(TotemsTeams) +
  aes(StrategyLabel, NumInnovations) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.3),
             alpha = 0.8) +
  geom_bar(aes(fill = StrategyLabel),
           stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = NumInnovations - SE, ymax = NumInnovations + SE),
                width = 0.2, data = inventory_preds) +
  ylab("Number of inventions") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank()
  )

performance_over_time_plot <- ggplot(TotemsSampledMeans) +
  aes(TeamTime, NumInnovations, color = StrategyLabel) +
  geom_line(aes(SampledTime, group = GenerationStrategy), size = 1.2) +
  scale_x_time("Team time", breaks = seconds(c(0, 25 * 60, 50 * 60))) +
  scale_y_continuous("Number of inventions", breaks = seq(0, 20, by = 2)) +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

grid.arrange(
  performance_over_time_plot,
  performance_plot,
  nrow = 1,
  widths = c(0.6, 0.4)
)

# ---- effectiveness
team_efficiency_mod <- lmer(
  Guesses ~
    Diachronic_v_Synchronic + Diachronic_v_Isolated +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|TeamInventory) +
    NumTeamInnovations +
    (1|TeamID),
  data = TeamInventoryGuesses
)

individual_efficiency_mod <- lmer(
  Guesses ~
    Diachronic_v_Synchronic + Diachronic_v_Isolated +
    (Diachronic_v_Synchronic + Diachronic_v_Isolated|TeamInventory) +
    NumInnovations +
    (1|PlayerID),
  data = IndividualInventoryGuesses 
)

team_efficiency_preds <- expand.grid(
  Strategy = totems::recode_strategy()$Strategy,
  NumTeamInnovations = mean(TeamInventoryGuesses$NumTeamInnovations),
  stringsAsFactors = FALSE
) %>%
  totems::recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(team_efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit)

individual_efficiency_preds <- expand.grid(
  Strategy = totems::recode_strategy()$Strategy,
  NumInnovations = mean(IndividualInventoryGuesses$NumInnovations),
  stringsAsFactors = FALSE
) %>%
  totems::recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(individual_efficiency_mod, newdata = ., se = TRUE)) %>%
  rename(Guesses = fit, SE = se.fit)

efficiency_preds <- bind_rows(
  `Individual guesses per invention` = individual_efficiency_preds,
  `Team guesses per invention` = team_efficiency_preds,
  .id = "GuessMeasure"
)

efficiency_plot <- ggplot(efficiency_preds) +
  aes(StrategyLabel, Guesses) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = Guesses - SE, ymax = Guesses + SE),
                width = 0.3) +
  facet_wrap("GuessMeasure", strip.position = "left") +
  totems_theme["scale_x_strategy"] +
  ylab("Guesses per invention") +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    axis.title.y = element_blank()
  )
efficiency_plot

# ---- redundancy
team_redundancy_mod <- lmer(
  Redundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|TeamID),
  data = TeamInventoryGuesses
)

individual_redundancy_mod <- lmer(
  Redundancy ~ Diachronic_v_Synchronic + Diachronic_v_Isolated + (1|PlayerID),
  data = IndividualInventoryGuesses
)

team_redundancy_preds <- totems::recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(team_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

individual_redundancy_preds <- totems::recode_strategy() %>%
  cbind(., AICcmodavg::predictSE(individual_redundancy_mod, newdata = ., se = TRUE)) %>%
  rename(Redundancy = fit, SE = se.fit)

Redundancy <- bind_rows(
  `Team redundancy` = TeamInventoryGuesses,
  `Individual redundancy` = IndividualInventoryGuesses,
  .id = "RedundancyMeasure"
)

redundancy_preds <- bind_rows(
  `Team redundancy` = team_redundancy_preds,
  `Individual redundancy` = individual_redundancy_preds,
  .id = "RedundancyMeasure"
)

redundancy_plot <- ggplot(redundancy_preds) +
  aes(StrategyLabel, Redundancy) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean", alpha = 0.6) +
  geom_errorbar(aes(ymin = Redundancy - SE, ymax = Redundancy + SE),
                width = 0.2) +
  facet_wrap("RedundancyMeasure", strip.position = "left") +
  coord_cartesian(ylim = c(0, 0.6)) +
  totems_theme["scale_x_strategy"] +
  scale_y_continuous("Redundancy", labels = scales::percent) +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.placement = "outside",
    axis.title.y = element_blank()
  )
redundancy_plot

# ---- trajectories
data("Trajectories")

Trajectories %<>%
  rename(Strategy = Treatment)

TrajectoryCounts <- Trajectories %>%
  group_by(Strategy) %>%
  summarize(
    NumUniqueTrajectories = length(unique(TrajectoryID)),
    NumTeams = length(unique(TeamID)),
    UniqueTrajectoriesPerTeam = NumUniqueTrajectories/NumTeams
  ) %>%
  totems::recode_strategy()

trajectory_count_plot <- ggplot(TrajectoryCounts) +
  aes(StrategyLabel, UniqueTrajectoriesPerTeam) +
  geom_bar(aes(fill = StrategyLabel), stat = "identity", alpha = 0.6) +
  ylab("Unique trajectories discovered per team") +
  totems_theme["scale_x_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )
trajectory_count_plot

# ---- totems-attempts
team_attempts_mod <- lm(
  NumGuesses ~ Diachronic_v_Synchronic + Diachronic_v_Isolated,
  data = TotemsTeams
)

team_attempts_preds <- get_lm_mod_preds(team_attempts_mod) %>%
  rename(NumGuesses = fit, SE = se.fit) %>%
  recode_strategy()

attempts_plot <- ggplot(TotemsTeams) +
  aes(StrategyLabel, NumGuesses) +
  geom_point(aes(color = StrategyLabel),
             position = position_jitter(width = 0.4)) +
  geom_bar(aes(fill = StrategyLabel), stat = "summary", fun.y = "mean",
           alpha = 0.6) +
  geom_errorbar(aes(ymin = NumGuesses - SE, ymax = NumGuesses + SE),
                data = team_attempts_preds, width = 0.2) +
  totems_theme["scale_x_strategy"] +
  scale_y_continuous("Number of attempts") +
  totems_theme["scale_color_strategy"] +
  totems_theme["scale_fill_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "none")

performance_by_attempts_plot <- ggplot(TotemsTeams) +
  aes(NumGuesses, NumInnovations, color = StrategyLabel) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous("Number of attempts") +
  scale_y_continuous("Number of inventions") +
  totems_theme["scale_color_strategy"] +
  totems_theme["base_theme"] +
  theme(legend.position = "top")

grid.arrange(
  attempts_plot,
  performance_by_attempts_plot,
  nrow = 1
)