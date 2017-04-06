source("R/setup.R")

# ---- kaggle-setup
library(ratchets)

z_score <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)

kaggle_db <- connect_kaggle()
submissions <- get_submissions(kaggle_db)
leaderboards <- make_leaderboards(kaggle_db, submissions)

# Investigate top 100 places only
top_100 <- leaderboards %>%
  filter(Place <= 100) %>%
  divide_into_team_types() %>%
  mutate(
    # Rescale vars for modeling
    TotalTimeZ = z_score(TotalTimeSec),
    TotalSubmissionsZ = z_score(TotalSubmissions)
  )

# Summarize team properties in each place.
top_100_places <- summarize_by_place(top_100)

# Summarize performance in groups:
top_100_by_team_size <- top_100 %>%
  group_by(TeamSize) %>%
  summarize_teams_in_group()

# By submission bin
top_100_by_submission_bin <- top_100 %>%
  label_submission_bins() %>%
  group_by(TotalSubmissionsBin) %>%
  summarize_teams_in_group()

# By submission interval bin
top_100_by_interval_bin <- top_100 %>%
  label_submission_interval_bins() %>%
  group_by(SubmissionIntervalBin) %>%
  summarize_teams_in_group()

kaggle_theme <- base_theme +
  theme(legend.position = "none")

remove_titles <- theme(plot.title = element_blank())
multiplot <- function(gg) gg + remove_titles

# Colors
colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("green", "orange", "blue", "pink")
get_colors <- function(names) unname(colors[names])

colors["first_place"] <- colors[["orange"]]
colors["submissions"] <- colors[["blue"]]
colors["team_size"] <- colors[["green"]]


team_type_colors <- unname(colors[c("green", "blue", "pink", "orange")])

default_alpha <- 0.6

# Scales
scale_x_place <- scale_x_continuous("place", breaks = c(1, seq(10, 100, by = 10)))
scale_y_place <- scale_y_reverse("place", breaks = c(1, seq(10, 100, by = 10)))

scale_x_team_size <- scale_x_continuous("team size", breaks = 1:4)
scale_y_team_size <- scale_y_continuous("team size", breaks = 1:4)

scale_x_total_submissions <- scale_x_continuous(
  "total submissions", breaks = c(1, seq(100, 600, by = 100)))
scale_x_submission_number <- scale_x_continuous(
  "submission number", breaks = c(1, seq(100, 600, by = 100)))
scale_y_submissions <- scale_y_continuous(
  "total submissions", breaks = c(1, seq(5, 100, by = 5)))

scale_y_total_time <- ratchets::make_time_scale("submission interval (days)",
                                                seq(0, 200, by = 20))

# Limits
top_100_submissions_ylim <- c(1, 39)
top_100_places_xlim <- c(1, 100)

# Team type colors
team_types <- recode_team_type()
scale_x_team_label <- scale_x_discrete("", labels = team_types$TeamLabel)
scale_x_team_num <- scale_x_continuous("", breaks = team_types$TeamNum,
                                       labels = team_types$TeamLabel)
scale_fill_team_type <- scale_fill_manual(values = team_type_colors)
scale_color_team_type <- scale_color_manual(values = team_type_colors)


# ---- titanic
data("titanic")
titanic %<>%
  select(Age, Sex, Survived) %>%
  filter(complete.cases(.)) %>%
  mutate(SexF = factor(Sex),
         AgeBin = cut(Age, breaks = 10))

age_bins <- data_frame(AgeBin = unique(titanic$AgeBin)) %>%
  mutate(AgeStr = as.character(AgeBin) %>% substring(., 2, nchar(.)-1),
         AgeMin = str_split_fixed(AgeStr, ",", n = 2)[,1] %>% as.numeric(),
         AgeMax = str_split_fixed(AgeStr, ",", n = 2)[,2] %>% as.numeric(),
         AgeBinMean = rowMeans(cbind(AgeMax, AgeMin))) %>%
  select(AgeBin, AgeBinMean) %>%
  left_join(titanic) %>%
  group_by(AgeBinMean, Sex) %>%
  summarize(Survived = mean(Survived, na.rm = TRUE),
            People = n())

# library(randomForest)
# f0 <- randomForest(Survived ~ SexF, data = titanic)
# f1 <- randomForest(Survived ~ SexF + Age, data = titanic)

scale_y_survival <- scale_y_continuous("survival rate (training data)",
                                       breaks = seq(0, 1, by = 0.25),
                                       labels = percent)
ylim_survival <- c(0, 1)

titanic_plot <- ggplot(titanic) +
  scale_y_survival +
  scale_fill_manual(values = get_colors(c("green", "blue"))) +
  scale_color_manual(values = get_colors(c("green", "blue"))) +
  coord_cartesian(ylim = ylim_survival) +
  kaggle_theme

overall_sex <- titanic_plot +
  aes(Sex, Survived, fill = Sex) +
  labs(x = "") +
  geom_bar(stat = "summary", fun.y = "mean", alpha = default_alpha) +
  theme(panel.grid.major.x = element_blank())

sex_by_age <- titanic_plot +
  aes(Age, Survived, color = Sex) +
  labs(x = "age") +
  geom_smooth(method = "glm", se = FALSE) +
  geom_point(aes(x = AgeBinMean, size = People),
             data = age_bins)

grid.arrange(overall_sex, sex_by_age,
             nrow = 1)

# ---- leaderboards
top_100 %<>%
  mutate(PlaceScaled = as.numeric(scale(Place)),
         PlaceScaledSqr = PlaceScaled^2)

submissions_per_place_mod <- lm(TotalSubmissions ~ PlaceScaled + PlaceScaledSqr,
                                data = top_100)

submissions_per_place_preds <- unique(top_100[, c("Place", "PlaceScaled")]) %>%
  mutate(PlaceScaledSqr = PlaceScaled^2) %>%
  cbind(predict(submissions_per_place_mod, newdata = ., se = TRUE)) %>%
  rename(TotalSubmissions = fit, TotalSubmissionsSE = se.fit) %>%
  label_place_groups()

gg_submissions_per_place <- ggplot(top_100_places) +
  aes(Place, TotalSubmissions) +
  geom_ribbon(aes(ymin = TotalSubmissions - TotalSubmissionsSE,
                  ymax = TotalSubmissions + TotalSubmissionsSE),
              fill = "gray", alpha = 0.4,
              data = submissions_per_place_preds) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim) +
  kaggle_theme +
  labs(title = "Top place teams make more submissions")

relative_submissions_per_place_mod <- lm(
  SubmissionsToFirstPlace ~ PlaceScaled + PlaceScaledSqr,
  data = top_100
)

relative_submissions_per_place_preds <- unique(top_100[, c("Place", "PlaceScaled")]) %>%
  mutate(PlaceScaledSqr = PlaceScaled^2) %>%
  cbind(predict(relative_submissions_per_place_mod, newdata = ., se = TRUE)) %>%
  rename(SubmissionsToFirstPlace = fit, SubmissionsToFirstPlaceSE = se.fit) %>%
  label_place_groups()

gg_relative_submissions_per_place <- ggplot(top_100_places) +
  aes(Place, SubmissionsToFirstPlace) +
  geom_ribbon(aes(ymin = SubmissionsToFirstPlace - SubmissionsToFirstPlaceSE,
                  ymax = SubmissionsToFirstPlace + SubmissionsToFirstPlaceSE),
              fill = "gray", alpha = 0.4,
              data = relative_submissions_per_place_preds) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha,
             stat = "summary", fun.y = "mean") +
  scale_x_place +
  scale_y_continuous("submissions\nrelative to first place team",
                     breaks = seq(-100, 10, by = 5)) +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim - 40) +
  kaggle_theme +
  labs(title = "Top place teams make more submissions")

place_mod <- glmer(Place ~ TotalSubmissions + (TotalSubmissions|CompetitionId),
                   family = "poisson", data = top_100)

gg_place_from_submissions <- ggplot(top_100_by_submission_bin, aes(TotalSubmissionsBin, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  scale_x_total_submissions +
  scale_y_place +
  scale_size_continuous("proportion of teams", labels = percent, breaks = rev(c(0.01, 0.05, 0.15, 0.6))) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  kaggle_theme +
  theme(legend.position = "bottom") +
  ggtitle("Making more submissions improves place")

place_preds <- get_place_mod_preds(place_mod, predict_fn = predictSE)
# The predictions for this hierarchical place mod do not align with means,
# indicating that there are large differences between competitions.
# The conclusions are the same, but the plot looks weird. In addition
# to showing the hierarchical model preds, here I'm also showing
# the predictions of a simple linear model.
place_mod_lm <- lm(Place ~ TotalSubmissions, data = top_100)
place_mod_lm_preds <- get_place_mod_preds(place_mod_lm)

gg_place_from_submissions <- gg_place_from_submissions +
  geom_line(data = place_mod_lm_preds, color = colors[["orange"]])
# geom_line(data = place_preds, color = colors[["green"]])

sample_teams <- function(n_teams = 1, min_submissions = 50, 
                         min_final_place = 100, seed = NA) {
  
  if (!is.na(seed)) set.seed(seed)
  
  team_ids <- leaderboards %>%
    filter(
      TotalSubmissions >= min_submissions,
      Place <= min_final_place
    ) %>%
    sample_n(n_teams) %>%
    .$TeamId
  
  submissions %>% filter(TeamId %in% team_ids)
}

n_teams <- 200
submissions_sample <- sample_teams(n_teams = n_teams, seed = 821)

gg_predicted_place_from_submissions <- ggplot(submissions_sample, aes(SubmissionNum, PredictedPlace)) +
  geom_smooth(aes(group = TeamId), method = "lm", se = FALSE,
              size = 0.4, alpha = 0.4, color = colors[["submissions"]]) +
  scale_x_submission_number +
  scale_y_reverse("place", breaks = c(1, 500, seq(1000, 5000, by = 1000))) +
  kaggle_theme +
  theme(legend.position = "none") +
  labs(title = paste("Changes in performance for", n_teams, "teams"))

team_submissions_mod <- lmer(PredictedPlace ~ SubmissionNum + 
                               (SubmissionNum|CompetitionId/TeamId),
                             data = submissions_sample)

team_submissions_preds <- data_frame(SubmissionNum = 1:100) %>%
  cbind(., predictSE(team_submissions_mod, newdata = .)) %>%
  rename(PredictedPlace = fit, SE = se.fit)

gg_predicted_place_from_submissions <- gg_predicted_place_from_submissions +
  geom_smooth(aes(ymin = PredictedPlace - SE, ymax = PredictedPlace + SE),
              data = team_submissions_preds, color = colors[["orange"]],
              size = 1.5)

grid.arrange(
  multiplot(gg_submissions_per_place),
  multiplot(gg_relative_submissions_per_place),
  multiplot(gg_place_from_submissions),
  multiplot(gg_predicted_place_from_submissions)
)

# ---- submission-interval
gg_submission_interval_per_place <- ggplot(top_100_places, aes(Place, TotalTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  make_time_scale("submission interval (days)", breaks_days = seq(0, 30, by = 5)) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 30 * 24 * 3600)) +
  kaggle_theme +
  labs(title = "Consistent submission intervals across places")


gg_prop_time_per_place <- ggplot(top_100_places, aes(Place, PropCompetitionTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_continuous("proportion time used", labels = percent) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 1)) +
  kaggle_theme

grid.arrange(
  multiplot(gg_submission_interval_per_place),
  multiplot(gg_prop_time_per_place),
  nrow = 1
)

# ---- team-types-quartet
submission_rates <- list(
  steady = data_frame(SubmissionNum = 1:10, SubmissionTime = 1:10),
  long   = data_frame(SubmissionNum = 1:2, SubmissionTime = c(1, 10)),
  short  = data_frame(SubmissionNum = 1:2, SubmissionTime = c(1, 2)),
  rapid  = data_frame(SubmissionNum = 1:10,
                      SubmissionTime = seq(1, 2, length.out = 10))
) %>% bind_rows(.id = "TeamType") %>% recode_team_type

# Select first and last points for arrow
arrow_data <- submission_rates %>%
  group_by(TeamType) %>%
  filter(SubmissionNum == min(SubmissionNum) | SubmissionNum == max(SubmissionNum)) %>%
  mutate(Arrow = c("Start", "End")) %>%
  ungroup() %>%
  select(TeamType, Arrow, SubmissionTime) %>%
  spread(Arrow, SubmissionTime) %>%
  recode_team_type()

# Create labels
label_data <- arrow_data %>%
  transmute(TeamType, SubmissionTime = rowMeans(cbind(End, Start))) %>%
  recode_team_type()

# Total time
total_time <- arrow_data %>%
  transmute(TeamType, TotalTime = End - Start) %>%
  recode_team_type()

# Quadrant plot data
team_type_points <- submission_rates %>%
  group_by(TeamType) %>%
  summarize(TotalSubmissions = n(),
            TotalTime = max(SubmissionTime) - min(SubmissionTime)) %>%
  recode_team_type() %>%
  mutate(
    NudgeY = ifelse(TotalTime > 5, -1.5, 1.5),
    NudgeX = ifelse(TotalSubmissions > 5, -1.5, 1.5)
  )

gg_base <- ggplot(submission_rates, aes(TeamType)) +
  kaggle_theme

geom_text_size <- 6

gg_timeline <- gg_base +
  geom_segment(
    aes(x = TeamType, xend = TeamType, y = Start, yend = End, color = TeamLabel),
    data = arrow_data, alpha = default_alpha
  ) +
  geom_point(aes(y = SubmissionTime, color = TeamLabel),
             size = 1.8, alpha = default_alpha) +
  geom_text(aes(y = SubmissionTime, label = TeamLabel, color = TeamLabel),
            data = label_data,
            vjust = 1.8, size = geom_text_size) +
  scale_x_discrete("") +
  scale_y_continuous("competition time", breaks = 1:10) +
  scale_color_team_type +
  coord_flip() +
  kaggle_theme +
  theme(axis.text.y = element_blank())

gg_num_submissions <- ggplot(submission_rates, aes(TeamLabel)) +
  geom_bar(aes(fill = TeamLabel), stat = "count", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("number of submissions", breaks = 1:10) +
  scale_fill_team_type +
  kaggle_theme +
  theme(panel.grid.major.x = element_blank())

gg_total_time <- ggplot(total_time, aes(TeamLabel, TotalTime)) +
  geom_bar(aes(fill = TeamLabel), stat = "identity", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("submission interval", breaks = 1:10) +
  scale_fill_manual(values = team_type_colors) +
  kaggle_theme +
  theme(panel.grid.major.x = element_blank())

gg_regions <- ggplot(team_type_points, aes(TotalSubmissions, TotalTime)) +
  geom_point(aes(color = TeamLabel), size = 3, alpha = default_alpha) +
  geom_text(
    aes(x = TotalSubmissions + NudgeX, y = TotalTime + NudgeY,
        label = TeamLabel, color = TeamLabel),
    size = geom_text_size) +
  geom_hline(yintercept = median(team_type_points$TotalTime),
             color = "gray", lty = 2) +
  geom_vline(xintercept = median(team_type_points$TotalSubmissions),
             color = "gray", lty = 2) +
  scale_x_continuous("number of submissions", breaks = 1:10) +
  scale_y_continuous("submission interval", breaks = 1:9) +
  scale_color_team_type +
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 10)) +
  kaggle_theme

grid.arrange(gg_timeline, gg_regions,
             gg_total_time, gg_num_submissions,
             nrow = 2)

# ---- team-types
gg_team_types_density <- ggplot(top_100, aes(TotalSubmissions, TotalTimeSec)) +
  geom_point(aes(color = TeamLabel), alpha = 0.2) +
  geom_hline(yintercept = median(top_100$TotalTimeSec),
             color = "gray", lty = 2) +
  geom_vline(xintercept = median(top_100$TotalSubmissions),
             color = "gray", lty = 2) +
  scale_x_total_submissions +
  make_time_scale("submission interval (days)", seq(0, 400, by = 100)) +
  # Bug! scale_color_team_type doesn't work. Have to set colors manually.
  scale_color_manual(values = get_colors(c("pink", "blue", "green", "orange"))) +
  kaggle_theme +
  coord_cartesian(
    xlim = c(1, 300),
    ylim = c(0, 200 * 24 * 60 * 60)
  )

gg_quadrant_sizes <- ggplot(top_100) +
  aes(TeamLabel, fill = TeamLabel) +
  geom_bar(stat = "count", alpha = default_alpha) +
  scale_x_team_label +
  scale_y_continuous("number of teams") +
  scale_fill_team_type +
  kaggle_theme

make_rev_rects <- function(frame) {
  width <- 0.9
  baseline <- 100
  frame %>%
    mutate(xmin = TeamNum - width/2, xmax = TeamNum + width/2,
           ymin = Place, ymax = baseline)
}

team_type_means <- top_100 %>%
  group_by(TeamType) %>%
  summarize(Place = mean(Place)) %>%
  recode_team_type() %>%
  make_rev_rects()

gg_team_types <- ggplot(top_100, aes(TeamNum, Place)) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=TeamLabel),
            data = team_type_means, alpha = default_alpha) +
  scale_x_team_num +
  scale_y_reverse("place", breaks = seq(2, 100, by = 2)) +
  scale_color_team_type +
  scale_fill_team_type +
  coord_cartesian(ylim = c(34, 50)) +
  kaggle_theme

team_types_lm_mod <- lm(Place ~ ShortVSteady + ShortVLong + ShortVRapid,
                        data = top_100)

team_types_preds <- recode_team_type() %>%
  cbind(., predict(team_types_lm_mod, newdata = ., se = TRUE)) %>%
  rename(Place = fit, SE = se.fit)

gg_team_types_place <- gg_team_types +
  geom_linerange(aes(ymin = Place + SE, ymax = Place - SE),
                 data = team_types_preds)

grid.arrange(
  multiplot(gg_timeline),
  multiplot(gg_team_types_density),
  multiplot(gg_quadrant_sizes),
  multiplot(gg_team_types_place),
  nrow = 2
)