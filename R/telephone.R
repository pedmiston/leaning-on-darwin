source("R/setup.R")

# ---- telephone-setup
library(wordsintransition)
scale_y_gts_accuracy <- scale_y_continuous("Accuracy", breaks = seq(0, 1, by = 0.1),
                                           labels = scales::percent)
chance_line <- geom_hline(yintercept = 0.25, lty = 2, alpha = 0.4, size = 1)
ylim_gts <- c(0.15, 0.75)

scale_x_block_ix <- scale_x_continuous("Block number (24 trials per block)",
                                       breaks = 1:4)
scale_y_rt <- scale_y_continuous("Reaction time (msec)")
scale_color_message_label <- scale_color_manual(
  "Transcription of",
  labels = c("Sound effect", "First generation imitation", "Last generation imitation"),
  values = unname(colors[c("orange", "green", "blue")])
)
scale_color_message_label_2 <- scale_color_manual(
  "Transcription of",
  labels = c("First generation imitation", "Last generation imitation"),
  values = unname(colors[c("green", "blue")])
)
scale_linetype_message_label_2 <- scale_linetype_manual(
  "Transcription of",
  labels = c("First generation imitation", "Last generation imitation"),
  values = c("solid", "longdash")
)


# ---- telephone-stabilization
data("acoustic_similarity_judgments")
acoustic_similarity_judgments %<>%
  mutate(similarity = ifelse(similarity == -1, NA, similarity)) %>%
  z_score_by_subj() %>%
  recode_edge_generations() %>%
  determine_trial_id()

similarity_judgments_mod <- lmer(
  similarity_z ~ edge_generation_n + (edge_generation_n|name) + (edge_generation_n|category),
  data = acoustic_similarity_judgments
)

similarity_judgments_preds <- data_frame(edge_generation_n = 1:7) %>%
  cbind(., predictSE(similarity_judgments_mod, newdata = ., se = TRUE)) %>%
  rename(similarity_z = fit, se = se.fit) %>%
  recode_edge_generations()

similarity_judgments_means <- acoustic_similarity_judgments %>%
  group_by(edge_generations, category) %>%
  summarize(similarity_z = mean(similarity_z, na.rm = TRUE)) %>%
  recode_edge_generations

set.seed(949)
gg_similarity_judgments <- ggplot(similarity_judgments_means) +
  aes(x = edge_generations, y = similarity_z) +
  geom_point(aes(color = category, shape = category),
             position = position_jitter(0.1, 0.0),
             size = 2.5) +
  geom_smooth(aes(group = 1, ymin = similarity_z - se, ymax = similarity_z + se),
              data = similarity_judgments_preds, stat = "identity",
              alpha = 0.2, color = "gray") +
  scale_x_discrete("Generation") +
  scale_y_continuous("Acoustic similarity (z-score)") +
  scale_color_brewer("", palette = "Set2") +
  scale_shape_discrete("") +
  coord_cartesian(ylim = c(-0.6, 0.8)) +
  base_theme +
  theme(legend.position = c(0.1, 0.85))


data("transcription_distances")
message_id_map <- select(imitations, message_id, chain_name, seed_id, generation)
transcription_distances %<>%
  left_join(message_id_map) %>%
  recode_transcription_frequency() %>%
  recode_message_type() %>%
  filter(message_type != "sound_effect")

orthographic_distance_mod <- lmer(distance ~ message_c + (message_c|chain_name/seed_id),
                                  data = transcription_distances)

orthographic_distance_lmertest_mod <- lmerTest::lmer(
  formula(orthographic_distance_mod), data = orthographic_distance_mod@frame
)

orthographic_distance_preds <- data_frame(message_c = c(-0.5, 0.5)) %>%
  cbind(., predictSE(orthographic_distance_mod, newdata = ., se = TRUE)) %>%
  rename(distance = fit, se = se.fit) %>%
  recode_message_type()

gg_distance <- ggplot(transcription_distances) +
  aes(message_label, distance, color = message_label) +
  geom_point(aes(group = message_id),
             stat = "summary", fun.y = "mean",
             position = position_jitter(0.2, 0.01),
             alpha = 0.8, size = 2) +
  geom_errorbar(aes(ymin = distance - se, ymax = distance + se),
                data = orthographic_distance_preds,
                size = 1.4, width = 0.1) +
  scale_x_discrete("", labels = c("First generation imitations", "Last generation imitations")) +
  scale_y_continuous("Distance between transcriptions", breaks = seq(0, 1, by = 0.2)) +
  scale_color_manual(values = get_colors(c("blue", "green"))) +
  scale_fill_manual(values = get_colors(c("blue", "green"))) +
  coord_cartesian(ylim = c(0.0, 0.8)) +
  base_theme +
  theme(legend.position = "none")


grid.arrange(
  gg_similarity_judgments,
  gg_distance,
  nrow = 1
)

# ---- guess-the-seed
data("imitation_matches")
imitation_matches %<>%
  filter(
    question_type != "catch_trial"
  ) %>%
  recode_generation() %>%
  recode_survey_type() %>%
  add_chance()

# q_true_seed <- read_graphviz("true-seed", "wordsintransition")
# q_category_match <- read_graphviz("category-match", "wordsintransition")
# q_specific_match <- read_graphviz("specific-match", "wordsintransition")

imitation_matches_overall_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 + (generation_1|chain_name/seed_id),
  family = "binomial", data = imitation_matches
)

imitation_matches_mod <- glmer(
  is_correct ~ offset(chance_log) + generation_1 * (same_v_between + same_v_within) +
    (generation_1|chain_name/seed_id) + (1|subj_id),
  family = "binomial", data = imitation_matches
)

x_preds <- expand.grid(
    generation_1 = unique(imitation_matches$generation_1) %>% na.omit(),
    survey_type = c("between", "same", "within"),
    stringsAsFactors = FALSE
  ) %>%
  recode_survey_type() %>%
  mutate(
    generation = generation_1 + 1,
    generation_label = paste("Generation", generation)
  ) %>%
  add_chance()

transition_preds <- predictSE(imitation_matches_mod, x_preds, se = TRUE) %>%
  cbind(x_preds, .) %>%
  rename(is_correct = fit, se = se.fit)

distractor_labels <- c("True seed", "Category match", "Specific match")
distractor_colors <- get_colors(c("green", "blue", "orange"))

scale_linetype_distractors <- scale_linetype_manual(
  "",
  values = c("longdash", "dotdash", "solid"),
  labels = distractor_labels
)

scale_color_distractors <- scale_color_manual(
  "",
  values = distractor_colors,
  labels = distractor_labels
)

gg_match_to_seed <- ggplot(imitation_matches) +
  aes(x = generation_1, y = is_correct) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se,
                  color = survey_type, linetype = survey_type),
              stat = "identity", data = transition_preds,
              size = 1.0) +
  scale_x_continuous(
    "Generation",
    breaks = 0:11,
    labels = 1:12
  ) +
  scale_y_gts_accuracy +
  scale_color_distractors +
  scale_linetype_distractors +
  chance_line +
  annotate("text", x = 0.5, y = 0.26, label = "chance",
           size = 4, vjust = -0.1, fontface = "italic",
           alpha = 0.6) +
  coord_cartesian(xlim = c(-0.2, 7.2), ylim = ylim_gts) +
  base_theme +
  theme(
    legend.position = c(0.8, 0.85),
    legend.key.width = unit(5, "lines"),
    panel.grid.minor.x = element_blank()
  )


data("transcription_matches")
transcription_match_failed_catch_trial <- transcription_matches %>%
  filter(question_type == "catch_trial", is_correct == 0) %>%
  .$subj_id %>%
  unique()

transcription_matches %<>%
  recode_question_type() %>%
  recode_message_type() %>%
  recode_version() %>%
  add_chance() %>%
  filter(
    message_type != "sound_effect",
    !(subj_id %in% transcription_match_failed_catch_trial)
  )

acc_mod <- glmer(
  is_correct ~ offset(chance_log) + question_c * message_c + (question_c * message_c|subj_id),
  family = binomial, data = transcription_matches
)

x_preds <- expand.grid(question_c = c(-0.5, 0.5), message_c = c(-0.5, 0.5)) %>%
  add_chance()
y_preds <- predictSE(acc_mod, x_preds, se = TRUE)

message_labels <- data_frame(
  message_type = c("first_gen_imitation", "last_gen_imitation"),
  message_label_2 = c("Transcription of first generation imitation",
                      "Transcription of last generation imitation")
)

preds <- cbind(x_preds, y_preds) %>%
  rename(is_correct = fit, se = se.fit) %>%
  recode_question_type() %>%
  recode_message_type() %>%
  left_join(message_labels)

gg_match_transcriptions <- ggplot(preds) +
  aes(question_c, is_correct) +
  geom_bar(aes(fill = question_type), stat = "identity", width = 0.95, alpha = 0.6) +
  geom_linerange(aes(group = question_type, ymin = is_correct - se, ymax = is_correct + se)) +
  scale_x_continuous("Question type", breaks = c(-0.5, 0.5), labels = c("True seed", "Category match")) +
  scale_y_gts_accuracy +
  scale_fill_manual("", values = unname(colors[c("blue", "green")])) +
  chance_line +
  geom_text(aes(label = label),
            data = data.frame(message_label_2 = "Transcription of first generation imitation",
                              question_c = -0.7, is_correct = 0.27, label = "chance"),
            fontface = "italic") +
  coord_cartesian(ylim = ylim_gts) +
  facet_wrap("message_label_2") +
  base_theme +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


grid.arrange(
  gg_match_to_seed,
  gg_match_transcriptions,
  nrow = 1
)


# ---- category-learning
data("learning_sound_names")

learning_sound_names %<>%
  mutate(rt = ifelse(is_correct == 1, rt, NA),
         is_error = 1 - is_correct) %>%
  mutate(word_category_by_block_ix = paste(word_category, block_ix, sep = ":")) %>%
  recode_lsn_word_type() %>%
  mutate(
    block_ix_sqr = block_ix^2
  )

lsn_outliers <- c("LSN102", "LSN148", "LSN104", "LSN147")
learning_sound_names %<>% filter(!(subj_id %in% lsn_outliers))

trials_per_block <- 24
n_trials <- 6

lsn_transition <- learning_sound_names %>%
  label_trial_in_block() %>%
  bin_trials("block_transition", "trial_in_block",
             before = (trials_per_block-n_trials):trials_per_block,
             after = 1:n_trials) %>%
  filter(
    !(block_ix == 1 & block_transition == "after"),
    !is.na(block_transition),
    message_type != "sound_effect"
  ) %>%
  recode_block_transition()

first_last_gen <- filter(learning_sound_names, message_type != "sound_effect") %>%
  mutate(block_ix_sqr = block_ix^2)

after_first_block <- filter(first_last_gen, block_ix > 1)
lsn_after_first_block_mod <- lmer(
  rt ~ message_c + block_ix + (1 + block_ix|subj_id),
  data = after_first_block)

lsn_quad_mod <- lmer(
  rt ~ message_c * (block_ix + block_ix_sqr) + (block_ix + block_ix_sqr|subj_id),
  data = first_last_gen
)

lsn_quad_preds <- expand.grid(message_c = c(-0.5, 0.5),
                              block_ix = 1:4) %>%
  mutate(block_ix_sqr = block_ix^2) %>%
  cbind(., predictSE(lsn_quad_mod, newdata = ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_message_type()

rt_plot <- ggplot(first_last_gen) +
  aes(block_ix, rt) +
  geom_smooth(aes(ymin = rt - se, ymax = rt + se, color = message_label,
                  linetype = message_label),
              fill = "gray", alpha = 0.4,
              stat = "identity", data = lsn_quad_preds) +
  scale_x_block_ix +
  scale_y_rt +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(legend.position = c(0.8, 0.7),
        legend.key.width = unit(5, "lines"))

transition_mod <- lmer(
  rt ~ block_transition_c * message_c + block_ix + (block_ix|subj_id),
  data = lsn_transition
)

transition_preds <- expand.grid(block_transition_c = c(-0.5, 0.5),
                                message_c = c(-0.5, 0.5),
                                block_ix = 3) %>%
  cbind(., predictSE(transition_mod, ., se = TRUE)) %>%
  rename(rt = fit, se = se.fit) %>%
  recode_block_transition() %>%
  recode_message_type()

dodger <- position_dodge(width = 0.1)

gg_transition <- ggplot(lsn_transition) +
  aes(block_transition_label, rt, color = message_type) +
  geom_linerange(aes(ymin = rt - se, ymax = rt + se),
                 data = transition_preds,
                 position = dodger, show.legend = FALSE,
                 size = 2) +
  geom_line(aes(group = message_type, linetype = message_type),
            data = transition_preds,
            position = dodger, size = 2) +
  scale_x_discrete("Block transition", labels = c("Before", "After")) +
  scale_y_rt +
  scale_color_message_label_2 +
  scale_linetype_message_label_2 +
  coord_cartesian(ylim = c(600, 1200)) +
  base_theme +
  theme(
    legend.position = c(0.7, 0.8),
    legend.key.width = unit(5, "lines")
  )


grid.arrange(
  rt_plot,
  gg_transition,
  nrow = 1
)
