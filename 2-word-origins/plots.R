source("2-word-origins/setup.R")
source("2-word-origins/data.R")
source("2-word-origins/models.R")

# ---- first-gen
ggplot(filter(transition_preds, generation_1 == 0), aes(x = survey_type, y = is_correct)) +
  geom_bar(aes(fill = survey_type), stat = "identity", width = 0.96) +
  geom_linerange(aes(ymin = is_correct - se, ymax = is_correct + se)) +
  scale_y_accuracy +
  scale_x_distractors +
  scale_fill_distractors +
  base_theme +
  chance_line +
  coord_cartesian(ylim = c(0, 1.0)) +
  theme(legend.position = "none") +
  ggtitle("First generation imitations")

# ---- match-to-seed
ggplot(responses, aes(x = generation_1, y = is_correct)) +
  geom_smooth(aes(ymin = is_correct - se, ymax = is_correct + se, color = survey_type),
              stat = "identity", data = transition_preds,
              size = 2.0) +
  scale_x_generation_1 +
  scale_y_accuracy +
  scale_color_distractors +
  chance_line +
  distractor_coords +
  base_theme +
  theme(legend.position = "top", legend.key.size = unit(2, "lines"))

# ---- snapshots
ggplot(filter(transition_preds, generation_1 %in% c(0,7)), aes(x = survey_type, y = is_correct)) +
  geom_bar(aes(fill = survey_type), stat = "identity", width = 0.96) +
  geom_linerange(aes(ymin = is_correct - se, ymax = is_correct + se)) +
  facet_wrap("generation_label") +
  scale_y_accuracy +
  scale_x_distractors +
  scale_fill_distractors +
  base_theme +
  chance_line +
  coord_cartesian(ylim = c(0, 1.0)) +
  theme(legend.position = "none")

# ---- transcription-agreement
examples <- transcriptions %>%
  filter(transcription_survey_name == "hand picked 1") %>%
  count(chain_name, seed_id, text) %>%
  arrange(desc(n)) %>%
  mutate(order = 1:n()) %>%
  filter(order == 1, n > 1) %>%
  select(-order) %>%
  ungroup() %>%
  arrange(desc(n))

examples$text <- factor(examples$text, levels = examples$text)

ggplot(examples, aes(x = text, y = n)) +
  geom_bar(aes(fill = chain_name), stat = "identity") +
  geom_text(aes(label = text), vjust = -0.4, size = 8, angle = 45, hjust = 0) + 
  scale_x_discrete("") +
  scale_y_continuous("Frequency of spelling", breaks = seq(0, 10, by = 2)) +
  scale_fill_categories +
  coord_cartesian(ylim = c(0, 11)) +
  base_theme +
  theme(
    legend.position = "top",
    axis.text.x = element_blank()
  ) +
  ggtitle("Transcription agreement")
