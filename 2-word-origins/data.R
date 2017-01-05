source("2-word-origins/setup.R")

# ---- data
library(wordsintransition)
data(responses)
data(transcriptions)
data(matches)

survey_map <- data_frame(
  survey_type = c("between", "same", "within"),
  # Treatment contrasts
  same_v_between = c(1, 0, 0),
  same_v_within = c(0, 0, 1)
)

responses <- responses %>%
  filter(question_type != "catch_trial") %>%
  mutate(generation_1 = generation - 1) %>%
  left_join(survey_map)

transcriptions <- transcriptions %>%
  filter(transcription_survey_name != "hand picked 1 test",
         !(chain_name %in% c("alligator_1.wav", "camel_1.wav")))

question_type_map <- data_frame(
  question_type = c("exact", "category"),
  question_f = factor(question_type, levels = question_type),
  question_c = c(-0.5, 0.5)
)

matches <- matches %>%
  left_join(question_type_map)