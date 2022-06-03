library(tidyverse) # Data wrangling + plotting
library(tidymodels) # reproducible preprocessing and 
library(tidytext) # Tidy text analysis
library(hunspell) # For finding typos
library(openxlsx) # Read/write xlsx
library(readxl) # Read xlsx
library(skimr) # Quick data exploration
# library(sentimentr)
# install.packages("redres")
# library(redres)
library(jtools) # For plot theme
library(lmer) # Mixed effects modeling
library(broom.mixed) # Tidy tables for mixed models
library(predictmeans) # residual plots for lmer
source("Scripts/calculate_sentiment.R")

# TODO: correct negating words

# Init project ----------------------------------------------------------------------
# This defines the groups that are related to the movies
groups <- 
  tibble(group = c("gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", "gr8", "gr9"),
                 video_valence = c(rep("Positive", 3),
                                rep("Neutral", 3),
                                rep("Negative", 3))) %>% 
  mutate(video_valence = fct_relevel(video_valence, "Neutral"))        

theme_set(theme_light())

sessions_to_exclude <- c(
  # Non-English response
  "qf3LHAoO3suS9hpjgDMR9uf-C-YnLaSlqCrkIVMZbpGTX4bZsVzU6bqevzaiZ80G"
  )

df_raw <- read_csv2("Data/data_final_long_scales_181121.csv")


# Extract and process textual info --------------------------------------------------
# Get all text data, put it into long format, and add time point and question
answer_df <-
    df_raw %>%
    # Exclude invalid sessions (see reasons above)
    filter(!session %in% sessions_to_exclude) %>% 
    select(session, matches("gr\\d+_qual")) %>% 
    gather(group_time, answer, -session, na.rm = TRUE) %>% 
    separate(group_time, c("group", "time"), sep = "_qual.t", convert = TRUE) %>% 
    # Add answer lenght
    mutate(answer_length = str_count(answer, "\\w+"))


# participant_df --------------------------------------------------------------------

participant_df <-
  df_raw %>% 
  select(session, bpd = BPD, cesd_ = CESD.t1:CESD.t4, genimp_ = GEN_IMPR.t1:GEN_IMPR.t4)

participant_long <-
  participant_df %>% 
  pivot_longer(cols = c(cesd_1:cesd_4, genimp_1:genimp_4), 
               names_to = c(".value", "time"),
               names_sep = "_",
               values_to = c("cesd", "genimp")) %>% 
  mutate(time = as.integer(time))
  

# Tokenize text by word
word_df <- 
  answer_df %>% 
  unnest_tokens(word, answer)

# Tokenize by sentence
sentence_df <- 
  answer_df %>% 
  unnest_tokens(sentence, token = "sentences", answer)


# Handle typos ----------------------------------------------------------------------
# Write typos with context to an xlsx for hand screening
# DO NOT RUN THIS BECAUSE IT OVERWRITES the hand corrected typo file
# word_df %>%
#   mutate(correct = hunspell_check(word)) %>% 
#   filter(!correct) %>% 
#   # Add the original sentence for context to help with manual coding
#   left_join(answer_df, by = c("session","group", "time")) %>% 
#   mutate(new_word = NA_character_) %>% 
#   # Make the dataframe easy to read for humans
#   select(session, group, time, context = value, typo = word, word = new_word) %>%
#   write.xlsx("Data/text_data_for_typos.xlsx")

# Create a hand corrected dictionary of typos
corrections <- 
  read_excel("Data/text_data_for_typos.xlsx") %>% 
  # Unselect context as it only served for manual coding
  select(typo, corrected_word = word) %>% 
  drop_na() %>% 
  # Keep only unique rows
  distinct(.keep_all = TRUE)

# Number of corrected words
nrow(corrections)

# Correct the typos and re-tokenize
corrected_word_df <-
  word_df %>% 
  left_join(corrections, by = c("word" = "typo")) %>% 
  mutate(word = coalesce(corrected_word, word)) %>% 
  select(-corrected_word) %>% 
  # There are several missing space errors that require re-tokenizing
  unnest_tokens(word, word)


# Negating words --------------------------------------------------------------------

negated_words <-
corrected_word_df %>% 
  mutate(negate = if_else(lag(str_detect(word, "^not$|^no$|'t")), -1, 1)) %>% 
  filter(negate == -1) %>%
  semi_join(get_sentiments("afinn"), by = "word") 
  # write_excel_csv("negating_words.csv")
  # view()

corrected_word_df %>% 
  mutate(amplifyer_word = lag(word),
         amplifyer = if_else(lag(str_detect(word, "^very$|^extremely$")), 1, -1)) %>% 
  filter(amplifyer == 1) %>%
  semi_join(get_sentiments("afinn"), by = "word") %>% 
  print(n = 100)

# Add sentiments --------------------------------------------------------------------
# Afinn returns a number that can be positive and negative, and reflects intensity
afinn_df <- 
  corrected_word_df %>% 
    # Add sentiments
    left_join(get_sentiments("afinn"), by = "word") %>% 
    group_by(session, group, time) %>% 
    # Get the summarised sentiment and the number of words in the answer
    summarise(answer_length = first(answer_length),
              sum_senti = sum(value, na.rm = TRUE),
              rel_senti = sum_senti/answer_length) %>% 
    ungroup() %>% 
    left_join(groups, by = "group") %>% 
    select(session, group, video_valence, everything()) %>% 
    # Add centered/scaled versions of variables for modeling
    mutate(sum_senti_std = scale(sum_senti) %>% as.numeric(),
           time_cent = scale(time, scale = FALSE) %>% as.numeric(),
           answer_length_std = scale(answer_length) %>% as.numeric())



afinn_df %>% 
  ggplot() +
  aes(x = rel_senti, fill = video_valence) +
  geom_histogram(position = "identity", alpha = .3, bins = 100)

afinn_df %>% 
  group_by(video_valence) %>% 
  skim()

# Test hypo1-2
# H3.1: The sentiment of words is polarised over time (positive video valence becomes more positive over time, negative becomes more negative, and there is no change in the neutral impression) (splitting)

# H3.2: The sentiment of words becomes negative over time (negativity bias)

h1.i <- 
  afinn_df %>% 
  lmer(sum_senti_std ~ time_cent * video_valence + answer_length_std + (1|session), 
       data = .)

summary(h1.i)

h1.is <- 
  afinn_df %>% 
  lmer(sum_senti_std ~ time_cent * video_valence + answer_length_std + (answer_length_std|session), 
       data = .)

summary(h1.is)

anova(h1.i, h1.is)

performance::r2(h1.is)

car::vif(h1.is)

residplot(h1.is)


afinn_mod <- 
  afinn_boot %>% 
  mutate(model = map(splits, 
                             ~lmer(sum_senti_std ~ time_cent * video_valence + 
                                   answer_length_std + (answer_length_std|session), 
                                   data = .x)),
         coef_info = map(model, broom.mixed::tidy),
         model_info = map(model, broom.mixed::glance)
         )

int_pctl(afinn_mod, coef_info)

library(sjPlot)
sjPlot::tab_model(h1.i, h1.is, 
                  dv.labels = c("Random intercept model", 
                                "Random intercept and slope model"), 
                  pred.labels = c("Intercept",
                                  "Time",
                                  "Video valence (Neg)",
                                  "Video valence (Pos)",
                                  "Answer length",
                                  "Time * Video valence (Neg)",
                                  "Time * Video valence (Pos)"), 
                  string.ci = "95% CI", 
                  show.dev = TRUE, 
                  show.loglik = TRUE, 
                  string.est = "Std. Beta",  
                  ci.hyphen = "|",
                  show.stat = TRUE,
                  show.aic = TRUE)

augment(h1.is) %>% 
  ggplot() +
  aes(x = .resid) +
  geom_histogram()

augment(h1.is) %>% 
  pull(.resid) %>% 

afinn_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti),
            se_senti = sd(sum_senti)/sqrt(n())) %>% 
  ggplot() +
  aes(x = time, y = avg_senti, 
      color = video_valence, group = video_valence,
      ymin = avg_senti - se_senti,
      ymax = avg_senti + se_senti) +
  geom_point(size = 2) +
  geom_line(size = 1.2) +
  geom_errorbar(color = "black", alpha = .5, width = .1) +
  labs(x = "Time",
       y = "Average sentiment of the answers (SEM)") +
  theme(legend.position = "top")


# Test hypo3
# H3.3: The sentiment of words is positively associated with mood in each point of time (mood congruence)

h3.is <- 
  afinn_df %>%
  left_join(participant_long, by = c("session", "time")) %>% 
  lmer(scale(sum_senti) ~ scale(cesd) * time * video_valence + (scale(answer_length)|session), data = .)

summary(h3.is)


afinn_df %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  ggplot() +
  aes(x = cesd, y = sum_senti, group = time, color = as.factor(time)) +
  # geom_point() +
  geom_smooth(method = "lm")

library(ggforce)

autoplot(h1.is)

augment(h3.is) %>% 
  ggplot() +
  aes(.resid) %>% 
  geom_histogram()

# Test hypo4
# The sentiment of words is positively associated with general impression (halo-effect)

h4.is <-
  afinn_df %>%
  left_join(participant_long, by = c("session", "time")) %>% 
  lmer(scale(sum_senti) ~ scale(genimp) * video_valence * time + (scale(answer_length)|session), data = .)

summary(h4.is)
car::vif(h4.is)

h5.is <-
  afinn_df %>%
  left_join(participant_long, by = c("session", "time")) %>% 
  lmer(scale(sum_senti) ~ scale(bpd) + video_valence * time + scale(genimp) + scale(cesd) + (scale(answer_length)|session), data = .)

icc

summary(h5.is)
car::vif(h5.is)

participant_long %>% 
  ggplot() +
  aes(x = bpd) +
  geom_histogram()

afinn_df %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  ggplot() +
  aes(x = genimp, y = sum_senti, color = video_valence, group = video_valence) +
  # geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~time)



afinn_df %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  ggplot() +
  aes(x = genimp, y = sum_senti) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~time)

afinn_df %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  ggplot() +
  aes(x = genimp, y = sum_senti, group = time, color = as.factor(time)) +
  geom_smooth(method = "lm", se = FALSE)


# Exploratory
# Only the negative videos have a polarization effect, so we only use those

afinn_df %>%
  left_join(participant_long, by = c("session", "time")) %>% 
  filter(video_valence == "Negative") %>% 
  lmer(scale(sum_senti) ~ scale(bpd) + scale(answer_length)*time + (1|session), data = .) %>% 
  summary()


afinn_df %>%
  left_join(participant_long, by = c("session", "time")) %>% 
  filter(video_valence == "Negative") %>% 
  ggplot() +
  aes(x = bpd, y = sum_senti, color = as.factor(time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# What are the most common words by video_valence
all_words <- 
  corrected_word_df %>% 
  anti_join(stop_words, by = "word") %>% 
  left_join(groups, by = "group") %>% 
  right_join(get_sentiments("afinn"), by = "word")
  
# TODO: Put in desc order!

all_words %>% 
  mutate(video_valence = as.character(video_valence)) %>% 
  count(video_valence, word, value, sort = TRUE) %>%
  group_by(video_valence) %>%
  top_n(10, wt = n) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot() +
  aes(x = word, y = n) +
  geom_col() +
  facet_wrap(~video_valence, scales = "free_y") +
  coord_flip()
  
# TODO: Check perdsonal pronoun difference in BPD
# USE: https://stackoverflow.com/questions/49809280/r-analyze-pronouns



# Bing, NRC and Loughran returns emotions and categories, and we don't do any weighting of 
# positive and negative emotions. But we do a relativization to the length of the answer.
# The number cannot get negative this way.
bing_df <-
  corrected_word_df %>% 
  calculate_sentiment(word, "bing") %>%
  gather(sentiment, score, word_negative:word_positive) %>% 
  mutate(sentiment = str_remove(sentiment, "word_")) %>% 
  group_by(session, group, time, sentiment) %>% 
  summarise(answer_length = first(answer_length),
            senti = sum(score, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(senti = if_else(sentiment == "negative", senti * -1, senti)) %>% 
  group_by(session, group, time, answer_length) %>% 
  summarise(sum_senti = sum(senti)) %>% 
  ungroup() %>% 
  mutate(rel_senti = sum_senti / answer_length) %>% 
  left_join(groups, by = "group") %>% 
  select(session, group, video_valence, everything())


lmer(scale(sum_senti) ~ video_valence * time + (scale(answer_length)|session), data = bing_df) %>% 
  summary()

nrc_df <-
  corrected_word_df %>% 
  calculate_sentiment(word, "nrc") %>%
  gather(sentiment, score, word_anger:word_trust, na.rm = TRUE) %>% 
  mutate(sentiment = str_remove(sentiment, "word_")) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  group_by(session, group, time, sentiment) %>% 
  summarise(answer_length = first(answer_length),
            senti = sum(score, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(senti = if_else(sentiment == "negative", senti * -1, senti)) %>% 
  group_by(session, group, time, answer_length) %>% 
  summarise(sum_senti = sum(senti)) %>% 
  ungroup() %>% 
  mutate(rel_senti = sum_senti / answer_length) %>% 
  left_join(groups, by = "group") %>% 
  select(session, group, video_valence, everything())
  
  

log_df <-
  corrected_word_df %>% 
  calculate_sentiment(word, "loughran") %>% 
  gather(sentiment, score, word_constraining:word_uncertainty, na.rm = TRUE) %>% 
  mutate(sentiment = str_remove(sentiment, "word_")) %>% 
  group_by(session, group, time, sentiment) %>% 
  summarise(answer_length = first(answer_length),
            sum_senti = sum(score, na.rm = TRUE),
            rel_senti = sum_senti/answer_length) %>% 
  ungroup()  %>% 
  left_join(groups, by = "group") %>% 
  select(session, group, video_valence, everything())

## Save the calculated sentiments to different files
# write_excel_csv2(afinn_df, "Data/sentiments_afinn.csv")
# write_excel_csv2(bing_df, "Data/sentiments_bing.csv")
# write_excel_csv2(nrc_df, "Data/sentiments_nrc.csv")
# write_excel_csv2(log_df, "Data/sentiments_loughran.csv")


# Compare sentiment dictionaries ----------------------------------------------------

bind_rows("nrc" = nrc_df, 
          "loughran" = log_df, 
          "bing" = bing_df,
          "afinn" = afinn_df,
          .id = "lexicon") %>% 
  filter(sentiment %in% c("positive", "negative") | is.na(sentiment)) %>% 
  filter(!sum_senti == 0) %>% 
  count(lexicon)
  

bing_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti),
            se_senti = sd(sum_senti)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = time, y = avg_senti, group = video_valence, color = video_valence,
      ymax = avg_senti + se_senti,
      ymin = avg_senti - se_senti) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = .1, color = "black", alpha = .5)

nrc_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti),
            se_senti = sd(sum_senti)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = time, y = avg_senti, group = video_valence, color = video_valence,
      ymax = avg_senti + se_senti,
      ymin = avg_senti - se_senti) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(width = .1, color = "black", alpha = .5)

afinn_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti),
            se_senti = sd(sum_senti)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = time, y = avg_senti, group = video_valence, color = video_valence,
      ymax = avg_senti + se_senti,
      ymin = avg_senti - se_senti) +
  geom_line(size = 1.1) +
  geom_point() +
  geom_errorbar(width = .1, color = "black", alpha = .5)
  theme_apa()


afinn_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti, na.rm = TRUE),
            se_senti = sd(sum_senti, na.rm = TRUE)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = time, y = avg_senti, group = video_valence,
      ymax = avg_senti + se_senti,
      ymin = avg_senti - se_senti) +
  geom_line(aes(color = video_valence), size = 1.1) +
  geom_point(aes(color = video_valence)) +
  geom_ribbon(alpha = .2) +
  theme_apa()

# Use sentimentr instead of tidytext ------------------------------------------------
# TODO: correct typos, then run sentimentr
library(magrittr)

corrections_vec <-
  corrections %>% 
  mutate(typo = str_glue(" {typo} "),
         corrected_word = str_glue(" {corrected_word} ")) %>% 
  as_vector()
  pull(corrected_word) 
  
  %>% 
  set_names(corrections$typo)

corrections_vec %>% 
  )

answer_df %>% 
  mutate(corrected_answer = str_replace_all(answer, corrections_vec)) %>% 
  select(session, time, answer, corrected_answer) %>% 
  filter(str_detect(answer, corrections$typo)) %>%
  write.xlsx("corrected_answers.xlsx")

answer_sentiments <- 
  answer_df %>%
  mutate(sentences = get_sentences(answer)) %$% 
  sentiment_by(sentences, list(session, group, time))

answer_sentiments %>% 
  group_by(group, time) %>% 
  summarise(sum_senti = mean(ave_sentiment, na.rm = TRUE),
            n = n()) %>% 
  ungroup() %>% 
  left_join(groups, by = "group") %>% 
  ggplot() +
  aes(x = time, y = sum_senti, group = group, color = video_valence) +
  geom_line() +
  geom_point() +
  facet_wrap(~video_valence)


# Use sentimentr --------------------------------------------------------------------

# Put the corrected words back into one string (puncuation is removed!)
corrected_sentences <- 
  corrected_word_df %>%
  group_by(session, group, time, answer_length) %>% 
  mutate(sentence = str_c(word, collapse = " ")) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-word)

sentence_senti <-
  answer_df %$% 
  sentiment_by(sentences, by = list(session, group, time, answer_length))




# Correct words in sentences --------------------------------------------------------
sentence_senti %>% 
  ggplot() +
  aes(x = ave_sentiment) +
  geom_histogram(bins = 30)

# compare_senti <- 
  afinn_df %>% 
  left_join(select(sentence_senti, session, time, ave_sentiment, word_count), 
            by = c("session", "time")) %>% view()
  
  mutate(rel_senti = sum_senti / word_count)


compare_senti %>% 
  ggplot() +
  aes(x = rel_senti) +
  geom_histogram(bins = 30)


bing_df %>% 
  lmer(scale(sum_senti) ~ time * video_valence + (scale(answer_length)|session), data = .)  %>% 
  summary()

bing_df %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(sum_senti),
            se_senti = sd(sum_senti)/sqrt(n())) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = time, y = avg_senti, group = video_valence, color = video_valence,
      ymax = avg_senti + se_senti,
      ymin = avg_senti - se_senti) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  geom_errorbar(width = .1, color = "black", alpha = .5)
theme_apa()

word_df %>% 
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% distinct(word)
  left_join(groups, by = "group")
