library(tidyverse)
library(sentimentr)
library(magrittr)
library(lmerTest)
library(sjPlot)
library(broom.mixed)
library(parameters)
library(performance)


# Init project ----------------------------------------------------------------------
# This defines the groups that are related to the movies
groups <- 
  tibble(group = c("gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", "gr8", "gr9"),
         video_valence = c(rep("Positive", 3),
                           rep("Neutral", 3),
                           rep("Negative", 3))) %>% 
  mutate(video_valence = fct_relevel(video_valence, "Neutral"))        

education_levels <- 
  c("No formal education",
    "Primary school (or equivalent)",
    "Secondary/high school (or equivalent)",
    "Vocational school (or equivalent)",
    "Undergraduate school/BA degree (or equivalent)",
    "Graduate school/MA degree (or equivalent)",
    "PhD/MBA or similar")

  
theme_set(theme_apa())

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

# Create dataframe that contains participant data
participant_df <-
  df_raw %>%
  select( session,
          bpd = BPD,
          cesd_ = CESD.t1:CESD.t4,
          genimp_ = GEN_IMPR.t1:GEN_IMPR.t4,
          education = edu_highest.t1) %>%
  mutate(education = factor(education, levels = education_levels),
         education_num = as.numeric(education),
         education = fct_relevel(education, 
                                 # New baseline: (most frequent)
                                 "Undergraduate school/BA degree (or equivalent)"))

participant_long <-
  participant_df %>% 
  pivot_longer(cols = c(cesd_1:cesd_4, genimp_1:genimp_4), 
               names_to = c(".value", "time"),
               names_sep = "_",
               values_to = c("cesd", "genimp")) %>% 
  mutate(time = as.integer(time))



# Calculate the sentiment for each sentence, accounting for negators, amplifiers, deamplifyers, etc.
sentiments <- 
  answer_df %>% 
  mutate(sentence_split = get_sentences(answer)) %$% 
  sentiment_by(sentence_split, list(session, group, time)) %>% 
  right_join(answer_df, by = c("session", "group", "time")) %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  left_join(groups, by = "group") %>% 
  # Add centered/scaled versions of variables for modeling
  mutate(sum_senti_std = scale(ave_sentiment) %>% as.numeric(),
         time_cent = scale(time, scale = FALSE) %>% as.numeric(),
         answer_length_std = scale(answer_length) %>% as.numeric()) %>% 
  as_tibble()


sentiment_wide <-
  sentiments %>%
  select(session, group, time, ave_sentiment) %>% 
  pivot_wider(names_from = time, values_from = ave_sentiment, names_prefix = "senti_")


# Explore data --------------------------------------------------------------------

participant_df %>% 
  left_join(sentiment_wide, by = "session") %>% 
  select(-session, -group, -education) %>% 
  sjp.corr(sort.corr = FALSE, decimals = 2, corr.method = "spearman")

participant_long %>%
  pivot_longer(names_to = "variable", values_to = "value", 
               cols = c("cesd", "genimp")) %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram(bins = 40) +
  facet_grid(time ~ variable, scales = "free")


sentiments %>% 
  ggplot() +
  aes(x = ave_sentiment, fill = video_valence) +
  geom_histogram(position = "identity", alpha = .3, bins = 100)


# Hypothesis tests
h1.i <- 
  sentiments %>% 
  lmer(sum_senti_std ~ time_cent * video_valence + answer_length_std + education_num + (1|session), 
       data = .)

summary(h1.i)

h1.is <- 
  sentiments %>% 
  lmer(sum_senti_std ~ time_cent * video_valence + answer_length_std + education_num + (answer_length_std|session), 
       data = .)

summary(h1.is)

anova(h1.i, h1.is)

performance::r2(h1.is)

car::vif(h1.is)

# Show table summary, using bootstrapping

set.seed(1)
sjPlot::tab_model(h1.i, h1.is, 
                  bootstrap = TRUE,
                  iterations = 1000,
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
                  show.stat = TRUE,
                  show.aic = TRUE)


h1.is %>% 
  parameters()

temp <- parameters::bootstrap_model(h1.i, iterations = 100)

temp %>% 
  pivot_longer(everything(),
               names_to = "term",
               values_to = "value") %>% 
  group_by(term) %>% 
  summarise(median_value = median(value))

sentiments %>% 
  group_by(video_valence, time) %>% 
  summarise(avg_senti = mean(ave_sentiment),
            se_senti = sd(ave_sentiment)/sqrt(n())) %>% 
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
  theme(legend.position = "top") +
  labs(color = "Video valence")

sentiments %>% 
  left_join(participant_long, by = c("session", "time")) %>% 
  lmer(sum_senti_std ~ time_cent * video_valence + answer_length_std + (bpd|session), 
       data = .) %>% 
  summary()
  


# The sentiment of words is positively associated with mood in each point of --------





# Sandbox ---------------------------------------------------------------------------

library(tidytext)
library(ggpubr)

# 10 most common sentiment words for each valence category
answer_df %>% 
  left_join(groups, by = "group") %>% 
  unnest_tokens(word, answer) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!str_detect(word, "^\\d+$")) %>% 
  count(video_valence, word, sort = TRUE) %>% 
  # Make it afinn based
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(video_valence, sentiment) %>% 
  top_n(10, wt = n) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, video_valence)) %>%
  # mutate(word = fct_reorder(word, n)) %>%
  ggplot() +
  aes(y = word, x = n, fill = sentiment) +
  geom_col() +
  facet_wrap(~video_valence, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "10 most common positive and negative sentiment words for video valence",
       y = NULL)
  
# 10 most common positive and negative sentiment words for each group
answer_df %>% 
  left_join(groups, by = "group") %>% 
  unnest_tokens(word, answer) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!str_detect(word, "^\\d+$")) %>% 
  count(group, video_valence, word, sort = TRUE) %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(group, sentiment) %>% 
  top_n(10, wt = n) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, group)) %>%
  ggplot() +
  aes(y = word, x = n, fill = sentiment) +
  geom_col() +
  facet_wrap(~group, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "10 most common positive and negative sentiment words for each group",
       y = NULL)

