library(tidyverse)
library(tidytext)
library(hunspell)
library(openxlsx)
library(readxl)

source("Scripts/calculate_sentiment.R")

# TODO: correct negating words

# Init project ----------------------------------------------------------------------
# This defines the groups that are related to the movies
groups <- tibble(group = c("gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", "gr8", "gr9"),
                 group_affect = c(rep("positive", 3),
                                rep("neutral", 3),
                                rep("negative", 3)))

sessions_to_exclude <- c(
  # Non-English response
  "qf3LHAoO3suS9hpjgDMR9uf-C-YnLaSlqCrkIVMZbpGTX4bZsVzU6bqevzaiZ80G"
  )

df_raw <- read_csv2("Data/data_final_long_scales_181121.csv")


# Extract and process textual info --------------------------------------------------
# Get all text data, put it into long format, and add time point and question
sentence_df <-
    df_raw %>%
    # Exclude invalid sessions (see reasons above)
    filter(!session %in% sessions_to_exclude) %>% 
    select(session, matches("gr\\d+_qual")) %>% 
    gather(group_time, answer, -session, na.rm = TRUE) %>% 
    separate(group_time, c("group", "time"), sep = "_qual.t", convert = TRUE) %>% 
    # Add answer lenght
    mutate(answer_length = str_count(answer, "\\w+"))

# Tokenize text by word
word_df <- 
  sentence_df %>% 
  unnest_tokens(word, answer)

# Handle typos ----------------------------------------------------------------------
# Write typos with context to an xlsx for hand screening
# DO NOT RUN THIS BECAUSE IT OVERWRITES the hand corrected typo file
# word_df %>%
#   mutate(correct = hunspell_check(word)) %>% 
#   filter(!correct) %>% 
#   # Add the original sentence for context to help with manual coding
#   left_join(sentence_df, by = c("session","group", "time")) %>% 
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
  # write_excel_csv("negating_words.csv")
  View()


# Add sentiments --------------------------------------------------------------------
# Afinn returns a number that can be positive and negative, and reflects intensity
afinn_df <- 
  corrected_word_df %>% 
    # Add sentiments
    left_join(get_sentiments("afinn"), by = "word") %>% 
    group_by(session, group, time) %>% 
    # Get the summarised sentiment and the number of words in the answer
    summarise(answer_length = first(answer_length),
              sum_senti = sum(score, na.rm = TRUE),
              rel_senti = sum_senti/answer_length) %>% 
    ungroup() %>% 
    left_join(groups, by = "group") %>% 
    select(session, group, group_affect, everything())

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
            sum_senti = sum(score, na.rm = TRUE),
            rel_senti = sum_senti/answer_length) %>% 
  ungroup() %>% 
  left_join(groups, by = "group") %>% 
  select(session, group, group_affect, everything())

nrc_df <- 
  corrected_word_df %>% 
  calculate_sentiment(word, "nrc") %>%
  gather(sentiment, score, word_anger:word_trust, na.rm = TRUE) %>% 
  mutate(sentiment = str_remove(sentiment, "word_")) %>% 
  group_by(session, group, time, sentiment) %>% 
  summarise(answer_length = first(answer_length),
            sum_senti = sum(score, na.rm = TRUE),
            rel_senti = sum_senti/answer_length) %>% 
  ungroup()  %>% 
  left_join(groups, by = "group") %>% 
  select(session, group, group_affect, everything())
  

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
  select(session, group, group_affect, everything())

## Save the calculated sentiments to different files
# write_excel_csv2(afinn_df, "Data/sentiments_afinn.csv")
# write_excel_csv2(bing_df, "Data/sentiments_bing.csv")
# write_excel_csv2(nrc_df, "Data/sentiments_nrc.csv")
# write_excel_csv2(log_df, "Data/sentiments_loughran.csv")


# Compare sentiment dictionaries ----------------------------------------------------

bind_rows("nrc" = nrc_df, 
          "loughran" = log_df, 
          "bing" = bing_df, .id = "lexicon") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  filter(!sum_senti == 0) %>% 
  count(lexicon)
  
afinn_df %>% 
  filter(!sum_senti == 0) %>% View()



