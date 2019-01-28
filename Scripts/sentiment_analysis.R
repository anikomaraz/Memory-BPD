library(tidyverse)
library(tidytext)
# library(proustr)
library(hunspell)

# TODO: stemming
# TODO: correct typos
# TODO: calculate bpd scale correctly

# "data_final_long_scales_181121.csv"
df_raw <- read_csv2("data/data_raw_full_180831.csv")
df_raw <- read_csv2("data/")

bpd_df <-
  df_raw %>% 
  select(session, matches("bpd_s\\d+")) %>% 
  gather(question, value, -session) %>% 
  group_by(session) %>% 
  summarise(sum_bpd = sum(value, na.rm = TRUE))

sentence_df <-
    df_raw %>%
    # Drop all testing rows
    filter(!str_detect(session, "TEST|correc|obedie")) %>% 
    # Get all text data
    select(session, matches("gr\\d+_qual")) %>% 
    gather(question_time, value, -session, na.rm = TRUE) %>% 
    separate(question_time, c("question", "time"), sep = "_qual.t", convert = TRUE) %>% 
    # Keep only participants that filled all 4 questionnaires
    add_count(session) %>% 
    filter(n == 4) %>% 
    select(-n)

# Tokenize by word
word_df <- 
  sentence_df %>% 
  unnest_tokens(word, value) %>% 
  mutate(correct = hunspell_check(word))
  
# Write 

word_df %>% 
  filter(!correct) %>% 
  View()

# temp <-
  word_df %>% 
  slice(1:1000) %>% 
  pull(word) %>%
  hunspell_check() %>% 
  View()

extract_first <- possibly(., otherwise = NA_character_)

word_df %>% 
  mutate(corrected = hunspell_suggest()
         stemmed = hunspell_stem(word)) %>% 
  unnest(stemmed) %>% 
  View()

stemmed_df <- pr_stem_words(word_df, word, language = "english")
  
senti_df <-
    word_df %>% 
    # Add sentiments (stemming might be needed before!)
    left_join(get_sentiments("afinn"), by = "word") %>% 
    group_by(session, question, time) %>% 
    # Get the summarised sentiment and the number of words in the answer
    summarise(sum_senti = sum(score, na.rm = TRUE),
              n = n(),
              rel_senti = sum_senti/n) %>% 
    ungroup %>% 
    left_join(bpd_df, by = "session")


ggplot(senti_df) +
  aes(x = time, y = rel_senti) +
  geom_point(alpha = .3) +
  facet_wrap(~sum_bpd) +
  geom_smooth(method = "lm")
  
    