library(tidyverse)
library(tidytext)

# TODO: stemming
# TODO: correct typos
# TODO: calculate bpd scale correctly

# "data_final_long_scales_181121.csv"
df_raw <- read_csv2("data/data_raw_full_180831.csv")

bpd_df <-
  df_raw %>% 
  select(session, matches("bpd_s\\d+")) %>% 
  gather(question, value, -session) %>% 
  group_by(session) %>% 
  summarise(sum_bpd = sum(value, na.rm = TRUE))


text_df <-
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

senti_df <-
  text_df %>% 
    # Tokenize by word
    unnest_tokens(word, value) %>% 
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
  
    