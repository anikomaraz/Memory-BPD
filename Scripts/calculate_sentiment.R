# Calculates the proportion of sentiments in a column of text data
# INPUT: df: data frame, 
#        column: text column to  use for sentiment calculation, 
#        lexicon: which sentiment lexicon to use? "bing" (default), "nrc", "loughran"
# OUTPUT: The original df, plus new sentiment columns 
# EXAMPLE: evaluate_sentiment(okcupid, essay0, lexicon = "nrc")

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(tidytext)) install.packages("tidytext")

calculate_sentiment <- function(df, 
                                column, 
                                lexicon = c("bing", "nrc", "loughran")){
        column <- rlang::ensym(column)
        df <-  dplyr::mutate(df, id = dplyr::row_number()) # Add an id     
        lexicon <- lexicon[1]
        
        df %>% 
                # Keep only the variable of importance
                dplyr::select(id, !!rlang::sym(column)) %>% 
                # Tokenize by word
                tidytext::unnest_tokens(word, 
                                        input = !!sym(column)) %>% 
                # Remove stopwords
                dplyr::anti_join(tidytext::stop_words, 
                                 by = "word") %>% 
                # Add sentiments
                dplyr::left_join(tidytext::get_sentiments(lexicon), 
                                 by = "word") %>% 
                # Count sentiments
                dplyr::count(id, sentiment) %>% 
                dplyr::group_by(id) %>% 
                # Count all words
                dplyr::mutate(words = sum(n)) %>% 
                tidyr::drop_na(sentiment) %>% 
                tidyr::spread(sentiment, n) %>% 
                # calculate proportion
                dplyr::mutate_at(dplyr::vars(-id, -words), 
                                 dplyr::funs(./words)) %>%
                # Add column name as prefix
                dplyr::rename_at(dplyr::vars(-id, -words), 
                                 dplyr::funs(paste0(column, "_", .))) %>% 
                # Put new variables next to the original data frame
                dplyr::left_join(df, ., by = "id") %>% 
                dplyr::select(-id, -words)
}