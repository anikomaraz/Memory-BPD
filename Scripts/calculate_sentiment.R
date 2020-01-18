# Calculates the proportion of sentiments in a column of words
# INPUT: df: data frame, 
#        column: text column to  use for sentiment calculation, 
#        lexicon: which sentiment lexicon to use? "bing" (default), "nrc", "loughran"
# OUTPUT: The original df, plus new sentiment columns 
# EXAMPLE: calculate_sentiment(okcupid, essay0, lexicon = "nrc")

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
                # Add sentiments
                dplyr::left_join(tidytext::get_sentiments(lexicon), 
                                 by = "word") %>% 
                # Count sentiments
                dplyr::count(id, sentiment) %>% 
                dplyr::group_by(id) %>% 
                # Count all words
                tidyr::drop_na(sentiment) %>% 
                tidyr::spread(sentiment, n) %>% 
                # Add column name as prefix
                dplyr::rename_at(dplyr::vars(-id), 
                                 ~paste0(column, "_", .)) %>% 
                # Put new variables next to the original data frame
                dplyr::left_join(df, ., by = "id") %>% 
                # Replace all NAs with 0s
                mutate_if(is.integer, ~tidyr::replace_na(., 0)) %>% 
                dplyr::select(-id)
}