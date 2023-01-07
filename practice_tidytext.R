# chapter 1 (good way to detect chapters and number them) ####
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, 
                                       regex("^chapter [\\divxlc]",
                                             ignore_case = TRUE)))) %>%
    ungroup()

View(original_books)

tidy_books <- original_books %>%
    unnest_tokens(word, text)

data(stop_words)

tidy_books <- tidy_books %>%
    anti_join(stop_words)

tidy_books %>%
    count(word, sort = TRUE) 

library(ggplot2)

tidy_books %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)
