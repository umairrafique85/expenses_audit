# Read in data ####
library(tidyverse)
library(tidytext)

df_statement <- read.csv("statements/AccountFullStatement.CSV")
df_statement |> 
    janitor::clean_names() |> 
    select(-x) -> df_statement
sapply(df_statement, class)

# cleaning and transformation ####
df_statement |> 
    mutate(date = lubridate::dmy(transaction_date),
           transaction_id = row_number()) |> 
    select(transaction_id, transaction_date, date, everything()) -> df_statement

saveRDS(df_statement, "df_statement.RDS")

df_statement |> 
    unnest_tokens(word, description, to_lower = FALSE, 
                  token = "regex", pattern = "\\s+") -> df_statement_tokenized_ws

df_statement_tokenized_ws |> 
    count(word, sort = TRUE) |> 
    filter(n > 1) |> 
    mutate(word = reorder(word, n)) |> 
    ggplot(aes(x = n, y = word)) +
    geom_col() +
    labs(y = NULL)

df_statement_tokenized_ws |> 
    count(word, sort = TRUE) |> 
    filter(n > 1)

df_statement |> 
    unnest_tokens(ngram, description, to_lower = FALSE, 
                  token = "ngrams") -> df_statement_tokenized_ngrams

df_statement_tokenized_ngrams |> 
    count(ngram, sort = TRUE) |> 
    filter(n > 1)

