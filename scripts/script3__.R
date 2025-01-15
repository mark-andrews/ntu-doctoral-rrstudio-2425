
# Data wrangling ----------------------------------------------------------

library(tidyverse)

# select
# filter
# mutate
# summarize
# the pipe: %>% |>

blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntu-doctoral-rrstudio-2425/refs/heads/main/data/blp-trials-short.txt")

select(blp_df, participant, spell, rt)
select(blp_df, id = participant, item = spell, rt)

select(blp_df, 1, 3, 5)
select(blp_df, participant, 3, 5)

select(blp_df, lex:rt)
select(blp_df, 2:5)

select(blp_df, participant, 3:5)

select(blp_df, starts_with('p'))
select(blp_df, ends_with('t'))

select(blp_df, ends_with('rt'))

select(blp_df, contains('rt'))

select(blp_df, matches('^rt|rt$'))
select(blp_df, matches('^r.*t$'))


