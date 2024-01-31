getwd()
setwd("/Users/Documents/mydir/")

library(stringi) #to generate random text
library(dplyr) #tidyverse package for wrangling data
install.packages("tidytext")
library(tidytext) #package for 'tidy' manipulation of text data
library(ggplot2) #package for visualizing data
library(scales) #additional package for formatting plot axes
library(kableExtra) #package for displaying data in html format (relevant for formatting this worksheet mainly)

lipsum_text <- data.frame(text = stri_rand_lipsum(1, start_lipsum = TRUE))
head(lipsum_text$text)

# tokenization
tokens <- lipsum_text %>%
  unnest_tokens(word, text)
head(tokens)

## Varying total words example
lipsum_text <- data.frame(text = stri_rand_lipsum(5000, start_lipsum = TRUE))

# make some weeks one to ten
lipsum_text$week <- as.integer(rep(seq.int(1:10), 5000/10)) # 1-10, 500times

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i, 2]
  morewords <-
    paste(rep("more lipsum words", times = sample(1:100, 1) * week), collapse = " ")
  lipsum_words <- lipsum_text[i, 1]
  new_lipsum_text <- paste0(morewords, lipsum_words, collapse = " ")
  lipsum_text[i, 1] <- new_lipsum_text
}
lipsum_text %>%
  unnest_tokens(word, text) %>%
  group_by(week) %>%
  count(word) %>%
  select(week, n) %>%
  distinct() %>%
  ggplot() +
  geom_bar(aes(week, n), stat = "identity") +
  labs(x = "Week", y = "n words")

# simulate decreasing words trend
lipsum_text <- data.frame(text = stri_rand_lipsum(5000, start_lipsum = TRUE))

# make some weeks one to ten
lipsum_text$week <- as.integer(rep(seq.int(1:10), 5000/10))

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i,2]
  morewords <- 
    paste(rep("more lipsum words", times = sample(1:100, 1)* 1/week), collapse = " ")
  lipsum_words <- lipsum_text[i,1]
  new_lipsum_text <- paste0(morewords, lipsum_words, collapse = " ")
  lipsum_text[i,1] <- new_lipsum_text
}

lipsum_text %>%
  unnest_tokens(word, text) %>%
  group_by(week) %>%
  count(word) %>%
  select(week, n) %>%
  distinct() %>%
  ggplot() +
  geom_bar(aes(week, n), stat = "identity") +
  labs(x = "Week", y = "n words")

#top frequency words
lipsum_text %>%
  unnest_tokens(word, text) %>%
  dplyr::count(word, sort = T) %>%
  top_n(5) %>%
  knitr::kable(format="html")

#total word frequencies
lipsum_totals <- lipsum_text %>%
  group_by(week) %>%
  unnest_tokens(word, text) %>%
  dplyr::count(word) %>%
  mutate(total = sum(n)) %>%
  distinct(week, total)

# let's look for "sed"
lipsum_sed <- lipsum_text %>%
  group_by(week) %>%
  unnest_tokens(word, text) %>%
  filter(word == "sed")  %>%
  dplyr::count(word) %>%
  mutate(total_sed = sum(n)) %>%
  distinct(week, total_sed)

lipsum_sed %>%
  left_join(lipsum_totals, by = "week") %>% ##two dataframes together
  mutate(sed_prop = total_sed/total) %>%
  ggplot() +
  geom_line(aes(week, sed_prop)) +
  labs(x = "Week", y = "
       Proportion sed word")




