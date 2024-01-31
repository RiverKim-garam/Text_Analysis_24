install.packages("academictwitteR")
install.packages("textdata")
library(academictwitteR) # for fetching Twitter data
library(tidyverse)
library(readr) # more informative and easy way to import data
library(stringr) # to handle text elements
library(tidytext) # includes set of functions useful for manipulating text
library(quanteda) # includes functions to implement Lexicoder
library(textdata)

newspapers = c("TheSun", "DailyMailUK", "MetroUK", "DailyMirror", 
               "EveningStandard", "thetimes", "Telegraph", "guardian")

set_bearer()

##fail from here---------------------------------------------
tweets <-
  get_all_tweets(
    users = newspapers,
    start_tweets = "2020-01-01T00:00:00Z",
    end_tweets = "2020-05-01T00:00:00Z",
    data_path = "data/sentanalysis/",
    n = Inf,
  )

tweets <- 
  bind_tweets(data_path = "data/sentanalysis/", output_format = "tidy")

saveRDS(tweets, "data/sentanalysis/newstweets.rds")

## To here---------------------------------------------------

## download dataset (why can't I try the first way?)

tweets  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/sentanalysis/newstweets.rds?raw=true")))

head(tweets)
colnames(tweets)

tweets <- tweets %>%
  select(user_username, text, created_at, user_name, retweet_count, like_count, quote_count) %>%
  rename(username = user_username,
         newspaper = user_name,
         tweet = text)

## tokenizing
tidy_tweets <- tweets %>%
  mutate(desc = tolower(tweet)) %>%
  unnest_tokens(word, desc) %>%     ## tokenize
  filter(str_detect(word, "[a-z]")) ## range = a-z alphabet

## stopwords
tidy_tweets <- tidy_tweets %>%
  filter(!word %in% stop_words$word)

## Inside tidytext, there are sentiment dictionaries..
get_sentiments("afinn") # -5-+5
get_sentiments("bing") #binary
get_sentiments("nrc") #one word scored for each of sentiments

#fear counting + filter
nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

tidy_tweets %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

## Over time (generate date, order)
tidy_tweets$date <- as.Date(tidy_tweets$created_at)
tidy_tweets <- tidy_tweets %>% arrange(date)
tidy_tweets$order <- 1:nrow(tidy_tweets) ##adding order row

#get tweet sentiment by date
tweets_nrc_sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("nrc")) %>% ## standard
  count(date, index = order %/% 1000, sentiment) %>% ##date + every 1000 rows
  spread(sentiment, n, fill = 0) %>% ##seperate sentiment columns
  mutate(sentiment = positive - negative) ##score

tweets_nrc_sentiment %>%
  ggplot(aes(date, sentiment)) +
  geom_point(alpha=0.5) +
  geom_smooth(method= loess, alpha=0.25)

## another dictionary
tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, index = order %/% 1000, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(date, sentiment)) +
  geom_point(alpha=0.5) +
  geom_smooth(method= loess, alpha=0.25) +
  ylab("bing sentiment")

tidy_tweets %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(date, index = order %/% 1000) %>% 
  summarise(sentiment = sum(value)) %>% 
  ggplot(aes(date, sentiment)) +
  geom_point(alpha=0.5) +
  geom_smooth(method= loess, alpha=0.25) +
  ylab("afinn sentiment")

## Making my lexicons
word <- c('death', 'illness', 'hospital', 'life', 'health',
          'fatality', 'morbidity', 'deadly', 'dead', 'victim')
value <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
mordict <- data.frame(word, value)
mordict

tidy_tweets %>%
  inner_join(mordict) %>%
  group_by(date, index = order %/% 1000) %>% 
  summarise(morwords = sum(value)) %>% 
  ggplot(aes(date, morwords)) +
  geom_bar(stat= "identity") +
  ylab("mortality words")

## standizing? (total)

mordict <- c('death', 'illness', 'hospital', 'life', 'health',
             'fatality', 'morbidity', 'deadly', 'dead', 'victim')

totals <- tidy_tweets %>%
  mutate(obs=1) %>%
  group_by(date) %>%
  summarise(sum_words = sum(obs))

tidy_tweets %>%
  mutate(obs=1) %>%
  filter(grepl(paste0(mordict, collapse = "|"),word, ignore.case = T)) %>%  ## idk this line
  group_by(date) %>%
  summarise(sum_mwords = sum(obs)) %>%
  full_join(totals, word, by="date") %>%
  mutate(sum_mwords= ifelse(is.na(sum_mwords), 0, sum_mwords), ## idk 2
         pctmwords = sum_mwords/sum_words) %>%
  ggplot(aes(date, pctmwords)) +
  geom_point(alpha=0.5) +
  geom_smooth(method= loess, alpha=0.25) +
  xlab("Date") + ylab("% mortality words")

## quanteda package
tweets$date <- as.Date(tweets$created_at)
tweet_corpus <- corpus(tweets, text_field = "tweet", docvars = "date")

toks_news <- tokens(tweet_corpus, remove_punct = TRUE)

# select only the "negative" and "positive" categories
data_dictionary_LSD2015_pos_neg <- data_dictionary_LSD2015[1:2]

toks_news_lsd <- tokens_lookup(toks_news, dictionary = data_dictionary_LSD2015_pos_neg)

# create a document document-feature matrix and group it by date
dfmat_news_lsd <- dfm(toks_news_lsd) %>% 
  dfm_group(groups = date)

# plot positive and negative valence over time
matplot(dfmat_news_lsd$date, dfmat_news_lsd, type = "l", lty = 1, col = 1:2,
        ylab = "Frequency", xlab = "")
grid()
legend("topleft", col = 1:2, legend = colnames(dfmat_news_lsd), lty = 1, bg = "white")


# plot overall sentiment (positive  - negative) over time

plot(dfmat_news_lsd$date, dfmat_news_lsd[,"positive"] - dfmat_news_lsd[,"negative"], 
     type = "l", ylab = "Sentiment", xlab = "")
grid()
abline(h = 0, lty = 2)

negative <- dfmat_news_lsd@x[1:121]
positive <- dfmat_news_lsd@x[122:242]
date <- dfmat_news_lsd@Dimnames$docs  ##idk


tidy_sent <- as.data.frame(cbind(negative, positive, date))

tidy_sent$negative <- as.numeric(tidy_sent$negative)
tidy_sent$positive <- as.numeric(tidy_sent$positive)
tidy_sent$sentiment <- tidy_sent$positive - tidy_sent$negative
tidy_sent$date <- as.Date(tidy_sent$date)

tidy_sent %>%
  ggplot() +
  geom_line(aes(date, sentiment))


##Exercises
##1. Take a subset of the tweets data by “user_name” These names describe the name of the newspaper source of the Twitter account. Do we see different sentiment dynamics if we look only at different newspaper sources?

bynews_tweets_nrc_sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("nrc")) %>% ## standard
  count(newspaper, date, index = order %/% 1000, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% ##seperate sentiment columns
  mutate(sentiment = positive - negative) ##score

ggplot(bynews_tweets_nrc_sentiment, aes(date, sentiment) + geom_line(aes(color = bynews_tweets_nrc_sentiment$newspaper))


##2.Build your own (minimal) dictionary-based filter technique and plot the result
my_dic <- c('cash', 'music')
dic_value <- c(1, 1)
my_dic_word <- data.frame(my_dic, dic_value)

tidy_tweets %>%
  inner_join(my_dic_word) %>%
  group_by(date, index = order %/% 100) %>% 
  summarise(dic_words = sum(value)) %>% 
  ggplot(aes(date, dic_words)) +
  geom_bar(stat= "identity") +
  ylab("my words")


## 3. Apply the Lexicoder Sentiment Dictionary to the news tweets, but break down the analysis by newspaper



