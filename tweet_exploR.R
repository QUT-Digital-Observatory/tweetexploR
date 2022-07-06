# SETUP ####

## Housekeeping ####

rm(list = ls())
options(scipen = 999) # don't display large numbers with scientific notation


## Libraries ####

library(DBI)
library(RSQLite)
library(tidyverse)
library(lubridate)
library(scales)
library(tidytext)


## Working directory ####

# Create new folder for outputs if it doesn't already exist
dir.create(file.path(getwd(), "outputs"), showWarnings = FALSE)

# Get subdirectory for outputs folder
outputs_folder <- file.path(getwd(), "outputs")


## ggplot settings ####

# Theme
my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10, face = "plain"),
        plot.title = element_text(size = 11, face = "plain"))

# y axis
my_y_axis <- scale_y_continuous(labels = number_format(accuracy = 1, big.mark = ","))

# Set value for n (used in top n charts)
n <- 10


# CONNECT TO SQLITE DATABASE ####

# Existing .db file in current working directory
con <- dbConnect(SQLite(), "auspol.db")
# auspol.jsonl -> tidy_tweet 0.4 -> auspol.db


# EXPLORE DATABASE ####

# List tables in db
# dbListTables(con)

# List fields in named table
# dbListFields(con, "_metadata")
# dbListFields(con, "hashtag")
# dbListFields(con, "media")
# dbListFields(con, "mention")
# dbListFields(con, "tweet")
# dbListFields(con, "url")
# dbListFields(con, "user")

# View records in named table
# dbReadTable(con, "tweet")

# Save named table as df
# metadata <- dbReadTable(con, "_metadata")
# hashtag <- dbReadTable(con, "hashtag")
# media <- dbReadTable(con, "media")
# mention <- dbReadTable(con, "mention")
# tweet <- dbReadTable(con, "tweet")
# url <- dbReadTable(con, "url")
# user <- dbReadTable(con, "user")


# VISUALISATIONS ####

## Number of tweets by username (top n usernames) ####
dbGetQuery(con,
           "SELECT count(*) as `tweet_count`, username
            FROM tweet
            LEFT JOIN (
              SELECT username, id
              FROM user ) user
            ON user.id = tweet.author_id
            GROUP BY username;") %>% 
  slice_max(n = n, order_by = tweet_count, with_ties = TRUE) %>% 
  ggplot(aes(reorder(username, tweet_count), tweet_count)) +
  geom_col() +
  labs(title = paste0("Top ", n, " tweet authors by number of tweets"),
       y = "Number of tweets") +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


## Number of tweets per hour/day/month ####

### Hour ####

# User supplied parameters
lower_limit <- "2022-06-20 06:00:00"
upper_limit <- "2022-06-21 00:00:00"

# Plot
dbGetQuery(con,
           "SELECT id, datetime(created_at) as `created_at_datetime`
            FROM tweet;") %>% 
  mutate(created_at_datetime = ymd_hms(created_at_datetime)) %>% 
  mutate(created_at_hour = floor_date(created_at_datetime, unit = "hour")) %>% 
  filter(created_at_hour >= ymd_hms(lower_limit) & created_at_hour <= ymd_hms(upper_limit)) %>% 
  group_by(created_at_hour) %>% 
  summarise(tweets = n()) %>% 
  ggplot(aes(created_at_hour, tweets)) +
  geom_line(group = 1) +
  labs(title = "Number of tweets per hour",
       x = "Hour",
       y = "Number of tweets") +
  my_y_axis +
  my_theme
  

### Day ####

# User supplier parameters
lower_limit <- "2022-06-14"
upper_limit <- "2022-06-20"

# Plot
dbGetQuery(con,
           "SELECT count(*) as `tweets`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  filter(day >= ymd(lower_limit) & day <= ymd(upper_limit)) %>% 
  ggplot(aes(ymd(day), tweets)) +
  geom_line(group = 1) +
  labs(title = "Number of tweets per day",
       x = "Day",
       y = "Number of tweets") +
  scale_x_date(date_labels = "%d/%m/%Y") +
  my_y_axis +
  my_theme


### Month ####

# User supplied parameters
# Must be the first of the month
lower_limit <- "2022-05-01"
upper_limit <- "2022-06-01"

# Plot
dbGetQuery(con,
           "SELECT count(*) as `tweets`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>%
  mutate(month = floor_date(ymd(day), "month")) %>% 
  group_by(month) %>% 
  summarise(tweets = sum(tweets)) %>%
  filter(month >= ymd(lower_limit) & month <= ymd(upper_limit)) %>% 
  ggplot(aes(month, tweets)) +
  geom_col() +
  labs(title = "Number of tweets per month",
       x = "Month",
       y = "Number of tweets") +
  scale_x_date(date_labels = "%b %Y") +
  my_y_axis +
  my_theme


## Number of unique accounts that tweeted per hour/day/month ####

### Hour ####

# User supplied parameters
lower_limit <- "2022-06-20 06:00:00"
upper_limit <- "2022-06-21 00:00:00"

# Plot
dbGetQuery(con,
           "SELECT author_id, datetime(created_at) as `created_at_datetime`
            FROM tweet;") %>% 
  mutate(created_at_datetime = ymd_hms(created_at_datetime)) %>% 
  mutate(created_at_hour = floor_date(created_at_datetime, unit = "hour")) %>% 
  filter(created_at_hour >= ymd_hms(lower_limit) & created_at_hour <= ymd_hms(upper_limit)) %>% 
  group_by(created_at_hour) %>% 
  summarise(accounts = n_distinct(author_id)) %>% 
  ggplot(aes(created_at_hour, accounts)) +
  geom_line(group = 1) +
  labs(title = "Number of unique accounts that tweeted per hour",
       x = "Hour",
       y = "Number of accounts") +
  my_y_axis +
  my_theme


### Day ####

# User supplied parameters
lower_limit <- "2022-06-14"
upper_limit <- "2022-06-20"

# Plot
dbGetQuery(con,
           "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  filter(day >= ymd(lower_limit) & day <= ymd(upper_limit)) %>% 
  ggplot(aes(ymd(day), accounts)) +
  geom_line(group = 1) +
  labs(title = "Number of unique accounts that tweeted per day",
       x = "Day",
       y = "Number of accounts") +
  scale_x_date(date_labels = "%d/%m/%Y") +
  my_y_axis +
  my_theme


### Month ####

# User supplied parameters
# Must be the first of the month
lower_limit <- "2022-05-01"
upper_limit <- "2022-06-01"

# Plot
dbGetQuery(con,
           "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  mutate(month = floor_date(ymd(day), "month")) %>% 
  group_by(month) %>% 
  summarise(accounts = sum(accounts)) %>% 
  filter(month >= ymd(lower_limit) & month <= ymd(upper_limit)) %>% 
  ggplot(aes(month, accounts)) +
  geom_col() +
  labs(title = "Number of unique accounts that tweeted per month",
       x = "Month",
       y = "Number of accounts") +
  scale_x_date(date_labels = "%b %Y") +
  my_y_axis +
  my_theme


## Top n hashtags ####

dbGetQuery(con,
           "SELECT tag, source_id
            FROM hashtag
            WHERE source_type = 'tweet';") %>% 
  mutate(tag = str_to_lower(tag)) %>% 
  rename(hashtag = tag) %>% 
  group_by(hashtag) %>% 
  summarise(tags = n()) %>% 
  slice_max(n = n, order_by = tags, with_ties = TRUE) %>% 
  ggplot(aes(reorder(hashtag, tags), tags)) +
  geom_col() +
  labs(title = paste0("Top ", n, " hashtags"),
       y = "Number of tweets") +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


## Top n account mentions ####

dbGetQuery(con,
           "SELECT username, source_id
            FROM mention
            WHERE source_type = 'tweet';") %>% 
  mutate(tag = str_to_lower(username)) %>% 
  rename(account = username) %>% 
  group_by(account) %>% 
  summarise(mentions = n()) %>% 
  slice_max(n = n, order_by = mentions, with_ties = TRUE) %>% 
  ggplot(aes(reorder(account, mentions), mentions)) +
  geom_col() +
  labs(title = paste0("Top ", n, " accounts mentioned in tweets"),
       y = "Number of tweets") +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


## Top n retweeted tweets ####

### Based on number of retweets inside the tweets that were collected ####

# Number of characters of the tweet to be displayed is determined by tweet_chars
tweet_chars <- 80

# Number of characters of the tweet to be displayed per line (without breaking a word)
chars_per_line <- 50
dbGetQuery(con,
           "SELECT retweeted_tweet_id, count(*) as `retweets`, text
            FROM tweet
            WHERE retweeted_tweet_id IS NOT NULL
            GROUP BY retweeted_tweet_id;") %>% 
  slice_max(n = n, order_by = retweets, with_ties = TRUE) %>% 
  ggplot(aes(reorder(substr(text, 1, tweet_chars), retweets), retweets)) +
  geom_col() +
  labs(title = paste0("Top ", n, " retweeted tweets (within collection)"),
       y = "Number of retweets") +
  scale_x_discrete(labels = label_wrap(chars_per_line))+
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


### Based on tweet.retweet_count (Twitter metrics) ####

# Number of characters of the tweet to be displayed is determined by tweet_chars
tweet_chars <- 80

# Number of characters of the tweet to be displayed per line (without breaking a word)
chars_per_line <- 50

dbGetQuery(con,
           "SELECT retweet_count, text
            FROM tweet;") %>% 
  distinct() %>% 
  slice_max(n = n, order_by = retweet_count, with_ties = TRUE) %>% 
  ggplot(aes(reorder(substr(text, 1, tweet_chars), retweet_count), retweet_count)) +
  geom_col() +
  labs(title = paste0("Top ", n, " retweeted tweets (Twitter metrics)"),
       y = "Number of retweets") +
  scale_x_discrete(labels = label_wrap(chars_per_line)) +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


## Top n liked tweets ####

### Based on tweet.like_count (Twitter metrics) ####

# Number of characters of the tweet to be displayed is determined by tweet_chars
tweet_chars <- 80

# Number of characters of the tweet to be displayed per line (without breaking a word)
chars_per_line <- 50

dbGetQuery(con,
           "SELECT id, like_count, text
            FROM tweet;") %>% 
  slice_max(n = n, order_by = like_count, with_ties = TRUE) %>% 
  ggplot(aes(reorder(substr(text, 1, tweet_chars), like_count), like_count)) +
  geom_col() +
  labs(title = paste0("Top ", n, " liked tweets (Twitter metrics)"),
       y = "Number of likes") +
  scale_x_discrete(labels = label_wrap(chars_per_line)) +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


## Top n replied to tweets ####

### Based on tweet.reply_count (Twitter metrics) ####

# Number of characters of the tweet to be displayed is determined by tweet_chars
tweet_chars <- 80

# Number of characters of the tweet to be displayed per line (without breaking a word)
chars_per_line <- 50

dbGetQuery(con,
           "SELECT id, reply_count, text
            FROM tweet;") %>% 
  slice_max(n = n, order_by = reply_count, with_ties = TRUE) %>% 
  ggplot(aes(reorder(substr(text, 1, tweet_chars), reply_count), reply_count)) +
  geom_col() +
  labs(title = paste0("Top ", n, " replied to tweets (Twitter metrics)"),
       y = "Number of replies") +
  scale_x_discrete(labels = label_wrap(chars_per_line)) +
  my_y_axis +
  coord_flip() +
  my_theme +
  theme(axis.title.y = element_blank())


# INCOMPLETE VISUALISATIONS ####

## Top n shared images ####

dbGetQuery(con,
           "SELECT url
            FROM media
            WHERE type = 'photo';")
# To find out how many tweets included the image, the media and/or media_key
# columns are required in the tweet table.
# https://developer.twitter.com/en/docs/twitter-ads-api/creatives/guides/identifying-media


## Top n URLs shared ####

# Based on sharing within the collection of tweets
results <-
dbGetQuery(con,
           "SELECT tweet.id as `tweet_id`,
              tweet.text as `text`,
              url.url as `url`,
              url.expanded_url as `expanded_url`,
              url.display_url as `display_url`
            FROM url
            LEFT JOIN tweet ON url.source_id = tweet.id
            WHERE source_type = 'tweet'
              AND field = 'text';")
# Need to figure out why the same url appears several times within the same tweet_id


# IDEAS FOR VISUALISATIONS ####

## Top n accounts being retweeted ####
# Regex to detect retweet in tweet text: "^RT"
# Can also use tweet.retweeted_tweet_id is not null

## Engagement metric summary ####
# Retweets, likes, replies, etc. in either a table or a graph of some sort?


# IDEAS FOR FUNCTIONALITY/FEATURES ####

## Ability for user to specify geom_col() or geom_line() ####

## Ability for user to export data frame and/or ggplot ####

## Ability for user to include/exclude retweets ####

## Ability for user to change colour of lines/bars in plots ####


# NOTES FOR DOCUMENTATION ####

## Include recommended values for n for each visualisation ####

## Include recommended time periods for the hourly/daily/monthly visualisations ####

## Possibly include a sample .sql file for distribution with the package ####
