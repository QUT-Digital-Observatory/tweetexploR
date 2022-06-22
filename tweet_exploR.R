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
n <- 20
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


## Number of tweets per day/month ####

### Day ####
dbGetQuery(con,
           "SELECT count(*) as `tweets`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  ggplot(aes(ymd(day), tweets)) +
  geom_line(group = 1) +
  labs(title = "Number of tweets per day",
       x = "Day",
       y = "Number of tweets") +
  scale_x_date(date_labels = "%d/%m/%Y") +
  my_y_axis +
  my_theme

### Month ####
dbGetQuery(con,
           "SELECT count(*) as `tweets`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>%
  mutate(month = floor_date(ymd(day), "month")) %>% 
  group_by(month) %>% 
  summarise(tweets = sum(tweets)) %>% 
  ggplot(aes(month, tweets)) +
  geom_col() +
  labs(title = "Number of tweets per month",
       x = "Month",
       y = "Number of tweets") +
  scale_x_date(date_labels = "%b %Y") +
  my_y_axis +
  my_theme


## Number of unique accounts that tweeted per day/month ####

### Day ####
dbGetQuery(con,
           "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  ggplot(aes(ymd(day), accounts)) +
  geom_line(group = 1) +
  labs(title = "Number of unique accounts that tweeted per day",
       x = "Day",
       y = "Number of accounts") +
  scale_x_date(date_labels = "%d/%m/%Y") +
  my_y_axis +
  my_theme

### Month ####
dbGetQuery(con,
           "SELECT count(distinct(author_id)) as `accounts`, date(created_at) as `day`
            FROM tweet
            GROUP BY day;") %>% 
  mutate(month = floor_date(ymd(day), "month")) %>% 
  group_by(month) %>% 
  summarise(accounts = sum(accounts)) %>% 
  ggplot(aes(month, accounts)) +
  geom_col() +
  labs(title = "Number of unique accounts that tweeted per month",
       x = "Month",
       y = "Number of accounts") +
  scale_x_date(date_labels = "%b %Y") +
  my_y_axis +
  my_theme


## Top n hashtags ####

n <- 20
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

n <- 20
dbGetQuery(con,
           "SELECT username, source_id
            FROM hashtag
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


# IDEAS FOR VISUALISATIONS ####

## Top n URLs shared ####
# Use url table (filter by source == tweet)

## Top n accounts being retweeted ####
# Regex to detect retweet: "^RT"

## Number of tweets per hour ####

## Most shared image ####
# Use media table where type == photo


# IDEAS FOR FUNCTIONALITY/FEATURES ####

## Ability for user to specify geom_col() or geom_line() ####

## Ability for user to export data frame and/or ggplot ####

## Ability for user to include/exclude retweets ####