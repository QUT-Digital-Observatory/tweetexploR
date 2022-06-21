# SETUP ####

## Housekeeping ####

rm(list = ls())
options(scipen = 999) # don't display large numbers with scientific notation


## Libraries ####

library(tidyverse)
library(DBI)
library(RSQLite)
library(lubridate)
library(scales)


## Working directory ####

# Create new folder for outputs if it doesn't already exist
dir.create(file.path(getwd(), "outputs"), showWarnings = FALSE)

# Get subdirectory for outputs folder
outputs_folder <- file.path(getwd(), "outputs")


## Custom ggplot2 theme ####
my_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 10, face = "plain"),
        axis.title = element_text(size = 10, face = "plain"),
        plot.title = element_text(size = 11, face = "plain"))


# CONNECT TO SQLITE DATABASE ####

# Existing .db file in current working directory
con <- dbConnect(SQLite(), "observatory-test.db")


# EXPLORE DATABASE ####

# List tables in db
# dbListTables(con)

# List fields in named table
# dbListFields(con, "tweet")
# dbListFields(con, "user")
# dbListFields(con, "media")
# dbListFields(con, "url")

# View records in named table
# dbReadTable(con, "tweet")

# Save named table as df
# tweet <- dbReadTable(con, "tweet")
# user <- dbReadTable(con, "user")


# TIDY DATA ####

# user$created_at_parsed <- ymd_hms(user$created_at)


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
  geom_bar(stat = "identity") +
  labs(title = paste0("Top ", n, " tweet authors by number of tweets"),
       x = "Username", y = "Number of tweets") +
  scale_y_continuous(label = comma) +
  coord_flip() +
  my_theme


## Who is being retweeted the most? ####
n <- 20
result <- 
dbGetQuery(con,
           "SELECT tweet.id, tweet.retweeted_tweet_id, tweet.text, user.username as `retweeter_username`
            FROM tweet
            LEFT JOIN (
              user )
            ON tweet.author_id = user.id
            WHERE retweeted_tweet_id IS NOT NULL;")
View(result)

dbGetQuery(con,
           "SELECT retweeted_tweet_id FROM tweet WHERE retweeted_tweet_id IS NOT NULL;")


# IDEAS ####

## Number of tweets per day/week/month ####

## Number of unique accounts per day/week/month ####

## Top n hashtags ####

## Top n account mentions ####

## Top n domains ####

