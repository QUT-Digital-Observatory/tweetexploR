# THINGS TO INVESTIGATE ####

## DESCRIPTION.Imports vs. @importFrom ####
# Using DESCRIPTION.Imports vs. @importFrom package function inside each
# function. Currently these functions are using @importFrom:
#  num_tweets_by_timeperiod()
#  num_users_by_timeperiod()


# IDEAS FOR TESTS ####

## db file exists ####
# Test that .db file exists before connecting to it, otherwise an empty .db is
# created and there's no error message.

## Results of a known query ####
# Add test to see if a .db can be queried (e.g., count(*)) and that results are
# as expected.


# IDEAS FOR VISUALISATIONS ####

## Top n accounts being retweeted ####
# Regex to detect retweet in tweet text: "^RT"
# Can also use tweet.retweeted_tweet_id is not null

## Engagement metric summary ####
# Retweets, likes, replies, etc. in either a table or a graph of some sort?


# IDEAS FOR FUNCTIONALITY/FEATURES ####

## Ability for user to specify geom_col() or geom_line() ####
# Possibly can be achieved with 'pass the dots'?

## Ability for user to export data frame and/or ggplot ####
# Possibly can access data from the ggplot object
# Build a wrapper for ggsave, or just allow people to use ggsave themselves?

## Ability for user to include/exclude retweets ####

## Ability for user to change colour of lines/bars in plots ####
# Possibly can be achieved with 'pass the dots'?


# NOTES FOR DOCUMENTATION ####

## Include recommended values for n for each visualisation ####

## Include recommended time periods for the hourly/daily/monthly visualisations ####

## Possibly include a sample .db file for distribution with the package ####


# TODO: Disconnect from database? But when? ####
