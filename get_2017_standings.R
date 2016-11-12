# 2017 Season

# load packages
library(rvest)
library(stringr)
library(dplyr)
library(readr)
library(ggplot2)

# Set working directory
setwd("C:/Users/fishe/Documents/nba_2017")

# Read in pt_diff probability
prob <- read_csv("data/probability_playoff.csv")

# Read in standings from Basketball Reference
url <- "http://www.basketball-reference.com/leagues/NBA_2017_standings.html" 

url <- read_html(url)

# Clean Eastern Conference
# Get the standings table
east <- html_table(html_nodes(url, "table"))[[1]]

# lowercase column names
names(east) <- tolower(names(east))

# remove division rows
# east <- east[-grep("Division", east$`eastern conference`),]

# convert tm_pts and op_pts to integers
east$tm_pts <- as.numeric(east$`ps/g`)
east$op_pts <- as.numeric(east$`pa/g`)

# remove pts columns
east$`ps/g` <- NULL
east$`pa/g` <- NULL

# rename eastern conference to team
east <- rename(east, "team" = `eastern conference`)

# add conference column; add "east" to it
east$conf <- "East"

# remove asterisk from team column
east$team <- str_replace_all(east$team, "\\*", "")

# conference rank from team column to separate column
east$conf_rank <- str_extract(east$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
east$conf_rank <- str_replace_all(east$conf_rank, "[[:punct:]]|[[:space:]]", "")
east$conf_rank <- as.integer(east$conf_rank)

# remove conference rank from team column
east$team <- str_replace_all(east$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim

# calculate point differential
east <- mutate(east, pt_diff = tm_pts - op_pts)

# Clean Western Conference
# Get the standings table
west <- html_table(html_nodes(url, "table"))[[2]]

# lowercase column names
names(west) <- tolower(names(west))

# convert tm_pts and op_pts to integers
west$tm_pts <- as.numeric(west$`ps/g`)
west$op_pts <- as.numeric(west$`pa/g`)

# remove pts columns
west$`ps/g` <- NULL
west$`pa/g` <- NULL

# rename western conference to team
west <- rename(west, "team" = `western conference`)

# add conference column; add "west" to it
west$conf <- "West"

# remove asterisk from team column
west$team <- str_replace_all(west$team, "\\*", "")

# conference rank from team column to separate column
west$conf_rank <- str_extract(west$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
west$conf_rank <- str_replace_all(west$conf_rank, "[[:punct:]]|[[:space:]]", "")
west$conf_rank <- as.integer(west$conf_rank)

# remove conference rank from team column
west$team <- str_replace_all(west$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim

# calculate point differential
west <- mutate(west, pt_diff = tm_pts - op_pts)

# Bind east and west conference; round pt_diff for matching
standings <- rbind(east, west) %>% arrange(conf, conf_rank) %>% select(-gb, -srs)

# Add rank
standings <- mutate(standings, off_rank = min_rank(desc(tm_pts)), def_rank = min_rank((op_pts)), pt_diff_rank = min_rank(desc(pt_diff)))

ggplot(standings, aes(x = pt_diff, y = conf_rank)) + 
  geom_point(size = 2) +
  geom_smooth() +
  theme_bw()

# Offense vs Defense Rank
ggplot(standings, aes(x = off_rank, y = def_rank)) + 
  geom_point(size = 2) +
  geom_smooth() +
  theme_bw()

# Point Difference vs. Winning Percentage Rank
ggplot(standings, aes(x = pt_diff_rank, y = `w/l%`)) + 
  geom_point(aes(color = factor(team)), size = 2) +
  geom_smooth() +
  theme_bw()

# Offense vs. Winning Percentage Rank
ggplot(standings, aes(x = off_rank, y = `w/l%`)) + 
  geom_point(aes(color = factor(team)), size = 2) +
  geom_smooth() +
  theme_bw()

# Defense vs. Winning Percentage Rank
ggplot(standings, aes(x = def_rank, y = `w/l%`)) + 
  geom_point(aes(color = factor(team)), size = 2) +
  geom_smooth() +
  theme_bw()

