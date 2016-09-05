# Historical Point Differential Data

# Load packages
library(rvest)
library(stringr)
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)

## Get URLs from NBA seasons from Basketball Reference
ssn_url1 <- "http://www.basketball-reference.com/leagues/NBA_"
ssn_url2 <- "1977"
ssn_url3 <- "_standings.html"

ssn_number <- data.frame(1977:2015)

ssn_url_begin <- rep(ssn_url1, length = nrow(ssn_number))
ssn_url_end <- rep(ssn_url3, length = nrow(ssn_number))
ssn_url <- cbind(begin = ssn_url_begin, num = ssn_number, end = ssn_url_end) %>%
  sapply(as.character)

# Each ssn's URL
ssn_url_all <- apply(ssn_url, 1, paste, collapse="")

# clean east
clean_east <- function(i) {
  # Read HTML from Basketball Reference
  url <- read_html(i)
  
  # Get the standings table
  east <- html_table(html_nodes(url, "table"))[[1]]
  
  # lowercase column names
  names(east) <- tolower(names(east))
  
  # remove division rows
  east <- east[-grep("Division", east$`eastern conference`),]
  
  # rename column
  # pts (column 6) to tm_pts
  east$tm_pts <- east[,6]
  # pts (column 7) to opp_pts
  east$op_pts <- east[,7]
  
  # convert tm_pts and op_pts to integers
  east$tm_pts <- as.numeric(east$tm_pts)
  east$op_pts <- as.numeric(east$op_pts)
  
  # remove pts columns
  east$pts <- NULL
  east$pts <- NULL
  
  # rename eastern conference to team
  east <- rename(east, "team" = `eastern conference`)
  
  # add column 
  
  # conference; add "east" to it
  east$conf <- "east"
  
  # season; add year 1977
  east$ssn <- str_extract(i, "[[:digit:]]{4}")
  
  # playoff; if team contains an *, y; if no *, n
  east$playoff <- ifelse(grepl("\\*", east$team), "y", "n")
  
  # remove asterisk
  east$team <- str_replace_all(east$team, "\\*", "")
  
  # conference rank from team column to separate column
  east$conf_rank <- str_extract(east$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
  east$conf_rank <- str_replace_all(east$conf_rank, "[[:punct:]]|[[:space:]]", "")
  east$conf_rank <- as.integer(east$conf_rank)
  
  # remove conference rank from team column
  # east$team <- str_replace_all(east$team, "[[:punct:]]|[[:digit:]]", "") %>% str_trim
  east$team <- str_replace_all(east$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim
  
  # calculate point differential
  east <- mutate(east, pt_diff = tm_pts - op_pts)
  
  east <- select(east, team, conf, w, l, tm_pts, op_pts, ssn, playoff, conf_rank, pt_diff)
}

# clean west
clean_west <- function(i) {
  # Read HTML from Basketball Reference
  url <- read_html(i)
  
  # Get the standings table
  west <- html_table(html_nodes(url, "table"))[[2]]
  
  # lowercase column names
  names(west) <- tolower(names(west))
  
  # remove division rows
  west <- west[-grep("Division", west$`western conference`),]
  
  # rename column
  # pts (column 6) to tm_pts
  west$tm_pts <- west[,6]
  # pts (column 7) to opp_pts
  west$op_pts <- west[,7]
  
  # convert tm_pts and op_pts to integers
  west$tm_pts <- as.numeric(west$tm_pts)
  west$op_pts <- as.numeric(west$op_pts)
  
  # remove pts columns
  west$pts <- NULL
  west$pts <- NULL
  
  # rename western conference to team
  west <- rename(west, "team" = `western conference`)
  
  # add column 
  
  # conference; add "west" to it
  west$conf <- "west"
  
  # season; add year 1977
  west$ssn <- str_extract(i, "[[:digit:]]{4}")
  
  # playoff; if team contains an *, y; if no *, n
  west$playoff <- ifelse(grepl("\\*", west$team), "y", "n")
  
  # remove asterisk
  west$team <- str_replace_all(west$team, "\\*", "")
  
  # conference rank from team column to separate column
  west$conf_rank <- str_extract(west$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
  west$conf_rank <- str_replace_all(west$conf_rank, "[[:punct:]]|[[:space:]]", "")
  west$conf_rank <- as.integer(west$conf_rank)
  
  # remove conference rank from team column
  # west$team <- str_replace_all(west$team, "[[:punct:]]|[[:digit:]]", "") %>% str_trim
  west$team <- str_replace_all(west$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim
  
  # calculate point differential
  west <- mutate(west, pt_diff = tm_pts - op_pts)
  
  west <- select(west, team, conf, w, l, tm_pts, op_pts, ssn, playoff, conf_rank, pt_diff)
}

# clean east 2016
clean_east_2016 <- function(i) {
  url <- read_html(i)
  
  # Get the standings table
  east <- html_table(html_nodes(url, "table"))[[3]]
  
  # lowercase column names
  names(east) <- tolower(names(east))
  
  # remove division rows
  east <- east[-grep("Division", east$`eastern conference`),]
  
  # rename column
  # pts (column 6) to tm_pts
  east$tm_pts <- east[,6]
  # pts (column 7) to opp_pts
  east$op_pts <- east[,7]
  
  # convert tm_pts and op_pts to integers
  east$tm_pts <- as.numeric(east$tm_pts)
  east$op_pts <- as.numeric(east$op_pts)
  
  # remove pts columns
  east$pts <- NULL
  east$pts <- NULL
  
  # rename eastern conference to team
  east <- rename(east, "team" = `eastern conference`)
  
  # add column 
  
  # conference; add "east" to it
  east$conf <- "east"
  
  # season; add year 1977
  east$ssn <- str_extract(i, "[[:digit:]]{4}")
  
  # playoff; if team contains an *, y; if no *, n
  east$playoff <- ifelse(grepl("\\*", east$team), "y", "n")
  
  # remove asterisk
  east$team <- str_replace_all(east$team, "\\*", "")
  
  # conference rank from team column to separate column
  east$conf_rank <- str_extract(east$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
  east$conf_rank <- str_replace_all(east$conf_rank, "[[:punct:]]|[[:space:]]", "")
  east$conf_rank <- as.integer(east$conf_rank)
  
  # remove conference rank from team column
  # east$team <- str_replace_all(east$team, "[[:punct:]]|[[:digit:]]", "") %>% str_trim
  east$team <- str_replace_all(east$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim
  
  # calculate point differential
  east <- mutate(east, pt_diff = tm_pts - op_pts)
  
  east <- select(east, team, conf, w, l, tm_pts, op_pts, ssn, playoff, conf_rank, pt_diff)
}

clean_west_2016 <- function(i) {
  url <- read_html(i)
  
  # Get the standings table
  west <- html_table(html_nodes(url, "table"))[[4]]
  
  # lowercase column names
  names(west) <- tolower(names(west))
  
  # remove division rows
  west <- west[-grep("Division", west$`western conference`),]
  
  # rename column
  # pts (column 6) to tm_pts
  west$tm_pts <- west[,6]
  # pts (column 7) to opp_pts
  west$op_pts <- west[,7]
  
  # convert tm_pts and op_pts to integers
  west$tm_pts <- as.numeric(west$tm_pts)
  west$op_pts <- as.numeric(west$op_pts)
  
  # remove pts columns
  west$pts <- NULL
  west$pts <- NULL
  
  # rename western conference to team
  west <- rename(west, "team" = `western conference`)
  
  # add column 
  
  # conference; add "west" to it
  west$conf <- "west"
  
  # season; add year 1977
  west$ssn <- str_extract(i, "[[:digit:]]{4}")
  
  # playoff; if team contains an *, y; if no *, n
  west$playoff <- ifelse(grepl("\\*", west$team), "y", "n")
  
  # remove asterisk
  west$team <- str_replace_all(west$team, "\\*", "")
  
  # conference rank from team column to separate column
  west$conf_rank <- str_extract(west$team, "[[:space:]]\\([[:alnum:]]{1,}\\)[[:space:]]")
  west$conf_rank <- str_replace_all(west$conf_rank, "[[:punct:]]|[[:space:]]", "")
  west$conf_rank <- as.integer(west$conf_rank)
  
  # remove conference rank from team column
  # west$team <- str_replace_all(west$team, "[[:punct:]]|[[:digit:]]", "") %>% str_trim
  west$team <- str_replace_all(west$team, "[[:punct:]][[:digit:]]{1,2}[[:punct:]]", "") %>% str_trim
  
  # calculate point differential
  west <- mutate(west, pt_diff = tm_pts - op_pts)
  
  west <- select(west, team, conf, w, l, tm_pts, op_pts, ssn, playoff, conf_rank, pt_diff)
}

# bind east and west
e <- plyr::ldply(ssn_url_all, clean_east)
w <- plyr::ldply(ssn_url_all, clean_west)
e_16 <- plyr::ldply("http://www.basketball-reference.com/leagues/NBA_2016_standings.html", clean_east_2016)
w_16 <- plyr::ldply("http://www.basketball-reference.com/leagues/NBA_2016_standings.html", clean_west_2016)
nba_standings_1977_2016 <- rbind(e, w, e_16, w_16)

# write data as a csv
write.csv(nba_standings_1977_2016, "nba_standings_1977_2016.csv", row.names = F)

# Plot
ggplot(nba_standings_1977_2016,aes(x = conf_rank, y = pt_diff)) +
  geom_point() +
  stat_smooth() +
  theme_bw()

## get champion
# play <- html_nodes(url_p, "#content") %>% html_text()
# html_nodes(url, "div div")[11] %>% html_text()# 
# url_p <- read_html("http://www.basketball-reference.com/playoffs/NBA_1977.html")
