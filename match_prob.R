# Generate JSON file to update NBA standings and point differential

# load packages
library(rvest)
library(stringr)
library(jsonlite)
library(dplyr)
library(readr)

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

# Change columns to character for matching (floating point issue)
standings$pt_diff <- as.character(round(standings$pt_diff, 1))
prob$pt_seq <- as.character(round(prob$pt_seq, 1))

# Join point differential with probability
s1 <- standings %>% 
  left_join(prob, by = c("pt_diff" = "pt_seq"))

# Turn prob into percentage format and reformat data frame
s2 <- select(s1, conf, conf_rank, team, w, l, `w/l%`, tm_pts, op_pts, pt_diff , prob, -pop_mean, -pop_sd, -zscore) %>%
    arrange(desc(prob))

# Turn probability into percentage format 
s3 <- mutate(s2, prob_pct = str_c(round(prob*100, 1), "%")) %>% select(-prob)

# Rename columns
s4 <- rename(s3, 
            Conf = conf,
            `Conf Rank` = conf_rank,
            Team = team,
            W = w,
            L = l,
            PCT = `w/l%`,
            PPG = tm_pts,
            oPPG = op_pts,
            Diff= pt_diff,
            `Playoff Prob` = prob_pct
          )

# To JSON file
standings_playoff_pct <- toJSON(s4, pretty = T)

write(standings_playoff_pct, "nba_playoff_pct.JSON")

ggplot(s2, aes(x = prob, y = as.numeric(pt_diff))) + 
  geom_point(aes(color = factor(team))) + 
  geom_hline(aes(yintercept = 0)) +
  theme_bw()

ggplot(s4, aes(x = rank(PPG), y = rank(oPPG))) + 
  geom_point(aes(color = factor(Team)), size = 2) +
  geom_smooth() +
  theme_bw()
