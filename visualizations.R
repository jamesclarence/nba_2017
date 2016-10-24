# Install packages
library(dplyr)
library(ggplot2)

# Set working directory
setwd("C:/Users/fishe/Documents/nba_2017")

# Read in standings and probability docs
x <- read.csv("data/nba_standings_1977_2016.csv", stringsAsFactors = F)
y <- read.csv("data/probability.csv", stringsAsFactors = F)

x <- mutate(x, pt_diff = tm_pts - op_pts)

# match z score with standings
df1 <- select(x, conf_rank, pt_diff) %>% inner_join(y, by = c("pt_diff" = "pt_seq"))
df2 <- select(x, conf_rank, pt_diff) %>% inner_join(y, by = c("conf_rank"))
filter(df1, conf_rank.x == conf_rank.y)
filter(df2, pt_diff == pt_seq)


### Visualizations of distribution
# density of x by conference rank
ggplot(x, aes(x = pt_diff, fill = as.factor(conf_rank))) +
  geom_density(alpha=0.25) + theme_bw()

filter(x, conf_rank == 1|conf_rank == 2|conf_rank == 3) %>% 
  ggplot(aes(x = pt_diff, fill = as.factor(conf_rank))) +
  geom_density(alpha=0.25) + theme_bw()

# scatter plot comparing conf_rank and pt_diff
ggplot(x,aes(x = conf_rank, y = pt_diff)) +
  geom_point(aes(color = factor(playoff))) +
  stat_smooth() +
  theme_bw()

# scatter plot comparing conf_rank and wins
x %>% mutate(win_pct = w/(w+l)) %>%
  ggplot(aes(x = conf_rank, y = win_pct)) +
  geom_point(aes(color = factor(playoff))) +
  stat_smooth() +
  theme_bw()

# What's the probability based on point differential a team will make a certain playoff spot?
# What's the chance of making the playoffs based on point differential?

filter(x, playoff =="y")

# range high-low pt diff by playoff spot
x %>% group_by(conf_rank, pt_diff) %>% mutate(avg = mean(pt_diff)) %>% 
  select(conf_rank, avg)  %>% 
  arrange(conf_rank)

# average pt_diff by conference rank
avg_pt_diff_conf_rank <- x %>% group_by(conf_rank) %>% summarise(count = n(), avg_pt_diff = mean(pt_diff))

## calculate z-score for entire standings
pop_sd <- sd(x$pt_diff)*sqrt((length(x$pt_diff)-1)/(length(x$pt_diff)))
pop_mean <- mean(x$pt_diff)