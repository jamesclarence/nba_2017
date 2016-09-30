# Find probabilities of a conference rank by point differential

# libraries
library(dplyr)
library(ggplot2)

# read in standings doc
setwd("C:/Users/fishe/Documents/nba_2017")
x <- read.csv("nba_standings_1977_2016.csv", stringsAsFactors = F)

### Visualizations of distribution
# density of x by conference rank
ggplot(x, aes(x = pt_diff, fill = as.factor(conf_rank))) +
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

# z-score: pnorm([number], pop_mean, pop_sd)
# http://stats.seandolinar.com/calculating-z-scores-with-r/


## calculate z-score for standing based on conference rank
c1 <- x %>% filter(conf_rank == 1)
pop_sd1 <- sd(c1$pt_diff)*sqrt((length(c1$pt_diff)-1)/(length(c1$pt_diff)))
pop_mean1 <- mean(c1$pt_diff)

pnorm(0, pop_mean1, pop_sd1)

c2 <- x %>% filter(conf_rank == 2)
pop_sd2 <- sd(c2$pt_diff)*sqrt((length(c2$pt_diff)-2)/(length(c2$pt_diff)))
pop_mean2 <- mean(c2$pt_diff)

pnorm(0, pop_mean2, pop_sd2)

# Create list of point diffs from -50.0:50.0
seq(-50, 50, by = 0.1)

# Find probability of each point differential and conf_rank combination
# Choose the highest probability for each




