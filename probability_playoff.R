# Find probability of making the playoffs by point differential

# libraries
library(dplyr)
library(ggplot2)
library(stringr)

# read in standings doc
setwd("C:/Users/fishe/Documents/nba_2017")
x <- read.csv("nba_standings_1977_2016.csv", stringsAsFactors = F)

# z-score: pnorm([number], pop_mean, pop_sd)
# http://stats.seandolinar.com/calculating-z-scores-with-r/

yes <- x %>% filter(playoff == 'y')
no <- x %>% filter(playoff == 'n')

# Calculate population sd and population mean for each conference rank
pop_sd_y <- sd(yes$pt_diff)*sqrt((length(yes$pt_diff)-1)/(length(yes$pt_diff)))
pop_mean_y <- mean(yes$pt_diff)

# Create list of point diffs from -15.0:0
pt_seq <- seq(-15, 15, by = 0.1)
pt_seq <- as.data.frame(pt_seq)

# Create data frame for each playoff status
df_y <- cbind(rep("y", nrow(pt_seq)), pt_seq, rep(pop_mean_y, nrow(pt_seq)), rep(pop_sd_y, nrow(pt_seq)))
colnames(df_y) <- c("playoff", "pt_seq", "pop_mean", "pop_sd")

# Combine playoff status and add probabilities
df_y <- df_y %>% mutate(zscore = (pt_seq - pop_mean / pop_sd))
df_y$prob <- pnorm(df_y$pt_seq, df_y$pop_mean, df_y$pop_sd)

df_y <- df_y %>% mutate(prob_pct = str_c(round(prob*100, 1), "%"))

# Write the probabilities as a csv
write.csv(df_y, "probability_playoff.csv", row.names = F)
