# Find probabilities of a conference rank by point differential

# libraries
library(dplyr)
library(ggplot2)

# read in standings doc
setwd("C:/Users/fishe/Documents/nba_2017")
x <- read.csv("nba_standings_1977_2016.csv", stringsAsFactors = F)

# z-score: pnorm([number], pop_mean, pop_sd)
# http://stats.seandolinar.com/calculating-z-scores-with-r/

## calculate z-score for standing based on conference rank
c1 <- x %>% filter(conf_rank == 1)
c2 <- x %>% filter(conf_rank == 2)
c3 <- x %>% filter(conf_rank == 3)
c4 <- x %>% filter(conf_rank == 4)
c5 <- x %>% filter(conf_rank == 5)
c6 <- x %>% filter(conf_rank == 6)
c7 <- x %>% filter(conf_rank == 7)
c8 <- x %>% filter(conf_rank == 8)
c9 <- x %>% filter(conf_rank == 9)
c10 <- x %>% filter(conf_rank == 10)
c11 <- x %>% filter(conf_rank == 11)
c12 <- x %>% filter(conf_rank == 12)
c13 <- x %>% filter(conf_rank == 13)
c14 <- x %>% filter(conf_rank == 14)
c15 <- x %>% filter(conf_rank == 15)

# Calculate population sd and population mean for each conference rank
pop_sd1 <- sd(c1$pt_diff)*sqrt((length(c1$pt_diff)-1)/(length(c1$pt_diff)))
pop_mean1 <- mean(c1$pt_diff)

pop_sd2 <- sd(c2$pt_diff)*sqrt((length(c2$pt_diff)-1)/(length(c2$pt_diff)))
pop_mean2 <- mean(c2$pt_diff)

pop_sd3 <- sd(c3$pt_diff)*sqrt((length(c3$pt_diff)-1)/(length(c3$pt_diff)))
pop_mean3 <- mean(c3$pt_diff)

pop_sd4 <- sd(c4$pt_diff)*sqrt((length(c4$pt_diff)-1)/(length(c4$pt_diff)))
pop_mean4 <- mean(c4$pt_diff)

pop_sd5 <- sd(c5$pt_diff)*sqrt((length(c5$pt_diff)-1)/(length(c5$pt_diff)))
pop_mean5 <- mean(c5$pt_diff)

pop_sd6 <- sd(c6$pt_diff)*sqrt((length(c6$pt_diff)-1)/(length(c6$pt_diff)))
pop_mean6 <- mean(c6$pt_diff)

pop_sd7 <- sd(c7$pt_diff)*sqrt((length(c7$pt_diff)-1)/(length(c7$pt_diff)))
pop_mean7 <- mean(c7$pt_diff)

pop_sd8 <- sd(c8$pt_diff)*sqrt((length(c8$pt_diff)-1)/(length(c8$pt_diff)))
pop_mean8 <- mean(c8$pt_diff)

pop_sd9 <- sd(c9$pt_diff)*sqrt((length(c9$pt_diff)-1)/(length(c9$pt_diff)))
pop_mean9 <- mean(c9$pt_diff)

pop_sd10 <- sd(c10$pt_diff)*sqrt((length(c10$pt_diff)-1)/(length(c10$pt_diff)))
pop_mean10 <- mean(c10$pt_diff)

pop_sd11 <- sd(c11$pt_diff)*sqrt((length(c11$pt_diff)-1)/(length(c11$pt_diff)))
pop_mean11 <- mean(c11$pt_diff)

pop_sd12 <- sd(c12$pt_diff)*sqrt((length(c12$pt_diff)-1)/(length(c12$pt_diff)))
pop_mean12 <- mean(c12$pt_diff)

pop_sd13 <- sd(c13$pt_diff)*sqrt((length(c13$pt_diff)-1)/(length(c13$pt_diff)))
pop_mean13 <- mean(c13$pt_diff)

pop_sd14 <- sd(c14$pt_diff)*sqrt((length(c14$pt_diff)-1)/(length(c14$pt_diff)))
pop_mean14 <- mean(c14$pt_diff)

pop_sd15 <- sd(c15$pt_diff)*sqrt((length(c15$pt_diff)-1)/(length(c15$pt_diff)))
pop_mean15 <- mean(c15$pt_diff)

# Create list of point diffs from -20.0:20.0
pt_seq <- seq(-15, 15, by = 0.1)
pt_seq <- as.data.frame(pt_seq)

# Create data frame for each conference rank
df1 <- cbind(rep("1", nrow(pt_seq)), pt_seq, rep(pop_mean1, nrow(pt_seq)), rep(pop_sd1, nrow(pt_seq)))
colnames(df1) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df2 <- cbind(rep("2", nrow(pt_seq)), pt_seq, rep(pop_mean2, nrow(pt_seq)), rep(pop_sd2, nrow(pt_seq)))
colnames(df2) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df3 <- cbind(rep("3", nrow(pt_seq)), pt_seq, rep(pop_mean3, nrow(pt_seq)), rep(pop_sd3, nrow(pt_seq)))
colnames(df3) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df4 <- cbind(rep("4", nrow(pt_seq)), pt_seq, rep(pop_mean4, nrow(pt_seq)), rep(pop_sd4, nrow(pt_seq)))
colnames(df4) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df5 <- cbind(rep("5", nrow(pt_seq)), pt_seq, rep(pop_mean5, nrow(pt_seq)), rep(pop_sd5, nrow(pt_seq)))
colnames(df5) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df6 <- cbind(rep("6", nrow(pt_seq)), pt_seq, rep(pop_mean6, nrow(pt_seq)), rep(pop_sd6, nrow(pt_seq)))
colnames(df6) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df7 <- cbind(rep("7", nrow(pt_seq)), pt_seq, rep(pop_mean7, nrow(pt_seq)), rep(pop_sd7, nrow(pt_seq)))
colnames(df7) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df8 <- cbind(rep("8", nrow(pt_seq)), pt_seq, rep(pop_mean8, nrow(pt_seq)), rep(pop_sd8, nrow(pt_seq)))
colnames(df8) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df9 <- cbind(rep("9", nrow(pt_seq)), pt_seq, rep(pop_mean9, nrow(pt_seq)), rep(pop_sd9, nrow(pt_seq)))
colnames(df9) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df10 <- cbind(rep("10", nrow(pt_seq)), pt_seq, rep(pop_mean10, nrow(pt_seq)), rep(pop_sd10, nrow(pt_seq)))
colnames(df10) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df11 <- cbind(rep("11", nrow(pt_seq)), pt_seq, rep(pop_mean11, nrow(pt_seq)), rep(pop_sd11, nrow(pt_seq)))
colnames(df11) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df12 <- cbind(rep("12", nrow(pt_seq)), pt_seq, rep(pop_mean12, nrow(pt_seq)), rep(pop_sd12, nrow(pt_seq)))
colnames(df12) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df13 <- cbind(rep("13", nrow(pt_seq)), pt_seq, rep(pop_mean13, nrow(pt_seq)), rep(pop_sd13, nrow(pt_seq)))
colnames(df13) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df14 <- cbind(rep("14", nrow(pt_seq)), pt_seq, rep(pop_mean14, nrow(pt_seq)), rep(pop_sd14, nrow(pt_seq)))
colnames(df14) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

df15 <- cbind(rep("15", nrow(pt_seq)), pt_seq, rep(pop_mean15, nrow(pt_seq)), rep(pop_sd15, nrow(pt_seq)))
colnames(df15) <- c("conf_rank", "pt_seq", "pop_mean", "pop_sd")

# Combine conference ranks and add probabilities
df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15)
df <- df %>% mutate(zscore = (pt_seq - pop_mean / pop_sd))
df$conf_rank <- as.integer(df$conf_rank)
df$prob <- pnorm(df$pt_seq, df$pop_mean, df$pop_sd)

# Write the probabilities as a csv
write.csv(df, "probability.csv", row.names = F)
