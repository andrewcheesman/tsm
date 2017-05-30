# Experimenting with the transition state matrix approach, using data from UCI
# May 6, 2017

library(ggplot2)
library(sqldf)
library(scales)

####################################################################################################
# Data from https://archive.ics.uci.edu/ml/machine-learning-databases/00350/
# Minimally cleaned by hand (changed encoding, killed Xn var name toppers, saved as cc_def.csv)

a1 <- read.csv("cc_def.csv", stringsAsFactors = F)

# -2 seems to denote full payment of loan during time period; removing all obs with full repayment for this analysis

a <- a1[a1$PAY_0 != -2 & 
          a1$PAY_2 != -2 & 
          a1$PAY_3 != -2 & 
          a1$PAY_4 != -2 & 
          a1$PAY_5 != -2 & 
          a1$PAY_6 != -2,]

rm(a1)

# Recoding the -1's to 0's (as they seem to be equivalent - signifying being up-to-date on payments)
# Also recoding 1's to 0's (there are very few observations for '1')

for (i in 7:12) {
  a[,i] <- ifelse(a[,i] == -1, 0, a[,i])
  a[,i] <- ifelse(a[,i] == 1, 0, a[,i])
}

rm(i)

# Mapping some basic trends

dummy <- data.frame()
for (i in 7:12) {
  b <- data.frame(var = colnames(a)[i],
                  val = a[,i])
  dummy <- rbind(dummy, b)
  }

rm(b, i)

ggplot(dummy, aes(x = val)) +
  geom_histogram(bins = 11) + 
  facet_wrap(~var) +
  theme_bw()

rm(dummy)

# Checking occurrence of movement from 1 @ t0 to 3+ at tn

c <- sqldf("select pay_0, pay_2, pay_3, pay_4, pay_5, pay_6, count(distinct(id)) from a group by 1, 2, 3, 4, 5, 6")
write.csv(c, "prog.csv", row.names = F)


####################################################################################################
# Calculating TSM probabilities

# Normalizing for time
dummy <- data.frame()
for (i in 7:11) {
  b <- data.frame(id = a$ID,
                  a = a[,i],
                  b = a[,i+1])
  dummy <- rbind(dummy, b)
}

# State-dependent frequencies
unqs <- sort(unique(dummy$a))

sdf <- data.frame(st = c(0:8))
for (j in 1:length(unqs)) {
  sdf1 <- aggregate(id ~ b, 
                   dummy[dummy$a == unqs[j], ], 
                   FUN = length)
  colnames(sdf1)[2] <- paste0(unqs[j],"_ct")
  sdf <- merge(sdf, sdf1, by.x = "st", by.y = "b", all = T)
  }

rm(j, unqs, sdf1)

sdf$total <- rowSums(sdf[,2:length(colnames(sdf))], na.rm = T)

sdf_pct <- data.frame(id = sdf$st,
                      sdf[,2] / sdf[,10],
                      sdf[,3] / sdf[,10],
                      sdf[,4] / sdf[,10],
                      sdf[,5] / sdf[,10],
                      sdf[,6] / sdf[,10],
                      sdf[,7] / sdf[,10],
                      sdf[,8] / sdf[,10],
                      sdf[,9] / sdf[,10])

sdf_pct[is.na(sdf_pct)] <- 0
sdf_pct[,c(2:9)] <- apply(sdf_pct[,c(2:9)], 2, percent)
colnames(sdf_pct)[2:9] <- c("pct0", "pct2", "pct3", "pct4", "pct5", "pct6", "pct7", "pct8")

for (k in 2:9) {
sdf_pct[sdf_pct[,k] == "0.0%", k] <- ""
}

# todo
# 0. separate into train and test
# 1. use dummy as starting data - universal before/after framework
# 2. simplify after values - should only be possible for individuals to increase by 1 (if they go another month without paying) or fall back to 0
# 2. pull in predictors from a (sex, education, marriage, age) into dummy
# 3. models for as many states as there exist
#    response var: EITHER treat anything BUT a movement back to 0 as a 1 (which means remaining behind on payments) 
#                  OR build multivariate logistic models
