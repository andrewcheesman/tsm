# Experimenting with the transition state matrix approach, using data from UCI
# May 6, 2017

library(ggplot2)

####################################################################################################
# Data from https://archive.ics.uci.edu/ml/machine-learning-databases/00350/
# Minimally cleaned by hand (changed encoding, killed Xn var name toppers, saved as cc_def.csv)

a <- read.csv("cc_def.csv", stringsAsFactors = F)

# Mapping some basic trends

dummy <- data.frame()
for (i in 7:12) {
  b <- data.frame(var = colnames(a)[i],
                  val = a[,i])
  dummy <- rbind(dummy, b)
  }

ggplot(dummy, aes(x = val)) +
  geom_histogram(bins = 11) + 
  facet_wrap(~var) +
  theme_bw()

for (k in 7:12) {
  a[,k] <- ifelse(a[,k] == -2, -1, a[,k])
  a[,k] <- ifelse(a[,k] == 0, 1, a[,k])
  }

library(sqldf)

c <- sqldf("select pay_0, pay_2, pay_3, pay_4, pay_5, pay_6, count(distinct(id)) from a group by 1, 2, 3, 4, 5, 6")
write.csv(c, "prog.csv", row.names = F)






































































































