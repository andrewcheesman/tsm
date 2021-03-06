# Experimenting with the transition state matrix approach, using data from UCI
# May 6, 2017

library(ggplot2)
library(scales)
library(markovchain)

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

save(a, file = "base.RDA")

# # Mapping some basic trends
# 
# dummy <- data.frame()
# for (i in 7:12) {
#   b <- data.frame(var = colnames(a)[i],
#                   val = a[,i])
#   dummy <- rbind(dummy, b)
#   }
# 
# rm(b, i)
# 
# ggplot(dummy, aes(x = factor(val))) +
#   geom_histogram(stat = "count") + 
#   facet_wrap(~var) +
#   # scale_y_log10() + 
#   theme_bw()
# 
# rm(dummy)rm(dummy)

# Basic Frequencies @ t0 to t6

ad <- data.frame(st = c(0, 2:8))
for (p in 7:12) {
  
  agg <- aggregate(a$ID ~ a[,p], a, FUN = length)
  colnames(agg) <- c("st", paste0("ct_", substr(colnames(a)[p], 5, 5)))
  ad <- merge(ad, agg, by = "st", all = T)
  
}

rm(p, agg)

ad_pct <- data.frame(id = ad$st,
                     st0 = percent(ad[,2] / nrow(a)),
                     st2 = percent(ad[,3] / nrow(a)),
                     st3 = percent(ad[,4] / nrow(a)),
                     st4 = percent(ad[,5] / nrow(a)),
                     st5 = percent(ad[,6] / nrow(a)),
                     st6 = percent(ad[,7] / nrow(a)))

####################################################################################################
# Prep for DTMC and modeling
# long-term, functionalize based on input dataset (to resolve duplicate code for test/train)

load("base.RDA")

# Bucketing states - comment out line 84-86 to go back to original

for (i in 7:12) {
  a[,i] <- ifelse(a[,i] >= 6, 6, a[,i])
}

unqs <- sort(unique(a$PAY_0))

ad <- data.frame(st = c(unqs))
for (p in 7:12) {
  agg <- aggregate(ID ~ a[,p], a, FUN = length)
  colnames(agg) <- c("st", paste0("tm_", substr(colnames(a)[p], 5, 5)))
  ad <- merge(ad, agg, by = "st", all = T)
}

ad_pct <- ad[,c(2:ncol(ad))]/nrow(a)
ad_pct <- apply(ad_pct, 2, percent)

print(ad_pct)

rm(p, i, agg, unqs, ad)

# Test/train

set.seed(521)
trn_ind <- sample(seq_len(nrow(a)), size = floor(0.7 * nrow(a)))

trn <- a[trn_ind, ]
tst <- a[-trn_ind, ]

# Normalizing for time - transferring all periods to simple before/after

trnl <- data.frame()
for (i in 7:11) {
  b <- data.frame(id = trn$ID,
                  a = trn[,i],
                  b = trn[,i+1])
  trnl <- rbind(trnl, b)
}

tstl <- data.frame()
for (i in 7:11) {
  c <- data.frame(id = tst$ID,
                  a = tst[,i],
                  b = tst[,i+1])
  tstl <- rbind(tstl, c)
}

rm(b, c, i)

save(trn, file = "train.RDA")
save(tst, file = "test.RDA")

save(trnl, file = "train_long.RDA")
save(tstl, file = "test_long.RDA")

####################################################################################################
# Calculate DTMCs - functionalized
# Detects states as per contents of both df fields

# load("train_long.RDA")
# load("test_long.RDA")

# takes as input long-format data.frame (three fields - "id", "a" (pre), and "b" (post)) formatted as above

mc_calc <- function(df) {
  
  # State-dependent frequencies
  unqs <- sort(unique(c(unique(df$a), 
                        unique(df$b))))
  
  sdf <- data.frame(st = c(unqs))
  for (j in 1:length(unqs)) {
    sdf1 <- aggregate(id ~ b, 
                      df[df$a == unqs[j], ], 
                      FUN = length)
    colnames(sdf1)[2] <- paste0(unqs[j],"_ct")
    sdf <- merge(sdf, sdf1, by.x = "st", by.y = "b", all = T)
  }
  
  rm(j, sdf1)
  
  sdf$total <- rowSums(sdf[,2:length(colnames(sdf))], na.rm = T)
  
  for (k in 1:length(unqs)) {
    sdf$a <- sdf[,1 + k] / sdf$total
    colnames(sdf)[which(colnames(sdf) == "a")] <- unqs[k]
  }
  
  sdf[is.na(sdf)] <- 0
  
  ndx <- which(colnames(sdf) == "total")+1
  sdf_pct <- sdf[,c(ndx:ncol(sdf))]
  
  # Working around the too-strict row sum requirements for package
  # Adjusts just the first group for the diff (in this case, the largest one, where it'll have the lowest comparative impact)
  # HACKETY HACK
  
  sdf_pct2 <- round(sdf_pct, 3)
  sdf_pct2$dff <- 1 - rowSums(sdf_pct2)
  sdf_pct2[, 1] <- sdf_pct2[, 1] + sdf_pct2$dff
  
  mat <- data.matrix(sdf_pct2[,-ncol(sdf_pct2)])
  rownames(mat) <- paste0("st_", unqs)
  colnames(mat) <- paste0("st_", unqs)
  
  dtmc <<- new("markovchain",
               states = paste0("st_", unqs),
               byrow = T, 
               transitionMatrix = mat,
               name = paste0(deparse(substitute(df))))
}

mc_calc(trnl)
assign("train_dtmc", dtmc)

mc_calc(tstl)
assign("test_dtmc", dtmc)

rm(dtmc)

get("train_dtmc")
get("test_dtmc")

plot(train_dtmc)
plot(test_dtmc)

save(train_dtmc, file = "train_dtmc.RDA")
save(test_dtmc, file = "test_dtmc.RDA")

####################################################################################################
# Predictions

load("train_dtmc.RDA")
load("test.RDA")

tm <- Sys.time()
sq <- rmarkovchain(n = 1000000, object = train_dtmc, t0 = "st_0")
ft <- markovchainFit(sq, "mle")

pds = 18

prdct <- function(inpt) {
  prd <<- predict(object = ft$estimate, newdata = inpt, n.ahead = pds)
  }

str <- data.frame()
for (j in 1:nrow(tst)) {
  prd2 <- data.frame(t(c(tst[j,1],
                         prdct(paste0("st_", tst[j,7])))))
  str <- rbind(str, prd2)
  }

colnames(str)[1] <- "id"

Sys.time() - tm

get("train_dtmc")
ft$estimate


####################################################################################################
# Testing a new dataset from
# https://assets.datacamp.com/course/credit-risk-modeling-in-r/loan_data_ch2.rds

download.file("https://assets.datacamp.com/course/credit-risk-modeling-in-r/loan_data_ch2.rds", destfile = "loan_data.rds")

dat <- readRDS("loan_data.rds")



















































