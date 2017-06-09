# Working with created cecl file (not available in this environment)
# starts as wide file, @ month level

library(ggplot2)
library(scales)
library(markovchain)
library(reshape)
library(igraph)
library(plyr)

a1 <- read.csv("base1.csv", stringsAsFactors = F)

a2 <- a1[,c(1:3)]
colnames(a2) <- c("cid", "pnm", "bcd")

# long to wide

a3 <- melt(a2, c(1, 2))
a <- data.frame(cast(a3, cid ~ pnm))
for (i in 2:ncol(a)) { colnames(a)[i] <- substr(colnames(a)[i], 2, 10) }

rm(a1, a2, a3)

save(a, file = "base.RDA")

####################################################################################################
# Prep for DTMC and modeling
# Recodes state values and produces test/control 
# (only re-run if need new values - otherwise start with next section)

load("base.RDA")

# Bucketing states - comment out line 84-86 to go back to original
# currently recodes 
# - current as 'current' (val = 0) 
# - 1-6 (months delinquent) as 'delinquent' (val= 1) 
# - 7 as 'charge-off' (val = 2) and 

# check values before recode
# u <- unique(ldply(apply(a[,c(2:38)], 2, unique), data.frame)[2])
# sort(u[,1], na.last = F)
# 
# for (i in 2:38) {
#   a[,i] <- ifelse(a[,i] %in% c(1, 2, 3, 4, 5, 6), 1, a[,i])
#   a[,i] <- ifelse(a[,i] == 7, 2, a[,i])
#   a[,i] <- ifelse(a[,i] == 0, 0, a[,i])
# }
# 
# rm(i)

# check values after recode
u <- unique(ldply(apply(a[,c(2:38)], 2, unique), data.frame)[2])
sort(u[,1], F, F)

unqs <- sort(u[,1], na.last = NA)
rm(u)

ad <- data.frame(st = c(unqs))
for (p in 2:38) {
  agg <- aggregate(cid ~ a[,p], a, FUN = length)
  colnames(agg) <- c("st", paste0("tm_", p-1))
  ad <- merge(ad, agg, by = "st", all = T)
}

ad_pct <- ad[,c(2:ncol(ad))]/nrow(a)
ad_pct <- apply(ad_pct, 2, percent)

print(ad_pct)

rm(p, agg, unqs, ad)

# Test/train

set.seed(521)
trn_ind <- sample(seq_len(nrow(a)), size = floor(0.7 * nrow(a)))

trn <- a[trn_ind, ]
tst <- a[-trn_ind, ]

# Normalizing for time - transferring all periods to simple before/after

trnl <- data.frame()
for (i in 2:37) {
  b <- data.frame(id = trn$cid,
                  a = trn[,i],
                  b = trn[,i+1])
  trnl <- rbind(trnl, b)
}

tstl <- data.frame()
for (i in 2:37) {
  c <- data.frame(id = tst$cid,
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
# Calculate DTMCs - functionalized for above data
# detects state input values

load("train_long.RDA")
load("test_long.RDA")

# takes as input long-format data.frame (three fields - "id", "a" (pre), and "b" (post)) formatted as above
# includes a way to get around states missing in individual months (if statement in sdf <- ...)

mc_calc <- function(df, bg, nd) {
  
  # State-dependent frequencies
  unqs <- sort(c(bg:nd))
  
  sdf <- data.frame(st = c(unqs))
  for (j in 1:length(unqs)) {
    
    if (nrow(df[df$a == unqs[j] & is.na(df$a) == F,]) > 0) {
      
      sdf1 <- aggregate(id ~ b, 
                        df[df$a == unqs[j] & is.na(df$a) == F, ], 
                        FUN = length)
    } else {
      
      sdf1 <- data.frame(b = unqs[j],
                         id = 0)
      
    }
    
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

save(mc_calc, file = "MC_CALC.RDA")

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
sq <- rmarkovchain(n = 1000000, object = train_dtmc, t0 = "st_7")
ft <- markovchainFit(sq, "mle")

pds = 6

prdct <- function(inpt) {
  prd <<- predict(object = ft$estimate, newdata = inpt, n.ahead = pds)
}

str <- data.frame()
for (j in 1:nrow(tst)) {
  prd2 <- data.frame(t(c(tst[j,1],
                         prdct(paste0("st_", tst[j,2])))))
  str <- rbind(str, prd2)
  if ((j/100)%%1 == 0) { print(paste0(percent(j/nrow(tst)))) }
}

colnames(str)[1] <- "id"

Sys.time() - tm

save(str, file = "preds.RDA")

get("train_dtmc")
ft$estimate

str$cnct <- paste0(substr(str[,2], 4, 5),
                   substr(str[,3], 4, 5),
                   substr(str[,4], 4, 5),
                   substr(str[,5], 4, 5),
                   substr(str[,6], 4, 5),
                   substr(str[,7], 4, 5))

aggregate(id ~ cnct, str, FUN = length)



plot(train_dtmc)


####################################################################################################
# Markov Stationarity
# use loop to generate long-format dataset for individual per_nums, run through mc_calc, save individual MCs

load("test.RDA")
load("MC_CALC.RDA")

npt <- tst
for (i in 2:37) {
  lng <- data.frame(id = npt$cid,
                    a = npt[,i],
                    b = npt[,i+1])
  mc_calc(lng, 0, 7)
  assign(paste0("dtmc_", i), dtmc)
}

catcher <- data.frame()
for (x in 2:37) {
  a <- data.frame(pd = x,
                  as(get(paste0("dtmc_", x)), "data.frame"))
  catcher <- rbind(catcher, a)
}

catcher[,2] <- as.numeric(substr(as.character(catcher[,2]), 4, 5))
catcher[,3] <- as.numeric(substr(as.character(catcher[,3]), 4, 5))

catcher$flg <- ifelse(catcher$t0 == catcher$t1 | catcher$t0 == (catcher$t1-1) | catcher$t0 == (catcher$t1 + 1), 1, 0)

catcher$flg2 <- ""
catcher$flg2 <- ifelse(catcher$t0 == (catcher$t1-1), paste0(catcher$t1, " A"), catcher$flg2)
catcher$flg2 <- ifelse(catcher$t0 == catcher$t1, paste0(catcher$t1, " B"), catcher$flg2)
catcher$flg2 <- ifelse(catcher$t0 == (catcher$t1+1), paste0(catcher$t1, " C"), catcher$flg2)

catcher$cnct <- paste0(catcher[,2], 
                       catcher[,3])
catcherout <- catcher[catcher$flg == 1,c(1, 7, 6, 4)]

zero <- data.frame(pd = c(2:37),
                   cnct = "0-1",
                   flg2 = "0 A",
                   prob = 0)

seven <- data.frame(pd = c(2:37),
                    cnct = "7D",
                    flg2 = "7 C",
                    prob = 0)

catcherout <- rbind(catcherout, zero, seven)

plt <- ggplot(catcherout, aes(x = pd, y = prob)) +
  geom_line() + 
  facet_wrap( ~ cnct, ncol = 3) +
  theme_bw()
plot(plt)

plt2 <- ggplot(catcherout, aes(x = pd, y = prob)) +
  geom_line() + 
  facet_wrap( ~ flg2, ncol = 3) +
  theme_bw()
plot(plt2)






































