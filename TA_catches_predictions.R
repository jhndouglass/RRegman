library(lme4)
#setwd("~/ShinyApps/CSF_web_regman")
setwd("C:/Users/jdouglass/Desktop/RRegman test")

# load CCM reduction data
DP_reducts <- read.csv("DP_reducts.csv")
TP_reducts <- read.csv("TP_reducts.csv")
N_reducts <- read.csv("N_reducts.csv")
SS_reducts <- read.csv("SS_reducts.csv")

load("amm.rdata")
load("ton.rdata")
load("op.rdata")
load("tp.rdata")
load("ss.rdata")
load("moddat.rdata")

# Make predictions

# AMM
amm.base <- moddat[, c("CATCHMENT", "propTACatchOverlap", 
                       "standardTotalCattleCalves", "upstream_avg_BFI",
                           "standardPropertyCount", "standardArable")]
amm.base <- merge(amm.base, N_reducts, by = "CATCHMENT")
n <- nrow(amm.base)

# baseline
base.out <- predict(amm, newdata = amm.base, type = "response")
amm.preds <- data.frame(CATCHMENT = amm.base$CATCHMENT, base.out)

# current
mult <- data.frame(1 -((amm.base$S1_typ) * (amm.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((amm.base$S1_typ) * (amm.base$propTACatchOverlap)))
curr <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                     "standardPropertyCount", "standardArable")] * mult
curr.out <- predict(amm, newdata = curr, type = "response")
amm.preds <- data.frame(amm.preds, curr.out)


# extrap
mult <- data.frame(1 -((amm.base$S2_typ) * (amm.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((amm.base$S2_typ) * (amm.base$propTACatchOverlap)))
extrap <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                     "standardPropertyCount", "standardArable")] * mult
extrap.out <- predict(amm, newdata = extrap, type = "response")
amm.preds <- data.frame(amm.preds, extrap.out)

# optim
mult <- data.frame(1 -((amm.base$S3a_typ) * (amm.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((amm.base$S3a_typ) * (amm.base$propTACatchOverlap)))
optim <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                       "standardPropertyCount", "standardArable")] * mult
optim.out <- predict(amm, newdata = optim, type = "response")
amm.preds <- data.frame(amm.preds, optim.out)


# max
mult <- data.frame(1 -((amm.base$S3b_typ) * (amm.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((amm.base$S3b_typ) * (amm.base$propTACatchOverlap)))
max <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                      "standardPropertyCount", "standardArable")] * mult
max.out <- predict(amm, newdata = max, type = "response")
amm.preds <- data.frame(amm.preds, max.out)

# maxbenefit
mult <- data.frame(1 -((amm.base$S4_typ) * (amm.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((amm.base$S4_typ) * (amm.base$propTACatchOverlap)))
maxben <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                    "standardPropertyCount", "standardArable")] * mult
maxben.out <- predict(amm, newdata = maxben, type = "response")
amm.preds <- data.frame(amm.preds, maxben.out)

# headroom
mult <- data.frame(rep(0, n), rep(1, n) , rep(1, n), rep(0, n))
head <- amm.base[, c("standardTotalCattleCalves", "upstream_avg_BFI",
                       "standardPropertyCount", "standardArable")] * mult
head.out <- predict(amm, newdata = head, type = "response")
amm.preds <- data.frame(amm.preds, head.out)

curr.reduct <- (1 - (amm.preds$curr.out / amm.preds$base.out))
optim.reduct <- (1 - (amm.preds$optim.out / amm.preds$base.out))
extrap.reduct <- (1 - (amm.preds$extrap.out / amm.preds$base.out))
max.reduct <- (1 - (amm.preds$max.out / amm.preds$base.out))
maxben.reduct <- (1 - (amm.preds$maxben.out / amm.preds$base.out))
head.reduct <- (1 - (amm.preds$head.out / amm.preds$base.out))
amm.preds <- data.frame(amm.preds, curr.reduct, extrap.reduct, optim.reduct,
                        max.reduct, maxben.reduct, head.reduct)

# TON
ton.base <- moddat[, c("CATCHMENT", "propTACatchOverlap", 
                       "standardArable", "upstream_avg_BFI",
                       "standardPropertyCount", "standardTotalCattleCalves")]
ton.base <- merge(ton.base, N_reducts, by = "CATCHMENT")
n <- nrow(ton.base)

# baseline
base.out <- predict(ton, newdata = ton.base, type = "response")
ton.preds <- data.frame(CATCHMENT = ton.base$CATCHMENT, base.out)

# current
mult <- data.frame(1 -((ton.base$S1_typ) * (ton.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((ton.base$S1_typ) * (ton.base$propTACatchOverlap)))
curr <- ton.base[, c("standardArable", "upstream_avg_BFI",
                     "standardPropertyCount", "standardTotalCattleCalves")] * mult
curr.out <- predict(ton, newdata = curr, type = "response")
ton.preds <- data.frame(ton.preds, curr.out)


# extrap
mult <- data.frame(1 -((ton.base$S2_typ) * (ton.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((ton.base$S2_typ) * (ton.base$propTACatchOverlap)))
extrap <- ton.base[, c("standardArable", "upstream_avg_BFI",
                       "standardPropertyCount", "standardTotalCattleCalves")] * mult
extrap.out <- predict(ton, newdata = extrap, type = "response")
ton.preds <- data.frame(ton.preds, extrap.out)

# optim
mult <- data.frame(1 -((ton.base$S3a_typ) * (ton.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((ton.base$S3a_typ) * (ton.base$propTACatchOverlap)))
optim <- ton.base[, c("standardArable", "upstream_avg_BFI",
                      "standardPropertyCount", "standardTotalCattleCalves")] * mult
optim.out <- predict(ton, newdata = optim, type = "response")
ton.preds <- data.frame(ton.preds, optim.out)


# max
mult <- data.frame(1 -((ton.base$S3b_typ) * (ton.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((ton.base$S3b_typ) * (ton.base$propTACatchOverlap)))
max <- ton.base[, c("standardArable", "upstream_avg_BFI",
                    "standardPropertyCount", "standardTotalCattleCalves")] * mult
max.out <- predict(ton, newdata = max, type = "response")
ton.preds <- data.frame(ton.preds, max.out)

# maxbenefit
mult <- data.frame(1 -((ton.base$S4_typ) * (ton.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((ton.base$S4_typ) * (ton.base$propTACatchOverlap)))
maxben <- ton.base[, c("standardArable", "upstream_avg_BFI",
                       "standardPropertyCount", "standardTotalCattleCalves")] * mult
maxben.out <- predict(ton, newdata = maxben, type = "response")
ton.preds <- data.frame(ton.preds, maxben.out)

# headroom
mult <- data.frame(rep(0, n), rep(1, n) , rep(1, n), rep(0, n))
head <- ton.base[, c("standardArable", "upstream_avg_BFI",
                     "standardPropertyCount", "standardTotalCattleCalves")] * mult
head.out <- predict(ton, newdata = head, type = "response")
ton.preds <- data.frame(ton.preds, head.out)

curr.reduct <- (1 - (ton.preds$curr.out / ton.preds$base.out))
optim.reduct <- (1 - (ton.preds$optim.out / ton.preds$base.out))
extrap.reduct <- (1 - (ton.preds$extrap.out / ton.preds$base.out))
max.reduct <- (1 - (ton.preds$max.out / ton.preds$base.out))
maxben.reduct <- (1 - (ton.preds$maxben.out / ton.preds$base.out))
head.reduct <- (1 - (ton.preds$head.out / ton.preds$base.out))
ton.preds <- data.frame(ton.preds, curr.reduct, extrap.reduct, optim.reduct,
                           max.reduct, maxben.reduct, head.reduct)

# OP
op.base <- moddat[, c("CATCHMENT", "propTACatchOverlap", "standardArable", 
                      "standardTotalPigs", "upstream_avg_BFI", 
                      "standardUrban", "standardTotalCattleCalves")]
op.base <- merge(op.base, DP_reducts, by = "CATCHMENT")
n <- nrow(op.base)

# baseline
base.out <- predict(op, newdata = op.base, type = "response")
op.preds <- data.frame(CATCHMENT = op.base$CATCHMENT, base.out)

# current
mult <- data.frame(1 -((op.base$S1_typ) * (op.base$propTACatchOverlap)),
                   1 -((op.base$S1_typ) * (op.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((op.base$S1_typ) * (op.base$propTACatchOverlap)))
curr <- op.base[, c("standardArable", 
                    "standardTotalPigs", "upstream_avg_BFI", 
                    "standardUrban", "standardTotalCattleCalves")] * mult
curr.out <- predict(op, newdata = curr, type = "response")
op.preds <- data.frame(op.preds, curr.out)


# extrap
mult <- data.frame(1 -((op.base$S2_typ) * (op.base$propTACatchOverlap)),
                   1 -((op.base$S2_typ) * (op.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((op.base$S2_typ) * (op.base$propTACatchOverlap)))
extrap <- op.base[, c("standardArable", 
                      "standardTotalPigs", "upstream_avg_BFI", 
                      "standardUrban", "standardTotalCattleCalves")] * mult
extrap.out <- predict(op, newdata = extrap, type = "response")
op.preds <- data.frame(op.preds, extrap.out)

# optim
mult <- data.frame(1 -((op.base$S3a_typ) * (op.base$propTACatchOverlap)),
                   1 -((op.base$S1_typ) * (op.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((op.base$S3a_typ) * (op.base$propTACatchOverlap)))
optim <- op.base[, c("standardArable", 
                     "standardTotalPigs", "upstream_avg_BFI", 
                     "standardUrban", "standardTotalCattleCalves")] * mult
optim.out <- predict(op, newdata = optim, type = "response")
op.preds <- data.frame(op.preds, optim.out)


# max
mult <- data.frame(1 -((op.base$S3b_typ) * (op.base$propTACatchOverlap)),
                   1 -((op.base$S3b_typ) * (op.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((op.base$S3b_typ) * (op.base$propTACatchOverlap)))
max <- op.base[, c("standardArable", 
                   "standardTotalPigs", "upstream_avg_BFI", 
                   "standardUrban", "standardTotalCattleCalves")] * mult
max.out <- predict(op, newdata = max, type = "response")
op.preds <- data.frame(op.preds, max.out)

# maxbenefit
mult <- data.frame(1 -((op.base$S4_typ) * (op.base$propTACatchOverlap)),
                   1 -((op.base$S4_typ) * (op.base$propTACatchOverlap)),
                   rep(1, n) , rep(1, n), 
                   1 -((op.base$S4_typ) * (op.base$propTACatchOverlap)))
maxben <- op.base[, c("standardArable", 
                      "standardTotalPigs", "upstream_avg_BFI", 
                      "standardUrban", "standardTotalCattleCalves")] * mult
maxben.out <- predict(op, newdata = maxben, type = "response")
op.preds <- data.frame(op.preds, maxben.out)

# headroom
mult <- data.frame(rep(0, n), rep(0, n), rep(1, n) , rep(1, n), rep(0, n))
head <- op.base[, c("standardArable", 
                    "standardTotalPigs", "upstream_avg_BFI", 
                    "standardUrban", "standardTotalCattleCalves")] * mult
head.out <- predict(op, newdata = head, type = "response")
op.preds <- data.frame(op.preds, head.out)

curr.reduct <- (1 - (op.preds$curr.out / op.preds$base.out))
optim.reduct <- (1 - (op.preds$optim.out / op.preds$base.out))
extrap.reduct <- (1 - (op.preds$extrap.out / op.preds$base.out))
max.reduct <- (1 - (op.preds$max.out / op.preds$base.out))
maxben.reduct <- (1 - (op.preds$maxben.out / op.preds$base.out))
head.reduct <- (1 - (op.preds$head.out / op.preds$base.out))
op.preds <- data.frame(op.preds, curr.reduct, extrap.reduct, optim.reduct,
                       max.reduct, maxben.reduct, head.reduct)


# TP
tp.base <- moddat[, c("CATCHMENT", "propTACatchOverlap", "standardUrban", 
                      "standardArable", "standardTotalCattleCalves",
                      "upstream_avg_BFI")]
tp.base <- merge(tp.base, TP_reducts, by = "CATCHMENT")
n <- nrow(tp.base)

# baseline
base.out <- predict(tp, newdata = tp.base, type = "response")
tp.preds <- data.frame(CATCHMENT = tp.base$CATCHMENT, base.out)

# current
mult <- data.frame(rep(1, n), 
                   1 -((tp.base$S1_typ) * (tp.base$propTACatchOverlap)),
                   1 -((tp.base$S1_typ) * (tp.base$propTACatchOverlap)),
                   rep(1, n))
curr <- tp.base[, c("standardUrban", 
                    "standardArable", "standardTotalCattleCalves",
                    "upstream_avg_BFI")] * mult
curr.out <- predict(tp, newdata = curr, type = "response")
tp.preds <- data.frame(tp.preds, curr.out)


# extrap
mult <- data.frame(rep(1, n), 
                   1 -((tp.base$S2_typ) * (tp.base$propTACatchOverlap)),
                   1 -((tp.base$S2_typ) * (tp.base$propTACatchOverlap)),
                   rep(1, n))
extrap <- tp.base[, c("standardUrban", 
                      "standardArable", "standardTotalCattleCalves",
                      "upstream_avg_BFI")] * mult
extrap.out <- predict(tp, newdata = extrap, type = "response")
tp.preds <- data.frame(tp.preds, extrap.out)

# optim
mult <- data.frame(rep(1, n), 
                   1 -((tp.base$S3a_typ) * (tp.base$propTACatchOverlap)),
                   1 -((tp.base$S3a_typ) * (tp.base$propTACatchOverlap)),
                   rep(1, n))
optim <- tp.base[, c("standardUrban", 
                     "standardArable", "standardTotalCattleCalves",
                     "upstream_avg_BFI")] * mult
optim.out <- predict(tp, newdata = optim, type = "response")
tp.preds <- data.frame(tp.preds, optim.out)


# max
mult <- data.frame(rep(1, n), 
                   1 -((tp.base$S3b_typ) * (tp.base$propTACatchOverlap)),
                   1 -((tp.base$S3b_typ) * (tp.base$propTACatchOverlap)),
                   rep(1, n))
max <- tp.base[, c("standardUrban", 
                   "standardArable", "standardTotalCattleCalves",
                   "upstream_avg_BFI")] * mult
max.out <- predict(tp, newdata = max, type = "response")
tp.preds <- data.frame(tp.preds, max.out)

# maxbenefit
mult <- data.frame(rep(1, n), 
                   1 -((tp.base$S4_typ) * (tp.base$propTACatchOverlap)),
                   1 -((tp.base$S4_typ) * (tp.base$propTACatchOverlap)),
                   rep(1, n))
maxben <- tp.base[, c("standardUrban", 
                      "standardArable", "standardTotalCattleCalves",
                      "upstream_avg_BFI")] * mult
maxben.out <- predict(tp, newdata = maxben, type = "response")
tp.preds <- data.frame(tp.preds, maxben.out)

# headroom
mult <- data.frame(rep(1, n), rep(0, n), rep(0, n) , rep(1, n))
head <- tp.base[, c("standardUrban", 
                    "standardArable", "standardTotalCattleCalves",
                    "upstream_avg_BFI")] * mult
head.out <- predict(tp, newdata = head, type = "response")
tp.preds <- data.frame(tp.preds, head.out)

curr.reduct <- (1 - (tp.preds$curr.out / tp.preds$base.out))
optim.reduct <- (1 - (tp.preds$optim.out / tp.preds$base.out))
extrap.reduct <- (1 - (tp.preds$extrap.out / tp.preds$base.out))
max.reduct <- (1 - (tp.preds$max.out / tp.preds$base.out))
maxben.reduct <- (1 - (tp.preds$maxben.out / tp.preds$base.out))
head.reduct <- (1 - (tp.preds$head.out / tp.preds$base.out))
tp.preds <- data.frame(tp.preds, curr.reduct, extrap.reduct, optim.reduct,
                       max.reduct, maxben.reduct, head.reduct)

# SS
ss.base <- moddat[, c("CATCHMENT", "propTACatchOverlap", "Rainfall", 
                      "standardGrass", "standardArable",
                      "upstream_avg_BFI", 
                      "standardMinorRoadLength")]
ss.base <- merge(ss.base, SS_reducts, by = "CATCHMENT")
n <- nrow(ss.base)

# baseline
base.out <- predict(ss, newdata = ss.base, type = "response")
ss.preds <- data.frame(CATCHMENT = ss.base$CATCHMENT, base.out)

# current
mult <- data.frame(rep(1, n), 
                   1 -((ss.base$S1_typ) * (ss.base$propTACatchOverlap)),
                   1 -((ss.base$S1_typ) * (ss.base$propTACatchOverlap)),
                   rep(1, n), rep(1, n))
curr <- ss.base[, c("Rainfall", 
                    "standardGrass", "standardArable",
                    "upstream_avg_BFI", 
                    "standardMinorRoadLength")] * mult
curr.out <- predict(ss, newdata = curr, type = "response")
ss.preds <- data.frame(ss.preds, curr.out)


# extrap
mult <- data.frame(rep(1, n), 
                   1 -((ss.base$S2_typ) * (ss.base$propTACatchOverlap)),
                   1 -((ss.base$S2_typ) * (ss.base$propTACatchOverlap)),
                   rep(1, n), rep(1, n))
extrap <- ss.base[, c("Rainfall", 
                      "standardGrass", "standardArable",
                      "upstream_avg_BFI", 
                      "standardMinorRoadLength")] * mult
extrap.out <- predict(ss, newdata = extrap, type = "response")
ss.preds <- data.frame(ss.preds, extrap.out)

# optim
mult <- data.frame(rep(1, n), 
                   1 -((ss.base$S3a_typ) * (ss.base$propTACatchOverlap)),
                   1 -((ss.base$S3a_typ) * (ss.base$propTACatchOverlap)),
                   rep(1, n), rep(1, n))
optim <- ss.base[, c("Rainfall", 
                     "standardGrass", "standardArable",
                     "upstream_avg_BFI", 
                     "standardMinorRoadLength")] * mult
optim.out <- predict(ss, newdata = optim, type = "response")
ss.preds <- data.frame(ss.preds, optim.out)


# max
mult <- data.frame(rep(1, n), 
                   1 -((ss.base$S3b_typ) * (ss.base$propTACatchOverlap)),
                   1 -((ss.base$S3b_typ) * (ss.base$propTACatchOverlap)),
                   rep(1, n), rep(1, n))
max <- ss.base[, c("Rainfall", 
                   "standardGrass", "standardArable",
                   "upstream_avg_BFI", 
                   "standardMinorRoadLength")] * mult
max.out <- predict(ss, newdata = max, type = "response")
ss.preds <- data.frame(ss.preds, max.out)

# maxbenefit
mult <- data.frame(rep(1, n), 
                   1 -((ss.base$S4_typ) * (ss.base$propTACatchOverlap)),
                   1 -((ss.base$S4_typ) * (ss.base$propTACatchOverlap)),
                   rep(1, n), rep(1, n))
maxben <- ss.base[, c("Rainfall", 
                      "standardGrass", "standardArable",
                      "upstream_avg_BFI", 
                      "standardMinorRoadLength")] * mult
maxben.out <- predict(ss, newdata = maxben, type = "response")
ss.preds <- data.frame(ss.preds, maxben.out)

# headroom
mult <- data.frame(rep(1, n), rep(0, n), rep(0, n) , rep(1, n), 
                   rep(1, n))
head <- ss.base[, c("Rainfall", 
                    "standardGrass", "standardArable",
                    "upstream_avg_BFI", 
                    "standardMinorRoadLength")] * mult
head.out <- predict(ss, newdata = head, type = "response")
ss.preds <- data.frame(ss.preds, head.out)

curr.reduct <- (1 - (ss.preds$curr.out / ss.preds$base.out))
optim.reduct <- (1 - (ss.preds$optim.out / ss.preds$base.out))
extrap.reduct <- (1 - (ss.preds$extrap.out / ss.preds$base.out))
max.reduct <- (1 - (ss.preds$max.out / ss.preds$base.out))
maxben.reduct <- (1 - (ss.preds$maxben.out / ss.preds$base.out))
head.reduct <- (1 - (ss.preds$head.out / ss.preds$base.out))
ss.preds <- data.frame(ss.preds, curr.reduct, extrap.reduct, optim.reduct,
                        max.reduct, maxben.reduct, head.reduct)

