#########################################################################
##
## This script runs the cross-sectional regressions of arbitrage
## return on the media measure.
## 
## Idea: For stock deals, use EXRATIO if it exists. Otherwise use
## something like mean(COLRATIOH, COLRATIOL), if it exists.
##
## There are several ways to consider exchange ratios. The first is
## the most straightforward way, and it simply uses the exchange ratio
## whenever it is present. The second way is to only look at deals
## that have the stock swap dummy set.
##
#########################################################################

library("lmtest")
library("sandwich")

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "110_event_time_regression")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))

## Load classified Factiva data.
load(fname.factiva)
f <- factiva; rm(factiva) # Use easy-to-type name.
## For backwards compatibility when we tried to separately predict the
## 'positive' and 'negative' media probabilities.
if ("classificationC" %in% colnames(f) && !"classification" %in% colnames(f))
  f$classification <- f$classificationC

## Load the log-return time series.
load(file.returns.00.00)
load(file.returns.01.00)

## Load data.frame containing deals and calculate additional variables
## (based on existing variables).
load(file.SDC.CCM)
sdc <- takeoverData; rm(takeoverData) # Use easy-to-type name.
sdc <- calc.variables(sdc) # Add additional variables.
sdc$TDARet_real <- annc.d.ret(Tret.00.00)
sdc$ADARet_real <- annc.d.ret(Aret.00.00)
stopifnot(length(unique(Tret.00.00$dealID)) == nrow(sdc), length(unique(Tret.01.00$dealID)) == nrow(sdc))

## Load market returns and Fama-French portfolio returns.
load(file.WRDS.Fama.French.Factors)
FF <- WRDS.Fama.French.factors; rm(WRDS.Fama.French.factors)
## Convert all (effective) returns to logreturns.
FF$mktrf_log <- log(1 + FF$mktrf)
FF$smb_log <- log(1 + FF$smb)
FF$hml_log <- log(1 + FF$hml)
FF$rf_log <- log(1 + FF$rf)
FF$umd_log <- log(1 + FF$umd) 



## ## Restrict media only to the largest article sources, e.g. 'Dow Jones
## ## News Service' or 'Reuters News'.
## if (FALSE) {
##   ## Extract largest providers of press articles.
##   largest <- attr(summary(as.factor(f$articleSource))[1:5], "names")
##   f <- f[which(f$articleSource %in% largest), ]
## }
## ## Rank article sources from JMP.
## if (FALSE) {
##   load("~/Documents/Research/Job_Market_Paper_2010/Empirical_Part/Data_2009-09-24_with_intermediate_corpora/big_fat_corpus_transformed_horowitz_IDs_fixed.RData")
##   cp <- big_fat_cp; rm(big_fat_cp)
##   orig <- as.factor(sapply(cp, function(x) attr(x, "Origin")))
##   summary(orig)
## }



## ## Restric sample to deals scraped by a given RA.
##   df.backup <- sdc
##   ## sdc <- df.backup
##   pos.deals.scraped <- which(work.div$RAName == "Gao")
##   ## pos.deals.scraped <- which(work.div$RAName == "Ma Ni")
##   ## pos.deals.scraped <- which(work.div$RAName == "Shen Cong")
##   ## pos.deals.scraped <- which(work.div$RAName == "Tian Ziyi")
##   dealIDs.scraped <- work.div[pos.deals.scraped, "dealID"]
##   pos.deals.scraped <- which(sdc$dealID %in% dealIDs.scraped)
##   sdc <- sdc[pos.deals.scraped, ]
## ## Restrict sample to largest deals, for comparison with previous
## ## paper.
##   df.backup <- sdc
##   ## sdc <- df.backup
##   no.lgst.dls <- 348
##   if (nrow(sdc) < no.lgst.dls)
##     stop("There are not enough deals in the data.")
##   perm <- order(sdc$VAL) # Permutation for ordering.
##   perm <- perm[(length(perm)-no.lgst.dls+1):length(perm)] # Extract positions of largest deals.
##   pos <- sort(perm) # Don't mess up the given ordering of 'df', just extract the largest deals.
##   sdc <- sdc[pos, ]



## ## Visually comparing ATQ-LTQ and CEQQ. It turns out that the two are
## ## exactly the same except for a few values.
## x <- cbind(sdc[, "Target.ATQ.CCM"] - sdc[, "Target.LTQ.CCM"],
##            sdc[, "Target.CEQQ.CCM"])
## print(x[sample(nrow(x), 20), ]); rm(x)
## all.equal(sdc[, "Target.ATQ.CCM"] - sdc[, "Target.LTQ.CCM"],
##           sdc[, "Target.CEQQ.CCM"])



## Consider top newspapers and top newswires.
f$articleSource <- as.factor(f$articleSource)
sort(summary(f$articleSource), decreasing = TRUE)[1:40]
## Top newswire.
f.top.newsw <- c("Dow Jones News Service")
f.pos.top.newsw <- which(f$articleSource %in% f.top.newsw)
length(f.pos.top.newsw)
## Top newswires.
f.top.newsws <- c("Dow Jones News Service",
                 "Reuters News",
                 "Associated Press Newswires",
                 "Reuters Significant Developments",
                 "Business Wire")
f.pos.top.newsws <- which(f$articleSource %in% f.top.newsws)
length(f.pos.top.newsws)
## Top newspapers.
f.top.newsp <- c("The Wall Street Journal",
                 "Financial Times", "Financial Times (FT.Com)",
                 "The New York Times", "The New York Times Abstracts",  "New York Times Abstracts",
                 "The Globe and Mail",
                 "eWEEK")
f.pos.top.newsp <- which(f$articleSource %in% f.top.newsp)
length(f.pos.top.newsp)
## Top domestic newspapers.
f.top.newsp.d <- c("The Wall Street Journal",
                 "The New York Times", "The New York Times Abstracts", "New York Times Abstracts",
                 "The Washington Post",
                 "The San Francisco Chronicle",
                 "Investor's Business Daily")
f.pos.top.newsp.d <- which(f$articleSource %in% f.top.newsp.d)
length(f.pos.top.newsp.d)




## Cutoff date for media measure and returns. E.g. days=1 means the
## media measure includes DA+1 and the returns are calculated starting
## at DA+2.
days <- 0

## For each deal, calculate cumulative stock returns of various sorts.
sdc$Tret <- cumret(Tret.00.00, days.omit.beg = days+1, DAs = sdc$DA)
sdc$Tret <- cumret(Tret.00.00, # Excess returns over market.
                   FF,
                   xs = "mktrf_log",
                   days.omit.beg = days+1,
                   days.include.after = NULL,
                   DAs = sdc$DA)
sdc$Tret <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                   FF,
                   xs = "rf",
                   days.omit.beg = days+1,
                   days.include.after = NULL,
                   DAs = sdc$DA)

## Calculate media measure.
sdc$media <- media(f[f.pos.top.newsw, ], sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media <- media(f[f.pos.top.newsws, ], sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media <- media(f[f.pos.top.newsp, ], sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media <- media(f[f.pos.top.newsp.d, ], sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media <- media(f[unique(union(f.pos.top.newsp, f.pos.top.newsp.d)), ],
                   sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days, mn = FALSE)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)

## Summary statistics.
sdc$Tret.00.00 <- cumret(Tret.00.00)
summary( sdc[, c("Tret.00.00", "Tret", "STAT", "media")])



#########################################################################
## Dependent variable: Target return
#########################################################################

sumry <- function(formula, data) { # Helper function to pretty-print lm.
  m.fitted <- lm(formula, data) # Fitted model.
  print(round(summary(m.fitted)$coefficients, 2))
  cat("Observations:", nrow(m.fitted$model), "\n")
}

## This is the basic formula
fmla <- as.formula(Tret ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + premium
                   + TDARet_real+ ADARet_real)
sumry(fmla, sdc)
coef.W(fmla, sdc)

## Variations of the RHS.
summary( lm(Tret ~ media, data = sdc))$coefficients
summary( lm(Tret ~ media + TBM + ABM, data = sdc) )$coefficients
summary( lm(update(fmla, . ~ . - UNSOLICITED - SWAP), sdc) )$coefficients

summary( lm(update(fmla, . ~ . - media - UNSOLICITED + media : UNSOLICITED), sdc) )$coefficients
coef.W(update(fmla, . ~ . - media - UNSOLICITED + media : UNSOLICITED), sdc)

summary( lm(update(fmla, . ~ . - media + media : UNSOLICITED), sdc) )$coefficients
coef.W(update(fmla, . ~ . - media + media : UNSOLICITED), sdc)

summary( lm(update(fmla, . ~ . - media - SWAP + media : SWAP), sdc) )$coefficients
coef.W(update(fmla, . ~ . - media - SWAP + media : SWAP), sdc)

summary( lm(update(fmla, . ~ . - media + media : SWAP), sdc) )$coefficients
coef.W(update(fmla, . ~ . - media + media : SWAP), sdc)


## Annualized cumulative returns.
coef.W(update(fmla, I(Tret/DAYS) ~ .), sdc)
sumry(update(fmla, I(Tret/DAYS) ~ .), sdc)




#########################################################################
## Dependent variable: STAT
#########################################################################

fmla <- as.formula(STAT ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + SWAP + UNSOLICITED + logDAYS
                   + premium
                   + TDARet_real+ ADARet_real)
round(summary( glm.fitted <- glm(fmla, data = sdc, family = binomial()) )$coefficients, 2)
nrow(glm.fitted$model) # Number of observations.
summary( glm(fmla, data = sdc, family = binomial(link = "probit")) )


summary( glm(update(fmla, . ~ . - media + media : UNSOLICITED),
             data = sdc, family = binomial()) )
summary( glm(update(fmla, . ~ . - media + media : UNSOLICITED),
             data = sdc, family = binomial(link = "probit")) )

summary( glm(update(fmla, . ~ . - media + media : SWAP),
             data = sdc, family = binomial()) )
summary( glm(update(fmla, . ~ . - media + media : SWAP),
             data = sdc, family = binomial(link = "probit")) )


#########################################################################
## Dependent variable: media
#########################################################################

## Calculate media measure.
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], Inf)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")])

fmla <- as.formula(media ~ premium
                   + TCashE_log+ ACashE_log
                   + TBM_log + ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + SWAP + UNSOLICITED + logDAYS
                   + premium
                   + TDARet_real+ ADARet_real)
coef.W(fmla, sdc)
round(summary(lm(fmla, sdc))$coefficients, 2)














#########################################################################
## This part varies the days after the announcement day that are used
## to calculate the arbitrage returns.
#########################################################################

fmla <- as.formula(Tret ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + premium
                   + TDARet_real+ ADARet_real)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = 0)

## Considering excess returns over market.
slope.xs <- pval.xs <- pval.W.xs <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     FF,
                     xs = "mktrf_log",
                     days.omit.beg = 1,
                     days.include.after = i,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  slope.xs <- c(slope.xs, x["media", "Estimate"])
  pval.xs <- c(pval.xs, x["media", "Pr(>|t|)"])
  pval.W.xs <- c(pval.W.xs, y["media", "Pr(>|t|)"])
}
plot(slope.xs)
plot(pval.xs)
plot(pval.W.xs)
plot(pval.xs, slope.xs)

## Considering excess returns over risk-free rate.
slope.xs.rf <- pval.xs.rf <- pval.W.xs.rf <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     FF,
                     xs = "rf",
                     days.omit.beg = 1,
                     days.include.after = i,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  slope.xs.rf <- c(slope.xs.rf, x["media", "Estimate"])
  pval.xs.rf <- c(pval.xs.rf, x["media", "Pr(>|t|)"])
  pval.W.xs.rf <- c(pval.W.xs.rf, y["media", "Pr(>|t|)"])
}
plot(slope.xs.rf)
plot(pval.xs.rf)
plot(pval.W.xs.rf)
plot(pval.xs.rf, slope.xs.rf)

## Considering raw returns.
slope <- pval <- pval.W <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     days.omit.beg = 1,
                     days.include.after = i,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  slope <- c(slope, x["media", "Estimate"])
  pval <- c(pval, x["media", "Pr(>|t|)"])
  pval.W <- c(pval.W, y["media", "Pr(>|t|)"])
}
plot(slope, xlab = "days included after DA", ylab = "media coefficient")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_include_days_after_DA_-_slope.pdf"))
plot(pval, xlab = "days included after DA", ylab = "media p-value")
plot(pval.W, xlab = "days included after DA", ylab = "media p-value")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_include_days_after_DA_-_pval.pdf"))
plot(pval, slope)






#########################################################################
## This part varies the days omitted after the announcement to
## construct cumulative returns. Summary: slope decreases slightly as
## more days are omitted. The p-values first seem to go down very
## slightly (min is when *15* days are omitted) and then increase
## afterwards.
#########################################################################

## Considering excess returns over market.
om.slope.xs <- om.pval.xs <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     FF,
                     xs = "mkt",
                     days.omit.beg = i,
                     days.include.after = NULL,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))
  om.slope.xs <- c(om.slope.xs, x$coefficients["media", "Estimate"])
  om.pval.xs <- c(om.pval.xs, x$coefficients["media", "Pr(>|t|)"])
}
plot(om.slope.xs)
plot(om.pval.xs)
plot(om.pval.xs, om.slope.xs)
which.min(om.pval.xs) # 15

## Considering excess returns over risk-free rate.
om.slope.xs.rf <- om.pval.xs.rf <- om.pval.W.xs.rf <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     FF,
                     xs = "rf",
                     days.omit.beg = i,
                     days.include.after = NULL,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  om.slope.xs.rf <- c(om.slope.xs.rf, x["media", "Estimate"])
  om.pval.xs.rf <- c(om.pval.xs.rf, x["media", "Pr(>|t|)"])
  om.pval.W.xs.rf <- c(om.pval.W.xs.rf, y["media", "Pr(>|t|)"])
}
plot(om.slope.xs.rf)
plot(om.pval.xs.rf)
plot(om.pval.W.xs.rf)
plot(om.pval.xs.rf, om.slope.xs.rf)
which.min(om.pval.xs.rf) # 15

## Considering raw returns.
om.slope <- om.pval <- om.pval.W <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$Tret <- cumret(Tret.00.00,
                     days.omit.beg = i,
                     days.include.after = NULL,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  om.slope <- c(om.slope, x["media", "Estimate"])
  om.pval <- c(om.pval, x["media", "Pr(>|t|)"])
  om.pval.W <- c(om.pval.W, y["media", "Pr(>|t|)"])
}
plot(om.slope, ylim = c(0.05, .2), xlab = "days omitted after DA", ylab = "media coefficient")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_omit_days_after_DA_-_slope.pdf"))
plot(om.pval, ylim = c(0, .36), xlab = "days omitted after DA", ylab = "media p-value")
plot(om.pval.W, ylim = c(0, .36), xlab = "days omitted after DA", ylab = "media p-value")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_omit_days_after_DA_-_pval.pdf"))
plot(om.pval, om.slope, xlab = "media p-value", ylab = "media coefficient")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_omit_days_after_DA_-_pval_vs_slope.pdf"))
which.min(om.pval) # 15







#########################################################################
## Here we omit more days after DA to construct returns, and at the
## same time we use more media information. Result: Overall, the
## results are a bit fuzzy. The slope seems to initially increase, and
## then stops to increase. The p-values initially jump upwards and
## then come back down. For larger i's the behavior of the p-values
## seems erratic. In sum, the results suggest that *maybe* it pays off
## for arbitrageurs to wait for about 20 days before opening
## positions. This is consistent with what Warren Buffett does.
#########################################################################


## Considering raw returns.
dom.slope <- dom.pval <- dom.pval.W <- NULL
for (i in 1:120) {
  cat("i =", i, "\n")
  sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], i - 1)
  sdc$Tret <- cumret(Tret.00.00,
                     days.omit.beg = i,
                     days.include.after = NULL,
                     DAs = sdc$DA)
  x <- summary(lm(fmla, sdc))$coefficients
  y <- coeftest(lm(fmla, sdc), vcov = function(...) vcovHC(..., type = "HC0"))
  stopifnot(isTRUE(all.equal(x["media", "Estimate"], y["media", "Estimate"])))
  dom.slope <- c(dom.slope, x["media", "Estimate"])
  dom.pval <- c(dom.pval, x["media", "Pr(>|t|)"])
  dom.pval.W <- c(dom.pval.W, y["media", "Pr(>|t|)"])
}
plot(dom.slope, ylim = c(0.05, .2), xlab = "k", ylab = "media coefficient")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_omit_days_after_DA_inc_media_-_slope.pdf"))
plot(dom.pval, ylim = c(0, .36), xlab = "k", ylab = "media p-value")
plot(dom.pval.W, ylim = c(0, .36), xlab = "k", ylab = "media p-value")
dev.copy2pdf(file = file.path(dir.code, "fig_returnsRaw_omit_days_after_DA_inc_media_-_pval.pdf"))
plot(dom.pval, dom.slope)





#########################################################################
## Omitting more days before DA. Result: Currently not enough data, we
## just get NAs for the p-values.
#########################################################################
sdc$Tret <- cumret(Tret.00.00,
                   FF,
                   xs = "rf",
                   days.omit.beg = 1,
                   days.include.after = NULL,
                   DAs = sdc$DA)
slope.media <- pval.media <- NULL
for (ds.om in 0:6) {
  sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], -ds.om)
  x <- summary(lm(fmla, sdc))
  slope.media <- c(slope.media, x$coefficients["media", "Estimate"])
  pval.media <- c(pval.media, x$coefficients["media", "Pr(>|t|)"])
}
plot(slope.media)
plot(pval.media)
plot(pval.media, slope.media)







#########################################################################
## Investigate the exchange ratio.
#########################################################################

i = which(!is.na(sdc$EXRATIO))[8]
statement <- paste("SELECT * FROM ",
                   table.name.CRSP,
                   " WHERE PERMNO='", sdc[i, "TPERMNO"],
                   "' AND DATE>='", as.numeric(sdc$DA[i]),
                   "' AND DATE<='", as.numeric(sdc$DRES[i]),
                   "';", sep = "")
Ta <- dbGetQuery(con.db, statement)
statement <- paste("SELECT * FROM ",
                   table.name.CRSP,
                   " WHERE PERMNO='", sdc[i, "APERMNO"],
                   "' AND DATE>='", as.numeric(sdc$DA[i]),
                   "' AND DATE<='", as.numeric(sdc$DRES[i]),
                   "';", sep = "")
Ac <- dbGetQuery(con.db, statement)

summary(Ta$PRC)
summary(Ac$PRC)
sdc$EXRATIO[i]

Ta$PRC[1]
Ta$PRC[max(which(!is.na(Ta$PRC)))]
sdc$PR[i]
Ac$PRC[1] * sdc$EXRATIO[i]
Ac$PRC[max(which(!is.na(Ac$PRC)))] * sdc$EXRATIO[i]











#########################################################################
## Plotting simple linear regression.
#########################################################################

days <- 0
sdc$Tret <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                   FF,
                   xs = "rf",
                   days.omit.beg = days + 1,
                   days.include.after = NULL,
                   DAs = sdc$DA)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)


data.subset <- sdc
fmla <- Tret ~ media
m.fitted <- lm(fmla, data.subset)
plot(fmla, data = data.subset, ylim = c(-.6, .6), xlab = "media", ylab = "excess return")
abline(m.fitted)
title(paste("All Deals: Slope =", round(coef(m.fitted)["media"], 2)))
dev.copy2pdf(file = file.path(dir.code, "fig_regression.pdf"))


data.subset <- sdc[which(sdc$UNSOLICITED == "Yes"), ]
fmla <- Tret ~ media
m.fitted <- lm(fmla, data.subset)
plot(fmla, data = data.subset, ylim = c(-.6, .6), xlab = "media", ylab = "excess return")
abline(m.fitted)
title(paste("Unsolicited Deals: Slope =", round(coef(m.fitted)["media"], 2)))
dev.copy2pdf(file = file.path(dir.code, "fig_regression_unsolicited.pdf"))






#########################################################################
## Trading strategy conditional on media threshold level.
#########################################################################



grid.steps <- seq(0, 1, 0.01)
days <- 0
sdc$Tret <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                   FF,
                   xs = "rf",
                   days.omit.beg = days + 1,
                   days.include.after = NULL,
                   DAs = sdc$DA)
sdc$media <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)


ret.ann <- function(data, col.ret = "Tret") {
  ## This function calculates the annualized returns.
  stopifnot(class(data) == "data.frame", length(col.ret) == 1)
  ret <- rep(NA_real_, nrow(data))
  for (i in which(!is.na(data[, col.ret]))) {
    denum <- as.numeric(sdc[i, "DRES"] - sdc[i, "DA"]) * 5/7
    if (is.finite(denum) & denum != 0) {
      ret[i] <- data[i, col.ret] * 252 / denum
    }
  }
  return(ret)
}

calc.ann.ret <- function(sdc, sb.set, grid.steps, col.ret = "Tret") {
  ## This function calculates the return above a media threshold given
  ## in 'grid.steps'.
  stopifnot(length(sb.set) > 0)
  ret.t <- rep(NA_real_, length(grid.steps))
  for (i in seq_along(grid.steps)) {
    pos.high <- which(sdc[, "media"] >= grid.steps[i])
    pos <- intersect(sb.set, pos.high)
    if (length(pos) > 0)
      ret.t[i] <- mean(ret.ann(sdc[pos, ], col.ret), na.rm = TRUE)
  }
  return(ret.t)
}

sb.set <- which(is.finite(sdc[, "media"])); titl <- "All Deals"
sb.set <- which(sdc[, "UNSOLICITED"] == "Yes" & is.finite(sdc[, "media"])); titl <- "Unsolicited Deals"

ret.t <- calc.ann.ret(sdc, sb.set, grid.steps, "Tret")

plot(grid.steps, ret.t,
     xlab = "Media Threshold",
     ylab = "Return",
     ylim = c(-0.3, 1.1))
abline(ret.t[1], 0) # Add benchmark return
title(titl)
if (titl == "All Deals") {
  dev.copy2pdf(file = file.path(dir.code, "fig_trading.pdf"))
} else {
  dev.copy2pdf(file = file.path(dir.code, "fig_trading_unsolicited.pdf"))
}

max(ret.t, na.rm = TRUE)
grid.steps[which.max(ret.t)]


