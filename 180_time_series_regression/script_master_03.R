#####################################################################
# This script regress arbitrge return against average market return
# and media content and coverage.
#####################################################################

#dir.base <- file.path("~/Documents/merger")
#dir.code <- file.path(dir.base, 'Code', '180_time_series_regression')
dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "180_time_series_regression")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Load data.frame 'Tret.01.00' and 'Aret.01.00'.
load(file.returns.01.00); rm(file.returns.01.00)

## Load Factiva classification results (data.frame 'factiva').
load(file.factiva); rm(file.factiva)

## Combine Tret.01.00m Aret.01.00 and Factiva information for further use.
TAret.factiva <- combine.TAret.factiva(Tret.01.00, Aret.01.00, factiva)

## Load Fama-French factors and use a simple name for the data.frame.
load(file.WRDS.Fama.French.Factors); rm(file.WRDS.Fama.French.Factors)
FF <- WRDS.Fama.French.factors; rm(WRDS.Fama.French.factors)

## Construc a dataframe containing average media measure across deals.
date <- sort(unique(TAret.factiva[,"DATE"]))
avgret <- mct <- mcv <- NA
for (i in seq_along(date)) {
  pos <- which(TAret.factiva[,"DATE"] == date[i])
  avgret[i] <- mean(TAret.factiva[pos, "LOGRET"], na.rm = TRUE)
  mct[i] <- mean(TAret.factiva[pos, "Content.lag0"], na.rm = TRUE)
  mcv[i] <- mean(TAret.factiva[pos, "Coverage.lag0"], na.rm = TRUE)
}
rm(pos)
tret.factiva <- cbind(date, avgret, mct, mcv)

## Match tret.factiva with FF.
mkt <- NA
for (i in seq_along(date)) {
  pos <- which(FF[,"date"] == date[i])
  mkt[i] <- FF[pos, "mktrf"] + FF[pos, "rf"]
}
rm(pos)
if (nrow(tret.factiva) == length(mkt))
df <- cbind(tret.factiva, mkt)

## Regression.
reg <- lm(avgret ~ mkt + mct + mcv)
summary(reg)
