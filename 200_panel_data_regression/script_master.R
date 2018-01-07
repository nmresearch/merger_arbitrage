############################################################### 
##
## This sciprt builds a panel dataframe containing returns 
## and media varaible, and apply regression on the panel data
## 
###############################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "200_panel_data_regression")
# dir.base <- file.path("~/Documents/merger")
# dir.code <- file.path(dir.base, "Code", "200_panel_data_regression")
dir.data <- file.path(dir.base, "Data")

## Load settings and functions.  
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Load data.frame 'Tret.00.00' and 'Aret.00.00'
load(file.returns.00.00); rm(file.returns.00.00)

## Load 'factiva'.
load(file.Factiva); rm(file.Factiva)

## Load data.frame 'takeoverData'.
load(file.SDC.complete); rm(file.SDC.complete)

## Build the first part of panel dataframe with Date and returns.
panel <- Tret.00.00[, c("dealID", "DATE", "DA.TD", "DA.CD", "LOGRET")]

## Add media content and coverage to panel dataframe. Note that
## factiva uses calendar day 'T'. When mapping with Tret dataframe,
## which only contains returns on trading day 't', we have the
## following settings:
## For lagged media measure, {T | t-1 < T <= t} 
## For contemporaneous ones, {T | t-1 <= T < t}
dealID.set <- sort(unique(panel$dealID))
panel$lcontent <- rep(NA_real_, nrow(panel))
panel$content <- panel$coverage <- rep(NA_real_, nrow(panel))

for (dealID in dealID.set) {
  pos1 <- which(panel$dealID == dealID )
  pos2 <- which(factiva$dealID == dealID)
  ## For The Lagged media content column.
  i <- findInterval(factiva[pos2, "Date"], na.omit(panel[pos1, "DATE"]))
  if (length(i) > 0 ) {
    for (j in sort(unique(i[is.finite(i)])) ) {
      k <- which(i == j)
      if(!is.na(panel[pos1, "LOGRET"][j+1]))
        panel[pos1, "lcontent"][j+1] <- mean(factiva[pos2, "classification"][k], na.rm=TRUE)
    }
    rm(i, j, k)
  }

  ## For the contemporaneous media columns.
  i <- findInterval(factiva[pos2, "Date"], na.omit(panel[pos1, "DATE"])+1)
  if (length(i) > 0 ) {
    for (j in sort(unique(i[is.finite(i)])) ) {
      k <- which(i == j)
      if(!is.na(panel[pos1, "LOGRET"][j+1])) {
        panel[pos1, "content"][j+1] <- mean(factiva[pos2, "classification"][k], na.rm=TRUE)
        panel[pos1, "coverage"][j+1] <- sum(is.finite(factiva[pos2, "classification"][k]))
      }
    }
    rm(i, j, k)
  }
  rm(pos1, pos2)
}

## Clean panel data.frame, remove rows that contain NA and delete duplicates.
## panel <- na.omit(panel)
panel <- unique(panel)

## further checking duplication. 
## It's possible that two rows are not exactly identical, but some critical
## variables are the same, which may affect model estimation.
nrow(panel[, c("dealID", "DA.TD")])
nrow(unique(panel[, c("dealID", "DA.TD")]))
row.names(panel) <- NULL

## Save panel dataframe to file.
save(panel, file = file.panel)

#####################################
## Basic Regression with panel data.
#####################################
library(plm)
load(file.panel); rm(file.panel)

## Transform the data to fit in plm functions.
panel.data <- pdata.frame(panel, index = c("dealID", "DA.TD"), drop.index = TRUE, row.names = TRUE)

## Fixed effect regression.
fe1 <- plm(LOGRET ~ content, data = panel.data, model = "within")
summary(fe1)
fe2 <- plm(LOGRET ~ content, data = panel.data, model = "within", effect = "twoways")
summary(fe2)
fe3 <- plm(LOGRET ~ content + coverage, data = panel.data, model = "within")
summary(fe3)

## Regression with mathmatic lags, which can be preset.
lags <- 0:2
felag <- plm(LOGRET ~ lag(panel.data$content, lags) # Lags for content.
             data = panel.data,
             model = "within",
             effect = "twoways")
summary(felag)

## Use GMM estimation for panel data. However, the formula takes a
## long time to run, and returns an error message "cannot allocate
## vector of size 20.8 Mb", and adjusting memory size won't help.
gmm <- pgmm(dynformula(LOGRET ~ content, lag.form = list(0, 1)), # Lags for each variable.
            data = panel.data,
            model = "twosteps",
            gmm.inst = ~ LOGRET,
            ## lag.gmm = list(c(0, 99)) # Lags used for instruments.
            lag.gmm = list(c(0, 4)) # Lags used for instruments.
            )
summary(gmm)

## Regression with pre-claculated lag_1 media content, as "lcontent"
## in the panel dataframe.
felag1 <- plm(LOGRET ~ content + lcontent, data = panel.data, model = "within")
summary(felag1)


##############################################
## Add control variables to panel data.frame.
##############################################

## Add control_premium.
takeoverData$premium <- log(takeoverData[, "PR"]/takeoverData[, "PR4WK"])
panel$premium <- rep(NA_real_, nrow(panel))
for (i in seq_along(takeoverData[, 1]) ) {
  pos <- which(panel$dealID == takeoverData[i, "dealID"])
  if (length(pos) > 0)
    panel[pos, "premium"] <- rep(takeoverData[i, "premium"], length(pos))
}

## Add control_swap.
panel$swap <- rep(takeoverData[1, "SWAP"], nrow(panel)) # Ensure contrasts match (class is 'factor').
is.na(panel$swap) <- TRUE 
for (i in seq_along(takeoverData[, 1]) ) {
  pos <- which(panel$dealID == takeoverData[i, "dealID"])
  if (length(pos) > 0)
    panel[pos, "swap"] <- rep(takeoverData[i, "SWAP"], length(pos))
}

## Regression with control variables.
## It turns out adding these variables doesn't affect the regression result.
panel.data <- pdata.frame(panel, index = c("dealID", "DA.TD"), drop.index = TRUE, row.names = TRUE)
fe_ctrl <- plm(LOGRET ~ content + swap, data = panel.data, model = "within")
summary(fe_ctrl)

#################################################
## Regression with different trading strategies.
#################################################

## Calculated the arbitrage returns when using 2 trading
## stratrgies. One is to long target and short acquirer when there is
## an non-NA 'EXRATIO'. The other is to long target and short acquirer
## when there is 'EXRATIO' and 'COLRATIO'.
panel.ret <- TAret(Tret.00.00, Aret.00.00, factiva)

## Transform the data to fit in plm functions.
panel.data <- pdata.frame(panel.ret, index = c("dealID", "day"), drop.index = TRUE, row.names = TRUE)

## Fixed effect regression.
fe1.1 <- plm(LOGRET.LongShort1 ~ Content, data = panel.data, model = "within")
summary(fe1.1)
fe1.2 <- plm(LOGRET.LongShort2 ~ Content, data = panel.data, model = "within")
summary(fe1.2)
fe2.1 <- plm(LOGRET.LongShort1 ~ Content + Coverage, data = panel.data, model = "within")
summary(fe2.1)
fe2.2 <- plm(LOGRET.LongShort2 ~ Content + Coverage, data = panel.data, model = "within")
summary(fe2.2)

## Fixed effect regression with lagged media variables.
fe3.1 <- plm(LOGRET.LongShort1 ~ Content + Content.lag1, data = panel.data, model = "within")
summary(fe3.1)
fe3.2 <- plm(LOGRET.LongShort2 ~ Content + Content.lag1, data = panel.data, model = "within")
summary(fe3.2)

