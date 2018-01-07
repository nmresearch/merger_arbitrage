#######################################################################
##
## This script saves the downloaded Fama-French factors as data.frame
## 'WRDS.Fama.French.factors'.
## 
#######################################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "025_importing_Fama-French")
dir.data <- file.path(dir.base, "Data")

## Load configuration.
source(file.path(dir.code, "config.R"))

## Read downloaded Fama-French factors.
WRDS.Fama.French.factors <-
  read.table(file = file.WRDS.Fama.French.Factors.downloads,
             header = TRUE,
             sep = "\t",
             quote="",
             dec = ".",
             colClasses = "character",
             fill = TRUE,
             comment.char = "")

## Convert dates.
WRDS.Fama.French.factors[, "date"] <-
  as.Date(WRDS.Fama.French.factors[, "date"], format = "%Y%m%d")

## Convert other columns to numeric.
for (i in c("mktrf", "smb", "hml", "rf", "umd"))
  WRDS.Fama.French.factors[, i] <- as.numeric(WRDS.Fama.French.factors[, i])

## Save Fama-French factors as .RData.
save(WRDS.Fama.French.factors, file = file.WRDS.Fama.French.Factors)
Sys.chmod(file.WRDS.Fama.French.Factors, mode = "0400")
