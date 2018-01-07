###################################################################
## 
## This file finds out how for how many deals we could not find
## CUSIPs.
## 
###################################################################


dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage",
                      "Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "050_consistency_check_for_CUSIP")
source(file.path(dir.code, "config.R"))

## Load data.frame 'SDC.data.PERMCO' and relabel it to the easier
## 'dfSDC'.
load(file.SDC.with.CUSIP9); rm(file.SDC.with.CUSIP9)
dfSDC <- SDC.data.PERMCO; rm(SDC.data.PERMCO)


## Initialize position vector of interesting rows in the date.frame
## 'dfSDC'. This vector will later contain the positions in
## the 'dfSDC' data.frame that don't have a firm identifier.
p <- NULL

## PERMNOs
pos.na <- union(which(is.na(dfSDC$Target_PERMNO)),
                which(is.na(dfSDC$Acquiror_PERMNO)))
pos.na <- unique(pos.na)
p <- sort(unique(union(p, pos.na)))
print(length(p))

## PERMCOs
pos.na <- union(which(is.na(dfSDC$Target_PERMCO)),
                which(is.na(dfSDC$Acquiror_PERMCO)))
pos.na <- unique(pos.na)
p <- sort(unique(union(p, pos.na)))
print(length(p))

## CUSIP
pos.na <- union(which(is.na(dfSDC$Target_CUSIP8)),
                which(is.na(dfSDC$Acquiror_CUSIP8)))
pos.na <- unique(pos.na)
p <- sort(unique(union(p, pos.na)))
print(length(p))
