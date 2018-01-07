#######################################################################
## This script checks whether the code in the 033_ and 034_
## directories yields largely consistent results. The result of this
## check is that indeed the code in 033_ and 034_ give largely the
## same results.
#######################################################################


dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage",
                      "Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "040_comparing_various_ways_of_getting_PERMCO")
source(file.path(dir.code, "config.R"))


## Load SDC data.frames containing old and new method of getting the
## PERMCO.
load(file.SDC.with.PERMCOs.old)
dfSDC.old <- SDC.data.PERMCO; rm(SDC.data.PERMCO)
load(file.SDC.with.PERMCOs.new)
dfSDC.new <- SDC.data.PERMCO; rm(SDC.data.PERMCO)

## The following result shows that when *both* new and old are not NA,
## then we got the same results.
lAc <- length(which(dfSDC.old$Acquiror_PERMCO != dfSDC.new$Acquiror_PERMCO))
lTa <- length(which(dfSDC.old$Target_PERMCO != dfSDC.new$Target_PERMCO))
if (lAc > 0 | lTa >0)
  stop("The two procedures resulted in different results.")

## However, we still have to deal with those cases when old is NA and
## new is not NA (or vice versa new is NA and old is not NA). We start
## with the acquirer...
AcOldNA  <- which(is.na(dfSDC.old$Acquiror_PERMCO))
AcOldNNA <- which(!is.na(dfSDC.old$Acquiror_PERMCO))
AcNewNA  <- which(is.na(dfSDC.new$Acquiror_PERMCO))
AcNewNNA <- which(!is.na(dfSDC.new$Acquiror_PERMCO))
## Old is NA and new is not NA.
intersect(AcOldNA, AcNewNNA)
## New is NA and old is not NA.
intersect(AcNewNA, AcOldNNA)

## ... and continue with the target.
TaOldNA  <- which(is.na(dfSDC.old$Target_PERMCO))
TaOldNNA <- which(!is.na(dfSDC.old$Target_PERMCO))
TaNewNA  <- which(is.na(dfSDC.new$Target_PERMCO))
TaNewNNA <- which(!is.na(dfSDC.new$Target_PERMCO))
## Old is NA and new is not NA.
intersect(TaOldNA, TaNewNNA)
## New is NA and old is not NA.
p <- intersect(TaNewNA, TaOldNNA)


dfSDC.old[p, c("Target_Name", "Target_CUSIP", "Target_PERMNO", "Target_PERMCO")]


##                  Target_Name Target_CUSIP Target_PERMNO Target_PERMCO
## 19               Writer Corp       982554         83652          4986
## 157        Stratosphere Corp       863106         80282         12877
## 540       WTC Industries Inc       929341         77253         11137
## 760      Delta Air Lines Inc       247361          <NA>         20569
## 851            McLeodUSA Inc       582266          <NA>         14707
## 859      Visual Sciences Inc       928445         81075          4833
## 914 Applied Biosystems Group       037906         14622          6115


