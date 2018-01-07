#######################################################################
## 
## This script adds various firm identifiers also to the new SDC data
## (downloaded 2012-03-07). These firm identifiers are not directly
## from WRDS queries, but they are instead from the old SDC data
## (downloaded 2012-02-11).
## 
#######################################################################


## Basic settings.
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "090_adding_PERMNO_et_al_to_merged_SDC")
source(file.path(dir.code, "config.R"))


## Load old SDC file (from 2012-02-11) that contains additional firm
## identifiers.
load(file.SDC.with.CUSIP9)
df.old <- SDC.data.PERMCO # Use more convenient name.
rm(SDC.data.PERMCO)

## Load recent SDC file (from 2012-03-08) that does not yet contain
## the firm identifiers.
load(file.SDC.merged)
df.new <- takeoverData # Use more convenient name.
rm(takeoverData)

## Sanity checks. Make sure there are no duplicate deal IDs in the
## data.
if (length(unique(df.old$dealID)) != length(df.old$dealID)) stop("inconsistency.")
if (length(unique(df.new$dealID)) != length(df.new$dealID)) stop("inconsistency.")

## Remove the deal from old SDC that could not be found anymore in the
## more recent SDC download because SDC rewrote history.
pos <- which(!df.old$dealID %in% df.new$dealID)
if (length(pos) != 1) stop("something has changed in the data. Please investigate")
if (length(pos) > 0) df.old <- df.old[-pos, ]

## Another sanity check. Make sure the deal IDs have the same sequence
## in both data.frames.
if (any(df.old$dealID != df.new$dealID)) stop("inconsistency.")

## Assign firm identifiers.
df.new$APERMNO <- df.old$Acquiror_PERMNO
df.new$TPERMNO <- df.old$Target_PERMNO
##
df.new$APERMCO <- df.old$Acquiror_PERMCO
df.new$TPERMCO <- df.old$Target_PERMCO
##
df.new$ACUSIP8 <- df.old$Acquiror_CUSIP8
df.new$TCUSIP8 <- df.old$Target_CUSIP8
##
df.new$ACUSIP9 <- df.old$Acquiror_CUSIP9
df.new$TCUSIP9 <- df.old$Target_CUSIP9

## Visually verify whether in a random draw the CUSIPs match.
print(df.new[sample.int(nrow(df.new), size = 20), c("ACU", "ACUSIP8", "ACUSIP9", "TCU", "TCUSIP8", "TCUSIP9", "APERMNO", "TPERMNO", "APERMCO", "TPERMCO")])

takeoverData <- df.new # Use more familiar name.
save(takeoverData, file = file.SDC.complete)
Sys.chmod(file.SDC.complete, mode = "0400") # Write-protect file.
