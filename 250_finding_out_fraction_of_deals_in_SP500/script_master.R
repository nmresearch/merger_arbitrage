
#######################################################################
## 
## This script tries to find out how many takeovers there are in the
## S&P500 index in a given time period.
## 
## Note that the matching in this script is done through the ticker of
## the target company. For more accurate matching, a different firm ID
## (or ID combinations) should be used.
## 
#######################################################################

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "250_finding_out_fraction_of_deals_in_SP500")
source(file.path(dir.code, "config.R"))

load(file.sdc)
sdc <- takeoverData; rm(takeoverData)

## Read stock market index data.
idx <- read.delim(file = file.index.constituents,
                  colClasses = NULL)
## idx <- read.table(file = file.index.constituents,
##                   header = TRUE,
##                   sep = "\t",
##                   dec = ".",
##                   colClasses = "character",
##                   fill = TRUE
##                   )
idx$from <- as.Date(idx$from, "%Y%m%d")
idx$thru <- as.Date(idx$thru, "%Y%m%d")

## Add 'DA' column to 'idx'.
idx$DA <- rep(as.Date(NA), nrow(idx))
for (i in seq_along(sdc[, 1])) { # Cycle through rows of 'sdc'.
  rows.idx <- which(idx$co_tic == sdc[i, "TTickerCCM"])
  if (length(rows.idx) > 0) {
    idx[rows.idx, "DA"] <- sdc[i, "DA"]
  }
}
stopifnot(any(!is.na(idx$DA)))


## Extract all rows in 'idx' where the announcement date falls between
## the 'from' and 'through' date.
stopifnot(all(!is.na(idx$from)))
rows.date <- NULL
for (i in which(!is.na(idx$DA))) { # First ensure that DA<=thru.
  if (is.na(idx$thru[i])) {
    rows.date <- c(rows.date, i)
  } else if (idx$DA[i] <= idx$thru[i]) {
    rows.date <- c(rows.date, i)
  }
}
rows.date <- sort(unique(rows.date)) # Just to be on the safe side.
rows.date <- intersect(which(idx$from <= idx$DA), rows.date)

## Extract takeovers in given time range.
d.beg <- as.Date("2000-01-01")
d.end <- as.Date("2000-12-31")
## d.beg <- as.Date("2000-01-01")
## d.end <- as.Date("2009-12-31")
rows.idx.date.constr <-
  which(d.beg <= idx$DA & idx$DA <= d.end)

## Extract index entries that correspond to S&P500.
rows.idx.idxmember <- which(idx$SPMI == "10")

rows <- intersect(rows.date, rows.idx.date.constr)
length(unique(idx[rows, "co_conm"]))

