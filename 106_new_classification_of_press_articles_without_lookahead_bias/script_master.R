
#########################################################################
## 
## This script extracts a number of training samples for estimation by
## naive Bayes. For each training sample, it also extracts the sample
## where the estimated naive Bayes model should predict a score. The
## basic idea is to avoid any look-ahead bias when estimating naive
## Bayes.
## 
## Currently the sample used has already for each deal the duplicate
## articles removed. This is intentional since we don't want to have
## duplicate press articles in any training sample.
## 
## The code below oversamples articles pertaining to failed deals by a
## factor of 'fac.oversmpl.w'. There are at least two reasons for
## doing so. First, deals are withdrawn only relatively infrequently
## (10% of deals are withdrawn). Second, we would like to have a
## higher precision for those articles pertaining to failed deals.
## 
#########################################################################

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "106_new_classification_of_press_articles_without_lookahead_bias")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))



## Load Factiva data.
load(file.factiva)
f <- factiva; rm(factiva, file.factiva) # easy-to-type.
f.orig <- f # back up original; needed for set to which to apply trained naive Bayes to.

## Load SDC data.
load(file.SDC.complete)
sdc <- takeoverData; rm(takeoverData, file.SDC.complete)  # easy-to-type.



## If Factiva data contains classification score from previous
## experiment, remove this column since it's not needed.  Remove all
## duplicated articles (this is OK since for the stratification and
## the classification we don't care about deals).
col.del <- which(colnames(f) %in% c("classification", "classificationW", "classificationC"))
if (length(col.del) >= 1) f <- f[, -col.del]
row.del <- which(duplicated(f$FactivaDocID))
if (length(row.del) > 0) f <- f[-row.del, ]

## Only keep articles that appear on or before DA. (There is also an
## option for DRES since those articles are not interesting for the
## classification -- they usually only summarize the outcome of the
## deal.)
f <- rm.article(sdc, f, "DA")

## Add deal status to Factiva data and delete those press articles
## where there is no deal status available.
f <- dealID.factiva(sdc, f)
rows <- which(is.na(f$STAT))
if (length(rows) > 0)
  f <- f[-rows, ]



## Get those rows in 'f' that correspond to withdrawn and completed
## deals.
rows.w.total <- which(f$STAT == "Withdrawn")
rows.c.total <- which(f$STAT == "Completed")
stopifnot(sort(union(rows.w.total, rows.c.total)) == seq_along(f$STAT)) # Partition?

## Starting date and ending date.
mi <- min(f.orig$Date)
ma <- max(f.orig$Date)

## Number of days in each interval.
step.size <- floor((ma - mi)/time.intervals)

spls <- list()
mapping <- rep(NA_real_, nrow(f.orig))
stopifnot(time.intervals - 1 >= 1) # Ensure not to count backwards in following loop.
for (i in 1:(time.intervals - 1)) {
  
  ## Calculate time interval for training sample.
  time.beg <- mi + step.size * (i - 1)
  time.end <- mi + step.size * i
  stopifnot(i < time.intervals) # If i=time.intervals, have to set 'time.end <- ma'.

  ## Find out which rows in 'f' are relevant for this time interval.
  rows <- rows.for.time(f, time.beg, time.end, ma)
  rows.w <- intersect(rows, rows.w.total)
  rows.c <- intersect(rows, rows.c.total)
  stopifnot(length(rows) > 0, length(rows.w) > 0, length(rows.c) > 0)

  frac.w <- fac.oversmpl.w * length(rows.w) / length(rows)
  if (frac.w > 0.5) frac.w <- 0.5 # Don't oversample too much.
  frac.c <- 1 - frac.w # ... assuming that completed and withdrawn are a partition.

  ## Do stratified sampling.
  indx <- sample(rows.w, min(length(rows.w), round(sp.sz * frac.w)))
  indx <- c(indx,
            sample(rows.c, min(length(rows.c), round(sp.sz * frac.c))))

  spls[[i]] <- data.frame(HadoopKey = f[indx, "HadoopKey"], 
                          STAT = f[indx, "STAT"], stringsAsFactors = FALSE)

  ## Calculate time interval for prediction.
  stopifnot(i < time.intervals) # Verify that still one time interval is left.
  time.beg <- mi + step.size * i
  time.end <- mi + step.size * (i + 1)
  if (i + 1 == time.intervals)
    time.end <- ma

  ## Rows in 'f.orig' that correspond to given time interval.
  rows.orig <- rows.for.time(f.orig, time.beg, time.end, ma)
  stopifnot(is.na(mapping[rows.orig])) # Don't accidentally overwrite something.
  mapping[rows.orig] <- i
}



## Deal with first time interval, i.e. use training sample from first
## interval to also predict the first interval. This introduces a
## look-ahead bias, which we should deal with later by downloading
## additional data from Factiva for 1999.
i <- 1
time.beg <- mi + step.size * (i - 1)
time.end <- mi + step.size * i
rows.orig <- rows.for.time(f.orig, time.beg, time.end, ma)
stopifnot(all(which(is.na(mapping)) == rows.orig))
mapping[rows.orig] <- i



## Put everything into a single 'list' object.
stopifnot(all(!is.na(mapping)))
clsf.params <- list()
clsf.params$Samples <- spls
clsf.params$Mapping <- data.frame(HadoopKey = f.orig$HadoopKey,
                                  train.spl = mapping,
                                  stringsAsFactors = FALSE)



## Save the training sample to file.
save(clsf.params, file = file.samples)
Sys.chmod(file.samples, mode = "0400")
