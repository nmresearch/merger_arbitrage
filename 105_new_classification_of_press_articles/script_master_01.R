#########################################################################
## 
## This file does stratified sampling to extract a training sample for
## classification. The stratification accounts for the number of
## articles per year and for the percentage of failed/successful deals
## in a given year. It also deals with the fact that some articles are
## duplicates in Factiva.
##
## The code below oversamples articles pertaining to failed deals by a
## factor of 'fac.oversmpl.w'. There are at least two reasons for
## doing so. First, deals are withdrawn only relatively infrequently
## (10% of deals are withdrawn). Second, we would like to have a
## higher precision for those articles pertaining to failed deals.
## 
## 
## Still to do: It might also be useful to somehow stratify along
## *deals*. For example, the way the sampling currently works, it
## often gets articles about, say, Microsoft trying to take over
## Yahoo, but it doesn't sample a lot from smaller deals. So it might
## be useful to add a constraint to not sample too much from the same
## deal.
## 
#########################################################################

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "105_new_classification_of_press_articles")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Load Factiva data and remove all duplicated articles (this is OK
## since for the stratification and the classification we don't care
## about deals). Furthermore, if Factiva data contains classification
## score from previous experiment, remove this column since it's not
## needed.
load(file.factiva)
f <- factiva; rm(factiva, file.factiva) # easy-to-type.
col.del <- which(colnames(f) %in% c("classification", "classificationW", "classificationC"))
if (length(col.del) >= 1) f <- f[, -col.del]
row.del <- which(duplicated(f$FactivaDocID))
if (length(row.del) > 0) f <- f[-row.del, ]

## Load SDC data.
load(file.SDC.complete)
sdc <- takeoverData; rm(takeoverData, file.SDC.complete)  # easy-to-type.


## Initialize beginning and ending dates of years.
yr.beg <- date.year.beg(years)
yr.end <- date.year.end(years)

## Only keep articles that appear in given time range.
f <- f[sort(which(min(yr.beg) <= f$Date & f$Date <= max(yr.end))), ]


## Only keep articles that appear before DRES (or on or before
## DA+1). The articles on or after DRES are not so interesting for the
## classification since those articles mainly summarize the outcome of
## the deal.
rm.article <- function(sdc, f, Dt = "DRES") {
  stopifnot(Dt %in% c("DRES", "DA"))
  pos <- NULL
  for (i in seq_along(sdc[, 1])) { # Cycle through _rows_ of 'sdc'.
    pos1 <- which(f$dealID == sdc[i, "dealID"])
    pos2 <-
      if (Dt == "DRES") {
        which(f$Date < sdc[i, Dt]) # Article should appear before DRES.
      } else { # Article should appear on or before DA+1 to allow publishing lag in daily newspapers.
        which(f$Date <= sdc[i, Dt] + 1)
      }
    pos <- c(pos, intersect(pos1, pos2))
  }
  pos <- sort(unique(pos))
  stopifnot(length(pos) > 0)
  return(f[pos, ])
}
f <- rm.article(sdc, f, "DA") # For predictive stuff it should be "DA" (not "DRES")!

## Get those positions in 'f' that correspond to withdrawn and
## completed deals.
pos.w <- articles.withdrawn(sdc, f)
pos.c <- articles.completed(sdc, f)
## Sanity check.
stopifnot(length(intersect(pos.w, pos.c)) == 0)

## Keep only those observation where it's possible to tell whether
## deal is withdrawn or completed.
f <- f[sort(union(pos.w, pos.c)), ]

## Recalculate those positions in 'f' that correspond to withdrawn and
## completed deals.
pos.w <- articles.withdrawn(sdc, f)
pos.c <- articles.completed(sdc, f)
## Sanity check.
stopifnot(length(intersect(pos.w, pos.c)) == 0)





## Get those positions in 'f' that pertain to the sequence of years.
pos.in.year <- list()
for (i in seq_along(years)) {
  pos.in.year[[i]] <- which(yr.beg[i] <= f$Date & f$Date <= yr.end[i])
}
## Sanity check.
stopifnot(all(lapply(pos.in.year, length) > 0))

## Calculate the fractions pertaining to years. Furthermore, for each
## year, calculate the fraction of withdrawn vs. completed deals.
frac.yr <- frac.yr.w <- frac.yr.c <- numeric()
for (i in seq_along(years)) {
  frac.yr[i] <- length(pos.in.year[[i]]) / nrow(f)
  frac.yr.w[i] <- length(intersect(pos.in.year[[i]], pos.w)) / length(pos.in.year[[i]])
  frac.yr.c[i] <- length(intersect(pos.in.year[[i]], pos.c)) / length(pos.in.year[[i]])
}

## Sanity checks.
stopifnot(isTRUE(all.equal(sum(frac.yr), 1)))
stopifnot(isTRUE(all.equal(frac.yr.w + frac.yr.c, rep(1, length(frac.yr.w)))))

summary(cbind(frac.yr, frac.yr.w, frac.yr.c))

## Oversample the withdrawn deals since we want to have a higher
## precision for those deals.
frac.yr.w <- fac.oversmpl.w * frac.yr.w
frac.yr.c <- fac.oversmpl.w * frac.yr.c - 1 # Also have to adjust completed deals accordingly.
## Sanity check for oversampling.
stopifnot(isTRUE(all.equal(frac.yr.w + frac.yr.c, rep(1, length(frac.yr.w)))))



## Sample from strata. Cycle through years and then, for each year,
## sample from press articles pertaining to completed and withdrawn
## deals.
indx <- NULL
for (i in seq_along(years)) {
  ## Sample from withdrawn deals in year i.
  smple.from <- intersect(pos.in.year[[i]], pos.w)
  smple.sze <- round(sp.sz * frac.yr[i] * frac.yr.w[i])
  if (length(smple.from) > 0 && smple.sze > 0)
    indx <- c(indx, sample(smple.from, smple.sze))
  ## Sample from completed deals in year i.
  smple.from <- intersect(pos.in.year[[i]], pos.c)
  smple.sze <- round(sp.sz * frac.yr[i] * frac.yr.c[i])
  if (length(smple.from) > 0 && smple.sze > 0)
    indx <- c(indx, sample(smple.from, smple.sze))
}
indx <- sort(indx)

length(indx)

## Sanity checks.
stopifnot(all(!duplicated(indx)))
stopifnot(all(sort(f[indx, "HadoopKey"]) == f[indx, "HadoopKey"]))

## Sanity check for proportions sampled from various years. (The
## numbers will not be exactly zero because of the small sample size.)
dev <- NULL # Deviation.
for (i in seq_along(years)) {
  frac.smple <- length(which(indx %in% pos.in.year[[i]])) / length(indx)
  frac.total <- length(pos.in.year[[i]]) / nrow(f)
  dev <- c(dev, abs(frac.smple - frac.total) / frac.total)
  cat(dev[length(dev)], "\n")
}
summary(dev)

## Sanity checks for withdrawn vs. completed deals.
stopifnot(length(which(indx %in% pos.c)) == length(indx) - length(which(indx %in% pos.w)))
## This number should be approximately 10%, i.e. the proportion of
## failed deals in the sample.
length(which(indx %in% pos.w)) / length(indx) / fac.oversmpl.w


## Sanity checks for withdrawn vs. completed deals.
dev <- NULL # Deviation.
for (i in seq_along(years)) {
  indx.in.yr <- which(indx %in% pos.in.year[[i]])
  indx.in.w <- which(indx %in% pos.w)
  frac.smple <- length(intersect(indx.in.yr, indx.in.w)) / length(indx.in.yr) / fac.oversmpl.w
  frac.total <- length(intersect(pos.in.year[[i]], pos.w)) / length(pos.in.year[[i]])
  dev <- c(dev, abs(frac.smple - frac.total) / frac.total)
  cat(dev[length(dev)], "\n")
}
summary(dev)


## Extract training sample.
training.sample <- f[indx, ]

## Add deal status to training sample. Maybe this is useful for
## training the classifier without human intervention, just for
## comparison.
training.sample$STAT <- # Ensure contrasts are consistent with SDC.
  rep(sdc[1, "STAT"], nrow(training.sample))
is.na(training.sample$STAT) <- TRUE
for (i in seq_along(training.sample[, 1])) {
  pos.sdc <- which(sdc$dealID == training.sample[i, "dealID"])
  stopifnot(length(pos.sdc) == 1)
  training.sample[i, "STAT"] <- sdc[pos.sdc, "STAT"]
}

## Save the training sample to file.
save(training.sample, file = file.training.sample)
Sys.chmod(file.training.sample, mode = "0400")
