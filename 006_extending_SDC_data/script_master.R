#######################################################################
## 
## This script reads in SDC data from 2012-03-07 and performs sample
## selection.
## 
## Note that if you run this script on new SDC data that contains
## new/additional variable names, and you have not yet updated the
## column classes in 'config_SDC.R', then you should run
## 'script_helper.R' to determine the column classes. This is
## necessary for proper conversion to a data.frame.
## 
#######################################################################



## Basic settings.
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "006_extending_SDC_data")
source(file.path(dir.base, "merger-arbitrage", "config_SDC.R"))
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))



## Import SDC data.frame.
df <- read.SDC.df(file.SDC.txt)
## Get column names for SDC.
colnam <- read.col.names(file.SDC.colnames)
if (length(colnam) != ncol(df)) stop("Row names are messed up.")
colnames(df) <- colnam



## Clean the SDC data.
df <- clean.SDC(df)



## Convert to class 'Date'. 
for (i in which(colnames(df) %in% col.Date.SDC))
  df[, i] <- as.Date(df[, i], format = date.format)
## Calculate resolution date.
df$DRES <- res.dt(df$DE, df$DW)
## Calculate the number of days between announcement date and
## resolution date.
df$DAYS <- df$DRES - df$DA

## Convert to 'numeric'. Some columns throw warnings because they
## contain strings 'np' or 'nm' when no data is available. These get
## automatically converted to 'NA'.
for (i in which(colnames(df) %in% col.numeric.SDC)) {
  ## Remove thousands separator because it messes up 'as.numeric'.
  df[, i] <- gsub("([0-9]),([0-9]{3})", "\\1\\2", df[, i], perl=TRUE)
  df[, i] <- as.numeric(df[, i])
}

## Convert CIDGEN to a ten digits character string (SDC claims that
## there should be nine digits, but I found at least one with ten
## digits in the data).
df$ACIDGEN <- unlist(lapply(df$ACIDGEN, function(x) conv.to.ID(x, 10)))
df$TCIDGEN <- unlist(lapply(df$TCIDGEN, function(x) conv.to.ID(x, 10)))

## Convert DEALNO to a ten-digit character string (SDC claims that
## there should be nine digits, but I found at least one with ten
## digits in the data).
df$DEALNO <- unlist(lapply(df$DEALNO, function(x) conv.to.ID(x, 10)))
stopifnot(!any(duplicated(df$DEALNO)),
          !any(duplicated(as.numeric(df$DEALNO))))

## Ensure all CUSIPS have six digits.
df$ACU <- unlist(lapply(df$ACU, function(x) conv.to.ID(x, 6)))
df$TCU <- unlist(lapply(df$TCU, function(x) conv.to.ID(x, 6)))



## Apply sample selection criteria.
df <- select.sample.SDC(df)

## Extract deals from a given date range.
pos <- intersect(which(dt.bg <= df$DA), which(df$DA <= dt.ed))
df <- df[sort(unique(pos)), ]

## Extract the largest deals.
stopifnot(nrow(df) >= no.lgst.dls)
perm <- order(df$VAL, decreasing = TRUE) # Permutation for ordering.
df <- df[sort(perm[1:no.lgst.dls]), ]



## Convert to class 'factor'. This step should happen at the end of
## this script to ensure that for every level there is at least one
## observation.
for (i in which(colnames(df) %in% col.factor.SDC)) {
  df[, i] <- as.factor(df[, i])
}
## Change contrast of deal status. 
df$STAT <- relevel(df$STAT, ref = "Withdrawn")

## Ensure that the observations are sorted according to announcement
## date.
perm <- order(df$DA) # Permutation used for sorting.
if (any(perm != sort(perm))) {
  warning("SDC data is not sorted according to announceement date. Sorting it now.")
  df <- df[perm, ]
}



## Save result to file.
row.names(df) <- NULL # Set to 'automatic' row names.
takeoverData <- df # Use a more familiar name for the data.frame.
save(takeoverData, file = file.SDC.reloaded)
Sys.chmod(file.SDC.reloaded, mode = "0400") # Write-protect file.
