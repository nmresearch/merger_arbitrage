
#######################################################################
## 
## This is a helper script that is mainly of historic interest. You
## only need to run it if you add additional variable names to the SDC
## query and want to determine the R 'class' of these additional
## variables.
## 
#######################################################################

## Filename to save the column classes to. This file saves it in
## RData.
file.colclass.R <- file.path(dir.code, "colClasses.RData")

## Filename to save the column classes to. This file saves it in
## txt format in R-code.
file.colclass <- file.path(dir.code, "colClasses.R")

## Custom function that is useful for printing to a text file.
catt <- function(...)
  cat(..., file = file.colclass, sep = "", append = TRUE)



## -------------------------------------------
## This part is copied from script_master_01.R
## -------------------------------------------

## Set up the basic things.
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

## Remove duplicate columns.
nam <- colnames(df)
drows <- which(duplicated(nam))
## Verify whether also the contents of the columns are identical (not
## just the column names).
for (i in drows) {
  dr <- grep(paste("^", nam[i], "$", sep=""), nam, perl = TRUE)
  if (length(dr) < 2) stop("Should at least find two duplicates.")
  for (k in 2:length(dr)) {
    if (!identical(df[, dr[1]], df[, dr[k]]))
      stop("Column names are the same, but contents differ.")
  }
}
## Remove duplicate columns.
if (length(drows) > 0)
  df <- df[, -drows]

## Remove leading and trailing spaces.
for (i in seq_along(df[1, ])) { # Traverse through columns of 'df'.
  df[, i] <- gsub("^[ ]*", "", df[, i], perl = TRUE)
  df[, i] <- gsub("[ ]*$", "", df[, i], perl = TRUE)
}

## Deal with missing values (at least in a superficial way).
for (i in seq_along(df[1, ])) { # Traverse through _columns_ of 'df'.
  rowpos <- which(df[, i] == "")
  if (length(rowpos) > 0)
    is.na(df[rowpos, i]) <- TRUE
}







## -------------------------------------------
## This part finds out the class of each col.
## -------------------------------------------



## Present the reader with column name and column entries and let the
## reader determine which class this column is.
cls <- rep(NA_character_, ncol(df)) # Will contain the class of the columns.
for (i in seq_along(df[1, ])) { # Cycle through cols.  
  print(head(df[which(!is.na(df[, i])), i, drop = FALSE], 40))
  repeat {
    cat("col ", i, "/", ncol(df), ": ", sep = "")
    x <- readline("character (c), numeric (n), factor (f), or Date (d)? ")
    if (x %in% c("c", "n", "f", "d")) break
  }
  cls[i] <- x
}

## Save the column classes 'cls' to file.
save(cls, file = file.colclass.R)
Sys.chmod(file.colclass.R, mode = "0400") # Write-protect file.

## ## For interactive use.
## load(file.colclass.R)

## Print everything into a text file. The trick here is that it prints
## already in R code.
cat("## SDC Column classes, to be manually  added to file 'config_SDC.R'.\n",
    file = file.colclass)

catt("col.numeric.SDC <- c(\n")
indx <- which(cls == "n")
for (i in indx) {
  catt("\"", colnames(df)[i], "\"")
  if (i != indx[length(indx)])
    catt(",")
  catt("\n")
}
catt(")\n")

catt("col.factor.SDC <- c(\n")
indx <- which(cls == "f")
for (i in indx) {
  catt("\"", colnames(df)[i], "\"")
  if (i != indx[length(indx)])
    catt(",")
  catt("\n")
}
catt(")\n")

catt("col.Date.SDC <- c(\n")
indx <- which(cls == "d")
for (i in indx) {
  catt("\"", colnames(df)[i], "\"")
  if (i != indx[length(indx)])
    catt(",")
  catt("\n")
}
catt(")\n")
