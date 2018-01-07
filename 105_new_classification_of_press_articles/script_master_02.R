#########################################################################
## 
## This file lets a human being do the classification.
##
#########################################################################

library("tm")
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "105_new_classification_of_press_articles")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Load Factiva data and remove all duplicated articles (this is OK
## since for the stratification and the classification we don't care
## about deals).
load(file.factiva)
f <- factiva; rm(factiva, file.factiva) # easy-to-type.

## Load SDC data.
load(file.SDC.complete)
sdc <- takeoverData; rm(takeoverData, file.SDC.complete)  # easy-to-type.

## Load training sample.
load(file.training.sample)
f.train <- training.sample; rm(training.sample, file.training.sample)
f.train$classification.training <- rep(NA_character_, nrow(f.train))

## Load corpus pertaining to training sample.
load(file.training.sample.corpus)
cp <- factiva.cp; rm(factiva.cp, file.training.sample.corpus)


## Sanity checks.
stopifnot(nrow(f.train) > 0, nrow(f.train) == length(cp))
for (i in seq_along(f.train[, 1])) { # Cycle through rows.
  stopifnot(f.train[i , "FactivaDocID"] == meta(cp[[i]], "ID"))
  stopifnot(f.train[i , "Date"] == as.Date(meta(cp[[i]], "DateTimeStamp")))
  stopifnot(f.train[i , "articleSource"] == meta(cp[[i]], "Origin"))
  stopifnot(length(which(sdc$dealID == f.train[i, "dealID"])) == 1)
}


## The classification takes place here. In case you go through it
## several times, this task should not become repetitive. That's why I
## use a random permutation of the indices (by using
## 'sample'). Furthermore, you can stop in between (Ctrl-C) and later
## resume this task, because the code below skips those articles that
## have already been classified.
ctr <- length(which(!is.na(f.train$classification.training)))
for (i in sample(which(is.na(f.train$classification.training)))) {
  ctr <- ctr + 1
  pos <- which(sdc$dealID == f.train[i, "dealID"])
  for (j in 1:80) cat("\n") # Clear screen.
  
  print(cp[[i]])
  cat("\nArticle no.   ", ctr, ".\n", sep = "")
  cat("Acquirer:     ", sdc[pos, "AN"], "\n", sep = "")
  cat("Target:       ", sdc[pos, "TN"], "\n", sep = "")
  cat("Status:       ", as.character(sdc[pos, "STAT"]), "\n", sep = "")
  cat("Dates:        ", as.character(sdc[pos, "DA"]), " -- ",
      as.character(sdc[pos, "DRES"]), "\n", sep = "")
  cat("Article Date: ", as.character(as.Date(meta(cp[[i]], "DateTimeStamp"))), "\n", sep = "")
  cat("Article Orig: ", meta(cp[[i]], "Origin"), "\n", sep = "")
  
  repeat { # Until the answer is valid.
    classification <- readline("positive (p), negative (n), or discard (d)? ")
    if (classification %in% c("p", "n", "d"))
      break
  }
  
  f.train$classification.training[i] <-
    meta(cp[[i]], "LocalMetaData")$classification.training <-
      classification
  
}



## ## Intermediate save and load in case you want to take a break from
## ## manual classification.
## save(f.train, cp, file = paste(file.classification.sample, "_tmp", sep = ""))
## load(paste(file.classification.sample, "_tmp", sep = ""))



unlist(lapply(cp, function(x) meta(x, "LocalMetaData")$classification.training))


## Save manual classification to file.
classification.human <- f.train
cp.classification.human <- cp
save(classification.human, cp.classification.human,
     file = file.classification.sample)
Sys.chmod(file.classification.sample, mode = "0400")
