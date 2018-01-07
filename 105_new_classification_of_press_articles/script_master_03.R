#########################################################################
## 
## This file saves only the 'positive' and 'negative' articles, and
## discards the articles that were marked as 'discard'.
##
#########################################################################

library("tm")
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "105_new_classification_of_press_articles")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Load manually classified articles.
load(file.classification.sample)
f.train <- classification.human; rm(classification.human)
cp <- cp.classification.human; rm(cp.classification.human)

## Sanity check.
stopifnot(all(unlist(lapply(cp, function(x) meta(x, "LocalMetaData")$classification.training)) ==
              f.train$classification.training))

## Only keep positive and negative classifications.
pos.keep <- which(f.train$classification.training == "p" | f.train$classification.training == "n")
f.train <- f.train[pos.keep, ]
cp <- cp[pos.keep]
## Convert to class 'factor' and relevel.
f.train$classification.training <- as.factor(f.train$classification.training)
f.train$classification.training <- relevel(f.train$classification.training, ref = "n")





## Save manual classification to file.
classification.human <- f.train
cp.classification.human <- cp
save(classification.human, cp.classification.human,
     file = file.classification.sample.only.pos.neg)
Sys.chmod(file.classification.sample.only.pos.neg, mode = "0400")
