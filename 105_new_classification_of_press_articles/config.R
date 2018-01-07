dir.data <- file.path(dir.base, "Data")

## This file is the output of dir 090_. In particular, it contains the
## SDC data downloaded on 2012-03-07 and in addition has various firm
## identifiers (e.g. PERMNO).
file.SDC.complete <- file.path(dir.data, "090_010_SDC_complete.RData")

## File created by the code in directory 100_, containing the
## classification results from Factiva.
file.factiva <- file.path(dir.data, "100_025_factiva_meta.RData")
## file.factiva <- file.path(dir.data, "100_030_factiva.RData")

## File containing the Hadoop keys and the FactivaDocID's of the
## training sample (but not yet the claassification).
file.training.sample <- file.path(dir.data, "105_010_classification_sample_indices.RData")

## File containint the corresponding corpus to the training sample
## (extracted from HDFS).
file.training.sample.corpus <- file.path(dir.data, "105_020_classification_sample_corpus.RData")

## This file contains the manual classification done by a human being.
file.classification.sample <- file.path(dir.data, "105_030_classification_sample.RData")

## This is the same file as above, except that it only contains
## 'positive' and 'negative' classifications (with 'discard' already
## being removed).
file.classification.sample.only.pos.neg <- file.path(dir.data, "105_040_classification_sample_only_pos_neg.RData")



## Years from which to draw random sample from.
years <- 2000:2009

## Sample size to draw for training data.
sp.sz <- 400

## Factor with which withdrawn deals will be oversampled.
fac.oversmpl.w <- 2
