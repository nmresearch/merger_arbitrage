
#########################################################################
## 
## This file contains configuration information.
## 
#########################################################################

dir.data <- file.path(dir.base, "Data")

## This file is the output of dir 090_. In particular, it contains the
## SDC data downloaded on 2012-03-07 and in addition has various firm
## identifiers (e.g. PERMNO).
file.SDC.complete <- file.path(dir.data, "090_010_SDC_complete.RData")

## File created by the code in directory 100_, containing the metadata
## from Factiva.
file.factiva <- file.path(dir.data, "100_025_factiva_meta.RData")

## Training samples and, for each training sample, the articles to
## which trained naive Bayes should be applied to.
file.samples <- file.path(dir.data, "106_010_training_samples_and_prediction_samples.RData")



## Sample size for each training sample.
sp.sz <- 400

## Oversampling factor for withdrawn deals. (Setting this variable to
## one means that no oversampling occurs.)
fac.oversmpl.w <- 1

## How many time intervals (i.e. # training samples - 1) should we
## consider?
time.intervals <- 10
