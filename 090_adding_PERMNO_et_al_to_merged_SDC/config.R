
dir.data <- file.path(dir.base, "Data")


## This file contains the old (2012-02-11) SDC data from dir 035_ that
## has added several firm identifiers to it, like PERMNO and PERMCO.
file.SDC.with.CUSIP9 <- file.path(dir.data, "035_030_SDC_data_with_CUSIP9.RData")

## This is the merged SDC data.frame from dir 006_.
file.SDC.merged <- file.path(dir.data, "006_040_SDC_merged.RData")

## This file is the output of dir 090_. In particular, it contains the
## SDC data downloaded on 2012-03-07 and in addition has various firm
## identifiers (e.g. PERMNO).
file.SDC.complete <- file.path(dir.data, "090_010_SDC_complete.RData")
