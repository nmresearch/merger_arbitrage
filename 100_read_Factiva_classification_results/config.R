dir.data <- file.path(dir.base, "Data")

## File containing the corpus metadata from Factiva.
fname.metadata <- file.path(dir.data, "100_010_factiva_metadata.txt")

## File containing the classification results.
fname.classification <- file.path(dir.data, "100_020_factiva_prob.txt")

## File containing only the meta data from Factiva.
fname.factiva.meta <- file.path(dir.data, "100_025_factiva_meta.RData")

## File containing both the Factiva metadata and also the
## classification.
fname.factiva <- file.path(dir.data, "100_030_factiva.RData")



## Hadoop keys where the document was empty, i.e. the probability
## produced by naive Bayes is not very meaningful (DTM contains only
## zeros).
emptyHadKey <- c("0000048305",
                 "0000053632",
                 "0000061751",
                 "0000087160",
                 "0000096896")


