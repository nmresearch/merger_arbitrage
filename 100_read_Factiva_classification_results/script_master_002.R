########################################################################
## 
## This script adds the classification results to the Factiva
## metadata.
## 
########################################################################


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "100_read_Factiva_classification_results")
source(file.path(dir.code, "config.R"))

## Load the Factiva metadata.
load(fname.factiva.meta)
meta <- factiva; rm(factiva)

## Read classification probabilities.
cl.prob <- read.table(file = fname.classification,
                      header = FALSE,
                      sep = "\t",
                      quote = "\"",
                      dec = ".",
                      col.names = c("HadoopKey", "classification"),
                      colClasses = NULL, # Surpress conversion
                      fill = TRUE,
                      comment.char = "")
## Convert classification probability to class 'numeric'.
cl.prob$classification <- as.numeric(cl.prob$classification)

## Delete duplicate press articles (the duplicates are calculted in
## the previous step when the Factiva meta data was imported).
rowdel <- which(cl.prob$HadoopKey %in% delDuplHadKeys)
if (length(rowdel) > 0)
  cl.prob <- cl.prob[-rowdel, ]

## Delete metadata when there is no classification result.
rowdel <- which(!meta$HadoopKey %in% cl.prob$HadoopKey)
rowdel <- sort(unique(rowdel))
if (length(rowdel) > 0)
  meta <- meta[-rowdel, ]

## Sanity checks.
stopifnot(nrow(meta) == nrow(cl.prob),
          all(meta$HadoopKey == cl.prob$HadoopKey))

## Merge Factiva metadata with classification results.
factiva <- cbind(meta, cl.prob[, 2])
colnames(factiva) <- c(colnames(meta), colnames(cl.prob)[2])

## Set row names to 'automatic' since the Hadoop keys serve as row
## identifiers.
rownames(factiva) <- NULL

## Save to file and write-protect it.
save(factiva, file = fname.factiva)
Sys.chmod(fname.factiva, mode = "0400") 
