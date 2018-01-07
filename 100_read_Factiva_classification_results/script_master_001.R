########################################################################
## 
## This script imports the Factiva metadata into R. Furthermore, for
## every takeover deal, it removes duplicate articles.
## 
########################################################################


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "100_read_Factiva_classification_results")
source(file.path(dir.code, "config.R"))

## Read corpus metadata.
factiva <- read.table(file = fname.metadata,
                      header = FALSE,
                      sep = "\t",
                      quote = "\"",
                      dec = ".",
                      col.names = c("HadoopKey", "FactivaDocID", "dealID",
                        "articleSource", "byLine", "Date"),
                      colClasses = NULL, # Surpress conversion
                      fill = TRUE,
                      comment.char = "")
## Fix dates.
factiva$Date <- as.Date(factiva$Date)

## Sanity checks.
if (length(factiva$FactivaDocID) != length(unique(factiva$FactivaDocID)))
  warning("There are non-unique Factiva document IDs. You should investigate this.")
stopifnot(anyDuplicated(factiva$HadoopKey) == 0,
          all(sort(factiva$HadoopKey) == factiva$HadoopKey))

## Remove those articles that do not contain any word (you can find
## out about those articles when the DTM has all-zeros).
pos.e <- which(factiva$HadoopKey %in% emptyHadKey)
if (any(pos.e != as.numeric(emptyHadKey)))
  stop("Ordering of Hadoop keys is messed up.")
if (length(pos.e) > 0)
  factiva <- factiva[-pos.e, ]


## Remove those articles that are the training sample for the naive
## Bayes classifier from Buehlmaier's job market paper.
pos <- grep("^/tmp/RtmpeKIksF/", factiva$FactivaDocID, perl = TRUE)
if (length(pos) > 0) {
  if (any(min(pos):max(pos) != pos))
    stop("Training sample should be in one sequence. Please investigate.")
  factiva <- factiva[-pos, ]
}

## Investigate non-unique Factiva document IDs. If this code section
## does not produce a 'stop', we can be relatively sure that the
## FactivaDocID only pertains to one article.
y <- NULL
d <- which(duplicated(factiva$FactivaDocID))
for (i in d) {
  pos <- which(factiva$FactivaDocID == factiva$FactivaDocID[i])
  y <- rbind(y, factiva[pos, ])

  ## Sanity checks.
  if (length(pos) <= 1) stop("There should be at least two duplicates.")
  concern <- factiva[pos, -which(colnames(factiva) %in% c("HadoopKey", "dealID"))]
  for (j in 2:length(pos))
    if (any(concern[1, ] != concern[j, ]))
      stop("Something is seriously wrong with FactivaDocID or the classification.")
}
print(head(y, 30))

## For each deal, delete duplicate press articles.
rowdel <- NULL
d <- which(duplicated(factiva$FactivaDocID))
## Cycle through duplicated press article indices.
for (i in d) {
  ## For entry 'i', find where all its other duplicate press articles are.
  pos <- which(factiva$FactivaDocID == factiva$FactivaDocID[i])
  ## Find all the deal IDs affected by these duplicate articles.
  deals <- unique(factiva[pos, "dealID"])
  ## Consider each deal separately. (We don't just want to erase all
  ## duplicate articles: some duplicate articles could appear in
  ## separate deals, and those articles should not be removed.)
  for (dl in deals) {
    ## Find duplicate press articles that pertain only to deal 'dl'.
    pos.dl <- intersect(pos, which(factiva$dealID == dl))
    if (length(pos.dl) >= 2)
      rowdel <- c(rowdel, pos.dl[-1]) # Mark all articles for deletion, except the first article.
  }
}
rowdel <- sort(unique(rowdel))
if (length(rowdel) > 0) {
  delDuplHadKeys <- factiva[rowdel, "HadoopKey"] # Back up Hadoop keys of duplicated articles.
  factiva <- factiva[-rowdel, ]                  # Delete duplicated articles.
}

## Set row names to 'automatic' since the Hadoop keys can be used as
## row identifiers.
rownames(factiva) <- NULL

## Save to file and write-protect it.
save(factiva, delDuplHadKeys, file = fname.factiva.meta)
Sys.chmod(fname.factiva.meta, mode = "0400")
