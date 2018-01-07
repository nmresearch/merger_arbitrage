
## ======================================================================
##
## This program parses a fixed-width text file that has been
## downloaded from SDC and stores it in a data.frame. It then applies
## several data inclusion criteria, e.g. the announcement date has to
## be within a given date range (the remaining criteria in the program
## below are self-explanatory). Finally, this program saves its output
## both as tab-delimited text file and as a .RData file that contains
## the resulting data.frame 'takeoverData'.
##
## To run this file as a batch job, change ('cd') into the directory
## where this file is saved and type the following command into the
## shell:
##
## R -q --no-save --no-restore --slave <script_master.R >script_master.Rout 2>&1 &
## 
## ======================================================================


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "005_importing_SDC_data_into_R")

## Load configuration options and functions.
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Column positions of both the acquirer and the target name.
col.ta.ac <- c(col.ta, col.ac)

## Make the column names more suitable for R and/or SQLite.
col.names <- gsub(" ", "_", col.names, perl = TRUE) # Replace space by '_'.
col.names <- gsub("[.]", "", col.names, perl = TRUE) # Remove '.'.
col.names <- gsub("[$]", "USD", col.names, perl = TRUE) # Replace '$' by 'USD'.
col.names <- gsub("percent", "pct", col.names, ignore.case = TRUE, perl = TRUE) # Replace 'percent' by 'pct'.
col.names <- gsub("Pct", "pct", col.names, perl = TRUE) # Replace 'Pct' by 'pct'.
col.names <- gsub("[%]", "pct", col.names, perl = TRUE) # Replace '%' by 'pct'.
col.names <- gsub("[()]", "", col.names, perl = TRUE) # Remove brackets.
col.names <- gsub("[/]", "_", col.names, perl = TRUE) # Replace '/' by '_'.
col.names <- gsub("[-]", "_", col.names, perl = TRUE) # Replace '-' by '_'.


## Read the file line-by-line and determine possible cutoff positions
## for fixed-width format. If a column in the fixed-width file does
## not contain any content (i.e. only spaces), it will not be included
## in the cutoff positions (and thus this column will later in this
## program be removed).
f1 <- file(file.SDC.fwf, open = "r") # Open file connection.
pos.cut <- c(min(col.ta)-1, max(col.ta), # Initialize w/ cols containing acquirer and target names.
             min(col.ac)-1, max(col.ac))
repeat {
  s <- readLines(f1, n = 1) # Read next line from file connection.
  if (length(s) == 0) break # Test if reading the file is finished.
  sp <- unlist(strsplit(s, "")) # Split version of 's'.

  for (i in 2:(length(sp)-1)) {
    if (sp[i-1] == " " & sp[i] == " " & sp[i+1] != " ") {
      if (! i %in% col.ta.ac) { # Only add it if we're not dealing with names cols.
        pos.cut <- c(pos.cut, i)
      }
    }
  }

  pos.cut <- unique(pos.cut)
}
close(f1) # Close file connection.
pos.cut <- sort(pos.cut) # Ensure cutoff positions are sorted.


## ## Roughly check if the cutoff works.
## for (i in seq_along(pos.cut)) {
##   if (i == 1)
##     cat(paste(sp[1:pos.cut[1]], collapse = ""), "\n")
##   else if (i < length(pos.cut))
##     cat(paste(sp[(pos.cut[i-1]+1):pos.cut[i]], collapse = ""), "\n")
##   else if (i == length(pos.cut))
##     cat(paste(sp[(pos.cut[length(pos.cut)]+1):length(sp)], collapse = ""), "\n")
## }

## Determine the width of the various columns.
w <- NULL
for (i in seq_along(pos.cut)) {
  if (i == 1) {
    w <- pos.cut[1]
  } else {
    w <- c(w, pos.cut[i] - pos.cut[i-1])
  }
}
w <- c(w, length(sp)-pos.cut[length(pos.cut)])
if (sum(w) != length(sp))
  stop("Something went wrong in the width calculation.")

## Read the fixed-width file into a data.frame.
df <- read.fwf(file.SDC.fwf, width = w, colClasses = "character")
colnames(df) <- col.names

## Remove leading and trailing spaces.
for (i in seq_along(df[1, ])) { # Traverse through columns of 'df'.
  df[, i] <- gsub("^[ ]*", "", df[, i], perl = TRUE)
  df[, i] <- gsub("[ ]*$", "", df[, i], perl = TRUE)
}

## Deal with missing values.
for (i in seq_along(df[1, ])) { # Traverse through columns of 'df'.
  rowpos <- which(df[, i] == "")
  if (length(rowpos) > 0)
    is.na(df[rowpos, i]) <- TRUE
}

## Convert CIDGEN to ten digits character string (SDC claims that they
## should be nine digits, but I found at least one with ten digits in
## the data.).
df$Acquiror_CIDGEN <- unlist(lapply(df$Acquiror_CIDGEN, function(x) conv.to.ID(x, 10)))
df$Target_CIDGEN <- unlist(lapply(df$Target_CIDGEN, function(x) conv.to.ID(x, 10)))

## Add randomly-created (but unique) list of deal IDs. This is later
## useful for merging data with the downloaded press articles from
## Factiva.
df$dealID <- genIDvec(nrow(df))








## Remove those deals where there is no CUSIP for either acquirer or
## target.
row.remove <- NULL
for (i in seq_along(df[, 1])) { # Cycle through rows.
  if (is.na(df[i, "Acquiror_CUSIP"]) | is.na(df[i, "Target_CUSIP"]))
    row.remove <- c(row.remove, i)
}
if(length(row.remove))
  df <- df[-row.remove, ]

## Remove those deals where acquirer and target CUSIPs are the
## same. These are self-tenders or recapitalizations and we are not
## interested in them.
row.remove <- NULL
for (i in seq_along(df[, 1])) { # Cycle through rows.
  if (df[i, "Acquiror_CUSIP"] == df[i, "Target_CUSIP"])
    row.remove <- c(row.remove, i)
}
if (length(row.remove))
  df <- df[-row.remove, ]

## Extract deals that are completed or withdrawn. It's imortant to do
## this before conversion to class 'factor' since otherwise you have
## to relevel the factors.
pos <- sort(unique(union(which(df[, "Status"] == "Completed"),
                         which(df[, "Status"] == "Withdrawn"))))
df <- df[pos, ]

## Convert to class 'Date'.
for (i in cols.date)
  df[, i] <- as.Date(df[, i], format = date.format)

## Sort deals according to announcement date (SDC should already
## return sorted queries, but it's better to double check and be on
## the safe side).
if (any(is.na(df[, "Date_Announced"])))
  stop("There are deals without any announcement date. Sorting might be problematic.")
perm <- order(df[, "Date_Announced"]) # Permutation used for sorting.
if (any(perm != sort(perm)))
  warning("Usually the SDC data is sorted, but here it's not.")
df <- df[perm, ] # Actually this should not be necessary.
row.names(df) <- NULL # Set to 'automatic' row names.

## Convert to class 'numeric'.
cols <- seq_along(df[1, ])
cols <- setdiff(cols, cols.date) # Don't convert dates.
cols <- setdiff(cols, cols.id) # Don't convert firm IDs.
for (i in cols) {
  ## Only convert those columns that don't contain alphabetic symbols.
  if (!any(grepl("[a-zA-z]", df[, i], perl = TRUE))) {
    df[, i] <- gsub("([0-9]),([0-9]{3})", "\\1\\2", df[, i], perl=TRUE) # Remove thousands separator.
    df[, i] <- as.numeric(df[, i])
  }
}

## Calculate the resolution date. It's important that this happens
## after the above conversion to numeric. Otherwise, the resolution
## will be converted to class 'numeric', which will result in NAs.
df$Date_Resolution <-
  get.res.dt(df$Date_Effective, df$Date_Withdrawn)
## ## Check whether resolution date was calculated correctly.
## print(df[, c("Date_Effective", "Date_Withdrawn", "Date_Resolution")])




## In addition to withdrawn deals, include only those completed deals
## where the acquirer owns at least 51% of the target shares following
## the merger.
rows <- which(df[, "pct_Owned_After_Transaction"] >= .51)
rows <- union(rows, which(df[, "Status"] == "Withdrawn")) # Don't throw away the withdrawn deals.
rows <- sort(unique(rows))
df <- df[rows, ]

## In addition to withdrawn deals, include only those completed deals
## where the acquirer purchases at least 20% of the outstanding
## shares.
rows <- which(df[, "pct_of_Shares_Acq"] >= .20)
rows <- union(rows, which(df[, "Status"] == "Withdrawn")) # Don't throw away the withdrawn deals.
rows <- sort(unique(rows))
df <- df[rows, ]

## Remove deals with unknown deal value.
pos.remove <- which(is.na(df[, "Value_of_Transaction_USDmil"]))
if (length(pos.remove) > 0)
  df <- df[-pos.remove, ]

## Extract deals from a given date range.
pos <- sort(unique(intersect(which(dt.bg <= df[, "Date_Announced"]),
                             which(df[, "Date_Announced"] <= dt.ed))))
df <- df[pos, ]

## Extract the largest deals.
if (nrow(df) < no.lgst.dls)
  stop("There are not enough deals in the data.")
perm <- order(df[, "Value_of_Transaction_USDmil"]) # Permutation for ordering.
perm <- perm[(length(perm)-no.lgst.dls+1):length(perm)] # Extract positions of largest deals.
pos <- sort(perm) # Don't mess up the given ordering of 'df', just extract the largest deals.
df <- df[pos, ]

## Convert to class 'factor'.
for (i in cols.factor)
  df[, i] <- as.factor(df[, i])
## Change contrasts (at some later point in time this will be useful
## for running GLM with 'Status' as the dependent variable).
df$Status <- relevel(df$Status, ref = "Withdrawn")

## ## Obtain some information about the data.
## hist(df[, "Date_Announced"], breaks = 2*round(as.numeric(dt.ed-dt.bg)/365))
## summary(df[, cols.factor])

## Save results to Excel-readable format (i.e. tab-separated text
## file).
write.table(df,
            file = file.SDC.cleaned,
            quote = FALSE, sep = "\t",
            row.names = FALSE, col.names = TRUE)
Sys.chmod(file.SDC.cleaned, mode = "0400") # Write-protect file.

## Save result in R format.
takeoverData <- df # Use a more familiar name for the data.frame.
save(takeoverData, file = file.SDC.df.cleaned)
Sys.chmod(file.SDC.df.cleaned, mode = "0400") # Write-protect file.
