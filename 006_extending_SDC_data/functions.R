select.sample.SDC <- function(df) {
  
  ## ##################################################################
  ## 
  ## This function applies a few sample selection criteria to the
  ## data.frame 'df' that contains the SDC takeover data. After sample
  ## selection, this function sets the row.names of 'df' to
  ## 'automatic.
  ## 
  ## ##################################################################

  ## Basic sanity check of function argument.
  stopifnot(class(df) == "data.frame")
  
  ## Remove those deals where there is no CUSIP for either acquirer or
  ## target.
  row.remove <- NULL
  for (i in seq_along(df[, 1])) { # Cycle through rows.
    if (is.na(df[i, "ACU"]) | is.na(df[i, "TCU"]))
      row.remove <- c(row.remove, i)
  }
  if(length(row.remove) > 0)
    df <- df[-row.remove, ]

  ## Remove those deals where acquirer and target CUSIPs are the
  ## same. These are self-tenders or recapitalizations and we are not
  ## interested in them.
  row.remove <- NULL
  for (i in seq_along(df[, 1])) { # Cycle through rows.
    if (df[i, "ACU"] == df[i, "TCU"])
      row.remove <- c(row.remove, i)
  }
  if (length(row.remove) > 0)
    df <- df[-row.remove, ]

  ## Only keep completed and withdrawn deals.
  pos <- union(which(df$STAT == "Completed"), which(df$STAT == "Withdrawn"))
  pos <- sort(unique(pos))
  df <- df[pos, ]
  
  ## In addition to withdrawn deals, include only those completed
  ## deals where the acquirer owns more than 50% of the target shares
  ## following the merger.
  rows <- which(df$PCTOWN > .50)
  rows <- union(rows, which(df$STAT == "Withdrawn")) # Don't throw away the withdrawn deals.
  rows <- sort(unique(rows))
  df <- df[rows, ]

  ## In addition to withdrawn deals, include only those completed deals
  ## where the acquirer purchases at least 20% of the outstanding
  ## shares.
  rows <- which(df$PCTACQ >= .20)
  rows <- union(rows, which(df$STAT == "Withdrawn")) # Don't throw away the withdrawn deals.
  rows <- sort(unique(rows))
  df <- df[rows, ]

  ## Remove deals with unknown deal value.
  pos.remove <- which(is.na(df$VAL))
  if (length(pos.remove) > 0)
    df <- df[-pos.remove, ]
  
  ## Remove deals with missing announcement date.
  pos.remove <- which(is.na(df$DA))
  if (length(pos.remove) > 0)
    df <- df[-pos.remove, ]
  
  row.names(df) <- NULL # Set to 'automatic'.
  
  return(df)
}



res.dt <- function(DE,   # Date Effective.
                   DW) { # Date Withdrawn.

  ## ##################################################################
  ## 
  ## This function calculates the resolution date, which is either the
  ## date withdrawn or the date effective.
  ## 
  ## ##################################################################
  
  ## Sanity check of function arguments.
  stopifnot(class(DE) == "Date",
            class(DW) == "Date", 
            length(DE) == length(DW),
            length(DE) >= 1)
  
  ## Initialize resolution date with NA.
  DRES <- rep(as.Date(NA), length(DE))
  ## Fill it up with either the effective date or the announcement
  ## date.
  for (i in seq_along(DE)) {
    if (is.finite(DE[i]) & is.finite(DW[i])) # Sanity check.
      stop("It cannot be that there is both a DE and a DW.")
    ## Note in the following that it's also possible that both DE and
    ## DW are NA. That's why we check for both separately.
    if (is.finite(DE[i]))
      DRES[i] <- DE[i]
    if (is.finite(DW[i]))
      DRES[i] <- DW[i]
  }
  return(DRES)
}


conv.to.ID <- function(x, lgth = 12) {

  ## ##################################################################
  ## 
  ## This function converts a firm identifier to a fixed-length
  ## string. If you want to convert a vector (and maybe in addition
  ## also want to change the length), you should use something like
  ## this:
  ## 
  ## unlist(lapply(CUSIP, function(x) conv.to.ID(x, 9)))
  ## 
  ## ##################################################################
  
  ## Sanity check of function argument.
  stopifnot(length(x) == 1)
  
  if (class(x) == "numeric" | class(x) == "integer")
    x <- as.character(x)
  stopifnot(class(x) == "character") # At the latest by now we need 'character'.
  zeros <- paste(rep("0", lgth - nchar(x)), collapse = "")
  return(paste(zeros, x, sep = ""))
}



read.col.names <- function(fname) {
  ## ##################################################################
  ## This function reads SDC column names from a tab-separated text
  ## file. The desired column names are in the second column. (The
  ## first column is from the output of SDC as in the Excel file --
  ## for some reason SDC doesn't use its internal variables names in
  ## the Excel output.)
  ## ##################################################################
  
  colnam <- read.table(file = fname,
                       header = FALSE,
                       sep = "\t",
                       quote="\"",
                       dec = ".",
                       colClasses = "character", # Surpress conversion
                       fill = TRUE,
                       comment.char = "")
  return(colnam[, 2])
}


read.SDC.df <- function(fname) {
  ## ##################################################################
  ## This function reads the tab-separated text file containing SDC
  ## raw data and returns a data.frame. This data.frame's columns are
  ## all of class 'character', so later conversion into other classes
  ## might be necessary.
  ## ##################################################################
  
  ## Sanity check of function argument.
  stopifnot(class(fname) == "character",
            file.exists(fname))
  
  ## Read in tab-separated text file containing SDC data.
  SDC.txt <- readLines(fname)

  ## Delete "empty" lines consisting only of tabs.
  lines.del <- grep("^\t+$", SDC.txt, perl = TRUE)
  lines.del <- sort(unique(lines.del))
  if (length(lines.del) > 0)
    SDC.txt <- SDC.txt[-lines.del]

  ## Cut off SDC session details at bottom of file, in case they
  ## exist.
  footerCutoff <- grep("session details", SDC.txt,
                       ignore.case = TRUE, perl = TRUE)
  lgth <- length(footerCutoff)
  stopifnot(lgth %in% c(0, 1)) # Either there is one or none footer.
  if (lgth == 1) {
    stopifnot(1 <= footerCutoff - 1) # Avoid unintended results from following line.
    SDC.txt <- SDC.txt[1:(footerCutoff - 1)] # Remove footer.
  }

  ## Save preprocessed CSV file to a temporary file 'fname.tmp'.
  fname.tmp <- paste(fname, "_temp", sep = "")
  stopifnot(!file.exists(fname.tmp)) # Don't accidentally overwrite other file.
  writeLines(text = SDC.txt, con = fname.tmp)
  ## Read preprocessed CSV file (from temporary file).
  df <- read.table(file = fname.tmp,
                   header = TRUE,
                   sep = "\t",
                   quote="\"",
                   dec = ".",
                   colClasses = "character", # Surpress conversion
                   fill = TRUE,
                   comment.char = "")
  ## Remove temporary file.
  file.remove(fname.tmp)
  return(df)
}




clean.SDC <- function(df) {
  ## ##################################################################
  ## This function does a basic cleanup of the data.frame 'df'.
  ## ##################################################################
  
  ## Basic sanity checks.
  stopifnot(class(df) == "data.frame")
  for (i in seq_along(df[1, ])) { # Cycle through columns of 'df'.
    stopifnot(class(df[, i]) == "character") # Classes of 'df' cols should not yet be converted.
  }


  
  ## Remove leading and trailing spaces.
  for (i in seq_along(df[1, ])) { # Traverse through columns of 'df'.
    df[, i] <- gsub("^[ ]*", "", df[, i], perl = TRUE)
    df[, i] <- gsub("[ ]*$", "", df[, i], perl = TRUE)
  }

  ## Remove duplicate columns.
  nam <- colnames(df)
  dcols <- which(duplicated(nam))
  ## Before removing, verify whether also the contents of the columns
  ## are identical (not just the column names). If column contents
  ## differ, the following loop throws an error.
  for (i in dcols) {
    dr <- grep(paste("^", nam[i], "$", sep=""), nam, perl = TRUE)
    stopifnot(length(dr) >= 2) # There should be at least two duplicates.
    for (k in 2:length(dr)) {
      stopifnot(identical(df[, dr[1]], df[, dr[k]]))
    }
  }
  if (length(dcols) > 0)
    df <- df[, -dcols]

  ## Deal with missing values.
  for (i in seq_along(df[1, ])) { # Traverse through columns of 'df'.
    rowpos <- which(df[, i] == "")
    if (length(rowpos) > 0)
      is.na(df[rowpos, i]) <- TRUE
  }
  
  return(df)
}
