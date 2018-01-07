
#######################################################################
##
## This program assigns 8-digit and 9-digit CUSIPs to the takeover
## data.frame 'SDC.data.PERMCO'.
##
#######################################################################


dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "035_add_CUSIP8_and_CUSIP9_to_SDC_data")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.code, "config.R"))
source(file.path(dir.base, "merger-arbitrage",
                 "function_CUSIP_conversion.R"))

## Load 'SDC.data.PERMCO'.
load(file.SDC.with.PERMCOs)
rm(file.SDC.with.PERMCOs) # Clean up.
## Use less complicated name for SDC data.frame (but revert to old
## name before saving it to file below to have consistent naming
## conventions).
dfSDC <- SDC.data.PERMCO
rm(SDC.data.PERMCO) # Make sure to not accidentally use it.
## Reset row names to automatic, otherwise things can get confusing.
row.names(dfSDC) <- NULL


## Some useful abbreviations, taking into the account that each
## observation has firm identifiers for both acquirer and target.
SDC.names.CUSIP6 <- c("Acquiror_CUSIP", "Target_CUSIP")
SDC.names.CUSIP8 <- c("Acquiror_CUSIP8", "Target_CUSIP8")
SDC.names.CUSIP9 <- c("Acquiror_CUSIP9", "Target_CUSIP9")
SDC.names.PERMNO <- c("Acquiror_PERMNO", "Target_PERMNO")
SDC.names.PERMCO <- c("Acquiror_PERMCO", "Target_PERMCO")
SDC.names.Company <- c("Acquiror_Name", "Target_Name")


#######################################################################
##
## First part of the program: Reading in the CRSP identifiers from
## the WRDS web query, and extract CUSIP8
##
#######################################################################
Identifiers <-
  read.table(file = file.CRSP.Ident,
             header = TRUE,
             sep = "\t",
             quote="",
             dec = ".",
             colClasses = "character", # Surpress conversion
             fill = TRUE,
             comment.char = "") # Turn off interpretation of comments.
## Convert dates.
Identifiers[, "DATE"] <-
  as.Date(strptime(Identifiers[, "DATE"], "%Y%m%d"))

write.table(unique(c(Identifiers[, "NCUSIP"])),
            file = file.CUSIP8.SDC,
            quote = FALSE, sep = "",
            row.names = FALSE, col.names = FALSE)
Sys.chmod(file.CUSIP8.SDC, mode = "0400") # Write-protect file.



#######################################################################
##
## Sencond part of the program: Here I'm trying to assign the CUSIP8's
## and CUSIP9's to the takeover data.frame.
##
#######################################################################


## Add empty columns for 8-digit CUSIPs to 'dfSDC'. These will
## later be filled up with matching 8-digit CUSIPs.
NAs <- character(nrow(dfSDC))
is.na(NAs) <- TRUE
NAs <- as.data.frame(cbind(NAs, NAs), stringsAsFactors = FALSE)
names(NAs) <- SDC.names.CUSIP8
dfSDC <- cbind(dfSDC, NAs)
rm(NAs) # Clean up.

## Try to find matching 8-digit CUSIPs.
for (j in seq_along(SDC.names.CUSIP6)) { # Deal with both acquirer and target.
  for (i in which(!is.na(dfSDC[, SDC.names.CUSIP6[j]]))) {
    
    pos.CUSIP <- grep(dfSDC[i, SDC.names.CUSIP6[j]],
                      Identifiers[, "NCUSIP"],
                      perl = TRUE)
    pos.PERMNO <- grep(dfSDC[i, SDC.names.PERMNO[j]],
                       Identifiers[, "PERMNO"],
                       perl = TRUE)
    pos <- intersect(pos.CUSIP, pos.PERMNO)
    
    if (length(pos) == 1)
      dfSDC[i, SDC.names.CUSIP8[j]] <- Identifiers[pos, "NCUSIP"]
    
    if (length(pos) > 1) {
      hlp.df <- Identifiers[pos, ]
      
      ## Order 'hlp' along DATE.
      if (names(hlp.df)[1] != "DATE")
        stop("Ordering of the column names has changed.")
      hlp.df <- hlp.df[do.call(order, hlp.df), ]
      
      ## Only consider those entries that occur before the deal's
      ## announcement date.
      hlp.df <- hlp.df[which(hlp.df$DATE <= dfSDC[i, "Date.Announced"]), ]
      
      if (nrow(hlp.df) > 0) {
        ## Chose most current date.
        CUSIP8.candidate <- hlp.df$NCUSIP[nrow(hlp.df)]
        if (is.na(CUSIP8.candidate)) stop("It's NA.")
        dfSDC[i, SDC.names.CUSIP8[j]] <- CUSIP8.candidate
        rm(CUSIP8.candidate) # Defensive programming.
      }
      rm(hlp.df) # Defensive programming.
    }
    
  } # Close inner 'for' loop.
} # Close outer 'for' loop.


## dfSDC[, c(SDC.names.CUSIP6, SDC.names.CUSIP8)]
## is.na(dfSDC[, SDC.names.CUSIP8]) <- TRUE # Reset.

## Where was the procedure above not successful?
pos.missing <- NULL
for (j in 1:2) {
  pos.missing <-
    append(pos.missing, 
           intersect(which(is.na(dfSDC[, SDC.names.CUSIP8[j]])), 
                     which(!is.na(dfSDC[, SDC.names.CUSIP6[j]]))))
}
## Show which entries still contain NA's.
dfSDC[pos.missing, c(SDC.names.Company,
                               SDC.names.CUSIP6,
                               SDC.names.CUSIP8,
                               SDC.names.PERMCO)]

## Manually fix the missing entries.


#######################################################################
##
## Fourth part of the program: Convert CUSIP8 to CUSIP9.
##
#######################################################################



## Add empty columns for 9-digit CUSIPs to the takeover data.frame.
NAs <- character(nrow(dfSDC))
is.na(NAs) <- TRUE
NAs <- as.data.frame(cbind(NAs, NAs), stringsAsFactors = FALSE)
names(NAs) <- SDC.names.CUSIP9
dfSDC <- cbind(dfSDC, NAs)
rm(NAs) # Clean up.

for (i in 1:2) {
  dfSDC[, SDC.names.CUSIP9[i]] <-
    add.check.digit(dfSDC[, SDC.names.CUSIP8[i]])
}

## Provide a visual overview.
print(dfSDC[, c(SDC.names.CUSIP6,
                          SDC.names.PERMCO,
                          SDC.names.CUSIP9)])

## Save the takeover data.frame to file.
SDC.data.PERMCO <- dfSDC # Be consistent with earlier naming convention.
save(SDC.data.PERMCO, file = file.SDC.with.CUSIP9)
Sys.chmod(file.SDC.with.CUSIP9, mode = "0400") # Write-protect file.


