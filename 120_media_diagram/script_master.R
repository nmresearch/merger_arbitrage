#######################################################################
##
## This script draws the media diagram
## 
## TODO: graph media 'backwards' with event time zero being the
## resolution date.
## 
#######################################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "120_media_diagram")
## dir.base <- file.path("D:\\RA\\merger")
## dir.code <- file.path(dir.base, "Code", "120_media_diagram")
dir.data <- file.path(dir.base, "Data")

## Load settings and functions.  
source(file.path(dir.code,"config.R"))

## Load data.frame 'takeoverData'. 
load(file.SDC.complete)
rm(file.SDC.complete)

## Load 'factiva'.
load(file.Factiva)
rm(file.Factiva)
## This is for backwards compatibility when we tried to separately
## predict _C_ompleted and _W_withdrawn deals.
if ("classificationC" %in% names(factiva) & !"classification" %in% names(factiva))
  factiva$classification <- factiva$classificationC

## Add column 'day' to data.frame 'factiva'.
NAs <- numeric(nrow(factiva))
is.na(NAs) <- TRUE
factiva$day <- NAs
rm(NAs) # Clean up.

## Sanity check. This sanity check is useful because later we use
## 'seq_along(takeoverData[, 1])' to cycle through the rows of
## 'takeoverData'.
if (length(unique(takeoverData$dealID)) != nrow(takeoverData))
  stop("takeoverData should contain a unique deal ID in every row.")

## Convert "Date" to "day" (event date), day0="Announcement date" from
## "takeoverData".
for (i in seq_along(takeoverData[, 1])) { # Cycle through _rows_.
  ## Get all row positions in 'factiva' that correspond to the
  ## 'dealID' in 'takeoverData'.
  pos <- which(factiva$dealID == takeoverData[i, "dealID"])
  ## Calculate event dates.
  if (length(pos) > 0)
    factiva[pos, "day"] <- factiva[pos, "Date"] - takeoverData[i, "DA"]
}



## Add deal status to 'factiva'.
NAs <- rep(takeoverData$STAT[1], nrow(factiva)) # Ensure levels are copied.
is.na(NAs) <- TRUE
factiva$STAT <- NAs; rm(NAs)
for (i in seq_along(takeoverData[, 1])) { # Cycle through _rows_.
  pos <- which(factiva$dealID == takeoverData[i, "dealID"])
  if (length(pos) > 0)
    factiva[pos, "STAT"] <- takeoverData[i, "STAT"]
}

## Add stock swap dummy to 'factiva'.
NAs <- rep(takeoverData$SWAP[1], nrow(factiva)) # Ensure levels are copied.
is.na(NAs) <- TRUE
factiva$SWAP <- NAs; rm(NAs)
for (i in seq_along(takeoverData[, 1])) { # Cycle through _rows_.
  pos <- which(factiva$dealID == takeoverData[i, "dealID"])
  if (length(pos) > 0)
    factiva[pos, "SWAP"] <- takeoverData[i, "SWAP"]
}

## Add unsolicited dummy to 'factiva'.
NAs <- rep(takeoverData$UNSOLICITED[1], nrow(factiva)) # Ensure levels are copied.
is.na(NAs) <- TRUE
factiva$UNSOLICITED <- NAs; rm(NAs)
for (i in seq_along(takeoverData[, 1])) { # Cycle through _rows_.
  pos <- which(factiva$dealID == takeoverData[i, "dealID"])
  if (length(pos) > 0)
    factiva[pos, "UNSOLICITED"] <- takeoverData[i, "UNSOLICITED"]
}



## Calculate event times that are both within a given time range
## ('timerange') and where at least one article was published in
## 'factiva'.
time <- sort(unique(factiva[, "day"]))
time <- intersect(time, timerange)


## Calculate event-time average media values based on deal status.
factiva.c <- factiva[which(factiva$STAT == "Completed"), ]
factiva.w <- factiva[which(factiva$STAT == "Withdrawn"), ]
media.c <- media.w <- NA
for (i in seq_along(time)) {
  pos <- which(factiva.c$day == time[i])
  media.c[i] <- mean(factiva.c[pos, "classification"], na.rm = TRUE)
  pos <- which(factiva.w$day == time[i])
  media.w[i] <- mean(factiva.w[pos, "classification"], na.rm = TRUE)    
}
## Draw the media diagram.
plot(time, media.c, type = "l", ylim = c(0,1), xlab = "Event Time", ylab = "Media Measure")
lines(time, media.w, lty = 2)
dev.copy2pdf(file = file.path(dir.code, "fig_media_status.pdf"))



## Calculate event-time average media values based on stock swap
## dummy.
factiva.sy <- factiva[which(factiva$SWAP == "Yes"), ]
factiva.sn <- factiva[which(factiva$SWAP == "No"), ]
media.sy <- media.sn <- NA
for (i in seq_along(time)) {
  pos <- which(factiva.sy$day == time[i])
  media.sy[i] <- mean(factiva.sy[pos, "classification"], na.rm = TRUE)
  pos <- which(factiva.sn$day == time[i])
  media.sn[i] <- mean(factiva.sn[pos, "classification"], na.rm = TRUE)    
}
## Draw the media diagram.
plot(time, media.sy, type = "l", ylim = c(0,1), xlab = "Event Time", ylab = "Media Measure")
lines(time, media.sn, lty = 2)
dev.copy2pdf(file = file.path(dir.code, "fig_media_swap.pdf"))



## Calculate event-time average media values based on whether the deal
## is unsolicited.
factiva.uy <- factiva[which(factiva$UNSOLICITED == "Yes"), ]
factiva.un <- factiva[which(factiva$UNSOLICITED == "No"), ]
media.uy <- media.un <- NA
for (i in seq_along(time)) {
  pos <- which(factiva.uy$day == time[i])
  media.uy[i] <- mean(factiva.uy[pos, "classification"], na.rm = TRUE)
  pos <- which(factiva.un$day == time[i])
  media.un[i] <- mean(factiva.un[pos, "classification"], na.rm = TRUE)    
}
## Draw the media diagram.
plot(time, media.uy, type = "l", ylim = c(0,1), xlab = "Event Time", ylab = "Media Measure")
lines(time, media.un, lty = 2)
dev.copy2pdf(file = file.path(dir.code, "fig_media_unsolicited.pdf"))

