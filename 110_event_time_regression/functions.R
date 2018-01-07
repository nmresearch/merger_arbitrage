

media <- function(f,               # Classified Factiva data.
                  sdc,             # Takeover data.
                  days.inc.DA = 0, # Use articles up to and including DA+days.inc.DA.
                  mn = TRUE) {     # Aggregate using mean (otherwise median).
  ## ###################################################################
  ## 
  ## For every takeover deal in 'sdc', this function creates the
  ## average press article sentiment from the 'f' Factiva data.frame.
  ## 
  ## ###################################################################
  
  ## Verify function arguments.
  stopifnot(nrow(sdc) > 0, class(sdc) == "data.frame",
            nrow(f) > 0, class(f) == "data.frame",
            all(c("dealID", "DA", "DRES") %in% colnames(sdc)),
            all(c("classification", "dealID", "Date") %in% colnames(f)),
            class(days.inc.DA) == "numeric" | class(days.inc.DA) == "integer",
            length(days.inc.DA) == 1)
  
  m <- rep(NA_real_, nrow(sdc)) # Media vector, to be filled.
  for (i in 1:nrow(sdc)) {
    sub1 <-
      if (isTRUE(is.finite(days.inc.DA))) {
        which(f$Date <= sdc[i, "DA"] + days.inc.DA & f$Date <= sdc[i, "DRES"])
      } else {
        which(f$Date <= sdc[i, "DRES"])
      }
    sub2 <- which(f$dealID == sdc[i, "dealID"])
    x <- # Use either mean or median to aggregate articles.
      if (mn) {
        mean(f[intersect(sub1, sub2), "classification"], na.rm = TRUE)
      } else {
        median(f[intersect(sub1, sub2), "classification"], na.rm = TRUE)
      }
    if (isTRUE(is.finite(x)))
      m[i] <- x
  }
  return(m)
}


cumret <- function(df,                        # 'data.frame' containing log-return time series.
                   FF = NULL,                 # Fama-French data.
                   xs = NULL,                 # Excess returns (i.e. which returns to subtract).
                   days.omit.beg = 0,         # Returns start on DA+days.omit.beg.
                   days.include.after = NULL, # Days to include after omitting 'days.omit.beg'.
                   DAs = NULL,                # Announcement dates.
                   lret = TRUE) {             # Output log-returns (not simple returns)?
  ## ###################################################################
  ## 
  ## This function calculates cumulative log-return. If 'FF' is
  ## provided, it calculates cumulative excess returns.
  ## 
  ## ###################################################################

  ## Check function arguments.
  n <- length(unique(df$dealID))
  stopifnot(n > 0, class(df) == "data.frame")
  
  l <- list()
  for (i in 1:n) {
  	l[[i]] <- df[which(df$dealID == unique(df$dealID)[i]), "LOGRET"]
  	attr (l[[i]], "DATE") <- df[which(df$dealID == unique(df$dealID)[i]), "DATE"] }
  	
  if (!is.null(FF))
    stopifnot(all(c("date", "rf", "mktrf", "mktrf_log") %in% colnames(FF)))
  if (!is.null(days.include.after))
    stopifnot(class(days.include.after) == "numeric" || class(days.include.after) == "integer",
              days.include.after == as.integer(days.include.after),
              days.include.after >= 1)
  if (!is.null(xs))
    stopifnot(xs %in% c("mkt", "rf"), !is.null(FF))
  if (days.omit.beg > 0)
    stopifnot(!is.null(DAs))

  ## Helper function that checks whether 'x' is approximately zero.
  is.zero <- function(x) {
    stopifnot(length(x) == 1)
    ret <- FALSE
    if (isTRUE(all.equal(x, 0)))
      ret <- TRUE
    return(ret)
  }
  
  ## Omit a certain number ('days.omit.beg') of days after the
  ## announcement date.
  if (days.omit.beg > 0) {
    for (i in 1:n) {
      pos <- which(attr(l[[i]], "DATE") >= DAs[i] + days.omit.beg)
      if (length(pos) > 0) {
        hlp <- attr(l[[i]], "DATE")[pos]
        l[[i]] <- l[[i]][pos]
        attr(l[[i]], "DATE") <- hlp
      } else {
        l[[i]] <- NA_real_
        attr(l[[i]], "DATE") <- as.Date(NA)
      }
    }
  }
  
  ## If provided, use only the first 'days.include.after' after the
  ## beginning of the time series.
  if (!is.null(days.include.after)) {
    for (i in 1:n) {
      if (days.include.after < length(l[[i]])) {
        hlp <- attr(l[[i]], "DATE")[1:days.include.after]
        l[[i]] <- l[[i]][1:days.include.after]
        attr(l[[i]], "DATE") <- hlp
      }
    }
  }
  
  ret <- rep(NA_real_, n) # Vector that will contain cumulative returns for each deal.
  for (i in 1:n) {
    ## The if statement is necessary because 'sum' returns zero if it
    ## only gets NA and na.rm is set, i.e. 'sum(NA, na.rm=TRUE)' is
    ## zero.
    if (any(!is.na(l[[i]]))) { # Only go ahead if there is at least one non-NA entry in 'l[[i]]'.

      ## Calculate excess returns over market or risk-free rate.
      if (!is.null(xs)) {
        for (k in which(!is.na(l[[i]]))) {
          pos <- which(FF$date == attr(l[[i]], "DATE")[k])
          stopifnot(length(pos) == 1)
          l[[i]][k] <- 
            if (xs == "mkt") {
              ## Should add risk-free rate to mktrf since this is
              ## already subtracted from mktrf.
              l[[i]][k] - log(1 + (FF[pos, "mktrf"] + FF[pos, "rf"]))
            } else if (xs == "rf") {
              l[[i]][k] - log(1 + FF[pos, "rf"])
            }
        }
      }
      
      ret[i] <- sum(l[[i]], na.rm = TRUE) # Calculate cumulative log-returns.
      if (!lret)
        ret[i] <- exp(ret[i]) - 1 # Convert to simple returns.
    }
    ## Consistency check.
    if (is.zero(ret[i])) {
      warning("For i = ", i, ", event-time return is zero, which is unlikely to occur by chance.")
    }
  }
  return(ret)
}



calc.variables <- function(sdc) {
  
  ## ###################################################################
  ## 
  ## This function adds a few variables to the data.frame (based on
  ## already existing variables).
  ## 
  ## ###################################################################
  
  stopifnot(class(sdc) == "data.frame")
  setNA <- function(x) {
    stopifnot(isTRUE(length(x) > 0))
    is.na(x[which(!is.finite(x))]) <- TRUE
    return(x)
  }
  ep <- 0.05
  fixed <- function(x, eps = ep) {
    stopifnot(isTRUE(length(x) > 0))
    x[which(x <= 0)] = eps
    return(x)
  }
  log.fix <- function(x, eps = ep) {
    return(log(fixed(x, eps)))
  }
  
  ## Take care of deal duration.
  sdc$DAYS <- as.numeric(sdc$DAYS)
  sdc$logDAYS <- log(sdc$DAYS)
  sdc$logDAYS <- setNA(sdc$logDAYS)
  
  ## Company size.
  sdc$Target.size.CCM <- sdc$Target.PRCCQ.CCM * sdc$Target.CSHOQ.CCM
  sdc$Acquiror.size.CCM <- sdc$Acquiror.PRCCQ.CCM * sdc$Acquiror.CSHOQ.CCM
  
  ## Book-to-market value.
  sdc$TBM <- sdc$Target.CEQQ.CCM / sdc$Target.size.CCM
  sdc$ABM <- sdc$Acquiror.CEQQ.CCM / sdc$Acquiror.size.CCM
  sdc$TBM.fixed <- fixed(sdc$TBM)
  sdc$ABM.fixed <- fixed(sdc$ABM)
  sdc$TBM_log <- log.fix(sdc$TBM)
  sdc$ABM_log <- log.fix(sdc$ABM)
  
  ## Cash to total assets.
  sdc$TCash <- sdc$Target.CHQ.CCM / sdc$Target.ATQ.CCM
  sdc$TCashE <- sdc$Target.CHEQ.CCM / sdc$Target.ATQ.CCM
  sdc$ACash <- sdc$Acquiror.CHQ.CCM / sdc$Acquiror.ATQ.CCM
  sdc$ACashE <- sdc$Acquiror.CHEQ.CCM / sdc$Acquiror.ATQ.CCM

  ## Log(cash/total assets)
  sdc$TCash_log <- log.fix(sdc$TCash)
  sdc$TCashE_log <- log.fix(sdc$TCashE)
  sdc$ACash_log <- log.fix(sdc$ACash)
  sdc$ACashE_log <- log.fix(sdc$ACashE)

  sdc$TCashE_log <- setNA(sdc$TCashE_log)
  sdc$ACashE_log <- setNA(sdc$ACashE_log)

  sdc$premium <- log(sdc$PR/sdc$PR4WK)
  sdc$TDARet <- log(sdc$TPRDAY/sdc$PR4WK)
  sdc$ADARet <- log(sdc$APRDAY/sdc$AC4WK)
  
  return(sdc)
}





annc.d.ret <- function(df) {
  ## ###################################################################
  ## 
  ## Calculate announcement date returns.
  ## 
  ## ###################################################################
  stopifnot(nrow(df) > 0, class(df) == "data.frame")
  r <- rep(NA_real_, length(unique(df$dealID)))
  for (i in 1:length(unique(df$dealID))) {
  	hlp	<- df[which(df$dealID == unique(df$dealID)[i]), "LOGRET"]
    pos <- which(is.finite(hlp))
    if (length(pos) > 0)
      r[i] <- hlp[min(pos)]
  }
  return(r)
}



coef.W <- function(formula, # Formula.
                   data) {  # data.frame.
  ## ###################################################################
  ## 
  ## Calculate matrix containing coefficients and White standard errors.
  ## 
  ## ###################################################################
  require("lmtest")
  require("sandwich")
  x <- lm(formula, data)
  coeffMatrix <- coeftest(x, vcov = function(...) vcovHC(..., type = "HC0"))
  return(round(coeffMatrix, digits = 2))
}



