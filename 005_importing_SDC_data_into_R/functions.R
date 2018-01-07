

get.res.dt <- function(dt.eff,  # Date Effective.
                       dt.wn) { # Date Withdrawn.

  ## ==================================================================
  ## 
  ## This function calculates the resolution date, which is either the
  ## date withdrawn or the date effective.
  ## 
  ## ==================================================================
  
  dt.res <- as.Date("2000-01-01"); is.na(dt.res) <- TRUE
  for (i in seq_along(dt.eff)) {
    dt.res[i] <-
      if (!is.na(dt.eff[i])) {
        dt.eff[i]
      } else {
        dt.wn[i]
      }
  }
  return(dt.res)
}



conv.to.ID <- function(x, lgth = 12) {

  ## ==================================================================
  ## 
  ## This function converts a firm identifier to a fixed-length
  ## string. If you want to convert a vector (and maybe in addition
  ## also want to change the length), you should use something like
  ## this:
  ## 
  ## unlist(lapply(CUSIP, function(x) conv.to.ID(x, 9)))
  ## 
  ## ==================================================================

  if (length(x) != 1)
    stop("Length of argument should be one.")
  
  if (class(x) == "numeric")
    x <- as.character(x)
  zeros <- paste(rep("0", lgth - nchar(x)), collapse = "")
  return(paste(zeros, x, sep = ""))
}




genIDvec <- function(n,               # Number of IDs to generate.
                     id.length = 8) { # Number of digits of each ID.

  ## ==================================================================
  ## 
  ## This function generates a list with unique alphabetic IDs.
  ## 
  ## ==================================================================

  IDs <- NULL
  repeat {
    id.cand <- paste(letters[runif(id.length, 1, 26)], collapse = "") # candidate
    if (!id.cand %in% IDs)
      IDs <- c(IDs, id.cand)
    if (length(IDs) == n)
      break
  }
  if (any(is.na(IDs)))
    stop("Something went wrong b/c we got NAs.")
  return(IDs)
}

