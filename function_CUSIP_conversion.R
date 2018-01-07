## This helper function returns the position of a character in the
## alphabet. There is probably a more efficient way to do this in R,
## but at least this function works.
pos.alphabet <- function(x) {
  if (!grepl("^[A-Za-z]$", x, perl = TRUE)) # Check if function argument is OK.
    stop("Function argument should be a single alphabetic character.")
  x <- tolower(x)
  if (x == "a") return(1)
  if (x == "b") return(2)
  if (x == "c") return(3)
  if (x == "d") return(4)
  if (x == "e") return(5)
  if (x == "f") return(6)
  if (x == "g") return(7)
  if (x == "h") return(8)
  if (x == "i") return(9)
  if (x == "j") return(10)
  if (x == "k") return(11)
  if (x == "l") return(12)
  if (x == "m") return(13)
  if (x == "n") return(14)
  if (x == "o") return(15)
  if (x == "p") return(16)
  if (x == "q") return(17)
  if (x == "r") return(18)
  if (x == "s") return(19)
  if (x == "t") return(20)
  if (x == "u") return(21)
  if (x == "v") return(22)
  if (x == "w") return(23)
  if (x == "x") return(24)
  if (x == "y") return(25)
  if (x == "z") return(26)
}


## This function takes as an argument a character vector containing
## 8-digit CUSIPs. It returns a character vector containing 9-digit
## CUSIPs. It does this by adding the CUSIP check digit, as described
## at http://en.wikipedia.org/wiki/CUSIP.
add.check.digit <- function(CUSIP8) {

  ## Sanity check.
  if (class(CUSIP8) != "character")
    stop("Function argument should be of class 'character'.")
  
  CUSIP9 <- character()
  for (k in seq_along(CUSIP8)) { # Cycle through the CUSIP8 vector.
    if (is.na(CUSIP8[k])) {
      CUSIP9[k] <- "empty" # Assign something.
      is.na(CUSIP9[k]) <- TRUE # Return 'NA'.
    } else {
      
      ## Sanity check.
      if (length(unlist(strsplit(CUSIP8[k], split = ""))) != 8)
        stop("The number of input digits is different than eight.")
      
      sum.cusip <- 0
      for (i in 1:8) { # Cycle through the 8 digits of the current CUSIP.
        c <- unlist(strsplit(CUSIP8[k], split = ""))[i] # Get i'th character.
        if (grepl("^[0-9]$", c, perl = TRUE)) # Is it a digit?
          v <- as.numeric(c)
        if (grepl("^[A-Za-z]$", c, perl = TRUE)) # Is it a letter?
          v <- pos.alphabet(c) + 9
        if (c == "*") v <- 36
        if (c == "@") v <- 37
        if (c == "#") v <- 38
        if ((i %% 2) == 0) # Multiply every second digit by two.
          v <- v * 2
        sum.cusip <- sum.cusip + v %/% 10 + v %% 10
        rm(v) # Defensive programming.
      }
      CUSIP9[k] <- paste(CUSIP8[k],
                         as.character((10 - (sum.cusip %% 10)) %% 10),
                         sep = "")
      ## Sanity check.
      if (length(unlist(strsplit(CUSIP9[k], split = ""))) != 9)
        stop("The number of output digits is different than nine.")
    }
  }
  return(CUSIP9)
}

