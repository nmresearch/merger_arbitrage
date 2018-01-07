df.augment <- function(takeoverData, use.log = TRUE) {
  ## #################################################################
  ## This function adds some variables to the data.frame to make it
  ## more easy to manage the data.
  ## #################################################################


  ## #################################################################
  ## Codify what 'use.log' means in terms of 'ln' and 'aret'.
  ## #################################################################
  if (use.log) {
    ln = TRUE
    aret = FALSE
  } else {
    ln = FALSE
    aret = TRUE
  }

  ## #################################################################
  ## Add modified book to market values by setting them close to zero in
  ## case they are negative. This is slightly different to what I did in
  ## my original JMP. The reason is that here I set B/M to epsilon>0
  ## (instead of zero) so that it's still possible to apply the
  ## logaritym to the fixed B/M entries.
  ## #################################################################
  takeoverData <- # Acquirer.
    BM.fix(takeoverData,
           takeoverData$aBookValuePerShare,
           takeoverData$aSharePrice1DPrior,
           "aBookToMarket.modified")
  takeoverData <- # Target.
    BM.fix(takeoverData,
           takeoverData$tBookValuePerShare,
           takeoverData$tSharePrice1DPrior,
           "tBookToMarket.modified")

  ## #################################################################
  ## Add runup, markup, and premium to 'takeoverData'. Note that this is
  ## different compared to, for example, Schwert-96-JFE, where he uses
  ## residuals to calculate abnormal returns that are in turn used to
  ## calculate runup and markup. 
  ## #################################################################
  takeoverData$runup <- eval(div("tSharePrice1DPrior", "tSharePrice4WPrior",
                                 "takeoverData", ln = ln, aret = aret))
  takeoverData$markup <- eval(div("initialOfferPrice", "tSharePrice1DPrior",
                                  "takeoverData", ln = ln, aret = aret))
  takeoverData$premium <- eval(div("initialOfferPrice", "tSharePrice4WPrior",
                                   "takeoverData", ln = ln, aret = aret))

  ## #################################################################
  ## Add announcement date return.
  ## #################################################################
  takeoverData$aAnncDtRet <- eval(div("aSharePriceAnn", "aSharePrice4WPrior",
                                      "takeoverData", ln = ln, aret = aret))
  takeoverData$tAnncDtRet <- eval(div("tSharePriceAnn", "tSharePrice4WPrior",
                                      "takeoverData", ln = ln, aret = aret))

  ## #################################################################
  ## Add scaled version of cash, as is common in the literature.
  ## #################################################################
  takeoverData$aCashToTotalAssets <- eval(div("aCash", "aTotalAssets", "takeoverData"))
  takeoverData$tCashToTotalAssets <- eval(div("tCash", "tTotalAssets", "takeoverData"))

  return(takeoverData)
}





fml.status <- function() {
  ###################################################################
  ## This function creates a formula of the form 'status ~
  ## controls'. This function, however, does not yet add the media
  ## variable because there are several media measures (e.g. mean and
  ## median aggregated). This has to be done after calling this
  ## function.
  ###################################################################
  
  ## Basic formula from original JMP.
  fmla <- status ~ log(aCash) + aBookToMarket.modified +
    stockSwapD + unsolicitedD + log(days) + aAnncDtRet
  
  ## Next, adjust the formula.
  
  ## If using log-returns, target's announcement date return is
  ## significant.
  fmla <- update(fmla, ~ . + tAnncDtRet)
  ## Adding target firm characteristics for those firm characteristics
  ## that are significant for the acquirer. This is purely for
  ## comparison, although they might be insignificant.
  fmla <- update(fmla, ~ . + log(tCash) + tBookToMarket.modified)

  ## Use log(B/M) instead of B/M.
  fmla <- update(fmla, ~ . - aBookToMarket.modified + log(aBookToMarket.modified) - tBookToMarket.modified + log(tBookToMarket.modified))
  
  ## Use scaled version of cash.
  fmla <- update(fmla, ~ . - log(aCash) + log(aCashToTotalAssets) - log(tCash) + log(tCashToTotalAssets))
  
  ## Objects of class 'formula' are treated like objects of class
  ## 'function'. Since 'fmla' is defined within this function, we have
  ## to fix the environment.
  environment(fmla) <- .GlobalEnv
  return(fmla)
}



extr.var.basic <- function(fmla,                # Formula.
                           depdt.var = FALSE) { # Should dependent variable also be extracted?
  ###################################################################
  ## This function extracts all the terms from a formula by splitting
  ## up the '+' and '-' terms. However, it does not split them up any
  ## further. For example, 'log(a/b)' will be returned as 'log(a/b)'
  ## and not as 'a' and 'b'. It also returns interaction terms
  ## unchanged, e.g. 'a:b' will be returned as "a:b' and not as 'a'
  ## and 'b'.
  ###################################################################

  if (class(fmla) != "formula")
    stop("Function argument should be of class formula.")
  if (class(depdt.var) != "logical")
    stop("Function argument should be of class logical.")

  vars <- unlist(strsplit(as.character(fmla)[3], split = " [+-] "))
  if (depdt.var)
    vars <- c(as.character(fmla)[2], vars) # Add dependent variable.
  
  return(unique(vars))
}



extr.var <- function(fmla,                # Formula.
                     depdt.var = FALSE) { # Should dependent variable also be extracted?
  ###################################################################
  ## This function extracts all variables that are in
  ## 'fmla'. Ultimately, this function should also be able to get,
  ## say, 'a', 'b', and 'c' from things like 'log((a-b)/c)', but see
  ## the next paragraph.
  ##
  ## So far this function is just a wrapper function for
  ## 'extr.var.basic'. This function is thus still very much
  ## incomplete. For example, it cannot deal with 'I(...)' parts of
  ## the formula or 'log(...)'  parts. It also cannot deal with
  ## interaction terms.
  ## 
  ## To extend this function, see '?terms', the example in Chambers
  ## p305, and the commented code at the bottom of this function
  ## definition.
  ###################################################################

  if (class(fmla) != "formula")
    stop("Function argument should be of class formula.")
  if (class(depdt.var) != "logical")
    stop("Function argument should be of class logical.")

  vars <- extr.var.basic(fmla, depdt.var)

  return(unique(vars))

  ## ## Some old code from a different context, maybe it's useful to
  ## ## generalize this function.
  ## x <- paste(as.character(fmla)[2], as.character(fmla)[3])
  ## x <- gsub("\\+", " ", x)
  ## x <- gsub("\\-", " ", x)
  ## x <- gsub("I\\(", " ", x)
  ## x <- gsub("\\(", " ", x)
  ## x <- gsub("\\)", " ", x)
  ## x <- gsub("\\/", " ", x)
  ## x <- gsub("[ ][ ]+", " ", x)
  ## x <- sort(unique(unlist(strsplit(x, " "))))
  ## x <- x[-which(x %in% c("", "log"))]
}




df.from.fml <- function(fmla,                # formula from which to get terms.
                        df,                  # data.frame from which to get data.
                        depdt.var = FALSE) { # Also get dependent variable from formula?
  ###################################################################
  ## This function creates a data.frame that contains all the *terms*
  ## used in the formula. For example, if there is 'log(a)' in the
  ## formula, this function returns a data.frame that has a column
  ## containing 'log(df$a)'. Note that this function does *not* return
  ## a column containing 'df$a$, unless the formula contains
  ## explicitly 'a'.
  ##
  ## This function is useful when you want to calculate a correlation
  ## matrix because you want to check for multicollinearity. In this
  ## case you really want to have a data.frame containing 'log(df$a)'
  ## instead of just 'df$a' whenever you have 'log(a)' in your
  ## formula.
  ##
  ## This function is also useful if you want to investigate where
  ## missing values are introduced because of the inclusion of certain
  ## variables in the formula. For example, in Buehlmaier's JMP the
  ## number of observations used for model fitting should preferrably
  ## *not* omit any observations that have 'status==Withdrawn' because
  ## there are so few of them.
  ##
  ## So far this function can only deal with relatively simple
  ## examples, like the one in the previous paragraph. It's possible
  ## to extend this function later on so that it can deal with more
  ## complicated cases, e.g. 'log(a/b)' becomes 'log(df$a/df$b)' or
  ## examples involving 'I(...)'.
  ###################################################################
  
  if (class(df) != "data.frame" | class(fmla) != "formula" | class(depdt.var) != "logical")
    stop("Wrong class of function args.")
    
  ## Get all variables from the formula.
  vars.orig <- extr.var.basic(fmla, depdt.var)
  ## Cleverly add 'df$' in front of 'vars.orig' contents. Store the
  ## result in 'vars'.
  vars <- character()
  for (i in seq_along(vars.orig)) {
    if (grepl("^log\\(", vars.orig[i])) {
      vars[i] <- sub("^log\\(", "log\\(df\\$", vars.orig[i])
    } else {
      vars[i] <- paste("df$", vars.orig[i], sep = "")
    }
  }

  ## Construct new data.frame 'x' containing all variables in 'vars'.
  x <- list() # Will later be converted to data.frame. Here it is
              # crucial that 'df' is already a data.frame, otherwise
              # the varying length of columns can lead to problems.
  ## Remove braces from 'vars.orig' for new column names of new
  ## data.frame 'x'.
  vars.orig <- sub(")$", "", vars.orig)
  vars.orig <- gsub("[()]", "_", vars.orig)
  for (i in seq_along(vars)) {
    eval(parse(text = paste("x$", vars.orig[i], "<-", vars[i])))
  }
  return(as.data.frame(x))
}


char.nam.chg <- function(x,      # Character vector to be changed. 
                         nmap) { # Matrix containing mapping from old (col 1) to new (col 2) names.
  ###################################################################
  ## This function changes elements of a character vector based on a
  ## given mapping from old to new names.
  ###################################################################

  ## Verify function arguments.
  if (class(x) != "character" | class(nmap) != "matrix")
    stop("Function arguments are of wrong class.")

  for (i in seq_along(nmap[, 1])) {
    ## Protect brackets, as occur for example in "(Intercept)".
    pattern <- gsub("(", "\\(", nmap[i, 1], fixed = TRUE)
    pattern <- gsub(")", "\\)", pattern, fixed = TRUE)
    pattern <- paste("^", pattern, "$", sep="")
    pos <- grep(pattern, x, perl = TRUE)
    x[pos] <- nmap[i, 2]
  }
  return(x)
}



col.nam.chg <- function(x,      # Data.frame whose colnames will be changed. 
                        cnam,   # Subset of colnames of x that should be changed.
                        nmap) { # Matrix containing mapping from old to new colnames.
  ###################################################################
  ## This function changes the column names based on a given mapping
  ## from old to new names. Note that this function could be
  ## simplified by depending on the function 'char.nam.chg'.
  ###################################################################

  ## Verify function arguments.
  if (class(x) != "data.frame" | class(cnam) != "character" | class(nmap) != "matrix")
    stop("Function arguments are of wrong class.")
  
  for (i in seq_along(nmap[, 1])) {
    ## Protect brackets, as occur for example in "(Intercept)".
    pattern <- gsub("(", "\\(", nmap[i, 1], fixed = TRUE)
    pattern <- gsub(")", "\\)", pattern, fixed = TRUE)
    pattern <- paste("^", pattern, "$", sep="")
    pos <- grep(pattern, cnam, perl = TRUE) # Exact match from ^ to $.
    colnames(x)[pos] <- nmap[i, 2]
  }
  return(x)
}



row.nam.chg <- function(x,      # Matrix whose rownames will be changed.
                        cnam,   # Subset of rownames of x that should be changed.
                        nmap) { # Matrix containing mapping from old to new rownames.
  ###################################################################
  ## This function changes the row names based on a given mapping from
  ## old to new names. Note that this function could be simplified by
  ## depending on the function 'char.nam.chg'.
  ###################################################################

  ## Verify function arguments.
  if (class(x) != "matrix" | class(cnam) != "character" | class(nmap) != "matrix")
    stop("Function arguments are of wrong class.")
  
  for (i in seq_along(nmap[, 1])) {
    ## Protect brackets, as occur for example in "(Intercept)".
    pattern <- gsub("(", "\\(", nmap[i, 1], fixed = TRUE)
    pattern <- gsub(")", "\\)", pattern, fixed = TRUE)
    pattern <- paste("^", pattern, "$", sep="")
    pos <- grep(pattern, cnam, perl = TRUE) # Exact match from ^ to $.
    rownames(x)[pos] <- nmap[i, 2]
  }
  return(x)
}






print.glm.TeX <- function(cat_file, dig = 2, 
                          dep.var, link.fcn, my.R.sq, my.obs, 
                          coeff.nam, my.coeff, my.effects, my.sig, p.vals) {
  ###################################################################
  ## Print GLM regression results to LaTeX.
  ###################################################################
  
  ## Need 'mycat' to be defined within this function, otherwise the
  ## thing with 'cat_file' will not always work as intended.
  mycat <- function(...) { 
    cat(..., file = cat_file, sep = "", append = TRUE) 
  }

  mdl.indx <- seq_len(ncol(my.coeff))
  no_models <- length(mdl.indx) # How many models to print. 

  ## Start with empty file.
  cat("", file = cat_file)

  ## Print name of dependent variable.
  mycat("Dependent Variable & ")
  ctr <- 0 
  for (j in mdl.indx) { 
    mycat("\\multicolumn{2}{c}{", dep.var[j], "}") 
    ctr <- ctr + 1 
    if (ctr < no_models) 
      mycat(" & \\qquad & ")
  }
  mycat(" \\\\ \n") 

  ## Print name of link function. 
  mycat("Link Function & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    mycat("\\multicolumn{2}{c}{", link.fcn[j], "}") 
    ctr <- ctr + 1 
    if (ctr < no_models) 
      mycat(" & \\qquad & ")
  }
  mycat(" \\\\ \n") 

  mycat("\\hline\\hline\n")

  ## Print column labels. 
  mycat(" & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    mycat("Coeff. & Marg.\\ Effect") 
    ctr <- ctr + 1 
    if (ctr < no_models) 
      mycat(" & \\qquad & ") 
  }
  mycat(" \\\\ \n")

  mycat("\\hline\n") 

  ## Print coefficient names, coefficient estimates, significance stars,
  ## marginal effects, z-values.
  for (i in seq_along(coeff.nam)) { # 'i' is the row to be printed.
    ## Print coefficient name.
    mycat(coeff.nam[i], " & ")  
    ctr <- 0 
    for (j in mdl.indx) { # 'j' is the column to be printed.
      ctr <- ctr + 1 
      if (!is.na(my.coeff[i, j])) { # Only print it if it's not NA.
        ## Print coeff estimate.
        mycat(pp(my.coeff[i, j], dig))
        mycat("$^{")
        ## Print significance star(s).
        mycat(
              if (my.sig[i, j] == "***") 
              "\\star\\star\\star"
              else if (my.sig[i, j] == "**") 
              "\\star\\star\\;\\:"
              else if (my.sig[i, j] == "*") 
              "\\star\\;\\:\\;\\:"
              else "\\;\\:\\;\\:\\;\\:"
              ) 
        mycat("}$ & ")
        ## Print marginal effect, but not for intercept.
        if (coeff.nam[i] != "Intercept")
          mycat(pp(my.effects[i, j], dig)) 
        if (ctr < no_models)
          mycat(" & \\qquad & ") 
      } else {
        mycat(" & ") 
        if (ctr < no_models) mycat(" & \\qquad & ") 
      } 
    }; rm(j, ctr)
    mycat("\\\\ \n") 
    mycat(" & ") 
    ## Print z-values.
    ctr <- 0 
    for (j in mdl.indx) { 
      ctr <- ctr + 1 
      if (!is.na(p.vals[i, j])) { 
        mycat("{\\relsize{-0.5} (") 
        mycat(pp(p.vals[i, j], dig)) 
        mycat(")} \\:\\:\\,") 
      } 
      if (ctr < no_models) mycat(" & \\qquad & & ") 
    }; rm(j, ctr)
    mycat(" \\\\ \n") 
  }; rm(i)

  mycat("\\hline\n") 

  ## Print pseud R-squared and the number of observations.
  mycat("Pseudo-$R^2$ & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    ctr <- ctr + 1 
    mycat(pp(my.R.sq[j], dig), "\\;\\;\\;\\;") 
    if (ctr < no_models) mycat(" & & \\qquad & ") 
  }; rm(j, ctr)
  mycat("\\\\ \n")
  mycat("Observations & ")
  ctr <- 0 
  for (j in mdl.indx) { 
    ctr <- ctr + 1 
    mycat(my.obs[j], "\\;\\;\\;\\;")
    if (ctr < no_models) mycat(" & & \\qquad & ") 
  }; rm(j, ctr)
  mycat("\\\\ \n")

  mycat("\\hline\n") 

  Sys.chmod(cat_file, mode = "0400") # Write-protect the file.


} # Close function.






print.lm.TeX <- function(cat_file, dig = 2, 
                         dep.var, my.R.sq, my.obs, 
                         coeff.nam, my.coeff, my.sig, t.stats) {
  ###################################################################
  ## Print OLS regression results to LaTeX.
  ###################################################################
  
  ## Need 'mycat' to be defined within this function, otherwise the
  ## thing with 'cat_file' will not always work as intended.
  mycat <- function(...) { 
    cat(..., file = cat_file, sep = "", append = TRUE) 
  }

  mdl.indx <- seq_len(ncol(my.coeff))
  no_models <- length(mdl.indx) # How many models to print. 

  ## Start with empty file.
  cat("", file = cat_file)

  ## Print name of dependent variable. 
  mycat("Dependent Variable & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    mycat(dep.var[j], "\\;\\;") 
    ctr <- ctr + 1 
    if (ctr < no_models) 
      mycat(" & \\qquad & ") 
  }
  mycat(" \\\\ \n")

  mycat("\\hline\\hline\n") 

  ## Print column labels. 
  mycat(" & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    mycat("Coeff.\\;\\;") 
    ctr <- ctr + 1 
    if (ctr < no_models) 
      mycat(" & \\qquad & ") 
  }; rm(j, ctr)
  mycat(" \\\\ \n")

  mycat("\\hline\n") 

  ## Print coefficient names, coefficient estimates, significance
  ## stars, p-values.
  for (i in seq_along(coeff.nam)) { # 'i' is the row to be printed.
    ## Print coefficient name.
    mycat(coeff.nam[i], " & ")  
    ctr <- 0 
    for (j in mdl.indx) { # 'j' is the column to be printed.
      ctr <- ctr + 1 
      if (!is.na(my.coeff[i, j])) { # Only print it if it's not NA.
        ## Print coeff estimate.
        mycat(pp(my.coeff[i, j], dig))
        mycat("$^{")
        ## Print significance star(s).
        mycat(
              if (my.sig[i, j] == "***") 
              "\\star\\star\\star"
              else if (my.sig[i, j] == "**") 
              "\\star\\star\\;\\:"
              else if (my.sig[i, j] == "*") 
              "\\star\\;\\:\\;\\:"
              else "\\;\\:\\;\\:\\;\\:"
              ) 
        mycat("}$ & ")
        if (ctr < no_models)
          mycat("& ") 
      } else {
        mycat(" & ") 
        if (ctr < no_models) mycat("& ") 
      } 
    }; rm(j, ctr)
    mycat("\\\\ \n") 
    mycat(" & ") 
    ## Print p-values.
    ctr <- 0 
    for (j in mdl.indx) { 
      ctr <- ctr + 1 
      if (!is.na(t.stats[i, j])) { 
        mycat("{\\relsize{-0.5} (") 
        mycat(pp(t.stats[i, j], dig)) 
        mycat(")} \\:\\:\\,") 
      } 
      if (ctr < no_models) mycat("\\qquad & & ") 
    }; rm(j, ctr)
    mycat(" \\\\ \n") 
  }; rm(i)

  mycat("\\hline\n") 

  ## Print R-squared and the number of observations.
  mycat("$R^2$ & ") 
  ctr <- 0 
  for (j in mdl.indx) { 
    ctr <- ctr + 1 
    mycat(pp(my.R.sq[j], dig), "\\;\\;\\;\\;") 
    if (ctr < no_models) mycat(" & & ") 
  }; rm(j, ctr)
  mycat("\\\\ \n")
  mycat("Observations & ")
  ctr <- 0 
  for (j in mdl.indx) { 
    ctr <- ctr + 1 
    mycat(my.obs[j], "\\;\\;\\;\\;")
    if (ctr < no_models) mycat(" & & ") 
  }; rm(j, ctr)
  mycat("\\\\ \n")

  mycat("\\hline\n") 

  Sys.chmod(cat_file, mode = "0400") # Write-protect the file.


} # Close function.






col.add.TeX <- function(x, y) {
  ###################################################################
  ## This function cbinds to the matrix 'x' the matrix 'y'. It is a
  ## helper function for printing regression results to LaTeX. If the
  ## rownames of 'x' and 'y' are not the same, this function cbinds
  ## them together in a clever way. If a new rowname appears in 'y'
  ## that is not in 'x', a new row is appended to 'x' and filled with
  ## the appropriate contents of 'y'. The rows appended to 'x' have
  ## the same sequence in which the new rownames appear in 'y'. When
  ## new rows have to be added to 'x' because there are new variables
  ## in 'y', the old column in 'x' gets NAs added for those new
  ## variables.
  ###################################################################

  ## Same behavior as 'cbind(NULL, y)'.
  if (is.null(x))
    return(y)
  
  ## Check function arguments.
  if (class(x) != "matrix" | class(y) != "matrix")
    stop("arguments should be of class matrix.")
  if (ncol(y) != 1)
    stop("'y' should only have one column.") # This can later be relaxed.
  if (length(rownames(x)) != nrow(x) | length(rownames(y)) != nrow(y))
    stop("All rows should have names (we should later print to LaTeX, after all).")
  
  if (length(intersect(rownames(x), rownames(y))) == length(rownames(x))) { # The easy case.
    x <- cbind(x, y)
  } else {
    ## This means we have to add something like 'y[pos, ]' to the names
    ## of the matrix and then cbind the contents of 'y' in a clever way.
    pos <- which(! rownames(y) %in% rownames(x))
    pos <- sort(pos) # probably not necessary, but it's better to be on the safe side.
    sNA = x[1,1]; names(sNA) <- NULL; is.na(sNA) <- TRUE # Special NA that has appropriate class.
    for (k in seq_along(pos)) {
      x <- rbind(x, rep(sNA, ncol(x))) # Add extra rows and fill w/ NA.
      rownames(x)[nrow(x)] <- rownames(y)[pos[k]] # Add new rowname.
    }

    ## Add new column containing 'y'.
    x <- cbind(x, rep(sNA, nrow(x))) # Start with NAs and fill w/ content later.
    for (k in seq_along(y[, 1])) {
      pattern <- paste("^", rownames(y)[k], "$", sep = "") # Match from beginning to end.
      pattern <- gsub("(", "\\(", pattern, fixed = TRUE)
      pattern <- gsub(")", "\\)", pattern, fixed = TRUE)
      if (!any(grepl(pattern, rownames(x), perl = TRUE))) # Sanity check.
        stop("Can't find the name (but should be able to find it).")
      pos.add <- grep(pattern, rownames(x), perl = TRUE)
      x[pos.add, ncol(x)] <- y[k, 1]
    }
  }
  
  return(x)
}






pp <- function(x, digits = 2) {
  #######################################################################
  ## Function for pretty printing stuff, e.g. to LaTeX. It also avoids
  ## ambiguities that result from rounding. For example, the original
  ## number is -0.001, then simply rounding yields 0.00. In the
  ## context of regression analysis, this i s not very informative
  ## since it matters whether a coefficient is positive or
  ## negative. In this case, the function outputs the largest negative
  ## number that is smaller than zero, given the number of digits it
  ## can print. In the example above, this function prints -0.01.
  #######################################################################

  ## Ensure function arguments are OK.
  if (class(x) != "numeric" | class(digits) != "numeric")
    stop("class should be numeric.")
  if (length(x) != 1 | length(digits) != 1)
    stop("length should be equal to one.")
  if (is.na(x))
    stop("x should not be NA.")
  
  ret <- format(round(x, digits = digits), nsmall = digits)
  if (!grepl("[1-9]", ret, perl = TRUE)) { # Only zeros.
    ret <- sub("0$", "1", ret) # Add 1 at end to make it more legible.
    if (x<0)
      ret <- paste("-", ret, sep = "")
  }
  return(ret)
}




mycat <- function(...) { 
  #######################################################################
  ## Custom cat function that is useful for printing to LaTeX files,
  ## for example.
  #######################################################################
  cat(..., file = cat_file, sep = "", append = TRUE) 
} 






## #######################################################################
## ## This function returns a fitted GLM model that has a binomial link
## ## function. If the link is complementary log-log, then additional
## ## arguments are used to improve convergence.
## #######################################################################
## glm.fitted <- function(fmla,
##                        d.f,
##                        uselink = "logit") {
  
##   ## Check function arguments.
##   if (class(fmla) != "formula")
##     stop("fmla should be of class formula.")
##   if (class(d.f) != "data.frame")
##     stop("d.f should be of class data.frame.")
##   if (class(uselink) != "character")
##     stop("uselink should be of class character.")
  
##   ## Calculate fitted GLM model.
##   glm.fitted <-
##     if (uselink != "cloglog") { 
##       glm(fmla, data = d.f, family = binomial(link = uselink))
##     } else { # Add 'control' because of convergence issues. 
##       glm(fmla, data = d.f, family = binomial(link = uselink), 
##           control = list(epsilon = 1e-15, maxit = 50, trace = FALSE))
##     }
##   return(glm.fitted)
## } 








av_sample_marg_eff <- function(my_probit) { 
  #######################################################################
  ## Average of the sample marginal effects: (AER page 126) 
  #######################################################################
  fav <- mean(dnorm(predict(my_probit, type = "link"))) 
  sampl_marg_eff <- fav * coef(my_probit) 
  return(sampl_marg_eff) 
} 



McFadden_ps_R_sq <- function(my_probit) { 
  #######################################################################
  ## McFadden's pseudo-R^2 (AER page 127) 
  #######################################################################
  my_probit0 <- update(my_probit, formula = . ~ 1) 
  McFaddenRsq <- 1 - as.vector(logLik(my_probit)/logLik(my_probit0))
  return(McFaddenRsq) 
} 



winsor <- function (x, fraction = .05) {
  #######################################################################
  ## This function winsorizes data the traditional way. It should be
  ## able to also deal with missing values (but I haven't tested it
  ## yet).
  ## 
  ## Idea modified from http://www.portfolioprobe.com/2011/06/30/winsorization/
  #######################################################################
  
  if(length(fraction) != 1 || fraction < 0 || fraction > 0.5)
    stop("bad value for 'fraction'")

  lim <- quantile(x, probs = c(fraction, 1-fraction), na.rm = TRUE)
  pos1 <- which(x < lim[1]) # Deal with NAs.
  pos2 <- which(x > lim[2])
  x[pos1] <- lim[1]
  x[pos2] <- lim[2]
  return(x)
}

winsor2 <- function (x, multiple = 3) {
  #######################################################################
  ## This function winsorizes data in a non-traditional but innovative
  ## way. It should be able to also deal with missing values (but I
  ## haven't tested it yet).
  ## 
  ## Idea modified from http://www.portfolioprobe.com/2011/06/30/winsorization/
  #######################################################################
  
  if(length(multiple) != 1 || multiple <= 0)
    stop("bad value for 'multiple'")

  med <- median(x, na.rm = TRUE)
  y <- x - med
  sc <- mad(y, center = 0, na.rm = TRUE) * multiple
  pos1 <- which(y > sc) # Deal with NAs.
  pos2 <- which(y < -sc)
  y[pos1] <- sc
  y[pos2] <- -sc
  return(y + med)
}



BM.fix <- function(takeoverData, # data.frame to which B/M will be added.
                   bkv,          # Book value.
                   spr,          # Stock price.
                   new.label,    # Column label for B/M.
                   eps = 0.01) { # Value to which B/M will be set.
  #######################################################################
  ## This function sets to 'eps' those book-to-market values that are
  ## nonpositive. 'eps' is by default not zero to allow taking
  ## logarithms later on. It then adds the fixed B/M values to the
  ## data.frame and returns this data.frame.
  #######################################################################

  ## Ensure that function arguments make sense.
  if (class(takeoverData)!="data.frame" |
      class(bkv)!="numeric" |
      class(spr)!="numeric" |
      class(new.label)!="character" |
      class(eps)!="numeric")
    stop("At least one function argument if of wrong class.")

  BM <- bkv/spr
  ## Get all negative positions. Note that you can also just use the
  ## logical entries, but then you have to type 'BM <= 0 & !is.na(BM)'
  ## to get rid of the NAs. Otherwise, the NA's will later also be
  ## assigned 'eps'.
  pos <- which(BM <= 0)
  ## is.na(BM[pos]) <- rep.int(TRUE, length(BM[pos])) # Set to NA. 
  BM[pos] <- rep.int(eps, length(BM[pos])) # Set to 'eps'.

  ## Add modified B/M values to data.frame.
  BM <- as.data.frame(BM)
  names(BM) <- new.label
  takeoverData <- cbind(takeoverData, BM)
  
  return(takeoverData)
}







excl.cols <- function(takeoverData,        # data.frame from which some columns will be deleted.
                      cutoff = .02,        # cutoff level that determines whether col has "many NAs".
                      excl.names = NULL) { # Additional col names that will be excluded.
  #######################################################################
  ## This function identifies which columns to exclude from
  ## 'takeoverData'.
  #######################################################################

  ## This function identifies columns that contain a lot of NA's or that
  ## have a factor with only one level.
  find.NA.cols <- function(takeoverData,   # Data.frame whose cols to cycle through.
                           cutoff = .02) { # Cutoff level that determines "many NAs".
    exclude <- numeric() # Columns that are to be excluded. 
    for (j in seq_along(takeoverData)) { # Cycle through columns.
      mycount <- 0 # Number of NA's in column j (TBD).
      for (i in seq_along(takeoverData[, j])) { # Cycle through rows.
        if (is.na(takeoverData[i, j])) { mycount <- mycount + 1 }
      }
      ## Write down those columns that have a lot of NA's or that
      ## contain a factor with only one level.
      if (mycount/length(takeoverData[, j]) > cutoff |
          (class(takeoverData[, j]) == "factor" &
           length(levels(takeoverData[, j])) <= 1)) {
        exclude <- c(exclude, j)
      } # Close 'if'.
    } # Close 'for'.
    return(exclude)
  }

  ## Verify function argument(s).
  if (class(takeoverData) != "data.frame")
    stop("Function argument is not a data.frame.")
  
  ## Use function defined above.
  exclude <- find.NA.cols(takeoverData, cutoff)
  ## Mark all dates for exclusion. 
  for (j in seq_along(takeoverData)) {
    if (class(takeoverData[, j]) == "Date") { exclude <- c(exclude, j) }
  }

  ## ## Mark some other stuff for exclusion.
  ## exclude <- c(exclude,
  ##              which(names(takeoverData) == "tName" | 
  ##                    names(takeoverData) == "tSIC" | 
  ##                    names(takeoverData) == "tIndustry" | 
  ##                    names(takeoverData) == "tState" | 
  ##                    names(takeoverData) == "aName" | 
  ##                    names(takeoverData) == "aIndustry" | 
  ##                    names(takeoverData) == "aSIC" | 
  ##                    names(takeoverData) == "aState" | 
  ##                    names(takeoverData) == "predicted_med" | # Avoid multicollinearity. 
  ##                    names(takeoverData) == "status.brief" | # Remove duplicate of 'status'.
  ##                    names(takeoverData) == "status" | # avoid reverse causality etc.? 
  ##                    names(takeoverData) == "statusRollupCode" # avoid reverse causality?
  ##                    )
  ##              )
  
  ## Mark content of 'excl.names' for exclusion.
  if (!is.null(excl.names)) {
    hlp <- rep(FALSE, length(names(takeoverData)))
    for (i in seq_along(excl.names)) {
      hlp <- hlp | names(takeoverData) == excl.names[i]
    }
    exclude <- c(exclude, which(hlp))    
  }
  
  ## Return those columns that should be excluded from the data.frame
  ## 'takeoverData' (one of the function's arguments).
  return(unique(exclude))
}






#######################################################################
## Function to calculate significance stars based on numeric vector
## 'pv' containing p-values. It returns a character vector that has
## the same length as 'pv' that contains the significance stars.
#######################################################################
sig <- function(pv) {

  ## Verify function argument.
  if (class(pv) != "numeric")
    stop("Function argument should be numeric vector.")

  ## ## Old version of this function.
  ## significance <- character() 
  ## for (i in seq_along(pv)) { 
  ##   significance[i] <- ""
  ##   if (0 <= pv[i] & pv[i] <= 0.01) 
  ##     significance[i] <- "***" 
  ##   if (0.01 < pv[i] & pv[i] <= 0.05) 
  ##     significance[i] <- "**" 
  ##   if (0.05 < pv[i] & pv[i] <= 0.1) 
  ##     significance[i] <- "*" 
  ## } 
  ## return(significance) 
  
  cutoffs <- c(0.01, 0.05, 0.1)
  ifelse(cutoffs[2]<pv & pv<=cutoffs[3], "*",
         ifelse(cutoffs[1]<pv & pv<=cutoffs[2], "**",
                ifelse(pv<=cutoffs[1], "***", "")
                )
         )
}





#######################################################################
## Return a data.frame showing coefficient and significance stars. 'o'
## means order the result, 'p'means show p-values, 'sh' means shorten
## row names if they are too long ('sh' is only relevant if it has
## value larger than zero).
#######################################################################
smry <- function(lm.obj,    # Object of class lm or glm.
                 o = FALSE, # Should the coefficients be ordered alphabetically?
                 p = FALSE, # Should p-values be printed?
                 sh = 0) {  # Should coefficient names be shortened if they are too long?
  ## Function to calculate the average of the sample marginal effects
  ## (Kleiber-Zeileis page 126).
  marg.eff <- function(glm.mdl) { 
    fav <- mean(dnorm(predict(glm.mdl, type = "link"))) 
    sampl_marg_eff <- fav * coef(glm.mdl) 
    return(sampl_marg_eff) 
  } 
  
  s <- summary(lm.obj)$coefficients # Get the coefficients matrix.
  x <- as.data.frame(s[, "Estimate"])
  names(x) <- "Estimate"
  
  if (any(grepl("glm", class(lm.obj), fixed = TRUE))) { # Calculate marginal effects.
    hlp <- names(x)
    x <- cbind(x, as.data.frame(marg.eff(lm.obj)))
    names(x) <- c(hlp, "Marg.Eff.")
  }
  
  pval.col <- grep("Pr\\(>\\|", colnames(s), perl = TRUE) # Column where p-values are stored.
  if (length(pval.col) != 1) stop("something is wrong with p-values.")
  hlp <- names(x)
  x <- cbind(x, as.data.frame(sig(s[, pval.col]), stringsAsFactors=FALSE))
  names(x) <- c(hlp, "Sig.")

  if (p == TRUE) { # Print p-values.
    hlp <- names(x)
    x <- cbind(x, as.data.frame(round(s[, pval.col], 2)))
    names(x) <- c(hlp, "Pr(>|.|)")
  }

  if (o == TRUE) { # Order the row names.
    rownames(x) <- sub("^I\\(+(.*)\\)$", "\\1", rownames(x)) # Get rid of 'I(...'
    rownames(x) <- sub("^log\\(+(.*)\\)$", "\\1__log", rownames(x)) # Get rid of 'log(...'
    x <- x[order(rownames(x)), , drop = FALSE]
  }

  if (sh > 0) { # Shorten row names.
    for (i in seq_along(rownames(x))) { # There is probably a more elegant way to do this...
      if (length(strsplit(rownames(x)[i], "")[[1]]) > sh) { # Prevent NAs from being introduced.
        rownames(x)[i] <- paste(strsplit(rownames(x)[i], "")[[1]][1:sh], collapse="")
      }
    }
  }

  return(x)
}





#######################################################################
## This function prints how many observations have been used for model
## fitting.
#######################################################################
cat.obs <- function(mod, d.f) {
  
  ## This function calculates the number of observations used in the
  ## estimation of a model. 'mod' is the estimated model (e.g. from
  ## lm(...)) and 'd.f' is the data frame that was used to as an input
  ## to the model fitting.
  no.obs <- function(mod, d.f) {
    nrow(d.f) - length(mod$na.action)
  }
  
  cat( no.obs(mod, d.f), "observations used for model fitting.\n")
  cat(round((nrow(d.f)-no.obs(mod, d.f))/nrow(d.f)*100),
      "% observations discarded due to NA's.\n", sep="")
}









#######################################################################
## This function creates a character string that can be used as a new
## column label. For example, this function can produce the character
## string 'aEBITDA.To.aTotalAssets'.
#######################################################################
div.names <- function(num,            # numerator 
                      denum,          # denumerator
                      ln = FALSE,     # calculate log(num/denum)?
                      aret = FALSE) { # calculate arithmetic return, i.e. (num-denum)/denum?

  ## Checking for consistency of function arguments.
  if (class(num) != "character" | class(denum) != "character")
    stop("num and denum should be of class character.")
  if (ln & aret)
    stop("ln and aret should not both be TRUE.")

  ## Pasting together what this function should return.
  ret <- paste(num, ".To.", denum, sep = "")
  if (ln)
    ret <- paste(ret, ".log", sep = "")
  if (aret)
    ret <- paste(ret, ".aret", sep = "")

  return(ret)
}




#######################################################################
## This function creates an expression that might later be useful for
## calculating new variables when some existing variables have to be
## divided. For example, this function generates something like
## 'expression(takeoverData$aEBITDA / takeoverData$aTotalAssets)'.
#######################################################################
div <- function(num,            # numerator 
                denum,          # denumerator
                d.f = NULL,     # 
                ln = FALSE,     # calculate log(num/denum)?
                aret = FALSE,   # calculate arithmetic return, i.e. (num-denum)/denum?
                addI = FALSE) { # Add "I(...)", useful for formulae.

  ## Checking for consistency of function arguments.
  if (class(num) != "character" | class(denum) != "character")
    stop("num and denum should be of class character.")
  if (ln & aret)
    stop("ln and aret should not both be TRUE.")
  if (ln & addI)
    stop("ln and addI should not both be TRUE.")
  ## if (class(d.f) != "NULL" & class(d.f) != "data.frame" & class(d.f) != "character")
  ##   stop("d.f should be a data.frame or a character string containing the name of the data.frame.")
  if (class(d.f) != "NULL" & class(d.f) != "character")
    stop("d.f should be a character string containing the name of the data.frame.")

  ## Adding data.frame name to 'num' and 'denum', in case it
  ## exists. The part that is commented also allows to provide the
  ## data.frame directly as the function argument while at the same
  ## time alternatively allowing to pass a character string
  ## "bla". However, this does not work if I give a variable of class
  ## character as the function argument, so that's why I've disabled
  ## this part by commenting it out.
  if (!is.null(d.f)) {
    num <- paste(d.f, "$", num, sep = "")
    denum <- paste(d.f, "$", denum, sep = "")
    ## num <- paste(as.character(substitute(d.f)), "$", num, sep = "")
    ## denum <- paste(as.character(substitute(d.f)), "$", denum, sep = "")
  }
  
  ## Pasting together the expression (at this stage still a character)
  ## that this function should return.
  ret <- paste(num, "/", denum)
  if (ln)
    ret <- paste("log(", ret, ")")
  if (aret)
    ret <- paste("(", num, "-", denum, ")/", denum)
  if (addI)
    ret <- paste("I(", ret, ")")

  ## Return result as expression. 
  return(parse(text = ret))
}



#######################################################################
## This function creates an expression that, when evaluated, adds a
## new column to an existing data.frame. If this column already
## exists, it will be overwritten. The variables used to calculate the
## new column have to exist already in the data.frame.
#######################################################################
col.add <- function(num,                  # character string for numerator. 
                    denum,                # character string for denumerator.
                    df,                   # character string for data.frame where col will be added.
                    ln = FALSE,           # calculate 'log(num/denum)' instead? 
                    aret = FALSE,         # calculate '(num-denum)/denum' instead?
                    custom.name = NULL) { # Custom name of new column.

  ## Verify that function arguments are OK.
  if (class(num) != "character" | class(denum) != "character" | class(df) != "character")
    stop("num, denum, df should be of class 'character'.")
  if (class(ln) != "logical" | class(aret) != "logical")
    stop("ln and aret should be of class 'logical'.")

  ## Create name of new column.
  if (is.null(custom.name)) {
    nam <- div.names(num, denum, ln, aret)
  } else {
    nam <- custom.name
  }

  ## Create expression to calculate contents of new column.
  var.new <- div(num, denum, df, ln, aret, addI = FALSE)

  ## Create expression that, if evaluated, appends column contents to 'df'.
  ret <- parse(text = paste(df, "$", nam, " <- ", as.character(var.new), sep = ""))
  
  return(ret)
}





#######################################################################
## This function adds additional rows to the end of a data frame. The
## idea is to add rows to a data frame that encodes what variables
## should be added to another data.frame. If the character vector 'v'
## is not long enough, there will be no recycling. Instead, the
## remaining columns of 'df' will be filled up with 'FALSE' (if 'df'
## column is 'logical') or NA (otherwise).
#######################################################################
rrbind <- function(df, # Data.frame to which row should be added.
                   v) {# Character vector that contains stuff to be added.
  ## Ensure correct inputs.
  if (class(df) != "data.frame" | class(v) != "character")
    stop("Incorrect classes as function argument(s).")
  if (length(v) > ncol(df)) {
    stop("Incorrect dimensions.")
  }

  ## Fill up 'v' to remaining length.
  if (length(v) < ncol(df)) {
    for (i in (length(v)+1):ncol(df)) {
      if (class(df[, i]) == "logical") {
        v[i] <- "FALSE"
      } else {
        is.na(v[i]) <- TRUE
      }
    }
  }
  
  v <- as.data.frame(t(v), stringsAsFactors = FALSE) # Convert to data.frame.
  colnames(v) <- colnames(df) # Set correct column names.
  for (i in seq_along(colnames(df))) { # Convert to same class.
    if (class(v[, i]) != class(df[, i])) {
      v[, i] <- eval(parse(text = paste("as.", class(df[, i]), "(v[, i])", sep="")))
    }
  }
  ret <- rbind(df, v, deparse.level = 0) # add new row.

  ## If we just added an empty row, we should again remove it.
  col.no <- nrow(ret)
  if (all(is.na(ret[col.no, ]))) {
    ret <- ret[-col.no, ]
  }
  
  return(ret)
}



















#######################################################################
## This old-style function calculated a few additional variables and
## adds them to the data.frame 'tO'. To do this more professionally,
## see the other functions in this file that do this more
## elegantly. If I recall correctly, the reason why I always used
## 'attach' and 'detach' is that it helped me to copy-paste variables
## from a 'formula' object.
#######################################################################
var.add <- function(tO) {

  tO <- cbind(tO, "aEBITDAToTotalAssets" = tO$aEBITDA/tO$aTotalAssets)

  ## attach(tO)
  ## hlp <- as.data.frame(tEBITDA/tTotalAssets) 
  ## detach(tO) 
  ## names(hlp) <- "tEBITDAToTotalAssets" 
  ## tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(sapply(aNetInc/aTotalAssets, function(x) {max(x, .001)}))) 
  detach(tO) 
  names(hlp) <- "logaNetIncToTotalAssets" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(tNetInc/tTotalAssets) 
  detach(tO) 
  names(hlp) <- "tNetIncToTotalAssets" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(aNetInc/aNetSales) 
  detach(tO) 
  names(hlp) <- "aNetIncToNetSales" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(tNetInc/tNetSales) 
  detach(tO) 
  names(hlp) <- "tNetIncToNetSales" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(aSharePriceAnn/aNetSales)) 
  detach(tO) 
  names(hlp) <- "logaPriceToSales" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(log(tSharePriceAnn/tNetSales)) 
  ##detach(tO) 
  ##names(hlp) <- "logtPriceToSales" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(aNetSales/aSharePriceAnn)) 
  detach(tO) 
  names(hlp) <- "logaSalesToPrice" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(tNetSales/tSharePriceAnn) 
  ##detach(tO) 
  ##names(hlp) <- "tSalesToPrice" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(sapply(aCommEquity, function(x) {max(x, .001)}))) 
  detach(tO) 
  names(hlp) <- "logaCommEquity" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(aCash)) 
  detach(tO) 
  names(hlp) <- "logaCash" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(tCash)) 
  detach(tO) 
  names(hlp) <- "logtCash" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(aCash/aTotalAssets) 
  detach(tO) 
  names(hlp) <- "aCashToTotalAssets" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(tCash/tTotalAssets)) 
  detach(tO) 
  names(hlp) <- "logtCashToTotalAssets" 
  tO <- cbind(tO, hlp) 

  ## Use 'aBookToMarket.modified' instead. 
  ##attach(tO)
  ##hlp <- as.data.frame(aBookValuePerShare/aSharePrice1DPrior) 
  ##detach(tO) 
  ##names(hlp) <- "aBookToMarket" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(tBookValuePerShare/tSharePrice1DPrior) 
  detach(tO) 
  names(hlp) <- "tBookToMarket" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(aNetDebt/aCommEquity) 
  detach(tO) 
  names(hlp) <- "aLeverage" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(tNetDebt/tCommEquity) 
  detach(tO) 
  names(hlp) <- "tLeverage" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(tSharesOutstanding*tSharePrice1DPrior)) 
  detach(tO) 
  names(hlp) <- "logtMktValueOfEquity" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(log(aNetDebt+aCommEquity)) 
  ##detach(tO) 
  ##names(hlp) <- "logaSize" 
  ##tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(log(tNetDebt+tCommEquity)) 
  ##detach(tO) 
  ##names(hlp) <- "logtSize" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(tValue)) 
  detach(tO) 
  names(hlp) <- "logtValue" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame((aCurrentAssets-aCurrentLiab)/aTotalAssets) 
  ##detach(tO) 
  ##names(hlp) <- "aNoncashWorkingCapToTotalAssets" 
  ##tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame((tCurrentAssets-tCurrentLiab)/tTotalAssets) 
  ##detach(tO) 
  ##names(hlp) <- "tNoncashWorkingCapToTotalAssets" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(dealValue/aEBITDA) 
  detach(tO) 
  names(hlp) <- "dealValueToaEBITDA" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(dealValue/tEBITDA) 
  ##detach(tO) 
  ##names(hlp) <- "dealValueTotEBITDA" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(dealValue/aNetSales) 
  detach(tO) 
  names(hlp) <- "dealValueToaNetSales" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(dealValue/tNetSales) 
  detach(tO) 
  names(hlp) <- "dealValueTotNetSales" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(days)) 
  detach(tO) 
  names(hlp) <- "logDays" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(aSharePrice1WPrior/aEPS) 
  detach(tO) 
  names(hlp) <- "aPE" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(aSharePriceAnn/aSharePrice4WPrior)) 
  detach(tO) 
  names(hlp) <- "aAnnReturn" 
  tO <- cbind(tO, hlp) 

  ##attach(tO)
  ##hlp <- as.data.frame(log(tSharePriceAnn/tSharePrice4WPrior)) 
  ##detach(tO) 
  ##names(hlp) <- "tAnnReturn" 
  ##tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(tSharePrice1DPrior/tSharePrice4WPrior)) 
  detach(tO) 
  names(hlp) <- "runup" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(initialOfferPrice/tSharePrice1DPrior)) 
  detach(tO) 
  names(hlp) <- "markup" 
  tO <- cbind(tO, hlp) 

  attach(tO)
  hlp <- as.data.frame(log(initialOfferPrice/tSharePrice4WPrior)) 
  detach(tO) 
  names(hlp) <- "runupPlusMarkup" 
  tO <- cbind(tO, hlp) 

  return(tO)
}
