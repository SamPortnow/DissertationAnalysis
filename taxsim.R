#####TAXSIM R CODE#####
#        V 1.0
#    Property of NBER
# Written by Daniel Feenberg and Brandon Kaplowitz

#' @title NBER Tax Simulator v. 9.0
#' @author Daniel Feenberg and Brandon Kaplowitz
#' @description A NBER Package for calculating US and State Tax Returns.
#' @seealso \url{http://users.nber.org/~taxsim/}
#' @export
#' @param datafr A vector, dataframe or matrix which is composed of either headers and data or just data and stores information for each individual on relevant financial and family information for tax code. Values not entered are assumed to be 0. Possible data fields (not including headers, which should be specified as variable names below) for individuals include:
#' \describe{\item{taxsimid:}{Case ID}\item{year:}{Tax year between 1960 and 2012}\item{state:}{State, 0 or 1 to 51 in alphabetical order}\item{mstat:}{Marrital Status, \describe{\item{1:}{Unmarried}\item{2:}{Married}\item{3:}{Head of household (single with dependent)}\item{8:}{Taxpaying dependent}}} \item{depx:}{Dependent exemptions}\item{agex:}{Age of primary taxpayer times 100 plus age of second taxpayer if relevant}\item{pwages:}{Wage income of Primary Taxpayer}\item{swages:}{Wage income of spouse}
#' \item{dividends:}{Dividend income}\item{otherprop:}{Other property income}\item{pensions:}{Taxable Pensions}\item{gssi:}{Gross Social Security Benefits}\item{transfers:}{Non-taxable transfer income}\item{rentpaid:}{Rent paid on property}\item{proptax:}{Real estate tax paid}\item{otheritem:}{Other itemized deductions}\item{childcare:}{Childcare expenses}\item{ui:}{Unemployment Compensation}\item{depchild:}{Number of dependents under age 17}\item{mortgage:}{Other mortgage deductions}\item{ltcg:}{Long-term Capital Gain or Loss}\item{stcg:}{Short-term Capital Gain or Loss}}
#' @param mtr Marginal tax rate calculation. Options available are \describe{\item{11}{Wages} \item{70}{Long-Term Capital Gains} \item{85}{Primary Wage Earner} \item{86}{Secondary wage earner}}
#' @param detail Detail determines level of detail that the output will return. Options are: \describe{\item{0}{Standard} \item{2}{Full variables} \item{8}{Full with text description}}
#' @param optiontax1 Together with optiontaxvalue jointly specify the tax plan chosen. The option number is represented by optiontax1. For example, chosing 50, 1 will suppress AMT.
#' @param optiontaxvalue Together with optiontax1 jointly specify the tax plan chosen.  The parameter is represented by optiontaxvalue. For example, chosing 50, 1 will suppress AMT.
#' @return Returns a dataframe containing the following results (with column names taking on acronyms listed): \describe{\item{taxsimid}{Case ID}\item{year}{Year}\item{state}{State}\item{fiitax}{Federal income tax liability... including capital gains rates surtaxes, AMT, and refundable/nonrefundable taxcredits}\item{fica}{FICA}\item{frate}{Federal Marginal Rate}\item{srate}{State Marginal Rate}\item{ficar}{Fica Rate}}
#' If detailed results are requested the following results are also added:\describe{\item{fagi}{Federal AGI}\item{uiagi}{UI in AGI}\item{ssagi}{Social Security in AGI}\item{zba}{Zero Bracket Amount}\item{persex}{Personal Exemptions}\item{exphase}{Exemption Phaseout}\item{dphase}{Deduction Phaseout}\item{dallow}{Deductions Allowed}\item{fti}{Federal Taxable Income}
#' \item{taxti}{Tax on Taxable Income}\item{exsur}{Exemption Surtax}\item{gencred}{General Tax Credit}\item{ctcred}{Child Tax Credit}\item{accred}{Additional Child Tax Credit}\item{cccred}{Child Care Credit}\item{eic}{Earned Income Credit}\item{iamt}{Income for Alternative Minimum Tax}\item{amtliab}{AMT Liability after credit for regular tax}\item{fitcred}{Federal Income Tax Before Credits}\item{fica}{FICA}}
#' Finally, if a state is specified, we also return the following state-specific results:\describe{\item{shi}{State Household Income}\item{srp}{State Rent Payments}\item{sagi}{State AGI}\item{sea}{State Exemption Amount}\item{ssd}{State Standard Deduction}\item{sid}{State Itemized Deductions}\item{sti}{State Taxable Income}\item{sptc}{State Property Tax Credit}\item{sccc}{State Child Care Credit}\item{seic}{State EIC}\item{stc}{State Total Credits}\item{sbr}{State Bracket Rate}}
#' @examples
#' taxsim9(c("year","mstat", "ltcg",1970,2,100000))
#' taxsim9(datafr=matrix(c("year","mstat", "ltcg",1970,2,100000),ncol=3,byrow=TRUE))
#' taxsim9(c(25,1970,0,1,0,0,100000,rep(0,15)))
#' taxsim9(datafr=c("mstat","ltcg","year",2,100000,1970),detail=2)

taxsim9 <- function(datafr,
                    mtr = 11,
                    detail = 0,
                    optiontax1 = 0,
                    optiontaxvalue = 0) {
  ##### load r curl
  if(!require(RCurl))  {
    install.packages(c('RCurl'))
  }
  
  ####Prereq####
  errorout <- ""
  output <- ""
  #FUNCTIONS
  #' @export
  cleaner <- function(dtab, k = 0) {
    vals <-
      c(
        'taxsimid',
        'year',
        'state',
        'mstat',
        'depx',
        'agex',
        'pwages',
        'swages',
        'dividends',
        'otherprop',
        'pensions',
        'gssi',
        'transfers',
        'rentpaid',
        'proptax',
        'otheritem',
        'childcare',
        'ui',
        'depchild',
        'mortgage',
        'ltcg',
        'stcg'
      )
    #generates options from specified option values
    options = as.data.frame(t(as.data.frame(
      c(9, mtr, detail, optiontax1, optiontaxvalue, rep(0, 17))
    )))
    #checks if matrix, dataframe etc. Not strictly necc.
    if (is.matrix(dtab) == TRUE)
    {
      numrow <- nrow(as.matrix(dtab, ncol = 22, byrow = TRUE))
      dfr <- as.data.frame(as.matrix(dtab, ncol = 22, byrow = TRUE))
    }
    else if (is.data.frame(dtab) == TRUE && ncol(dtab) == 1)
    {
      dfr <- t(dtab)
      numrow <- nrow(as.matrix(dtab, ncol = 22, byrow = TRUE))
    }
    else if (is.data.frame(dtab) == TRUE && ncol(dtab) == 22)
    {
      numrow <- nrow(as.matrix(dtab, ncol = 22, byrow = TRUE))
      dfr <- dtab
      
    }
    else if (is.character(dtab) == TRUE)
    {
      k = 0
      l = 0
      startnum = 0
      startchar = 0
      
      if (is.matrix(dtab) || is.data.frame(dtab))
      {
        numrow <- nrow(dtab)
      }
      else
      {
        for (i in 1:length(dtab)) {
          if (k == 0 && is.character(dtab[i]))
          {
            startchar = i
            k = 1
          }
          if (l == 0 &&
              !is.na(suppressWarnings((as.numeric(dtab[i])))))
          {
            startnum = i
            l = 1
          }
          
        }
        #calculates length of characters to determine breakpoints
        if (startchar > startnum)
        {
          vecchar <- dtab[startchar:length(dtab)]
          veccnum <- dtab[startnum:startchar - 1]
          vecnumlist <- split(veccnum, length(vecchar))
        }
        else {
          veccnum <- dtab[startnum:length(dtab)]
          vecchar <- dtab[startchar:startnum - 1]
          vecnumlist <- split(veccnum, length(vecchar))
        }
        numcol <- length(vecchar)
      }
      #determines number of rows
      dfr <- matrix(dtab, ncol = numcol, byrow = TRUE)
      numrow <- nrow(dfr)
    }
    else{
      #base case with vector entry
      dfr <- as.data.frame(matrix(dtab, ncol = 22, byrow = TRUE))
      numrow <- nrow(dfr)
    }
    #creates matrix of the right size to serve as holder of 0s
    refmatrix <- matrix(c(vals,
                          rep(0, 22 * (numrow - 1))),
                        ncol = 22,
                        byrow = TRUE)
    #checks for headers
    
    #Fixes order of Headers and replaces other entries with zeros
    print(dfr)
    if (is.numeric(as.matrix(dfr[1, ])) == FALSE) {
      matcher <- match(as.vector(t(dfr[1, ])), vals)
      matcher <- matcher[!is.na(matcher)]
      refmatrix[, matcher] <- as.matrix(dfr[, 1:ncol(dfr)])
      dfr <- refmatrix
      colnames(dfr) <- as.matrix(dfr[1,])
      dfr = dfr[-1,]
    }
    else
    {
      print(
        'Warning: You have no headers or headers in the wrong place in your data. Please make sure your data is in the correct order as per the NBER website before continuing. '
      )
      
      colnames(dfr) <- vals
    }
    colnames(options) <- vals
    rownames(options) <- "options"
    dfr[is.na(dfr)] <- 0
    #check to ensure correct formating of options
    if (!(options[2] == 11 ||
          70 || 85 || 86) || !(options[3] == 0 || 2 || 5))
    {
      print("Wrong options specified. Using defaults...")
      options[2] = 11
      options[3] = 0
    }
    #adds options
    #if 5, then needs 3rd value to be 2 nominally
    
    dfr2 <- rbind(options, dfr)
    if (detail == 5)
    {
      options[3] = 2
    }
    if (k == 1) {
      return(dfr2)
    }
    else{
      dfr <- rbind(options, dfr)
      print(dfr)
      return(dfr)
    }
  }
  #Names first 9 columns and turns raw values to character values
  #' @export
  output <- function(x, dfr, k = 0) {
    if (k == 1) {
      if (is.raw(x))
      {
        write(rawToChar(x, multiple = FALSE), file = "output2")
        file.show("output2")
      }
      else{
        write(x, file = "output2")
        file.show("output2")
      }
    } else{
      if (is.raw(x)) {
        write(rawToChar(x, multiple = FALSE), file = "output")
        
        
      }
      else{
        write(x, file = "output")
      }
      #' @importFrom utils read.table
      outputfile <- read.table(file = "output")
      #correctly formats output as data frame
      outputdfr = as.data.frame(outputfile)
      #Checks level of detail and State
      if (detail == 0) {
        coldetails <-
          c(
            "taxsimid",
            "year",
            "state",
            "fiitax",
            "siitax",
            "fica",
            "frate",
            "srate",
            "ficar"
          )
      }
      else if (detail == 2) {
        coldetails <-
          c(
            "taxsimid",
            "year",
            "state",
            "fiitax",
            "siitax",
            "fica",
            "frate",
            "srate",
            "ficar",
            "fagi",
            "uiagi",
            "ssagi",
            "zba",
            "persex",
            "exphase",
            "dphase",
            "dallow",
            "fti",
            "taxti",
            "exsur",
            "gencred",
            "ctcred",
            "accred",
            "cccred",
            "eic",
            "iamt",
            "amtliab",
            "fitcred",
            "fica"
          )
        
      }
      else if (detail == 5) {
        coldetails <-
          c(
            "Case ID",
            "Year",
            "State",
            "Federal Income Tax Liability (including capital gains rates, surtaxes, AMT and refundable and non-refundable credits.)",
            "State income tax liability",
            "FICA (OADSI and HI, sum of employee AND employer)",
            "federal marginal rate",
            "state marginal rate",
            "FICA rate",
            "Federal AGI",
            "UI in AGI",
            "Social Security in AGI",
            "Zero Bracket Amount",
            "Personal Exemptions",
            "Exemption Phaseout",
            "Deduction Phaseout",
            "Deductions Allowed (Zero for non-itemizers)",
            "Federal Taxable Income",
            "Tax on Taxable Income (no special capital gains rates)",
            "Exemption Surtax",
            "General Tax Credit",
            "Child Tax Credit (as adjusted)",
            "Additional Child Tax Credit (refundable)",
            "Child Care Credit",
            "Earned Income Credit (total federal)",
            "Income for the Alternative Minimum Tax",
            "AMT Liability after credit for regular tax and other allowed credits.",
            "Federal Income Tax Before Credits (includes special treatment of Capital gains, exemption surtax (1988-1996) and 15% rate phaseout (1988-1990) but not AMT)",
            "FICA"
          )
      }
      #Checks whether state variables included
      if (!(0 %in% dfr$state[2:nrow(dfr)]) && detail == 5) {
        stateval = c(
          "State Household Income",
          "State Rent Payments",
          "State AGI",
          "State Exemption amount",
          "State Standard Deductions",
          "State Itemized Deductions",
          "State Taxable Income",
          "State Property Tax Credit",
          "State Child Care Credit",
          "State EIC",
          "State Total Credits",
          "State Bracket Rates"
        )
        coldetails <- c(coldetails, stateval)
      }
      if (!(0 %in% dfr$state[2:nrow(dfr)]) && detail == 2) {
        stateval = c(
          "shi",
          "srp",
          "sagi",
          "sea",
          "ssd",
          "sid",
          "sti",
          "sptc",
          "sccc",
          "seic",
          "stc",
          "sbr"
        )
        coldetails <- c(coldetails, stateval)
      }
      
      colnames(outputdfr) <- coldetails
      print(outputdfr)
      
      return(outputdfr)
    }
  }
  
  ####DATA####
  
  
  ####MAIN CODE####
  y <-
    regexpr('[0-9][0-9][:punct:][0-9][0-9][:punct:][0-9][0-9]',
            Sys.time(),
            TRUE)
  z <- y + attr(y, "match.length") - 1
  time <- unlist(strsplit(substr(Sys.time(), y, z), ":"))
  time2 <- paste(time, collapse = '')
  DATAFile = paste0(time2, Sys.getpid())
  dfr = cleaner(datafr)
  #' @importFrom utils write.table
  write.table(
    dfr,
    as.character(DATAFile),
    row.names = FALSE,
    quote = FALSE,
    sep = ',',
    col.names = FALSE
  )
  if (detail == 5)
  {
    dfr2 = cleaner(datafr, k = 1)
    print("hi")
    print(dfr2)
    #' @importFrom utils write.table
    write.table(
      dfr2,
      paste0(as.character(DATAFile), "2"),
      row.names = FALSE,
      quote = FALSE,
      sep = ',',
      col.names = FALSE
    )
    #' @importFrom RCurl ftpUpload
    ftpUpload(
      paste0(as.character(DATAFile), "2"),
      paste0(
        "ftp://taxsim:02138@taxsimftp.nber.org/tmp/",
        paste0(as.character(DATAFile), "2")
      )
    )
    #' @importFrom RCurl getURLContent
    data2 <- getURLContent(
      url = paste0(
        "ftp://taxsimftp.nber.org/tmp/",
        paste0(as.character(DATAFile), "2"),
        ".taxsim"
      ),
      userpwd = 'taxsim:02138',
      ftp.use.epsv = FALSE,
      dirlistonly = FALSE
    )
    #' @importFrom RCurl getURLContent
    error <- getURLContent(
      url = paste0(
        "ftp://taxsimftp.nber.org/tmp/",
        paste0(as.character(DATAFile), "2"),
        ".msg"
      ),
      userpwd = 'taxsim:02138',
      ftp.use.epsv = FALSE,
      dirlistonly = FALSE
    )
    #outputs error message for reading
    if (is.raw(error)) {
      write(rawToChar(error, multiple = FALSE), file = "error2")
    }
    else{
      write(error, file = "error")
    }
    #' @importFrom utils read.table
    errorout <- read.table(file = "error", sep = "\n")
    
    output(data2, dfr, k = 1)
  }
  #' @importFrom RCurl ftpUpload
  ftpUpload(
    as.character(DATAFile),
    paste0(
      "ftp://taxsim:02138@taxsimftp.nber.org/tmp/",
      as.character(DATAFile)
    )
  )
  #FETCHES FILE
  #' @importFrom RCurl getURLContent
  data <- getURLContent(
    url = paste0(
      "ftp://taxsimftp.nber.org/tmp/",
      as.character(DATAFile),
      ".taxsim"
    ),
    userpwd = 'taxsim:02138',
    ftp.use.epsv = FALSE,
    dirlistonly = FALSE
  )
  #' @importFrom RCurl getURLContent
  error <- getURLContent(
    url = paste0(
      "ftp://taxsimftp.nber.org/tmp/",
      as.character(DATAFile),
      ".msg"
    ),
    userpwd = 'taxsim:02138',
    ftp.use.epsv = FALSE,
    dirlistonly = FALSE
  )
  if (is.raw(error)) {
    write(rawToChar(error, multiple = FALSE), file = "error")
  }
  else{
    write(error, file = "error")
  }
  #OUTPUT
  
  #Print ERROR MESSAGES IF ANY
  #optional second display....
  file.show("error")
  #' @importFrom utils read.table
  errorout <- read.table(file = "error", sep = "\n")
  print(errorout)
  #OUTPUTS DATA TO OUTPUT FILE
  listfiles = output(data, dfr)
  calcdfr <- listfiles
  #Return original data +calculated one or just original
  return(calcdfr)
}