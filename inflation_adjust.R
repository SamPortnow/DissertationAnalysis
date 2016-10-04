inflation_adjust <- function(base_year=NA){  
  require(dplyr)
  require(xts)
  if (nchar(base_year) == 4){
    #Load file from BLS servers
    temp<-tempfile()
    download.file("http://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems",temp)
    cu_main<-read.table(temp,
                        header=FALSE,
                        sep="t",
                        skip=1,
                        stringsAsFactors=FALSE,
                        strip.white=TRUE)
    colnames(cu_main)<-c("series_id", "year", "period", "value", "footnote_codes")
    unlink(temp)
    #Get rid of annual time periods and add real dates.
    cu_main <- subset(cu_main,series_id=="CUSR0000SA0")
    cu_main <- subset (cu_main,period!="M13")
    cu_main <- subset (cu_main,period!="S01")
    cu_main <- subset (cu_main,period!="S02")
    cu_main <- subset (cu_main,period!="S03")
    cu_main$date <-as.Date(paste(cu_main$year, cu_main$period,"01",sep="-"),"%Y-M%m-%d")
    cu_main <- cu_main[c('date','value')]
    cu_main <- xts(cu_main[,-1], order.by=cu_main[,1])
    cu_main <- round(cu_main, 1)
    
    # Get average yearly CPI.
    avg.cpi <- apply.yearly(cu_main, mean)
    avg.cpi <- round(avg.cpi, 1)
    # Formula for calculating inflation example: $1.00 * (1980 CPI/ 2014 CPI) = 1980 price
    cf <- avg.cpi/as.numeric(avg.cpi[as.character(base_year)])
    colnames(cf) <- "adj_value"
    #cf <- round(cf, 2)
    dat <- merge(avg.cpi, cf)
    # Xts object to data frame
    dat <- data.frame(date=index(dat), coredata(dat))
    dat$base.year <- as.character(base_year)
    dat$pct_increase <- (1-dat$adj_value) * -100
    # Round dollar amounts
    dat$adj_value <- round(dat$adj_value, 2)
    # Reorder cols in a more usable fassion
    dat <- dat[c('date','base.year', 'adj_value', 'pct_increase')]
    return(dat)
  }
  else {(message(
    "***************************************************************************************
    Please input a valid four digit year without quotes. For example: 2015.
    ***************************************************************************************", appendLF = TRUE))
  }
}

y2001 <- 1.36
y2002 <- 1.34
y2003 <- 1.31
y2004 <- 1.27
y2005 <- 1.23
y2006 <- 1.19
y2007 <- 1.16
y2008 <- 1.12