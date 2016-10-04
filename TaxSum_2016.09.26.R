#2016.09.26
library(readstata13)
library(plyr)
library(ggplot2)

setwd('~/Dropbox/PhD/Diss/Analyses/ECLS-B')

taxout <- read.dta13('taxsimout.dta')
names(taxout)[30:61] <- c('fed_agi', 'ui_agi', 'ss_agi', 'zero_bracket', 'personal_ex',
                          'ex_phase', 'dedu_phase', 'dedu_all', 'fti', 'tti', 'ex_sur',
                          'gen_tx_c', 'ctc', 'a_ctc', 'ccc', 'eitc', 'inc_amt',
                          'amt_l', 'fed_tbc', 'fica_1', 'shh_i', 's_r', 's_agi',
                          's_e', 's_sd', 's_id', 's_ti', 's_pc', 's_ccc', 's_eitc',
                          's_c', 's_b')

lm.s <- lm(s_eitc ~state + year + pwages + swages + depchild + mstat, data = taxout)
summary(lm.s)
round(coef(lm.s), 4)
# need to see if the stuff i don't have matters

lm.c <- lm(ctc ~ dividends + otherprop + pensions + gssi + transfers + rentpaid + proptax + otheritem + childcare + ui + mortgage + stcg + ltcg, data = taxout)
summary(lm.c)

lm.a <- lm(a_ctc ~ dividends + otherprop + pensions + gssi + transfers + rentpaid + proptax + otheritem + childcare + ui + mortgage + stcg + ltcg, data = taxout)
summary(lm.a)

lm.s <- lm(s_eitc ~ dividends + otherprop + pensions + gssi + transfers + rentpaid + proptax + otheritem + childcare + ui + mortgage + stcg + ltcg, data = taxout)
summary(lm.s)

# what level of income does seitc apply to?

ggplot(taxout, aes(x=pwages, y=s_eitc)) + geom_point()
ggplot(taxout[taxout$s_eitc > 0,], aes(x=pwages, y=s_eitc)) + geom_point()
ggplot(taxout[taxout$s_eitc > 0,], aes(x=swages, y=s_eitc)) + geom_point()


head(taxout[taxout$s_eitc >0 & taxout$pwages == 0,])
hist(taxout[taxout$s_eitc > 0,]$pwages)

# 
tax.sum <- ddply(taxout, .(year, state, depchild), summarise, eitc = mean(eitc, na.rm=T), seitc = mean(s_eitc, na.rm=T), ctc = mean(ctc, na.rm=T), actc = mean(a_ctc, na.rm=T))

adjust <- function(x)
{
  y2001 <- 1.36
  y2002 <- 1.34
  y2003 <- 1.31
  y2004 <- 1.27
  y2005 <- 1.23
  y2006 <- 1.19
  y2007 <- 1.16
  y2008 <- 1.12
  
  if (unique(x$year) == 2001)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2001
  }
  if (unique(x$year) == 2002)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2002
  }
  if (unique(x$year) == 2003)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2003
  }
  if (unique(x$year) == 2004)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2004
  }  
  if (unique(x$year) == 2005)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2005
  }
  if (unique(x$year) == 2006)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2006
  }
  if (unique(x$year) == 2007)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2007
  }
  if (unique(x$year) == 2008)
  {
    x[,7:ncol(x)] <- x[,7:ncol(x)]*y2008
  }
  return (x)
}

tax.sum <- ddply(tax.sum, .(year), adjust)

taxout$state <- as.factor(taxout$state)

tax.sum$state <- as.factor(tax.sum$state)

tax.sum$state_lt[tax.sum$state==1] <- 'AL'
tax.sum$state_lt[tax.sum$state==2] <- 'AK' 
tax.sum$state_lt[tax.sum$state==4] <- 'AR'
tax.sum$state_lt[tax.sum$state==3] <- 'AZ'
tax.sum$state_lt[tax.sum$state==5] <-  'CA'
tax.sum$state_lt[tax.sum$state==6] <-  'CO'
tax.sum$state_lt[tax.sum$state==7] <-  'CT'
tax.sum$state_lt[tax.sum$state==9] <-  'DC'
tax.sum$state_lt[tax.sum$state==8] <-  'DE'
tax.sum$state_lt[tax.sum$state==10] <-  'FL'
tax.sum$state_lt[tax.sum$state==11] <-  'FA'
tax.sum$state_lt[tax.sum$state==12] <-  'HI'
tax.sum$state_lt[tax.sum$state==13] <-  'ID'
tax.sum$state_lt[tax.sum$state==14] <-  'IL'
tax.sum$state_lt[tax.sum$state==15] <-  'IN'
tax.sum$state_lt[tax.sum$state==16] <-  'IA'
tax.sum$state_lt[tax.sum$state==17] <-  'KS'
tax.sum$state_lt[tax.sum$state==18] <-  'KY'
tax.sum$state_lt[tax.sum$state==19] <-  'LA'
tax.sum$state_lt[tax.sum$state==22] <-  'MA'
tax.sum$state_lt[tax.sum$state==21] <-  'MD'
tax.sum$state_lt[tax.sum$state==20] <-  'ME'
tax.sum$state_lt[tax.sum$state==23] <-  'MI'
tax.sum$state_lt[tax.sum$state==24] <-  'MN'
tax.sum$state_lt[tax.sum$state==26] <-  'MO'
tax.sum$state_lt[tax.sum$state==25] <-  'MS'
tax.sum$state_lt[tax.sum$state==27] <-  'MT'
tax.sum$state_lt[tax.sum$state==24] <-  'NC'
tax.sum$state_lt[tax.sum$state==35] <-  'ND'
tax.sum$state_lt[tax.sum$state==33] <-  'NY'
tax.sum$state_lt[tax.sum$state==28] <-  'NE'
tax.sum$state_lt[tax.sum$state==30] <-  'NH'
tax.sum$state_lt[tax.sum$state==31] <-  'NJ'
tax.sum$state_lt[tax.sum$state==32] <-  'NM'
tax.sum$state_lt[tax.sum$state==29] <-  'NV'
tax.sum$state_lt[tax.sum$state==36] <-  'OH'
tax.sum$state_lt[tax.sum$state==37] <-  'OK'
tax.sum$state_lt[tax.sum$state==38] <-  'OR'
tax.sum$state_lt[tax.sum$state==39] <-  'PA'
tax.sum$state_lt[tax.sum$state==40] <-  'RI'
tax.sum$state_lt[tax.sum$state==41] <-  'SC'
tax.sum$state_lt[tax.sum$state==42] <-  'SD'
tax.sum$state_lt[tax.sum$state==43] <-   'TN'
tax.sum$state_lt[tax.sum$state==44] <-  'TX'
tax.sum$state_lt[tax.sum$state==45] <-  'UT'
tax.sum$state_lt[tax.sum$state==47] <-  'VA'
tax.sum$state_lt[tax.sum$state==46] <-  'VT'
tax.sum$state_lt[tax.sum$state==48] <-  'WA'
tax.sum$state_lt[tax.sum$state==50] <-  'WI'
tax.sum$state_lt[tax.sum$state==49] <- 'WV'
tax.sum$state_lt[tax.sum$state==51] <-  'WY'


write.csv(tax.sum, 'tax_sum.csv')

ggplot(tax.sum, aes(x=year, y=seitc, colour=state_lt)) + geom_point()
ggplot(tax.sum, aes(x=year, y=ctc, colour=state_lt)) + geom_point()
ggplot(tax.sum, aes(x=year, y=actc, colour=state_lt)) + geom_point()
