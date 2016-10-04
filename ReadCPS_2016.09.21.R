# September 22, 2016
# This file is for reading in the cps data
# and filtering the variables I need for taxism
# amended script to do all the processing after binding
# have to file different returns for single partners living together

library(foreign)
library(plyr)
library(psych)
library(maps)
library(RCurl)
library(dplyr)
library(ggplot2)

setwd('~/Dropbox/PhD/Diss/Analyses/ECLS-B')

y01 <- read.spss('./CPS/cpsmar01.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y02 <- read.spss('./CPS/cpsmar02.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y03 <- read.spss('./CPS/cpsmar03.sav', to.data.frame=T, use.missings = T, use.value.labels = F)

y04 <- read.spss('./CPS/cpsmar04.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
pp04 <- read.spss('./CPS/pp04.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
hh04 <- read.spss('./CPS/hh04.sav', to.data.frame=T, use.missings = T, use.value.labels = F)

names(pp04) <- tolower(names(pp04))
names(hh04) <- tolower(names(hh04))

names(pp04)[4:19] <- paste0(names(pp04)[4:19], '_upd')

y04 <- join(y04, pp04, by=c('precord', 'ph_seq', 'pppos'))

table(pp04$filestat_upd)
replace <- select_vars(names(y04), contains('_upd'))
original <- sub('_upd', "", replace)
y04[,original] <- y04[,replace]
y04 <- y04[, ! names(y04)  %in% replace ]

y04$hrecord <- as.numeric(y04$hrecord)
names(hh04)[4:7] <- paste0(names(hh04)[4:7], '_upd')
y04 <- join(y04, hh04, by=c('hrecord', 'h_seq', 'hhpos'))

replace <- select_vars(names(y04), contains('_upd'))
original <- sub('_upd', "", replace)
y04[,original] <- y04[,replace]
y04 <- y04[, ! names(y04)  %in% replace ]

y04 <- y04[,names(y04) %in% names(y01)]

y05 <- read.spss('./CPS/cpsmar05.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y06 <- read.spss('./CPS/cpsmar06.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y07 <- read.spss('./CPS/cpsmar07.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y08 <- read.spss('./CPS/cpsmar08.sav', to.data.frame=T, use.missings = T, use.value.labels = F)

unique(y01$h_year)
unique(y02$h_year)
unique(y03$h_year)
unique(y04$h_year)
unique(y05$h_year)
unique(y06$h_year)
unique(y07$h_year)
unique(y08$h_year)

# variables i need

# id - CASEID
# role - a_exprrp
# year - YEAR | year
# state - State (SOI Codes) | hg_st60
# mstat - Marital Status | a_maritl
# depx - Dependent Exemptions |
# agex - Age of primary taxpayer*100 plus the age the secondary taxpayer (if any) | a_age
# pwages - wage and salary income of Taxpayer | hwsval
# swages - wage and salary income of Spouse | hwsval
# dividends - Dividend income | hdivval 
# otherprop - Other property | hrntval (household rental income), halm_val, hintval (household interest income),
# pensions - taxable pensions | hretval (Pension or retirement income other than Social Sec. or Veterans benefits )
# gssi - Gross Social Security Benefits | hssval 
# transfers - Other non-taxable transfer Income | hcspval (child support), hinc_wc (workers comp), hvetval (veterans benefits), hpawval (public assistance)
# rentpaid - Rent Paid | 
# proptax - Real Estate taxes paid | prop_tax
# otheritem - Other itemized deductions | 
# childcare - Child care expenses |
# ui - Unemployment compensation received | huc_val
# depchild - Number of dependents under age 17 | 
# mortgage - Home mortage interest; charitable contributions | 
# stcg - Short term capital gains or losses | cap_gain
# ltcg - Long term capital gains or losses | cap_loss
y01$wc

vars <- c('h_seq', 'a_exprrp', 'h_year', 'gestfips',
          'a_maritl', 'a_age', 
          'pearnval', 'hdivval', 
          'ws_val',  'se_val', 'div_val', 'rnt_val', 
          'alm_val', 'int_val', 'ret_val1', 'ret_val2', 'ss_val', 'csp_val', 'wc_val', 'vet_val', 'paw_val',          
          'prop_tax', 'uc_val', 'cap_gain', 'cap_loss', 'hunder18', 'dep_stat', 'filestat', 'a_famrel', 'ffpos')

y01 <- y01[,vars]
y02 <- y02[,vars]
y03 <- y03[,vars]
y04 <- y04[,vars]
y05 <- y05[,vars]
y06 <- y06[,vars]
y07 <- y07[,vars]
y08 <- y08[,vars]





### put it all together

df <- rbind(y01, y02, y03, y04, y05, y06, y07, y08)
df <- df[df$filestat!=6,]
# need to fix 2004
df <- df[df$filestat!=0,]

# set status
df$status[df$filestat==5] <- 1
df$status[df$filestat==1 | df$filestat==2 | df$filestat==3] <- 2
df$status[df$filestat==4] <- 3

# 65 recodes # this is wrong
df$a65[df$filestat==2] <- 1
df$a65[df$filestat==3] <- 2
df$a65 <- ifelse(df$filestat==1 | df$filestat==2, df$filestat, 0)

#df$other_items <- df$fed_ret + df$fica
df$dep <- df$hunder18

df <- df[df$a_famrel != 0,]
df <- df[df$dep > 0 & df$dep < 4,]

df$h_w <- 0
df$s_w <- 0
df$k_w <- 0

df[df$a_famrel==1,]$h_w <- df[df$a_famrel==1,]$pearnval
df[df$a_famrel==4,]$h_w <- df[df$a_famrel==4,]$pearnval
df[df$a_famrel==2,]$s_w <- df[df$a_famrel==2,]$pearnval
df[df$a_famrel==3,]$k_w <- df[df$a_famrel==3,]$pearnval 

df$other<- df$rnt_val + df$alm_val + df$int_val
df$transfers <- df$csp_val + df$wc_val + df$vet_val + df$paw_val

reducedf <- function(x)
{
  # if there is a spouse
  if (2 %in% x$a_famrel)
  {
    x$s_w <- x$s_w + x$k_w
    d <- x[x$a_famrel==1 | x$a_famrel==2,]
    return (d)
  }
  # if there is NOT a spouse
  else
  {
    x$h_w <- x$h_w + x$k_w
    d <- x[x$a_famrel==1,]
    return (d)
  }
}

df.fam <- ddply(df, .(h_seq, ffpos, h_year), reducedf)

# remove people who arent filing
df.fam <- df.fam[df.fam$filestat != 6,]

### one last time, what are the vars

df.fam$rentpaid <- 0
df.fam$cc <- 0
df.fam$deduc <- 0
df.fam$otheritem <- 0


vars <- c('h_seq', 'h_year', 'gestfips', 'filestat', 'dep', 'a65', 'h_w', 's_w', 'div_val', 'other', 
          'ret_val1', 'ret_val2', 'ss_val', 'transfers', 'rentpaid', 'prop_tax', 'otheritem', 'cc', 'uc_val', 'dep', 'deduc', 
          'cap_gain', 'cap_loss')

df.fam <- df.fam[,vars]
#########

names(state.fips)[1] <- 'gestfips'
df.fam <- join(df.fam, state.fips, by='gestfips')

df.fam[is.na(df.fam$abb),]$gestfips

df.fam$state[df.fam$abb=='AL'] <- 1
df.fam$state[df.fam$gestfips==2] <- 2
df.fam$state[df.fam$abb=='AR'] <- 4
df.fam$state[df.fam$abb=='AZ'] <- 3
df.fam$state[df.fam$gestfips==6] <- 5
df.fam$state[df.fam$abb=='CO'] <- 6
df.fam$state[df.fam$gestfips==9] <- 7
df.fam$state[df.fam$gestfips==11] <- 9
df.fam$state[df.fam$abb=='DE'] <- 8
df.fam$state[df.fam$abb=='FL'] <- 10
df.fam$state[df.fam$abb=='GA'] <- 11
df.fam$state[df.fam$gestfips==15] <- 12
df.fam$state[df.fam$abb=='ID'] <- 13
df.fam$state[df.fam$abb=='IL'] <- 14
df.fam$state[df.fam$abb=='IN'] <- 15
df.fam$state[df.fam$abb=='IA'] <- 16
df.fam$state[df.fam$abb=='KS'] <- 17
df.fam$state[df.fam$abb=='KY'] <- 18
df.fam$state[df.fam$abb=='LA'] <- 19
df.fam$state[df.fam$abb=='MA'] <- 22
df.fam$state[df.fam$abb=='MD'] <- 21
df.fam$state[df.fam$abb=='ME'] <- 20
df.fam$state[df.fam$abb=='MI'] <- 23
df.fam$state[df.fam$abb=='MN'] <- 24
df.fam$state[df.fam$abb=='MO'] <- 26
df.fam$state[df.fam$abb=='MS'] <- 25
df.fam$state[df.fam$abb=='MT'] <- 27
df.fam$state[df.fam$abb=='NC'] <- 34
df.fam$state[df.fam$abb=='ND'] <- 35
df.fam$state[df.fam$abb=='NY'] <- 33
df.fam$state[df.fam$abb=='NE'] <- 28
df.fam$state[df.fam$abb=='NH'] <- 30
df.fam$state[df.fam$abb=='NJ'] <- 31
df.fam$state[df.fam$abb=='NM'] <- 32
df.fam$state[df.fam$abb=='NV'] <- 29
df.fam$state[df.fam$abb=='OH'] <- 36
df.fam$state[df.fam$abb=='OK'] <- 37
df.fam$state[df.fam$abb=='OR'] <- 38
df.fam$state[df.fam$abb=='PA'] <- 39
df.fam$state[df.fam$abb=='RI'] <- 40
df.fam$state[df.fam$abb=='SC'] <- 41
df.fam$state[df.fam$abb=='SD'] <- 42
df.fam$state[df.fam$abb=='TN'] <-  43
df.fam$state[df.fam$abb=='TX'] <- 44
df.fam$state[df.fam$abb=='UT'] <- 45
df.fam$state[df.fam$abb=='VA'] <- 47
df.fam$state[df.fam$abb=='VT'] <- 46
df.fam$state[df.fam$abb=='WA'] <- 48
df.fam$state[df.fam$abb=='WI'] <- 50
df.fam$state[df.fam$abb=='WV'] <- 49
df.fam$state[df.fam$abb=='WY'] <- 51  

unique(df.fam[is.na(df.fam$state),][,c('state','gestfips')])

df.fam$ret_val <- df.fam$ret_val1 + df.fam$ret_val2

vars <- c('h_seq', 'h_year', 'state', 'filestat', 'dep', 'a65', 'h_w', 's_w', 'div_val', 'other', 
          'ret_val', 'ss_val', 'transfers', 'rentpaid', 'prop_tax', 'otheritem', 'cc', 'uc_val', 'dep', 'deduc', 
          'cap_gain', 'cap_loss')

df.fam <- df.fam[,vars]

table(df.fam$mstat)

head(df.fam)
names(df.fam) <- c('id', 'year', 'state', 'mstat', 'depx', 'agex', 'pwages', 'swages', 'dividends', 'otherprop', 'pensions', 'gssi', 'transfers', 'rentpaid', 'proptax', 
                   'otheritem', 'childcare', 'ui', 'depchild', 'mortgage', 'stcg', 'ltcg')

df.fam[df.fam$mstat==1 | df.fam$mstat == 2 | df.fam$mstat == 3,]$mstat <- 2
df.fam[df.fam$mstat==4 | df.fam$mstat == 5,]$mstat <- 3


write.table(df.fam, 'taxism.csv', col.names =F, row.names = F, sep=',')
write.dta(df.fam, 'taxsim.dta')


taxout1 <- taxsim9(df.fam[df.fam$year==2001,], detail=2)
taxout2 <- taxsim9(df.fam[df.fam$year==2002,], detail=2)
taxout3 <- taxsim9(df.fam[df.fam$year==2003,], detail=2)
taxout4 <- taxsim9(df.fam[df.fam$year==2004,], detail=2)
taxout5 <- taxsim9(df.fam[df.fam$year==2005,], detail=2)
taxout6 <- taxsim9(df.fam[df.fam$year==2006,], detail=2)
taxout7 <- taxsim9(df.fam[df.fam$year==2007,], detail=2)
taxout8 <- taxsim9(df.fam[df.fam$year==2008,], detail=2)

taxout <- rbind(taxout1, taxout2, taxout3, taxout4, taxout5, taxout6, taxout7, taxout8)

taxout <- taxout[,! duplicated(names(taxout))]

write.table(taxout, 'taxism_out.csv', col.names =F, row.names = F, sep=',')

#' @return Returns a dataframe containing the following results (with column names taking on acronyms listed): \describe{\item{taxsimid}{Case ID}\item{year}{Year}\item{state}{State}\item{fiitax}{Federal income tax liability... including capital gains rates surtaxes, AMT, and refundable/nonrefundable taxcredits}\item{fica}{FICA}\item{frate}{Federal Marginal Rate}\item{srate}{State Marginal Rate}\item{ficar}{Fica Rate}}
#' If detailed results are requested the following results are also added:\describe{\item{fagi}{Federal AGI}\item{uiagi}{UI in AGI}\item{ssagi}{Social Security in AGI}\item{zba}{Zero Bracket Amount}\item{persex}{Personal Exemptions}\item{exphase}{Exemption Phaseout}\item{dphase}{Deduction Phaseout}\item{dallow}{Deductions Allowed}\item{fti}{Federal Taxable Income}
#' \item{taxti}{Tax on Taxable Income}\item{exsur}{Exemption Surtax}\item{gencred}{General Tax Credit}\item{ctcred}{Child Tax Credit}\item{accred}{Additional Child Tax Credit}\item{cccred}{Child Care Credit}\item{eic}{Earned Income Credit}\item{iamt}{Income for Alternative Minimum Tax}\item{amtliab}{AMT Liability after credit for regular tax}\item{fitcred}{Federal Income Tax Before Credits}\item{fica}{FICA}}
#' Finally, if a state is specified, we also return the following state-specific results:\describe{\item{shi}{State Household Income}\item{srp}{State Rent Payments}\item{sagi}{State AGI}\item{sea}{State Exemption Amount}\item{ssd}{State Standard Deduction}\item{sid}{State Itemized Deductions}\item{sti}{State Taxable Income}\item{sptc}{State Property Tax Credit}\item{sccc}{State Child Care Credit}\item{seic}{State EIC}\item{stc}{State Total Credits}\item{sbr}{State Bracket Rate}}
