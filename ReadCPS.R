# September 13, 2016
# This file is for reading in the cps data
# and filtering the variables I need for taxism

library(foreign)
library(plyr)
library(psych)
library(maps)

setwd('~/Dropbox/PhD/Diss/Analyses/ECLS-B')

y01 <- read.spss('./CPS/cpsmar01.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y02 <- read.spss('./CPS/cpsmar02.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y03 <- read.spss('./CPS/cpsmar03.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
y04 <- read.spss('./CPS/cpsmar04.sav', to.data.frame=T, use.missings = T, use.value.labels = F)
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

vars <- c('h_seq', 'a_exprrp', 'h_year', 'gestfips',
          'a_maritl', 'a_age', 'hwsval', 'pearnval', 'hdivval', 'hrntval', 
          'halmval', 'hintval', 'hretval', 'hssval', 'hcspval', 'hinc_wc', 'hvetval', 'hpawval',
          'prop_tax', 'hucval', 'cap_gain', 'cap_loss', 'hunder18', 'dep_stat', 'filestat')

y01 <- y01[,vars]
y02 <- y02[,vars]
y03 <- y03[,vars]
y04 <- y04[,vars]
y05 <- y05[,vars]
y06 <- y06[,vars]
y07 <- y07[,vars]
y08 <- y08[,vars]

################# y01

y01$maritl <- ifelse(y01$a_maritl == 1 | y01$a_maritl == 2 | y01$a_maritl == 3, 1, 0)

h_w <- ddply(y01, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y01, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y01 <- join(y01, h_w, by='h_seq')
y01 <- join(y01, s_w, by='h_seq')

y01$other<- y01$hrntval + y01$halmval + y01$hintval
y01$transfers <- y01$hcspval + y01$hinc_wc + y01$hvetval + y01$hpawval

age <- ddply(y01, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y01 <- join(y01, age, by='h_seq')

y01 <- y01[y01$a_exprrp != 2,]

dpndnts <- ddply(y01, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y01, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y01 <- join(y01, dpndnts, by='h_seq')
y01 <- join(y01, children, by='h_seq')

y01 <- y01[y01$dp_c > 0,]
y01 <- y01[y01$dp_c < 4,]
y01 <- y01[y01$a_exprrp==1,]


############### y02

y02$maritl <- ifelse(y02$a_maritl == 1 | y02$a_maritl == 2 | y02$a_maritl == 3, 1, 0)

h_w <- ddply(y02, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y02, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y02 <- join(y02, h_w, by='h_seq')
y02 <- join(y02, s_w, by='h_seq')

y02$other<- y02$hrntval + y02$halmval + y02$hintval
y02$transfers <- y02$hcspval + y02$hinc_wc + y02$hvetval + y02$hpawval

age <- ddply(y02, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y02 <- join(y02, age, by='h_seq')

y02 <- y02[y02$a_exprrp != 2,]

dpndnts <- ddply(y02, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y02, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y02 <- join(y02, dpndnts, by='h_seq')
y02 <- join(y02, children, by='h_seq')

y02 <- y02[y02$dp_c > 0,]
y02 <- y02[y02$dp_c < 4,]
y02 <- y02[y02$a_exprrp==1,]

################### y03

y03$maritl <- ifelse(y03$a_maritl == 1 | y03$a_maritl == 2 | y03$a_maritl == 3, 1, 0)

h_w <- ddply(y03, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y03, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y03 <- join(y03, h_w, by='h_seq')
y03 <- join(y03, s_w, by='h_seq')

y03$other<- y03$hrntval + y03$halmval + y03$hintval
y03$transfers <- y03$hcspval + y03$hinc_wc + y03$hvetval + y03$hpawval

age <- ddply(y03, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y03 <- join(y03, age, by='h_seq')

y03 <- y03[y03$a_exprrp != 2,]

dpndnts <- ddply(y03, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y03, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y03 <- join(y03, dpndnts, by='h_seq')
y03 <- join(y03, children, by='h_seq')

y03 <- y03[y03$dp_c > 0,]
y03 <- y03[y03$dp_c < 4,]
y03 <- y03[y03$a_exprrp==1,]

############### y04

y04$maritl <- ifelse(y04$a_maritl == 1 | y04$a_maritl == 2 | y04$a_maritl == 3, 1, 0)

h_w <- ddply(y04, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y04, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y04 <- join(y04, h_w, by='h_seq')
y04 <- join(y04, s_w, by='h_seq')

y04$other<- y04$hrntval + y04$halmval + y04$hintval
y04$transfers <- y04$hcspval + y04$hinc_wc + y04$hvetval + y04$hpawval

age <- ddply(y04, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y04 <- join(y04, age, by='h_seq')

y04 <- y04[y04$a_exprrp != 2,]

dpndnts <- ddply(y04, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y04, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y04 <- join(y04, dpndnts, by='h_seq')
y04 <- join(y04, children, by='h_seq')

y04 <- y04[y04$dp_c > 0,]
y04 <- y04[y04$dp_c < 4,]
y04 <- y04[y04$a_exprrp==1,]


#################### y05

y05$maritl <- ifelse(y05$a_maritl == 1 | y05$a_maritl == 2 | y05$a_maritl == 3, 1, 0)

h_w <- ddply(y05, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y05, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y05 <- join(y05, h_w, by='h_seq')
y05 <- join(y05, s_w, by='h_seq')

y05$other<- y05$hrntval + y05$halmval + y05$hintval
y05$transfers <- y05$hcspval + y05$hinc_wc + y05$hvetval + y05$hpawval

age <- ddply(y05, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y05 <- join(y05, age, by='h_seq')

y05 <- y05[y05$a_exprrp != 2,]

dpndnts <- ddply(y05, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y05, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y05 <- join(y05, dpndnts, by='h_seq')
y05 <- join(y05, children, by='h_seq')

y05 <- y05[y05$dp_c > 0,]
y05 <- y05[y05$dp_c < 4,]
y05 <- y05[y05$a_exprrp==1,]


##################### y06

y06$maritl <- ifelse(y06$a_maritl == 1 | y06$a_maritl == 2 | y06$a_maritl == 3, 1, 0)

h_w <- ddply(y06, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y06, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y06 <- join(y06, h_w, by='h_seq')
y06 <- join(y06, s_w, by='h_seq')

y06$other<- y06$hrntval + y06$halmval + y06$hintval
y06$transfers <- y06$hcspval + y06$hinc_wc + y06$hvetval + y06$hpawval

age <- ddply(y06, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y06 <- join(y06, age, by='h_seq')

y06 <- y06[y06$a_exprrp != 2,]

dpndnts <- ddply(y06, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y06, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y06 <- join(y06, dpndnts, by='h_seq')
y06 <- join(y06, children, by='h_seq')

y06 <- y06[y06$dp_c > 0,]
y06 <- y06[y06$dp_c < 4,]
y06 <- y06[y06$a_exprrp==1,]

########################### y07

y07$maritl <- ifelse(y07$a_maritl == 1 | y07$a_maritl == 2 | y07$a_maritl == 3, 1, 0)

h_w <- ddply(y07, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y07, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y07 <- join(y07, h_w, by='h_seq')
y07 <- join(y07, s_w, by='h_seq')

y07$other<- y07$hrntval + y07$halmval + y07$hintval
y07$transfers <- y07$hcspval + y07$hinc_wc + y07$hvetval + y07$hpawval

age <- ddply(y07, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y07 <- join(y07, age, by='h_seq')

y07 <- y07[y07$a_exprrp != 2,]

dpndnts <- ddply(y07, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y07, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y07 <- join(y07, dpndnts, by='h_seq')
y07 <- join(y07, children, by='h_seq')

y07 <- y07[y07$dp_c > 0,]
y07 <- y07[y07$dp_c < 4,]
y07 <- y07[y07$a_exprrp==1,]


################# y08


y08$maritl <- ifelse(y08$a_maritl == 1 | y08$a_maritl == 2 | y08$a_maritl == 3, 1, 0)

h_w <- ddply(y08, .(h_seq), summarise, h_wage = pearnval[a_exprrp==1])
s_w <- ddply(y08, .(h_seq), summarise, s_wage = pearnval[a_exprrp==3 | a_exprrp == 4])

y08 <- join(y08, h_w, by='h_seq')
y08 <- join(y08, s_w, by='h_seq')

y08$other<- y08$hrntval + y08$halmval + y08$hintval
y08$transfers <- y08$hcspval + y08$hinc_wc + y08$hvetval + y08$hpawval

age <- ddply(y08, .(h_seq), summarise, age = sum(a_age[a_exprrp==1] * 100, a_age[a_exprrp==3 | a_exprrp == 4], na.rm=T))
y08 <- join(y08, age, by='h_seq')

y08 <- y08[y08$a_exprrp != 2,]

dpndnts <- ddply(y08, .(h_seq), summarise, dp = sum(dep_stat > 0))
children <- ddply(y08, .(h_seq), summarise, dp_c = sum(dep_stat[a_age <19] > 0))

y08 <- join(y08, dpndnts, by='h_seq')
y08 <- join(y08, children, by='h_seq')

y08 <- y08[y08$dp_c > 0,]
y08 <- y08[y08$dp_c < 4,]
y08 <- y08[y08$a_exprrp==1,]


### put it all together

df <- rbind(y01, y02, y03, y04, y05, y06, y07, y08)

### one last time, what are the vars

df$rentpaid <- 0
df$otheritem <- 0
df$cc <- 0
df$deduc <- 0


vars <- c('h_seq', 'h_year', 'gestfips', 'filestat', 'dp', 'age', 'h_wage', 's_wage', 'hdivval', 'other', 
           'hretval', 'hssval', 'transfers', 'rentpaid', 'prop_tax','otheritem', 'cc', 'hucval', 'dp_c', 'deduc', 'cap_gain', 'cap_loss')
df <- df[,vars]
df[is.na(df$s_wage),]$s_wage <- 0
df[is.na(df$h_wage),]$h_wage <- 0

# convert to 2016 
y2001 <- 1.36
y2002 <- 1.34
y2003 <- 1.31
y2004 <- 1.27
y2005 <- 1.23
y2006 <- 1.19
y2007 <- 1.16
y2008 <- 1.12
# 
df[df$h_year==2001,]$h_wage <- df[df$h_year==2001,]$h_wage * y2001
df[df$h_year==2002,]$h_wage <- df[df$h_year==2002,]$h_wage * y2002
df[df$h_year==2003,]$h_wage <- df[df$h_year==2003,]$h_wage * y2003
df[df$h_year==2004,]$h_wage <- df[df$h_year==2004,]$h_wage * y2004
df[df$h_year==2005,]$h_wage <- df[df$h_year==2005,]$h_wage * y2005
df[df$h_year==2006,]$h_wage <- df[df$h_year==2006,]$h_wage * y2006
df[df$h_year==2007,]$h_wage <- df[df$h_year==2007,]$h_wage * y2007
df[df$h_year==2008,]$h_wage <- df[df$h_year==2008,]$h_wage * y2008

df[df$h_year==2001,]$s_wage <- df[df$h_year==2001,]$s_wage * y2001
df[df$h_year==2002,]$s_wage <- df[df$h_year==2002,]$s_wage * y2002
df[df$h_year==2003,]$s_wage <- df[df$h_year==2003,]$s_wage * y2003
df[df$h_year==2004,]$s_wage <- df[df$h_year==2004,]$s_wage * y2004
df[df$h_year==2005,]$s_wage <- df[df$h_year==2005,]$s_wage * y2005
df[df$h_year==2006,]$s_wage <- df[df$h_year==2006,]$s_wage * y2006
df[df$h_year==2007,]$s_wage <- df[df$h_year==2007,]$s_wage * y2007
df[df$h_year==2008,]$s_wage <- df[df$h_year==2008,]$s_wage * y2008

names(state.fips)[1] <- 'gestfips'
df <- join(df, state.fips, by='gestfips')
head(df$abb)

df[is.na(df$abb),]$gestfips

df$state[df$abb=='AL'] <- 1
df$state[df$gestfips==2] <- 2
df$state[df$abb=='AR'] <- 4
df$state[df$ab=='AZ'] <- 3
df$state[df$gestfips==6] <- 5
df$state[df$abb=='CO'] <- 6
df$state[df$gestfips==9] <- 7
df$state[df$gestfips==11] <- 9
df$state[df$abb=='DE'] <- 8
df$state[df$abb=='FL'] <- 10
df$state[df$abb=='GA'] <- 11
df$state[df$gestfips==15] <- 12
df$state[df$abb=='ID'] <- 13
df$state[df$abb=='IL'] <- 14
df$state[df$abb=='IN'] <- 15
df$state[df$abb=='IA'] <- 16
df$state[df$abb=='KS'] <- 17
df$state[df$abb=='KY'] <- 18
df$state[df$abb=='LA'] <- 19
df$state[df$abb=='MA'] <- 22
df$state[df$abb=='MD'] <- 21
df$state[df$abb=='ME'] <- 20
df$state[df$abb=='MI'] <- 23
df$state[df$abb=='MN'] <- 24
df$state[df$abb=='MO'] <- 26
df$state[df$abb=='MS'] <- 25
df$state[df$abb=='MT'] <- 27
df$state[df$abb=='NC'] <- 34
df$state[df$abb=='ND'] <- 35
df$state[df$abb=='NY'] <- 33
df$state[df$abb=='NE'] <- 28
df$state[df$abb=='NH'] <- 30
df$state[df$abb=='NJ'] <- 31
df$state[df$abb=='NM'] <- 32
df$state[df$abb=='NV'] <- 29
df$state[df$abb=='OH'] <- 36
df$state[df$abb=='OK'] <- 37
df$state[df$abb=='OR'] <- 38
df$state[df$abb=='PA'] <- 39
df$state[df$abb=='RI'] <- 40
df$state[df$abb=='SC'] <- 41
df$state[df$abb=='SD'] <- 42
df$state[df$abb=='TN'] <-  43
df$state[df$abb=='TX'] <- 44
df$state[df$abb=='UT'] <- 45
df$state[df$abb=='VA'] <- 47
df$state[df$abb=='VT'] <- 46
df$state[df$abb=='WA'] <- 48
df$state[df$abb=='WI'] <- 50
df$state[df$abb=='WV'] <- 49
df$state[df$abb=='WY'] <- 51  

unique(df[is.na(df$state),][,c('state','gestfips')])

vars <- c('h_seq', 'h_year', 'state', 'filestat', 'dp', 'age', 'h_wage', 's_wage', 'hdivval', 'other', 
          'hretval', 'hssval', 'transfers', 'rentpaid', 'prop_tax','otheritem', 'cc', 'hucval', 'dp_c', 'deduc', 'cap_gain', 'cap_loss')
df <- df[,vars]


names(df) <- c('id', 'year', 'state', 'mstat', 'depx', 'agex', 'pwages', 'swages', 'dividends', 'otherprop', 'pensions', 'gssi', 'transfers', 'rentpaid', 'proptax', 
               'otheritem', 'childcare', 'ui', 'depchild', 'mortgage', 'stcg', 'ltcg')

df[df$mstat==1 | df$mstat == 2 | df$mstat == 3,]$mstat <- 2
df[df$mstat==4 | df$mstat == 5,]$mstat <- 3
df <- df[df$mstat != 6,]

write.table(df, 'taxism.csv', col.names =F, row.names = F, sep=',')
write.dta(df, 'taxism.dta')

