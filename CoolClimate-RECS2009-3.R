### Loading packages
library(MASS)
library(readr)


######### IMPORTING DATA ######
### Setting working directory to root folder

##IMPORTANT INSTRUCTIONS

# Be sure to download recs2009_public.csv from github 

# Or clean the data youself by:

# 1) Download recs2009_public.csv from the RECS website:
# 2) In MS Excel or compatible program remove all -2, -8, -9 values (find and replace all)

# 3)This will show your working directory. 
    getwd()
# 4) By default, Rstudio chooses your user account's root directory. 
# Use this function to change your working directory to uplaod the RECS dataset. 
# setwd(dir) 
# This is also the directory RStudio will use to export your results files
    
setwd("/Users/cmjones/Documents/COOLCLIMATENETWORK/URAP/Github/RECS2009")

recs <- read.csv("/Users/cmjones/Documents/COOLCLIMATENETWORK/URAP/Github/RECS2009/recs2009_public.csv")
recs2009 <- read.csv("/Users/cmjones/Documents/COOLCLIMATENETWORK/URAP/Github/RECS2009/recs2009_public.csv")



######### RECODING VARIABLES AND CLEANING DATA ########

HHSIZE<-recs$NHSLDMEM

LNHHSIZE<-log(HHSIZE)

HHAGE<-recs$HHAGE

LNHHAGE<-log(HHAGE)

WHITE = recs$Householder_Race
WHITE[recs$Householder_Race != 1] = 0

BLACK = recs$Householder_Race
BLACK[recs$Householder_Race != 2] = 0
BLACK[recs$Householder_Race == 2] = 1

ASIAN = recs$Householder_Race
ASIAN[recs$Householder_Race != 4] = 0
ASIAN[recs$Householder_Race == 4] = 1

DEGREE = recs$EDUCATION
DEGREE[recs$EDUCATION == 0] = 0
DEGREE[recs$EDUCATION == 1] = 0
DEGREE[recs$EDUCATION == 2] = 0
DEGREE[recs$EDUCATION == 3] = 0
DEGREE[recs$EDUCATION == 4] = 0
DEGREE[recs$EDUCATION == 5] = 1
DEGREE[recs$EDUCATION == 6] = 1
DEGREE[recs$EDUCATION == 7] = 1
DEGREE[recs$EDUCATION == 8] = 1

ROOMS <- recs$TOTROOMS

LNROOMS <- log(ROOMS)

YEARBUILT <- recs$YEARMADE

HEATKWH <- recs$FUELHEAT ==5

HEATGAS <- recs$FUELHEAT == 1

HEATOTH <- recs$FUELHEAT == 2 | recs$FUELHEAT == 3 | recs$FUELHEAT == 4 | recs$FUELHEAT == 7 | recs$FUELHEAT == 8 | recs$FUELHEAT == 9 | recs$FUELHEAT == 21

HDD65 <- recs$HDD65

CDD65 <- recs$CDD65

AVGINCOME = recs2009$MONEYPY
AVGINCOME[recs2009$MONEYPY == 1] = 1250
AVGINCOME[recs2009$MONEYPY == 2] = (2500+4999)/2
AVGINCOME[recs2009$MONEYPY == 3] = (5000+7499)/2
AVGINCOME[recs2009$MONEYPY == 4] = (7500+9999)/2
AVGINCOME[recs2009$MONEYPY == 5] = (10000+14999)/2
AVGINCOME[recs2009$MONEYPY == 6] = (15000+19999)/2
AVGINCOME[recs2009$MONEYPY == 7] = (20000+24999)/2
AVGINCOME[recs2009$MONEYPY == 8] = (25000+29999)/2
AVGINCOME[recs2009$MONEYPY == 9] = (30000+34999)/2
AVGINCOME[recs2009$MONEYPY == 10] = (35000+39999)/2
AVGINCOME[recs2009$MONEYPY == 11] = (40000+44999)/2
AVGINCOME[recs2009$MONEYPY == 12] = (45000+49999)/2
AVGINCOME[recs2009$MONEYPY == 13] = (50000+54999)/2
AVGINCOME[recs2009$MONEYPY == 14] = (55000+59999)/2
AVGINCOME[recs2009$MONEYPY == 15] = (60000+64999)/2
AVGINCOME[recs2009$MONEYPY == 16] = (65000+69999)/2
AVGINCOME[recs2009$MONEYPY == 17] = (70000+74999)/2
AVGINCOME[recs2009$MONEYPY == 18] = (75000+79999)/2
AVGINCOME[recs2009$MONEYPY == 19] = (80000+84999)/2
AVGINCOME[recs2009$MONEYPY == 20] = (85000+89999)/2
AVGINCOME[recs2009$MONEYPY == 21] = (90000+94999)/2
AVGINCOME[recs2009$MONEYPY == 22] = (95000+99999)/2
AVGINCOME[recs2009$MONEYPY == 23] = (100000+119999)/2
AVGINCOME[recs2009$MONEYPY == 24] = 150000

LNAVGINCOME <- log(AVGINCOME)

OWN = recs$KOWNRENT
OWN[OWN == 2 | OWN == 3] = 0

HOMEAGE = 2009 - recs$YEARMADE

DETACHED = recs$TYPEHUQ
DETACHED[DETACHED != 2] = 0
DETACHED[DETACHED == 2] = 1

PRICENATGAS <- recs2009$DOLLARNG/recs2009$CUFEETNG
PRICENATGAS[ PRICENATGAS == NaN] <-NA

PRICEKWH <- recs2009$DOLLAREL/recs2009$KWH

PRICEKWHSQ <- PRICEKWH^2

LOCATION <- recs$REPORTABLE_DOMAIN

DIVISION <- recs$DIVISION

REGIONC  <- recs$REGIONC

summary(recs2009$KWH)

df1[mapply(is.infinite, df1)] <- NA


#DEPENDENT VARIABLES ___________________________-

# combine RFG and oth, separate out WTH. Remove wood from OTH. Combine KER and FO. Separate BTUFO and BTULP
KWH <- recs$KWH
KWHSPH <- recs$KWHSPH
KWHCOL <- recs$KWHCOL
KWHWTH <- recs$KWHWTH
#KWHRFG <- recs$KWHRFG
KWHOTH <- recs$KWHOTH + recs$KWHRFG

CUFEETNG <- recs$CUFEETNG
CUFEETNGSPH <- recs$CUFEETNGSPH
CUFEETNGWTH <- recs$CUFEETNGWTH
CUFEETNGOTH <- recs$CUFEETNGOTH

BTUOTHER    <- recs$BTUFO + recs$BTUKER
BTULP    <- recs$BTULP 
# BTUOTHERSPH <- recs$BTULPSPH + recs$BTUFOSPH + recs$BTUKERSPH
# BTUOTHERWTH <- recs$BTULPWTH + recs$BTUFOWTH + recs$BTUKERWTH
# BTUOTHEROTH <- recs$BTULPOTH + recs$BTUFOOTH + recs$BTUKEROTH



######### CHOOSING PREDICTIVE INDEPENDENT VARIABLES BETWEEN LN AND REGULAR #####
#Choose variables that are more predictive - between ln and regular - run simple regression 
#summary(lm(log(KWH) ~ AVGINCOME, data = df1))
#summary(lm(log(KWH) ~ LNAVGINCOME, data = df1))
  #AVGINCOME more predictive
#summary(lm(log(KWH) ~ HHSIZE, data = df1))
#summary(lm(log(KWH) ~ LNHHSIZE, data = df1))
  #LNHHSIZE more predictive
#summary(lm(log(KWH) ~ ROOMS, data = df1))
#summary(lm(log(KWH) ~ LNROOMS, data = df1))
  #LNROOMS more predictive
#summary(lm(log(KWH) ~ HHAGE, data = df1))
#summary(lm(log(KWH) ~ LNHHAGE, data = df1))
  #HHAGE more predictive
#summary(lm(log(KWH) ~ PRICEKWH, data = df1))
#summary(lm(log(KWH) ~ LNPRICEKWH, data = df1))
  #PRICEKWH more predictive
#summary(lm(log(KWH) ~ PRICENATGAS, data = df1))
#summary(lm(log(KWH) ~ LNPRICENATGAS, data = df1))
  #PRICENATGAS more predictive
#so use these variables in model: AVGINCOME, LNHHSIZE,CDD65,LNROOMS,HDD65,HHAGE,PRICEKWH,PRICENATGAS in addition to other variables we recoded


######### CREATING THE MODELS #####

## FULL DATA FRAME (USED IN ALL MODELS)
#full data frame of KWH for US with all variables
df1 <- data.frame(KWH, ASIAN,  AVGINCOME,  BLACK,   CDD65,  DEGREE,  DETACHED,  HDD65,  HHAGE,  HEATKWH,  HEATGAS,  HEATOTH,  HHSIZE,  HOMEAGE,   OWN,   PRICEKWH,  PRICENATGAS,   ROOMS , WHITE , LNHHSIZE,  LNROOMS,  LNAVGINCOME,  LNHHAGE, DIVISION,  REGIONC, KWHSPH, KWHCOL, KWHOTH, CUFEETNG, CUFEETNGSPH, CUFEETNGWTH, CUFEETNGOTH, BTUOTHER, BTULP, KWHWTH, recs$REPORTABLE_DOMAIN)



######### REPORTABLE DOMAIN & FUELHEAT MODELS #####

## NOTES
## Only use electricity prices for reportable domains with more than one state
## Only use natural gas prices for reportable domains with more than one state and HEATGAS == 1
##Always run KWH and all electricity end use models
##If main heating fuel is electricity, then run models for CUFEETNG and BTUOTHER but not their end uses
##If main heating fuel is gas, then run BTUOTHER but not its end uses
##If main heating fuel is other, then assume no natural gas
##Remove race variables except white from models because they are significant for some reportable domains, but some cities and rural areas have over or under representation of some races




### ALL MODELS

# LOCATION DATAFRAMES

dfrd1 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 1))
dfrd2 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 2))
dfrd3 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 3))
dfrd4 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 4))
dfrd5 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 5))
dfrd6 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 6))
dfrd7 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 7))
dfrd8 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 8))
dfrd9 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 9))
dfrd10 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 10))
dfrd11 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 11))
dfrd12 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 12))
dfrd13 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 13))
dfrd14 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 14))
dfrd15 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 15))
dfrd16 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 16))
dfrd17 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 17))
dfrd18 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 18))
dfrd19 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 19))
dfrd20 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 20))
dfrd21 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 21))
dfrd22 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 22))
dfrd23 = data.frame(subset(df1, recs$DIVISION == 8))
dfrd24 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 24))
dfrd25 = data.frame(subset(df1, recs$REGIONC == 4))
dfrd26 = data.frame(subset(df1, recs$REPORTABLE_DOMAIN == 26))
dfrd27 = data.frame(subset(df1, recs$REGIONC == 4))


# + HEATING FUEL DATAFRAMES
dfrd1heatgas = data.frame(subset(dfrd1, HEATGAS == TRUE))
dfrd2heatgas = data.frame(subset(dfrd2, HEATGAS == TRUE))
dfrd3heatgas = data.frame(subset(dfrd3, HEATGAS == TRUE))
dfrd4heatgas = data.frame(subset(dfrd4, HEATGAS == TRUE))
dfrd5heatgas = data.frame(subset(dfrd5, HEATGAS == TRUE))
dfrd6heatgas = data.frame(subset(dfrd6, HEATGAS == TRUE))
dfrd7heatgas = data.frame(subset(dfrd7, HEATGAS == TRUE))
dfrd8heatgas = data.frame(subset(dfrd8, HEATGAS == TRUE))
dfrd9heatgas = data.frame(subset(dfrd9, HEATGAS == TRUE))
dfrd10heatgas = data.frame(subset(dfrd10, HEATGAS == TRUE))
dfrd11heatgas = data.frame(subset(dfrd11, HEATGAS == TRUE))
dfrd12heatgas = data.frame(subset(dfrd12, HEATGAS == TRUE))
dfrd13heatgas = data.frame(subset(dfrd13, HEATGAS == TRUE))
dfrd14heatgas = data.frame(subset(dfrd14, HEATGAS == TRUE))
dfrd15heatgas = data.frame(subset(dfrd15, HEATGAS == TRUE))
dfrd16heatgas = data.frame(subset(dfrd16, HEATGAS == TRUE))
dfrd17heatgas = data.frame(subset(dfrd17, HEATGAS == TRUE))
dfrd18heatgas = data.frame(subset(dfrd18, HEATGAS == TRUE))
dfrd19heatgas = data.frame(subset(dfrd19, HEATGAS == TRUE))
dfrd20heatgas = data.frame(subset(dfrd20, HEATGAS == TRUE))
dfrd21heatgas = data.frame(subset(dfrd21, HEATGAS == TRUE))
dfrd22heatgas = data.frame(subset(dfrd22, HEATGAS == TRUE))
dfrd23heatgas = data.frame(subset(dfrd23, HEATGAS == TRUE))
dfrd24heatgas = data.frame(subset(dfrd24, HEATGAS == TRUE))
dfrd25heatgas = data.frame(subset(dfrd25, HEATGAS == TRUE))
dfrd26heatgas = data.frame(subset(dfrd26, HEATGAS == TRUE))
dfrd27heatgas = data.frame(subset(df1, HEATGAS == TRUE & REGIONC ==4))
dfrd1heatkwh = data.frame(subset(df1, HEATKWH == TRUE & REGIONC == 1))
dfrd2heatkwh = data.frame(subset(dfrd2, HEATKWH == TRUE))
dfrd3heatkwh = data.frame(subset(dfrd3, HEATKWH == TRUE))
dfrd4heatkwh = data.frame(subset(df1, HEATKWH == TRUE))
dfrd5heatkwh = data.frame(subset(dfrd5, HEATKWH == TRUE))
dfrd6heatkwh = data.frame(subset(df1, HEATKWH == TRUE & DIVISION == 3))
dfrd7heatkwh = data.frame(subset(dfrd7, HEATKWH == TRUE))
dfrd8heatkwh = data.frame(subset(df1, HEATKWH == TRUE))
dfrd9heatkwh = data.frame(subset(df1, HEATKWH == TRUE & DIVISION == 3))
dfrd10heatkwh = data.frame(subset(dfrd10, HEATKWH == TRUE))
dfrd11heatkwh = data.frame(subset(dfrd11, HEATKWH == TRUE))
dfrd12heatkwh = data.frame(subset(dfrd12, HEATKWH == TRUE))
dfrd13heatkwh = data.frame(subset(dfrd13, HEATKWH == TRUE))
dfrd14heatkwh = data.frame(subset(dfrd14, HEATKWH == TRUE))
dfrd15heatkwh = data.frame(subset(dfrd15, HEATKWH == TRUE))
dfrd16heatkwh = data.frame(subset(dfrd16, HEATKWH == TRUE))
dfrd17heatkwh = data.frame(subset(dfrd17, HEATKWH == TRUE))
dfrd18heatkwh = data.frame(subset(dfrd18, HEATKWH == TRUE))
dfrd19heatkwh = data.frame(subset(dfrd19, HEATKWH == TRUE))
dfrd20heatkwh = data.frame(subset(dfrd20, HEATKWH == TRUE))
dfrd21heatkwh = data.frame(subset(dfrd21, HEATKWH == TRUE))
dfrd22heatkwh = data.frame(subset(dfrd22, HEATKWH == TRUE))
dfrd23heatkwh = data.frame(subset(dfrd23, HEATKWH == TRUE))
dfrd24heatkwh = data.frame(subset(dfrd24, HEATKWH == TRUE))
dfrd25heatkwh = data.frame(subset(dfrd25, HEATKWH == TRUE))
dfrd26heatkwh = data.frame(subset(dfrd26, HEATKWH == TRUE))
dfrd27heatkwh = data.frame(subset(df1, HEATKWH == TRUE & REGIONC ==4))
dfrd1heatoth = data.frame(subset(dfrd1, HEATOTH == TRUE))
dfrd2heatoth = data.frame(subset(dfrd2, HEATOTH == TRUE))
dfrd3heatoth = data.frame(subset(dfrd3, HEATOTH == TRUE))
dfrd4heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 1))
dfrd5heatoth = data.frame(subset(dfrd5, HEATOTH == TRUE))
dfrd6heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 2))
dfrd7heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 2))
dfrd8heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 2))
dfrd9heatoth = data.frame(subset(dfrd9, HEATOTH == TRUE))
dfrd10heatoth = data.frame(subset(dfrd10, HEATOTH == TRUE))
dfrd11heatoth = data.frame(subset(df1, HEATOTH == TRUE & DIVISION == 4))
dfrd12heatoth = data.frame(subset(df1, HEATOTH == TRUE & DIVISION == 4))
dfrd13heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 3))
dfrd14heatoth = data.frame(subset(dfrd14, HEATOTH == TRUE))
dfrd15heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 3))
dfrd16heatoth = data.frame(subset(dfrd16, HEATOTH == TRUE))
dfrd17heatoth = data.frame(subset(dfrd17, HEATOTH == TRUE))
dfrd18heatoth = data.frame(subset(df1, HEATOTH == TRUE & DIVISION == 6))
dfrd19heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 3))
dfrd20heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 3))
dfrd21heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 4))
dfrd22heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC ==4))
dfrd23heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 4))
dfrd24heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 4))
dfrd25heatoth = data.frame(subset(dfrd25, HEATOTH == TRUE))
dfrd26heatoth = data.frame(subset(df1, HEATOTH == TRUE & DIVISION ==10))
dfrd27heatoth = data.frame(subset(df1, HEATOTH == TRUE & REGIONC == 4))
dfrd1heatany = data.frame(dfrd1)
dfrd2heatany = data.frame(dfrd2)
dfrd3heatany = data.frame(dfrd3)
dfrd4heatany = data.frame(dfrd4)
dfrd5heatany = data.frame(dfrd5)
dfrd6heatany = data.frame(dfrd6)
dfrd7heatany = data.frame(dfrd7)
dfrd8heatany = data.frame(dfrd8)
dfrd9heatany = data.frame(dfrd9)
dfrd10heatany = data.frame(dfrd10)
dfrd11heatany = data.frame(dfrd11)
dfrd12heatany = data.frame(dfrd12)
dfrd13heatany = data.frame(dfrd13)
dfrd14heatany = data.frame(dfrd14)
dfrd15heatany = data.frame(dfrd15)
dfrd16heatany = data.frame(dfrd16)
dfrd17heatany = data.frame(dfrd17)
dfrd18heatany = data.frame(dfrd18)
dfrd19heatany = data.frame(dfrd19)
dfrd20heatany = data.frame(dfrd20)
dfrd21heatany = data.frame(dfrd21)
dfrd22heatany = data.frame(dfrd22)
dfrd23heatany = data.frame(dfrd23)
dfrd24heatany = data.frame(dfrd24)
dfrd25heatany = data.frame(dfrd25)
dfrd26heatany = data.frame(dfrd26)
dfrd27heatany = data.frame(df1, REGIONC ==4)
df1heatany = data.frame(df1)
df1heatgas = data.frame(subset(df1, HEATGAS == TRUE))
df1heatkwh = data.frame(subset(df1, HEATKWH == TRUE))
df1heatoth = data.frame(subset(df1, HEATOTH == TRUE))


##NULL MODELS
btulp.null.modelrd1heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd2heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd3heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd4heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd5heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd6heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd7heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd8heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd9heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd10heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd11heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd12heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd13heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd14heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd15heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd16heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd17heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd18heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd19heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd20heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd21heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd22heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd23heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd24heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd25heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd26heatany <- lm(BTULP ~ 1, data = df1heatany)
btulp.null.modelrd27heatany <- lm(BTULP ~ 1, data = df1heatany)
kwhcol.null.modelrd1heatany <- lm(KWHCOL ~ 1, data = dfrd1heatany)
kwhcol.null.modelrd2heatany <- lm(KWHCOL ~ 1, data = dfrd2heatany)
kwhcol.null.modelrd3heatany <- lm(KWHCOL ~ 1, data = dfrd3heatany)
kwhcol.null.modelrd4heatany <- lm(KWHCOL ~ 1, data = dfrd4heatany)
kwhcol.null.modelrd5heatany <- lm(KWHCOL ~ 1, data = dfrd5heatany)
kwhcol.null.modelrd6heatany <- lm(KWHCOL ~ 1, data = dfrd6heatany)
kwhcol.null.modelrd7heatany <- lm(KWHCOL ~ 1, data = dfrd7heatany)
kwhcol.null.modelrd8heatany <- lm(KWHCOL ~ 1, data = dfrd8heatany)
kwhcol.null.modelrd9heatany <- lm(KWHCOL ~ 1, data = dfrd9heatany)
kwhcol.null.modelrd10heatany <- lm(KWHCOL ~ 1, data = dfrd10heatany)
kwhcol.null.modelrd11heatany <- lm(KWHCOL ~ 1, data = dfrd11heatany)
kwhcol.null.modelrd12heatany <- lm(KWHCOL ~ 1, data = dfrd12heatany)
kwhcol.null.modelrd13heatany <- lm(KWHCOL ~ 1, data = dfrd13heatany)
kwhcol.null.modelrd14heatany <- lm(KWHCOL ~ 1, data = dfrd14heatany)
kwhcol.null.modelrd15heatany <- lm(KWHCOL ~ 1, data = dfrd15heatany)
kwhcol.null.modelrd16heatany <- lm(KWHCOL ~ 1, data = dfrd16heatany)
kwhcol.null.modelrd17heatany <- lm(KWHCOL ~ 1, data = dfrd17heatany)
kwhcol.null.modelrd18heatany <- lm(KWHCOL ~ 1, data = dfrd18heatany)
kwhcol.null.modelrd19heatany <- lm(KWHCOL ~ 1, data = dfrd19heatany)
kwhcol.null.modelrd20heatany <- lm(KWHCOL ~ 1, data = dfrd20heatany)
kwhcol.null.modelrd21heatany <- lm(KWHCOL ~ 1, data = dfrd21heatany)
kwhcol.null.modelrd22heatany <- lm(KWHCOL ~ 1, data = dfrd22heatany)
kwhcol.null.modelrd23heatany <- lm(KWHCOL ~ 1, data = dfrd23heatany)
kwhcol.null.modelrd24heatany <- lm(KWHCOL ~ 1, data = dfrd24heatany)
kwhcol.null.modelrd25heatany <- lm(KWHCOL ~ 1, data = dfrd25heatany)
kwhcol.null.modelrd26heatany <- lm(KWHCOL ~ 1, data = dfrd26heatany)
kwhcol.null.modelrd27heatany <- lm(KWHCOL ~ 1, data = dfrd27heatany)
cufeetng.null.modelrd1heatgas <- lm(CUFEETNG ~ 1, data = dfrd1heatgas)
cufeetng.null.modelrd2heatgas <- lm(CUFEETNG ~ 1, data = dfrd2heatgas)
cufeetng.null.modelrd3heatgas <- lm(CUFEETNG ~ 1, data = dfrd3heatgas)
cufeetng.null.modelrd4heatgas <- lm(CUFEETNG ~ 1, data = dfrd4heatgas)
cufeetng.null.modelrd5heatgas <- lm(CUFEETNG ~ 1, data = dfrd5heatgas)
cufeetng.null.modelrd6heatgas <- lm(CUFEETNG ~ 1, data = dfrd6heatgas)
cufeetng.null.modelrd7heatgas <- lm(CUFEETNG ~ 1, data = dfrd7heatgas)
cufeetng.null.modelrd8heatgas <- lm(CUFEETNG ~ 1, data = dfrd8heatgas)
cufeetng.null.modelrd9heatgas <- lm(CUFEETNG ~ 1, data = dfrd9heatgas)
cufeetng.null.modelrd10heatgas <- lm(CUFEETNG ~ 1, data = dfrd10heatgas)
cufeetng.null.modelrd11heatgas <- lm(CUFEETNG ~ 1, data = dfrd11heatgas)
cufeetng.null.modelrd12heatgas <- lm(CUFEETNG ~ 1, data = dfrd12heatgas)
cufeetng.null.modelrd13heatgas <- lm(CUFEETNG ~ 1, data = dfrd13heatgas)
cufeetng.null.modelrd14heatgas <- lm(CUFEETNG ~ 1, data = dfrd14heatgas)
cufeetng.null.modelrd15heatgas <- lm(CUFEETNG ~ 1, data = dfrd15heatgas)
cufeetng.null.modelrd16heatgas <- lm(CUFEETNG ~ 1, data = dfrd16heatgas)
cufeetng.null.modelrd17heatgas <- lm(CUFEETNG ~ 1, data = dfrd17heatgas)
cufeetng.null.modelrd18heatgas <- lm(CUFEETNG ~ 1, data = dfrd18heatgas)
cufeetng.null.modelrd19heatgas <- lm(CUFEETNG ~ 1, data = dfrd19heatgas)
cufeetng.null.modelrd20heatgas <- lm(CUFEETNG ~ 1, data = dfrd20heatgas)
cufeetng.null.modelrd21heatgas <- lm(CUFEETNG ~ 1, data = dfrd21heatgas)
cufeetng.null.modelrd22heatgas <- lm(CUFEETNG ~ 1, data = dfrd22heatgas)
cufeetng.null.modelrd23heatgas <- lm(CUFEETNG ~ 1, data = dfrd23heatgas)
cufeetng.null.modelrd24heatgas <- lm(CUFEETNG ~ 1, data = dfrd24heatgas)
cufeetng.null.modelrd25heatgas <- lm(CUFEETNG ~ 1, data = dfrd25heatgas)
cufeetng.null.modelrd26heatgas <- lm(CUFEETNG ~ 1, data = dfrd26heatgas)
cufeetng.null.modelrd27heatgas <- lm(CUFEETNG ~ 1, data = dfrd27heatgas)
cufeetngsph.null.modelrd1heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd1heatgas)
cufeetngsph.null.modelrd2heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd2heatgas)
cufeetngsph.null.modelrd3heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd3heatgas)
cufeetngsph.null.modelrd4heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd4heatgas)
cufeetngsph.null.modelrd5heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd5heatgas)
cufeetngsph.null.modelrd6heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd6heatgas)
cufeetngsph.null.modelrd7heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd7heatgas)
cufeetngsph.null.modelrd8heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd8heatgas)
cufeetngsph.null.modelrd9heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd9heatgas)
cufeetngsph.null.modelrd10heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd10heatgas)
cufeetngsph.null.modelrd11heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd11heatgas)
cufeetngsph.null.modelrd12heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd12heatgas)
cufeetngsph.null.modelrd13heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd13heatgas)
cufeetngsph.null.modelrd14heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd14heatgas)
cufeetngsph.null.modelrd15heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd15heatgas)
cufeetngsph.null.modelrd16heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd16heatgas)
cufeetngsph.null.modelrd17heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd17heatgas)
cufeetngsph.null.modelrd18heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd18heatgas)
cufeetngsph.null.modelrd19heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd19heatgas)
cufeetngsph.null.modelrd20heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd20heatgas)
cufeetngsph.null.modelrd21heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd21heatgas)
cufeetngsph.null.modelrd22heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd22heatgas)
cufeetngsph.null.modelrd23heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd23heatgas)
cufeetngsph.null.modelrd24heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd24heatgas)
cufeetngsph.null.modelrd25heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd25heatgas)
cufeetngsph.null.modelrd26heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd26heatgas)
cufeetngsph.null.modelrd27heatgas <- lm(CUFEETNGSPH ~ 1, data = dfrd27heatgas)
cufeetngwth.null.modelrd1heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd1heatgas)
cufeetngwth.null.modelrd2heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd2heatgas)
cufeetngwth.null.modelrd3heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd3heatgas)
cufeetngwth.null.modelrd4heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd4heatgas)
cufeetngwth.null.modelrd5heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd5heatgas)
cufeetngwth.null.modelrd6heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd6heatgas)
cufeetngwth.null.modelrd7heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd7heatgas)
cufeetngwth.null.modelrd8heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd8heatgas)
cufeetngwth.null.modelrd9heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd9heatgas)
cufeetngwth.null.modelrd10heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd10heatgas)
cufeetngwth.null.modelrd11heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd11heatgas)
cufeetngwth.null.modelrd12heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd12heatgas)
cufeetngwth.null.modelrd13heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd13heatgas)
cufeetngwth.null.modelrd14heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd14heatgas)
cufeetngwth.null.modelrd15heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd15heatgas)
cufeetngwth.null.modelrd16heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd16heatgas)
cufeetngwth.null.modelrd17heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd17heatgas)
cufeetngwth.null.modelrd18heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd18heatgas)
cufeetngwth.null.modelrd19heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd19heatgas)
cufeetngwth.null.modelrd20heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd20heatgas)
cufeetngwth.null.modelrd21heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd21heatgas)
cufeetngwth.null.modelrd22heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd22heatgas)
cufeetngwth.null.modelrd23heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd23heatgas)
cufeetngwth.null.modelrd24heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd24heatgas)
cufeetngwth.null.modelrd25heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd25heatgas)
cufeetngwth.null.modelrd26heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd26heatgas)
cufeetngwth.null.modelrd27heatgas <- lm(CUFEETNGWTH ~ 1, data = dfrd27heatgas)
kwh.null.modelrd1heatgas <- lm(KWH ~ 1, data = dfrd1heatgas)
kwh.null.modelrd2heatgas <- lm(KWH ~ 1, data = dfrd2heatgas)
kwh.null.modelrd3heatgas <- lm(KWH ~ 1, data = dfrd3heatgas)
kwh.null.modelrd4heatgas <- lm(KWH ~ 1, data = dfrd4heatgas)
kwh.null.modelrd5heatgas <- lm(KWH ~ 1, data = dfrd5heatgas)
kwh.null.modelrd6heatgas <- lm(KWH ~ 1, data = dfrd6heatgas)
kwh.null.modelrd7heatgas <- lm(KWH ~ 1, data = dfrd7heatgas)
kwh.null.modelrd8heatgas <- lm(KWH ~ 1, data = dfrd8heatgas)
kwh.null.modelrd9heatgas <- lm(KWH ~ 1, data = dfrd9heatgas)
kwh.null.modelrd10heatgas <- lm(KWH ~ 1, data = dfrd10heatgas)
kwh.null.modelrd11heatgas <- lm(KWH ~ 1, data = dfrd11heatgas)
kwh.null.modelrd12heatgas <- lm(KWH ~ 1, data = dfrd12heatgas)
kwh.null.modelrd13heatgas <- lm(KWH ~ 1, data = dfrd13heatgas)
kwh.null.modelrd14heatgas <- lm(KWH ~ 1, data = dfrd14heatgas)
kwh.null.modelrd15heatgas <- lm(KWH ~ 1, data = dfrd15heatgas)
kwh.null.modelrd16heatgas <- lm(KWH ~ 1, data = dfrd16heatgas)
kwh.null.modelrd17heatgas <- lm(KWH ~ 1, data = dfrd17heatgas)
kwh.null.modelrd18heatgas <- lm(KWH ~ 1, data = dfrd18heatgas)
kwh.null.modelrd19heatgas <- lm(KWH ~ 1, data = dfrd19heatgas)
kwh.null.modelrd20heatgas <- lm(KWH ~ 1, data = dfrd20heatgas)
kwh.null.modelrd21heatgas <- lm(KWH ~ 1, data = dfrd21heatgas)
kwh.null.modelrd22heatgas <- lm(KWH ~ 1, data = dfrd22heatgas)
kwh.null.modelrd23heatgas <- lm(KWH ~ 1, data = dfrd23heatgas)
kwh.null.modelrd24heatgas <- lm(KWH ~ 1, data = dfrd24heatgas)
kwh.null.modelrd25heatgas <- lm(KWH ~ 1, data = dfrd25heatgas)
kwh.null.modelrd26heatgas <- lm(KWH ~ 1, data = dfrd26heatgas)
kwh.null.modelrd27heatgas <- lm(KWH ~ 1, data = dfrd27heatgas)
kwhoth.null.modelrd1heatgas <- lm(KWHOTH ~ 1, data = dfrd1heatgas)
kwhoth.null.modelrd2heatgas <- lm(KWHOTH ~ 1, data = dfrd2heatgas)
kwhoth.null.modelrd3heatgas <- lm(KWHOTH ~ 1, data = dfrd3heatgas)
kwhoth.null.modelrd4heatgas <- lm(KWHOTH ~ 1, data = dfrd4heatgas)
kwhoth.null.modelrd5heatgas <- lm(KWHOTH ~ 1, data = dfrd5heatgas)
kwhoth.null.modelrd6heatgas <- lm(KWHOTH ~ 1, data = dfrd6heatgas)
kwhoth.null.modelrd7heatgas <- lm(KWHOTH ~ 1, data = dfrd7heatgas)
kwhoth.null.modelrd8heatgas <- lm(KWHOTH ~ 1, data = dfrd8heatgas)
kwhoth.null.modelrd9heatgas <- lm(KWHOTH ~ 1, data = dfrd9heatgas)
kwhoth.null.modelrd10heatgas <- lm(KWHOTH ~ 1, data = dfrd10heatgas)
kwhoth.null.modelrd11heatgas <- lm(KWHOTH ~ 1, data = dfrd11heatgas)
kwhoth.null.modelrd12heatgas <- lm(KWHOTH ~ 1, data = dfrd12heatgas)
kwhoth.null.modelrd13heatgas <- lm(KWHOTH ~ 1, data = dfrd13heatgas)
kwhoth.null.modelrd14heatgas <- lm(KWHOTH ~ 1, data = dfrd14heatgas)
kwhoth.null.modelrd15heatgas <- lm(KWHOTH ~ 1, data = dfrd15heatgas)
kwhoth.null.modelrd16heatgas <- lm(KWHOTH ~ 1, data = dfrd16heatgas)
kwhoth.null.modelrd17heatgas <- lm(KWHOTH ~ 1, data = dfrd17heatgas)
kwhoth.null.modelrd18heatgas <- lm(KWHOTH ~ 1, data = dfrd18heatgas)
kwhoth.null.modelrd19heatgas <- lm(KWHOTH ~ 1, data = dfrd19heatgas)
kwhoth.null.modelrd20heatgas <- lm(KWHOTH ~ 1, data = dfrd20heatgas)
kwhoth.null.modelrd21heatgas <- lm(KWHOTH ~ 1, data = dfrd21heatgas)
kwhoth.null.modelrd22heatgas <- lm(KWHOTH ~ 1, data = dfrd22heatgas)
kwhoth.null.modelrd23heatgas <- lm(KWHOTH ~ 1, data = dfrd23heatgas)
kwhoth.null.modelrd24heatgas <- lm(KWHOTH ~ 1, data = dfrd24heatgas)
kwhoth.null.modelrd25heatgas <- lm(KWHOTH ~ 1, data = dfrd25heatgas)
kwhoth.null.modelrd26heatgas <- lm(KWHOTH ~ 1, data = dfrd26heatgas)
kwhoth.null.modelrd27heatgas <- lm(KWHOTH ~ 1, data = dfrd27heatgas)
kwhsph.null.modelrd1heatgas <- lm(KWHSPH ~ 1, data = dfrd1heatgas)
kwhsph.null.modelrd2heatgas <- lm(KWHSPH ~ 1, data = dfrd2heatgas)
kwhsph.null.modelrd3heatgas <- lm(KWHSPH ~ 1, data = dfrd3heatgas)
kwhsph.null.modelrd4heatgas <- lm(KWHSPH ~ 1, data = dfrd4heatgas)
kwhsph.null.modelrd5heatgas <- lm(KWHSPH ~ 1, data = dfrd5heatgas)
kwhsph.null.modelrd6heatgas <- lm(KWHSPH ~ 1, data = dfrd6heatgas)
kwhsph.null.modelrd7heatgas <- lm(KWHSPH ~ 1, data = dfrd7heatgas)
kwhsph.null.modelrd8heatgas <- lm(KWHSPH ~ 1, data = dfrd8heatgas)
kwhsph.null.modelrd9heatgas <- lm(KWHSPH ~ 1, data = dfrd9heatgas)
kwhsph.null.modelrd10heatgas <- lm(KWHSPH ~ 1, data = dfrd10heatgas)
kwhsph.null.modelrd11heatgas <- lm(KWHSPH ~ 1, data = dfrd11heatgas)
kwhsph.null.modelrd12heatgas <- lm(KWHSPH ~ 1, data = dfrd12heatgas)
kwhsph.null.modelrd13heatgas <- lm(KWHSPH ~ 1, data = dfrd13heatgas)
kwhsph.null.modelrd14heatgas <- lm(KWHSPH ~ 1, data = dfrd14heatgas)
kwhsph.null.modelrd15heatgas <- lm(KWHSPH ~ 1, data = dfrd15heatgas)
kwhsph.null.modelrd16heatgas <- lm(KWHSPH ~ 1, data = dfrd16heatgas)
kwhsph.null.modelrd17heatgas <- lm(KWHSPH ~ 1, data = dfrd17heatgas)
kwhsph.null.modelrd18heatgas <- lm(KWHSPH ~ 1, data = dfrd18heatgas)
kwhsph.null.modelrd19heatgas <- lm(KWHSPH ~ 1, data = dfrd19heatgas)
kwhsph.null.modelrd20heatgas <- lm(KWHSPH ~ 1, data = dfrd20heatgas)
kwhsph.null.modelrd21heatgas <- lm(KWHSPH ~ 1, data = dfrd21heatgas)
kwhsph.null.modelrd22heatgas <- lm(KWHSPH ~ 1, data = dfrd22heatgas)
kwhsph.null.modelrd23heatgas <- lm(KWHSPH ~ 1, data = dfrd23heatgas)
kwhsph.null.modelrd24heatgas <- lm(KWHSPH ~ 1, data = dfrd24heatgas)
kwhsph.null.modelrd25heatgas <- lm(KWHSPH ~ 1, data = dfrd25heatgas)
kwhsph.null.modelrd26heatgas <- lm(KWHSPH ~ 1, data = dfrd26heatgas)
kwhsph.null.modelrd27heatgas <- lm(KWHSPH ~ 1, data = dfrd27heatgas)
kwhwth.null.modelrd1heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd2heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd3heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd4heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd5heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd6heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd7heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd8heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd9heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd10heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd11heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd12heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd13heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd14heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd15heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd16heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd17heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd18heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd19heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd20heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd21heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd22heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd23heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd24heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd25heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd26heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwhwth.null.modelrd27heatgas <- lm(KWHWTH ~ 1, data = df1heatgas)
kwh.null.modelrd1heatkwh <- lm(KWH ~ 1, data = dfrd1heatkwh)
kwh.null.modelrd2heatkwh <- lm(KWH ~ 1, data = dfrd2heatkwh)
kwh.null.modelrd3heatkwh <- lm(KWH ~ 1, data = dfrd3heatkwh)
kwh.null.modelrd4heatkwh <- lm(KWH ~ 1, data = dfrd4heatkwh)
kwh.null.modelrd5heatkwh <- lm(KWH ~ 1, data = dfrd5heatkwh)
kwh.null.modelrd6heatkwh <- lm(KWH ~ 1, data = dfrd6heatkwh)
kwh.null.modelrd7heatkwh <- lm(KWH ~ 1, data = dfrd7heatkwh)
kwh.null.modelrd8heatkwh <- lm(KWH ~ 1, data = dfrd8heatkwh)
kwh.null.modelrd9heatkwh <- lm(KWH ~ 1, data = dfrd9heatkwh)
kwh.null.modelrd10heatkwh <- lm(KWH ~ 1, data = dfrd10heatkwh)
kwh.null.modelrd11heatkwh <- lm(KWH ~ 1, data = dfrd11heatkwh)
kwh.null.modelrd12heatkwh <- lm(KWH ~ 1, data = dfrd12heatkwh)
kwh.null.modelrd13heatkwh <- lm(KWH ~ 1, data = dfrd13heatkwh)
kwh.null.modelrd14heatkwh <- lm(KWH ~ 1, data = dfrd14heatkwh)
kwh.null.modelrd15heatkwh <- lm(KWH ~ 1, data = dfrd15heatkwh)
kwh.null.modelrd16heatkwh <- lm(KWH ~ 1, data = dfrd16heatkwh)
kwh.null.modelrd17heatkwh <- lm(KWH ~ 1, data = dfrd17heatkwh)
kwh.null.modelrd18heatkwh <- lm(KWH ~ 1, data = dfrd18heatkwh)
kwh.null.modelrd19heatkwh <- lm(KWH ~ 1, data = dfrd19heatkwh)
kwh.null.modelrd20heatkwh <- lm(KWH ~ 1, data = dfrd20heatkwh)
kwh.null.modelrd21heatkwh <- lm(KWH ~ 1, data = dfrd21heatkwh)
kwh.null.modelrd22heatkwh <- lm(KWH ~ 1, data = dfrd22heatkwh)
kwh.null.modelrd23heatkwh <- lm(KWH ~ 1, data = dfrd23heatkwh)
kwh.null.modelrd24heatkwh <- lm(KWH ~ 1, data = dfrd24heatkwh)
kwh.null.modelrd25heatkwh <- lm(KWH ~ 1, data = dfrd25heatkwh)
kwh.null.modelrd26heatkwh <- lm(KWH ~ 1, data = dfrd26heatkwh)
kwh.null.modelrd27heatkwh <- lm(KWH ~ 1, data = dfrd27heatkwh)
kwhoth.null.modelrd1heatkwh <- lm(KWHOTH ~ 1, data = dfrd1heatkwh)
kwhoth.null.modelrd2heatkwh <- lm(KWHOTH ~ 1, data = dfrd2heatkwh)
kwhoth.null.modelrd3heatkwh <- lm(KWHOTH ~ 1, data = dfrd3heatkwh)
kwhoth.null.modelrd4heatkwh <- lm(KWHOTH ~ 1, data = dfrd4heatkwh)
kwhoth.null.modelrd5heatkwh <- lm(KWHOTH ~ 1, data = dfrd5heatkwh)
kwhoth.null.modelrd6heatkwh <- lm(KWHOTH ~ 1, data = dfrd6heatkwh)
kwhoth.null.modelrd7heatkwh <- lm(KWHOTH ~ 1, data = dfrd7heatkwh)
kwhoth.null.modelrd8heatkwh <- lm(KWHOTH ~ 1, data = dfrd8heatkwh)
kwhoth.null.modelrd9heatkwh <- lm(KWHOTH ~ 1, data = dfrd9heatkwh)
kwhoth.null.modelrd10heatkwh <- lm(KWHOTH ~ 1, data = dfrd10heatkwh)
kwhoth.null.modelrd11heatkwh <- lm(KWHOTH ~ 1, data = dfrd11heatkwh)
kwhoth.null.modelrd12heatkwh <- lm(KWHOTH ~ 1, data = dfrd12heatkwh)
kwhoth.null.modelrd13heatkwh <- lm(KWHOTH ~ 1, data = dfrd13heatkwh)
kwhoth.null.modelrd14heatkwh <- lm(KWHOTH ~ 1, data = dfrd14heatkwh)
kwhoth.null.modelrd15heatkwh <- lm(KWHOTH ~ 1, data = dfrd15heatkwh)
kwhoth.null.modelrd16heatkwh <- lm(KWHOTH ~ 1, data = dfrd16heatkwh)
kwhoth.null.modelrd17heatkwh <- lm(KWHOTH ~ 1, data = dfrd17heatkwh)
kwhoth.null.modelrd18heatkwh <- lm(KWHOTH ~ 1, data = dfrd18heatkwh)
kwhoth.null.modelrd19heatkwh <- lm(KWHOTH ~ 1, data = dfrd19heatkwh)
kwhoth.null.modelrd20heatkwh <- lm(KWHOTH ~ 1, data = dfrd20heatkwh)
kwhoth.null.modelrd21heatkwh <- lm(KWHOTH ~ 1, data = dfrd21heatkwh)
kwhoth.null.modelrd22heatkwh <- lm(KWHOTH ~ 1, data = dfrd22heatkwh)
kwhoth.null.modelrd23heatkwh <- lm(KWHOTH ~ 1, data = dfrd23heatkwh)
kwhoth.null.modelrd24heatkwh <- lm(KWHOTH ~ 1, data = dfrd24heatkwh)
kwhoth.null.modelrd25heatkwh <- lm(KWHOTH ~ 1, data = dfrd25heatkwh)
kwhoth.null.modelrd26heatkwh <- lm(KWHOTH ~ 1, data = dfrd26heatkwh)
kwhoth.null.modelrd27heatkwh <- lm(KWHOTH ~ 1, data = dfrd27heatkwh)
kwhsph.null.modelrd1heatkwh <- lm(KWHSPH ~ 1, data = dfrd1heatkwh)
kwhsph.null.modelrd2heatkwh <- lm(KWHSPH ~ 1, data = dfrd2heatkwh)
kwhsph.null.modelrd3heatkwh <- lm(KWHSPH ~ 1, data = dfrd3heatkwh)
kwhsph.null.modelrd4heatkwh <- lm(KWHSPH ~ 1, data = dfrd4heatkwh)
kwhsph.null.modelrd5heatkwh <- lm(KWHSPH ~ 1, data = dfrd5heatkwh)
kwhsph.null.modelrd6heatkwh <- lm(KWHSPH ~ 1, data = dfrd6heatkwh)
kwhsph.null.modelrd7heatkwh <- lm(KWHSPH ~ 1, data = dfrd7heatkwh)
kwhsph.null.modelrd8heatkwh <- lm(KWHSPH ~ 1, data = dfrd8heatkwh)
kwhsph.null.modelrd9heatkwh <- lm(KWHSPH ~ 1, data = dfrd9heatkwh)
kwhsph.null.modelrd10heatkwh <- lm(KWHSPH ~ 1, data = dfrd10heatkwh)
kwhsph.null.modelrd11heatkwh <- lm(KWHSPH ~ 1, data = dfrd11heatkwh)
kwhsph.null.modelrd12heatkwh <- lm(KWHSPH ~ 1, data = dfrd12heatkwh)
kwhsph.null.modelrd13heatkwh <- lm(KWHSPH ~ 1, data = dfrd13heatkwh)
kwhsph.null.modelrd14heatkwh <- lm(KWHSPH ~ 1, data = dfrd14heatkwh)
kwhsph.null.modelrd15heatkwh <- lm(KWHSPH ~ 1, data = dfrd15heatkwh)
kwhsph.null.modelrd16heatkwh <- lm(KWHSPH ~ 1, data = dfrd16heatkwh)
kwhsph.null.modelrd17heatkwh <- lm(KWHSPH ~ 1, data = dfrd17heatkwh)
kwhsph.null.modelrd18heatkwh <- lm(KWHSPH ~ 1, data = dfrd18heatkwh)
kwhsph.null.modelrd19heatkwh <- lm(KWHSPH ~ 1, data = dfrd19heatkwh)
kwhsph.null.modelrd20heatkwh <- lm(KWHSPH ~ 1, data = dfrd20heatkwh)
kwhsph.null.modelrd21heatkwh <- lm(KWHSPH ~ 1, data = dfrd21heatkwh)
kwhsph.null.modelrd22heatkwh <- lm(KWHSPH ~ 1, data = dfrd22heatkwh)
kwhsph.null.modelrd23heatkwh <- lm(KWHSPH ~ 1, data = dfrd23heatkwh)
kwhsph.null.modelrd24heatkwh <- lm(KWHSPH ~ 1, data = dfrd24heatkwh)
kwhsph.null.modelrd25heatkwh <- lm(KWHSPH ~ 1, data = dfrd25heatkwh)
kwhsph.null.modelrd26heatkwh <- lm(KWHSPH ~ 1, data = dfrd26heatkwh)
kwhsph.null.modelrd27heatkwh <- lm(KWHSPH ~ 1, data = dfrd27heatkwh)
kwhwth.null.modelrd1heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd2heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd3heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd4heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd5heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd6heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd7heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd8heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd9heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd10heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd11heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd12heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd13heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd14heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd15heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd16heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd17heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd18heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd19heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd20heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd21heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd22heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd23heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd24heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd25heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd26heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
kwhwth.null.modelrd27heatkwh <- lm(KWHWTH ~ 1, data = df1heatkwh)
btuother.null.modelrd1heatoth <- lm(BTUOTHER ~ 1, data = dfrd1heatoth)
btuother.null.modelrd2heatoth <- lm(BTUOTHER ~ 1, data = dfrd2heatoth)
btuother.null.modelrd3heatoth <- lm(BTUOTHER ~ 1, data = dfrd3heatoth)
btuother.null.modelrd4heatoth <- lm(BTUOTHER ~ 1, data = dfrd4heatoth)
btuother.null.modelrd5heatoth <- lm(BTUOTHER ~ 1, data = dfrd5heatoth)
btuother.null.modelrd6heatoth <- lm(BTUOTHER ~ 1, data = dfrd6heatoth)
btuother.null.modelrd7heatoth <- lm(BTUOTHER ~ 1, data = dfrd7heatoth)
btuother.null.modelrd8heatoth <- lm(BTUOTHER ~ 1, data = dfrd8heatoth)
btuother.null.modelrd9heatoth <- lm(BTUOTHER ~ 1, data = dfrd9heatoth)
btuother.null.modelrd10heatoth <- lm(BTUOTHER ~ 1, data = dfrd10heatoth)
btuother.null.modelrd11heatoth <- lm(BTUOTHER ~ 1, data = dfrd11heatoth)
btuother.null.modelrd12heatoth <- lm(BTUOTHER ~ 1, data = dfrd12heatoth)
btuother.null.modelrd13heatoth <- lm(BTUOTHER ~ 1, data = dfrd13heatoth)
btuother.null.modelrd14heatoth <- lm(BTUOTHER ~ 1, data = dfrd14heatoth)
btuother.null.modelrd15heatoth <- lm(BTUOTHER ~ 1, data = dfrd15heatoth)
btuother.null.modelrd16heatoth <- lm(BTUOTHER ~ 1, data = dfrd16heatoth)
btuother.null.modelrd17heatoth <- lm(BTUOTHER ~ 1, data = dfrd17heatoth)
btuother.null.modelrd18heatoth <- lm(BTUOTHER ~ 1, data = dfrd18heatoth)
btuother.null.modelrd19heatoth <- lm(BTUOTHER ~ 1, data = dfrd19heatoth)
btuother.null.modelrd20heatoth <- lm(BTUOTHER ~ 1, data = dfrd20heatoth)
btuother.null.modelrd21heatoth <- lm(BTUOTHER ~ 1, data = dfrd21heatoth)
btuother.null.modelrd22heatoth <- lm(BTUOTHER ~ 1, data = dfrd22heatoth)
btuother.null.modelrd23heatoth <- lm(BTUOTHER ~ 1, data = dfrd23heatoth)
btuother.null.modelrd24heatoth <- lm(BTUOTHER ~ 1, data = dfrd24heatoth)
btuother.null.modelrd25heatoth <- lm(BTUOTHER ~ 1, data = dfrd25heatoth)
btuother.null.modelrd26heatoth <- lm(BTUOTHER ~ 1, data = dfrd26heatoth)
btuother.null.modelrd27heatoth <- lm(BTUOTHER ~ 1, data = dfrd27heatoth)
kwh.null.modelrd1heatoth <- lm(KWH ~ 1, data = dfrd1heatoth)
kwh.null.modelrd2heatoth <- lm(KWH ~ 1, data = dfrd2heatoth)
kwh.null.modelrd3heatoth <- lm(KWH ~ 1, data = dfrd3heatoth)
kwh.null.modelrd4heatoth <- lm(KWH ~ 1, data = dfrd4heatoth)
kwh.null.modelrd5heatoth <- lm(KWH ~ 1, data = dfrd5heatoth)
kwh.null.modelrd6heatoth <- lm(KWH ~ 1, data = dfrd6heatoth)
kwh.null.modelrd7heatoth <- lm(KWH ~ 1, data = dfrd7heatoth)
kwh.null.modelrd8heatoth <- lm(KWH ~ 1, data = dfrd8heatoth)
kwh.null.modelrd9heatoth <- lm(KWH ~ 1, data = dfrd9heatoth)
kwh.null.modelrd10heatoth <- lm(KWH ~ 1, data = dfrd10heatoth)
kwh.null.modelrd11heatoth <- lm(KWH ~ 1, data = dfrd11heatoth)
kwh.null.modelrd12heatoth <- lm(KWH ~ 1, data = dfrd12heatoth)
kwh.null.modelrd13heatoth <- lm(KWH ~ 1, data = dfrd13heatoth)
kwh.null.modelrd14heatoth <- lm(KWH ~ 1, data = dfrd14heatoth)
kwh.null.modelrd15heatoth <- lm(KWH ~ 1, data = dfrd15heatoth)
kwh.null.modelrd16heatoth <- lm(KWH ~ 1, data = dfrd16heatoth)
kwh.null.modelrd17heatoth <- lm(KWH ~ 1, data = dfrd17heatoth)
kwh.null.modelrd18heatoth <- lm(KWH ~ 1, data = dfrd18heatoth)
kwh.null.modelrd19heatoth <- lm(KWH ~ 1, data = dfrd19heatoth)
kwh.null.modelrd20heatoth <- lm(KWH ~ 1, data = dfrd20heatoth)
kwh.null.modelrd21heatoth <- lm(KWH ~ 1, data = dfrd21heatoth)
kwh.null.modelrd22heatoth <- lm(KWH ~ 1, data = dfrd22heatoth)
kwh.null.modelrd23heatoth <- lm(KWH ~ 1, data = dfrd23heatoth)
kwh.null.modelrd24heatoth <- lm(KWH ~ 1, data = dfrd24heatoth)
kwh.null.modelrd25heatoth <- lm(KWH ~ 1, data = dfrd25heatoth)
kwh.null.modelrd26heatoth <- lm(KWH ~ 1, data = dfrd26heatoth)
kwh.null.modelrd27heatoth <- lm(KWH ~ 1, data = dfrd27heatoth)
kwhoth.null.modelrd1heatoth <- lm(KWHOTH ~ 1, data = dfrd1heatoth)
kwhoth.null.modelrd2heatoth <- lm(KWHOTH ~ 1, data = dfrd2heatoth)
kwhoth.null.modelrd3heatoth <- lm(KWHOTH ~ 1, data = dfrd3heatoth)
kwhoth.null.modelrd4heatoth <- lm(KWHOTH ~ 1, data = dfrd4heatoth)
kwhoth.null.modelrd5heatoth <- lm(KWHOTH ~ 1, data = dfrd5heatoth)
kwhoth.null.modelrd6heatoth <- lm(KWHOTH ~ 1, data = dfrd6heatoth)
kwhoth.null.modelrd7heatoth <- lm(KWHOTH ~ 1, data = dfrd7heatoth)
kwhoth.null.modelrd8heatoth <- lm(KWHOTH ~ 1, data = dfrd8heatoth)
kwhoth.null.modelrd9heatoth <- lm(KWHOTH ~ 1, data = dfrd9heatoth)
kwhoth.null.modelrd10heatoth <- lm(KWHOTH ~ 1, data = dfrd10heatoth)
kwhoth.null.modelrd11heatoth <- lm(KWHOTH ~ 1, data = dfrd11heatoth)
kwhoth.null.modelrd12heatoth <- lm(KWHOTH ~ 1, data = dfrd12heatoth)
kwhoth.null.modelrd13heatoth <- lm(KWHOTH ~ 1, data = dfrd13heatoth)
kwhoth.null.modelrd14heatoth <- lm(KWHOTH ~ 1, data = dfrd14heatoth)
kwhoth.null.modelrd15heatoth <- lm(KWHOTH ~ 1, data = dfrd15heatoth)
kwhoth.null.modelrd16heatoth <- lm(KWHOTH ~ 1, data = dfrd16heatoth)
kwhoth.null.modelrd17heatoth <- lm(KWHOTH ~ 1, data = dfrd17heatoth)
kwhoth.null.modelrd18heatoth <- lm(KWHOTH ~ 1, data = dfrd18heatoth)
kwhoth.null.modelrd19heatoth <- lm(KWHOTH ~ 1, data = dfrd19heatoth)
kwhoth.null.modelrd20heatoth <- lm(KWHOTH ~ 1, data = dfrd20heatoth)
kwhoth.null.modelrd21heatoth <- lm(KWHOTH ~ 1, data = dfrd21heatoth)
kwhoth.null.modelrd22heatoth <- lm(KWHOTH ~ 1, data = dfrd22heatoth)
kwhoth.null.modelrd23heatoth <- lm(KWHOTH ~ 1, data = dfrd23heatoth)
kwhoth.null.modelrd24heatoth <- lm(KWHOTH ~ 1, data = dfrd24heatoth)
kwhoth.null.modelrd25heatoth <- lm(KWHOTH ~ 1, data = dfrd25heatoth)
kwhoth.null.modelrd26heatoth <- lm(KWHOTH ~ 1, data = dfrd26heatoth)
kwhoth.null.modelrd27heatoth <- lm(KWHOTH ~ 1, data = dfrd27heatoth)
kwhsph.null.modelrd1heatoth <- lm(KWHSPH ~ 1, data = dfrd1heatoth)
kwhsph.null.modelrd2heatoth <- lm(KWHSPH ~ 1, data = dfrd2heatoth)
kwhsph.null.modelrd3heatoth <- lm(KWHSPH ~ 1, data = dfrd3heatoth)
kwhsph.null.modelrd4heatoth <- lm(KWHSPH ~ 1, data = dfrd4heatoth)
kwhsph.null.modelrd5heatoth <- lm(KWHSPH ~ 1, data = dfrd5heatoth)
kwhsph.null.modelrd6heatoth <- lm(KWHSPH ~ 1, data = dfrd6heatoth)
kwhsph.null.modelrd7heatoth <- lm(KWHSPH ~ 1, data = dfrd7heatoth)
kwhsph.null.modelrd8heatoth <- lm(KWHSPH ~ 1, data = dfrd8heatoth)
kwhsph.null.modelrd9heatoth <- lm(KWHSPH ~ 1, data = dfrd9heatoth)
kwhsph.null.modelrd10heatoth <- lm(KWHSPH ~ 1, data = dfrd10heatoth)
kwhsph.null.modelrd11heatoth <- lm(KWHSPH ~ 1, data = dfrd11heatoth)
kwhsph.null.modelrd12heatoth <- lm(KWHSPH ~ 1, data = dfrd12heatoth)
kwhsph.null.modelrd13heatoth <- lm(KWHSPH ~ 1, data = dfrd13heatoth)
kwhsph.null.modelrd14heatoth <- lm(KWHSPH ~ 1, data = dfrd14heatoth)
kwhsph.null.modelrd15heatoth <- lm(KWHSPH ~ 1, data = dfrd15heatoth)
kwhsph.null.modelrd16heatoth <- lm(KWHSPH ~ 1, data = dfrd16heatoth)
kwhsph.null.modelrd17heatoth <- lm(KWHSPH ~ 1, data = dfrd17heatoth)
kwhsph.null.modelrd18heatoth <- lm(KWHSPH ~ 1, data = dfrd18heatoth)
kwhsph.null.modelrd19heatoth <- lm(KWHSPH ~ 1, data = dfrd19heatoth)
kwhsph.null.modelrd20heatoth <- lm(KWHSPH ~ 1, data = dfrd20heatoth)
kwhsph.null.modelrd21heatoth <- lm(KWHSPH ~ 1, data = dfrd21heatoth)
kwhsph.null.modelrd22heatoth <- lm(KWHSPH ~ 1, data = dfrd22heatoth)
kwhsph.null.modelrd23heatoth <- lm(KWHSPH ~ 1, data = dfrd23heatoth)
kwhsph.null.modelrd24heatoth <- lm(KWHSPH ~ 1, data = dfrd24heatoth)
kwhsph.null.modelrd25heatoth <- lm(KWHSPH ~ 1, data = dfrd25heatoth)
kwhsph.null.modelrd26heatoth <- lm(KWHSPH ~ 1, data = dfrd26heatoth)
kwhsph.null.modelrd27heatoth <- lm(KWHSPH ~ 1, data = dfrd27heatoth)
kwhwth.null.modelrd1heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd2heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd3heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd4heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd5heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd6heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd7heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd8heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd9heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd10heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd11heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd12heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd13heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd14heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd15heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd16heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd17heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd18heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd19heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd20heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd21heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd22heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd23heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd24heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd25heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd26heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)
kwhwth.null.modelrd27heatoth <- lm(KWHWTH ~ 1, data = df1heatoth)


##Full Models
btulp.full.modelrd1heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd2heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd3heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd4heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd5heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd6heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd7heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd8heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd9heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd10heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd11heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd12heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd13heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd14heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd15heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd16heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd17heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd18heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd19heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd20heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd21heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd22heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd23heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd24heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd25heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
btulp.full.modelrd26heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatany)
btulp.full.modelrd27heatany <- lm(BTULP ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatany)
kwhcol.full.modelrd1heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatany)
kwhcol.full.modelrd2heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatany)
kwhcol.full.modelrd3heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatany)
kwhcol.full.modelrd4heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatany)
kwhcol.full.modelrd5heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatany)
kwhcol.full.modelrd6heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatany)
kwhcol.full.modelrd7heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatany)
kwhcol.full.modelrd8heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatany)
kwhcol.full.modelrd9heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatany)
kwhcol.full.modelrd10heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatany)
kwhcol.full.modelrd11heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatany)
kwhcol.full.modelrd12heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatany)
kwhcol.full.modelrd13heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatany)
kwhcol.full.modelrd14heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatany)
kwhcol.full.modelrd15heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatany)
kwhcol.full.modelrd16heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatany)
kwhcol.full.modelrd17heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatany)
kwhcol.full.modelrd18heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatany)
kwhcol.full.modelrd19heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatany)
kwhcol.full.modelrd20heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatany)
kwhcol.full.modelrd21heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatany)
kwhcol.full.modelrd22heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatany)
kwhcol.full.modelrd23heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatany)
kwhcol.full.modelrd24heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatany)
kwhcol.full.modelrd25heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatany)
kwhcol.full.modelrd26heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatany)
kwhcol.full.modelrd27heatany <- lm(KWHCOL ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatany)
cufeetng.full.modelrd1heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
cufeetng.full.modelrd2heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
cufeetng.full.modelrd3heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
cufeetng.full.modelrd4heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
cufeetng.full.modelrd5heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
cufeetng.full.modelrd6heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
cufeetng.full.modelrd7heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
cufeetng.full.modelrd8heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
cufeetng.full.modelrd9heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
cufeetng.full.modelrd10heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
cufeetng.full.modelrd11heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
cufeetng.full.modelrd12heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
cufeetng.full.modelrd13heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
cufeetng.full.modelrd14heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
cufeetng.full.modelrd15heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
cufeetng.full.modelrd16heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
cufeetng.full.modelrd17heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
cufeetng.full.modelrd18heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
cufeetng.full.modelrd19heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
cufeetng.full.modelrd20heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
cufeetng.full.modelrd21heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
cufeetng.full.modelrd22heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
cufeetng.full.modelrd23heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
cufeetng.full.modelrd24heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
cufeetng.full.modelrd25heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
cufeetng.full.modelrd26heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
cufeetng.full.modelrd27heatgas <- lm(CUFEETNG ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
cufeetngsph.full.modelrd1heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
cufeetngsph.full.modelrd2heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
cufeetngsph.full.modelrd3heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
cufeetngsph.full.modelrd4heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
cufeetngsph.full.modelrd5heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
cufeetngsph.full.modelrd6heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
cufeetngsph.full.modelrd7heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
cufeetngsph.full.modelrd8heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
cufeetngsph.full.modelrd9heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
cufeetngsph.full.modelrd10heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
cufeetngsph.full.modelrd11heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
cufeetngsph.full.modelrd12heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
cufeetngsph.full.modelrd13heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
cufeetngsph.full.modelrd14heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
cufeetngsph.full.modelrd15heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
cufeetngsph.full.modelrd16heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
cufeetngsph.full.modelrd17heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
cufeetngsph.full.modelrd18heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
cufeetngsph.full.modelrd19heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
cufeetngsph.full.modelrd20heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
cufeetngsph.full.modelrd21heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
cufeetngsph.full.modelrd22heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
cufeetngsph.full.modelrd23heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
cufeetngsph.full.modelrd24heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
cufeetngsph.full.modelrd25heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
cufeetngsph.full.modelrd26heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
cufeetngsph.full.modelrd27heatgas <- lm(CUFEETNGSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
cufeetngwth.full.modelrd1heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
cufeetngwth.full.modelrd2heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
cufeetngwth.full.modelrd3heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
cufeetngwth.full.modelrd4heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
cufeetngwth.full.modelrd5heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
cufeetngwth.full.modelrd6heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
cufeetngwth.full.modelrd7heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
cufeetngwth.full.modelrd8heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
cufeetngwth.full.modelrd9heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
cufeetngwth.full.modelrd10heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
cufeetngwth.full.modelrd11heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
cufeetngwth.full.modelrd12heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
cufeetngwth.full.modelrd13heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
cufeetngwth.full.modelrd14heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
cufeetngwth.full.modelrd15heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
cufeetngwth.full.modelrd16heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
cufeetngwth.full.modelrd17heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
cufeetngwth.full.modelrd18heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
cufeetngwth.full.modelrd19heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
cufeetngwth.full.modelrd20heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
cufeetngwth.full.modelrd21heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
cufeetngwth.full.modelrd22heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
cufeetngwth.full.modelrd23heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
cufeetngwth.full.modelrd24heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
cufeetngwth.full.modelrd25heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
cufeetngwth.full.modelrd26heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
cufeetngwth.full.modelrd27heatgas <- lm(CUFEETNGWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
kwh.full.modelrd1heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
kwh.full.modelrd2heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
kwh.full.modelrd3heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
kwh.full.modelrd4heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
kwh.full.modelrd5heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
kwh.full.modelrd6heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
kwh.full.modelrd7heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
kwh.full.modelrd8heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
kwh.full.modelrd9heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
kwh.full.modelrd10heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
kwh.full.modelrd11heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
kwh.full.modelrd12heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
kwh.full.modelrd13heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
kwh.full.modelrd14heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
kwh.full.modelrd15heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
kwh.full.modelrd16heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
kwh.full.modelrd17heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
kwh.full.modelrd18heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
kwh.full.modelrd19heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
kwh.full.modelrd20heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
kwh.full.modelrd21heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
kwh.full.modelrd22heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
kwh.full.modelrd23heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
kwh.full.modelrd24heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
kwh.full.modelrd25heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
kwh.full.modelrd26heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
kwh.full.modelrd27heatgas <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
kwhoth.full.modelrd1heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
kwhoth.full.modelrd2heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
kwhoth.full.modelrd3heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
kwhoth.full.modelrd4heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
kwhoth.full.modelrd5heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
kwhoth.full.modelrd6heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
kwhoth.full.modelrd7heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
kwhoth.full.modelrd8heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
kwhoth.full.modelrd9heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
kwhoth.full.modelrd10heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
kwhoth.full.modelrd11heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
kwhoth.full.modelrd12heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
kwhoth.full.modelrd13heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
kwhoth.full.modelrd14heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
kwhoth.full.modelrd15heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
kwhoth.full.modelrd16heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
kwhoth.full.modelrd17heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
kwhoth.full.modelrd18heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
kwhoth.full.modelrd19heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
kwhoth.full.modelrd20heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
kwhoth.full.modelrd21heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
kwhoth.full.modelrd22heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
kwhoth.full.modelrd23heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
kwhoth.full.modelrd24heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
kwhoth.full.modelrd25heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
kwhoth.full.modelrd26heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
kwhoth.full.modelrd27heatgas <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
kwhsph.full.modelrd1heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd1heatgas)
kwhsph.full.modelrd2heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatgas)
kwhsph.full.modelrd3heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatgas)
kwhsph.full.modelrd4heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatgas)
kwhsph.full.modelrd5heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatgas)
kwhsph.full.modelrd6heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatgas)
kwhsph.full.modelrd7heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatgas)
kwhsph.full.modelrd8heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatgas)
kwhsph.full.modelrd9heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatgas)
kwhsph.full.modelrd10heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd10heatgas)
kwhsph.full.modelrd11heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd11heatgas)
kwhsph.full.modelrd12heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatgas)
kwhsph.full.modelrd13heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatgas)
kwhsph.full.modelrd14heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd14heatgas)
kwhsph.full.modelrd15heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatgas)
kwhsph.full.modelrd16heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd16heatgas)
kwhsph.full.modelrd17heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatgas)
kwhsph.full.modelrd18heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd18heatgas)
kwhsph.full.modelrd19heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatgas)
kwhsph.full.modelrd20heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd20heatgas)
kwhsph.full.modelrd21heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatgas)
kwhsph.full.modelrd22heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatgas)
kwhsph.full.modelrd23heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd23heatgas)
kwhsph.full.modelrd24heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatgas)
kwhsph.full.modelrd25heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd25heatgas)
kwhsph.full.modelrd26heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatgas)
kwhsph.full.modelrd27heatgas <- lm(KWHSPH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=dfrd27heatgas)
kwhwth.full.modelrd1heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd2heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd3heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd4heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd5heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd6heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd7heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd8heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd9heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd10heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd11heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd12heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd13heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd14heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd15heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd16heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd17heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd18heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd19heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd20heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd21heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd22heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd23heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd24heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd25heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwhwth.full.modelrd26heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatgas)
kwhwth.full.modelrd27heatgas <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatgas)
kwh.full.modelrd1heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatkwh)
kwh.full.modelrd2heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatkwh)
kwh.full.modelrd3heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatkwh)
kwh.full.modelrd4heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatkwh)
kwh.full.modelrd5heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatkwh)
kwh.full.modelrd6heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatkwh)
kwh.full.modelrd7heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatkwh)
kwh.full.modelrd8heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatkwh)
kwh.full.modelrd9heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatkwh)
kwh.full.modelrd10heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatkwh)
kwh.full.modelrd11heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatkwh)
kwh.full.modelrd12heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatkwh)
kwh.full.modelrd13heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatkwh)
kwh.full.modelrd14heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatkwh)
kwh.full.modelrd15heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatkwh)
kwh.full.modelrd16heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatkwh)
kwh.full.modelrd17heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatkwh)
kwh.full.modelrd18heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatkwh)
kwh.full.modelrd19heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatkwh)
kwh.full.modelrd20heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatkwh)
kwh.full.modelrd21heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatkwh)
kwh.full.modelrd22heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatkwh)
kwh.full.modelrd23heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatkwh)
kwh.full.modelrd24heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatkwh)
kwh.full.modelrd25heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatkwh)
kwh.full.modelrd26heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatkwh)
kwh.full.modelrd27heatkwh <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatkwh)
kwhoth.full.modelrd1heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatkwh)
kwhoth.full.modelrd2heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatkwh)
kwhoth.full.modelrd3heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatkwh)
kwhoth.full.modelrd4heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatkwh)
kwhoth.full.modelrd5heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatkwh)
kwhoth.full.modelrd6heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatkwh)
kwhoth.full.modelrd7heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatkwh)
kwhoth.full.modelrd8heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatkwh)
kwhoth.full.modelrd9heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatkwh)
kwhoth.full.modelrd10heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatkwh)
kwhoth.full.modelrd11heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatkwh)
kwhoth.full.modelrd12heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatkwh)
kwhoth.full.modelrd13heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatkwh)
kwhoth.full.modelrd14heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatkwh)
kwhoth.full.modelrd15heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatkwh)
kwhoth.full.modelrd16heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatkwh)
kwhoth.full.modelrd17heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatkwh)
kwhoth.full.modelrd18heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatkwh)
kwhoth.full.modelrd19heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatkwh)
kwhoth.full.modelrd20heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatkwh)
kwhoth.full.modelrd21heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatkwh)
kwhoth.full.modelrd22heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatkwh)
kwhoth.full.modelrd23heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatkwh)
kwhoth.full.modelrd24heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatkwh)
kwhoth.full.modelrd25heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatkwh)
kwhoth.full.modelrd26heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatkwh)
kwhoth.full.modelrd27heatkwh <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatkwh)
kwhsph.full.modelrd1heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatkwh)
kwhsph.full.modelrd2heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatkwh)
kwhsph.full.modelrd3heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatkwh)
kwhsph.full.modelrd4heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatkwh)
kwhsph.full.modelrd5heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatkwh)
kwhsph.full.modelrd6heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatkwh)
kwhsph.full.modelrd7heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatkwh)
kwhsph.full.modelrd8heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatkwh)
kwhsph.full.modelrd9heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatkwh)
kwhsph.full.modelrd10heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatkwh)
kwhsph.full.modelrd11heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatkwh)
kwhsph.full.modelrd12heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatkwh)
kwhsph.full.modelrd13heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatkwh)
kwhsph.full.modelrd14heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatkwh)
kwhsph.full.modelrd15heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatkwh)
kwhsph.full.modelrd16heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatkwh)
kwhsph.full.modelrd17heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatkwh)
kwhsph.full.modelrd18heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatkwh)
kwhsph.full.modelrd19heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatkwh)
kwhsph.full.modelrd20heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatkwh)
kwhsph.full.modelrd21heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatkwh)
kwhsph.full.modelrd22heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatkwh)
kwhsph.full.modelrd23heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatkwh)
kwhsph.full.modelrd24heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatkwh)
kwhsph.full.modelrd25heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatkwh)
kwhsph.full.modelrd26heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatkwh)
kwhsph.full.modelrd27heatkwh <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatkwh)
kwhwth.full.modelrd1heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd2heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd3heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd4heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd5heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd6heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd7heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd8heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd9heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd10heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd11heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd12heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd13heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd14heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd15heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd16heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd17heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd18heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd19heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd20heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd21heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd22heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd23heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd24heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd25heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
kwhwth.full.modelrd26heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatkwh)
kwhwth.full.modelrd27heatkwh <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatkwh)
btuother.full.modelrd1heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatoth)
btuother.full.modelrd2heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatoth)
btuother.full.modelrd3heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatoth)
btuother.full.modelrd4heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatoth)
btuother.full.modelrd5heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatoth)
btuother.full.modelrd6heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatoth)
btuother.full.modelrd7heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatoth)
btuother.full.modelrd8heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatoth)
btuother.full.modelrd9heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatoth)
btuother.full.modelrd10heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatoth)
btuother.full.modelrd11heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatoth)
btuother.full.modelrd12heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatoth)
btuother.full.modelrd13heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatoth)
btuother.full.modelrd14heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatoth)
btuother.full.modelrd15heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatoth)
btuother.full.modelrd16heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatoth)
btuother.full.modelrd17heatoth <- lm(BTUOTHER ~ AVGINCOME + DEGREE + DETACHED + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatoth)
btuother.full.modelrd18heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatoth)
btuother.full.modelrd19heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatoth)
btuother.full.modelrd20heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatoth)
btuother.full.modelrd21heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatoth)
btuother.full.modelrd22heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatoth)
btuother.full.modelrd23heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatoth)
btuother.full.modelrd24heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatoth)
btuother.full.modelrd25heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatoth)
btuother.full.modelrd26heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatoth)
btuother.full.modelrd27heatoth <- lm(BTUOTHER ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatoth)
kwh.full.modelrd1heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatoth)
kwh.full.modelrd2heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatoth)
kwh.full.modelrd3heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatoth)
kwh.full.modelrd4heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatoth)
kwh.full.modelrd5heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatoth)
kwh.full.modelrd6heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatoth)
kwh.full.modelrd7heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatoth)
kwh.full.modelrd8heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatoth)
kwh.full.modelrd9heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatoth)
kwh.full.modelrd10heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatoth)
kwh.full.modelrd11heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatoth)
kwh.full.modelrd12heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatoth)
kwh.full.modelrd13heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatoth)
kwh.full.modelrd14heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatoth)
kwh.full.modelrd15heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatoth)
kwh.full.modelrd16heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatoth)
kwh.full.modelrd17heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatoth)
kwh.full.modelrd18heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatoth)
kwh.full.modelrd19heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatoth)
kwh.full.modelrd20heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatoth)
kwh.full.modelrd21heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatoth)
kwh.full.modelrd22heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatoth)
kwh.full.modelrd23heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatoth)
kwh.full.modelrd24heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatoth)
kwh.full.modelrd25heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatoth)
kwh.full.modelrd26heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatoth)
kwh.full.modelrd27heatoth <- lm(KWH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatoth)
kwhoth.full.modelrd1heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatoth)
kwhoth.full.modelrd2heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatoth)
kwhoth.full.modelrd3heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatoth)
kwhoth.full.modelrd4heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatoth)
kwhoth.full.modelrd5heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatoth)
kwhoth.full.modelrd6heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatoth)
kwhoth.full.modelrd7heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatoth)
kwhoth.full.modelrd8heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatoth)
kwhoth.full.modelrd9heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatoth)
kwhoth.full.modelrd10heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatoth)
kwhoth.full.modelrd11heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatoth)
kwhoth.full.modelrd12heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatoth)
kwhoth.full.modelrd13heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatoth)
kwhoth.full.modelrd14heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatoth)
kwhoth.full.modelrd15heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatoth)
kwhoth.full.modelrd16heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatoth)
kwhoth.full.modelrd17heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatoth)
kwhoth.full.modelrd18heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatoth)
kwhoth.full.modelrd19heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatoth)
kwhoth.full.modelrd20heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatoth)
kwhoth.full.modelrd21heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatoth)
kwhoth.full.modelrd22heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatoth)
kwhoth.full.modelrd23heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatoth)
kwhoth.full.modelrd24heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatoth)
kwhoth.full.modelrd25heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatoth)
kwhoth.full.modelrd26heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatoth)
kwhoth.full.modelrd27heatoth <- lm(KWHOTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatoth)
kwhsph.full.modelrd1heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd1heatoth)
kwhsph.full.modelrd2heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd2heatoth)
kwhsph.full.modelrd3heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd3heatoth)
kwhsph.full.modelrd4heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd4heatoth)
kwhsph.full.modelrd5heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd5heatoth)
kwhsph.full.modelrd6heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd6heatoth)
kwhsph.full.modelrd7heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd7heatoth)
kwhsph.full.modelrd8heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd8heatoth)
kwhsph.full.modelrd9heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd9heatoth)
kwhsph.full.modelrd10heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd10heatoth)
kwhsph.full.modelrd11heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd11heatoth)
kwhsph.full.modelrd12heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd12heatoth)
kwhsph.full.modelrd13heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd13heatoth)
kwhsph.full.modelrd14heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd14heatoth)
kwhsph.full.modelrd15heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd15heatoth)
kwhsph.full.modelrd16heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd16heatoth)
kwhsph.full.modelrd17heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd17heatoth)
kwhsph.full.modelrd18heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd18heatoth)
kwhsph.full.modelrd19heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd19heatoth)
kwhsph.full.modelrd20heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd20heatoth)
kwhsph.full.modelrd21heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd21heatoth)
kwhsph.full.modelrd22heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd22heatoth)
kwhsph.full.modelrd23heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd23heatoth)
kwhsph.full.modelrd24heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd24heatoth)
kwhsph.full.modelrd25heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd25heatoth)
kwhsph.full.modelrd26heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=dfrd26heatoth)
kwhsph.full.modelrd27heatoth <- lm(KWHSPH ~ AVGINCOME + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH, data=dfrd27heatoth)
kwhwth.full.modelrd1heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd2heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd3heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd4heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd5heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd6heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd7heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd8heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd9heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd10heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd11heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd12heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd13heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd14heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd15heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd16heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd17heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd18heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd19heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd20heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd21heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd22heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd23heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd24heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd25heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)
kwhwth.full.modelrd26heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE, data=df1heatoth)
kwhwth.full.modelrd27heatoth <- lm(KWHWTH ~ AVGINCOME + CDD65 + DEGREE + DETACHED + HDD65 + HHAGE  + HHSIZE + HOMEAGE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME + LNHHAGE + PRICEKWH + PRICENATGAS, data=df1heatoth)


## STEP MODELS
btulp.step.modelrd1heatany <- stepAIC(btulp.null.modelrd1heatany, scope = formula(btulp.full.modelrd1heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd2heatany <- stepAIC(btulp.null.modelrd2heatany, scope = formula(btulp.full.modelrd2heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd3heatany <- stepAIC(btulp.null.modelrd3heatany, scope = formula(btulp.full.modelrd3heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd4heatany <- stepAIC(btulp.null.modelrd4heatany, scope = formula(btulp.full.modelrd4heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd5heatany <- stepAIC(btulp.null.modelrd5heatany, scope = formula(btulp.full.modelrd5heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd6heatany <- stepAIC(btulp.null.modelrd6heatany, scope = formula(btulp.full.modelrd6heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd7heatany <- stepAIC(btulp.null.modelrd7heatany, scope = formula(btulp.full.modelrd7heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd8heatany <- stepAIC(btulp.null.modelrd8heatany, scope = formula(btulp.full.modelrd8heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd9heatany <- stepAIC(btulp.null.modelrd9heatany, scope = formula(btulp.full.modelrd9heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd10heatany <- stepAIC(btulp.null.modelrd10heatany, scope = formula(btulp.full.modelrd10heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd11heatany <- stepAIC(btulp.null.modelrd11heatany, scope = formula(btulp.full.modelrd11heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd12heatany <- stepAIC(btulp.null.modelrd12heatany, scope = formula(btulp.full.modelrd12heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd13heatany <- stepAIC(btulp.null.modelrd13heatany, scope = formula(btulp.full.modelrd13heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd14heatany <- stepAIC(btulp.null.modelrd14heatany, scope = formula(btulp.full.modelrd14heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd15heatany <- stepAIC(btulp.null.modelrd15heatany, scope = formula(btulp.full.modelrd15heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd16heatany <- stepAIC(btulp.null.modelrd16heatany, scope = formula(btulp.full.modelrd16heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd17heatany <- stepAIC(btulp.null.modelrd17heatany, scope = formula(btulp.full.modelrd17heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd18heatany <- stepAIC(btulp.null.modelrd18heatany, scope = formula(btulp.full.modelrd18heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd19heatany <- stepAIC(btulp.null.modelrd19heatany, scope = formula(btulp.full.modelrd19heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd20heatany <- stepAIC(btulp.null.modelrd20heatany, scope = formula(btulp.full.modelrd20heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd21heatany <- stepAIC(btulp.null.modelrd21heatany, scope = formula(btulp.full.modelrd21heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd22heatany <- stepAIC(btulp.null.modelrd22heatany, scope = formula(btulp.full.modelrd22heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd23heatany <- stepAIC(btulp.null.modelrd23heatany, scope = formula(btulp.full.modelrd23heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd24heatany <- stepAIC(btulp.null.modelrd24heatany, scope = formula(btulp.full.modelrd24heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd25heatany <- stepAIC(btulp.null.modelrd25heatany, scope = formula(btulp.full.modelrd25heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd26heatany <- stepAIC(btulp.null.modelrd26heatany, scope = formula(btulp.full.modelrd26heatany),  direction = "forward", weights = recs$NWEIGHT)
btulp.step.modelrd27heatany <- stepAIC(btulp.null.modelrd27heatany, scope = formula(btulp.full.modelrd27heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd1heatany <- stepAIC(kwhcol.null.modelrd1heatany, scope = formula(kwhcol.full.modelrd1heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd2heatany <- stepAIC(kwhcol.null.modelrd2heatany, scope = formula(kwhcol.full.modelrd2heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd3heatany <- stepAIC(kwhcol.null.modelrd3heatany, scope = formula(kwhcol.full.modelrd3heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd4heatany <- stepAIC(kwhcol.null.modelrd4heatany, scope = formula(kwhcol.full.modelrd4heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd5heatany <- stepAIC(kwhcol.null.modelrd5heatany, scope = formula(kwhcol.full.modelrd5heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd6heatany <- stepAIC(kwhcol.null.modelrd6heatany, scope = formula(kwhcol.full.modelrd6heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd7heatany <- stepAIC(kwhcol.null.modelrd7heatany, scope = formula(kwhcol.full.modelrd7heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd8heatany <- stepAIC(kwhcol.null.modelrd8heatany, scope = formula(kwhcol.full.modelrd8heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd9heatany <- stepAIC(kwhcol.null.modelrd9heatany, scope = formula(kwhcol.full.modelrd9heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd10heatany <- stepAIC(kwhcol.null.modelrd10heatany, scope = formula(kwhcol.full.modelrd10heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd11heatany <- stepAIC(kwhcol.null.modelrd11heatany, scope = formula(kwhcol.full.modelrd11heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd12heatany <- stepAIC(kwhcol.null.modelrd12heatany, scope = formula(kwhcol.full.modelrd12heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd13heatany <- stepAIC(kwhcol.null.modelrd13heatany, scope = formula(kwhcol.full.modelrd13heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd14heatany <- stepAIC(kwhcol.null.modelrd14heatany, scope = formula(kwhcol.full.modelrd14heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd15heatany <- stepAIC(kwhcol.null.modelrd15heatany, scope = formula(kwhcol.full.modelrd15heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd16heatany <- stepAIC(kwhcol.null.modelrd16heatany, scope = formula(kwhcol.full.modelrd16heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd17heatany <- stepAIC(kwhcol.null.modelrd17heatany, scope = formula(kwhcol.full.modelrd17heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd18heatany <- stepAIC(kwhcol.null.modelrd18heatany, scope = formula(kwhcol.full.modelrd18heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd19heatany <- stepAIC(kwhcol.null.modelrd19heatany, scope = formula(kwhcol.full.modelrd19heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd20heatany <- stepAIC(kwhcol.null.modelrd20heatany, scope = formula(kwhcol.full.modelrd20heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd21heatany <- stepAIC(kwhcol.null.modelrd21heatany, scope = formula(kwhcol.full.modelrd21heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd22heatany <- stepAIC(kwhcol.null.modelrd22heatany, scope = formula(kwhcol.full.modelrd22heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd23heatany <- stepAIC(kwhcol.null.modelrd23heatany, scope = formula(kwhcol.full.modelrd23heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd24heatany <- stepAIC(kwhcol.null.modelrd24heatany, scope = formula(kwhcol.full.modelrd24heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd25heatany <- stepAIC(kwhcol.null.modelrd25heatany, scope = formula(kwhcol.full.modelrd25heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd26heatany <- stepAIC(kwhcol.null.modelrd26heatany, scope = formula(kwhcol.full.modelrd26heatany),  direction = "forward", weights = recs$NWEIGHT)
kwhcol.step.modelrd27heatany <- stepAIC(kwhcol.null.modelrd27heatany, scope = formula(kwhcol.full.modelrd27heatany),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd1heatgas <- stepAIC(cufeetng.null.modelrd1heatgas, scope = formula(cufeetng.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd2heatgas <- stepAIC(cufeetng.null.modelrd2heatgas, scope = formula(cufeetng.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd3heatgas <- stepAIC(cufeetng.null.modelrd3heatgas, scope = formula(cufeetng.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd4heatgas <- stepAIC(cufeetng.null.modelrd4heatgas, scope = formula(cufeetng.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd5heatgas <- stepAIC(cufeetng.null.modelrd5heatgas, scope = formula(cufeetng.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd6heatgas <- stepAIC(cufeetng.null.modelrd6heatgas, scope = formula(cufeetng.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd7heatgas <- stepAIC(cufeetng.null.modelrd7heatgas, scope = formula(cufeetng.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd8heatgas <- stepAIC(cufeetng.null.modelrd8heatgas, scope = formula(cufeetng.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd9heatgas <- stepAIC(cufeetng.null.modelrd9heatgas, scope = formula(cufeetng.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd10heatgas <- stepAIC(cufeetng.null.modelrd10heatgas, scope = formula(cufeetng.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd11heatgas <- stepAIC(cufeetng.null.modelrd11heatgas, scope = formula(cufeetng.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd12heatgas <- stepAIC(cufeetng.null.modelrd12heatgas, scope = formula(cufeetng.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd13heatgas <- stepAIC(cufeetng.null.modelrd13heatgas, scope = formula(cufeetng.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd14heatgas <- stepAIC(cufeetng.null.modelrd14heatgas, scope = formula(cufeetng.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd15heatgas <- stepAIC(cufeetng.null.modelrd15heatgas, scope = formula(cufeetng.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd16heatgas <- stepAIC(cufeetng.null.modelrd16heatgas, scope = formula(cufeetng.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd17heatgas <- stepAIC(cufeetng.null.modelrd17heatgas, scope = formula(cufeetng.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd18heatgas <- stepAIC(cufeetng.null.modelrd18heatgas, scope = formula(cufeetng.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd19heatgas <- stepAIC(cufeetng.null.modelrd19heatgas, scope = formula(cufeetng.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd20heatgas <- stepAIC(cufeetng.null.modelrd20heatgas, scope = formula(cufeetng.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd21heatgas <- stepAIC(cufeetng.null.modelrd21heatgas, scope = formula(cufeetng.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd22heatgas <- stepAIC(cufeetng.null.modelrd22heatgas, scope = formula(cufeetng.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd23heatgas <- stepAIC(cufeetng.null.modelrd23heatgas, scope = formula(cufeetng.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd24heatgas <- stepAIC(cufeetng.null.modelrd24heatgas, scope = formula(cufeetng.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd25heatgas <- stepAIC(cufeetng.null.modelrd25heatgas, scope = formula(cufeetng.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd26heatgas <- stepAIC(cufeetng.null.modelrd26heatgas, scope = formula(cufeetng.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetng.step.modelrd27heatgas <- stepAIC(cufeetng.null.modelrd27heatgas, scope = formula(cufeetng.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd1heatgas <- stepAIC(cufeetngsph.null.modelrd1heatgas, scope = formula(cufeetngsph.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd2heatgas <- stepAIC(cufeetngsph.null.modelrd2heatgas, scope = formula(cufeetngsph.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd3heatgas <- stepAIC(cufeetngsph.null.modelrd3heatgas, scope = formula(cufeetngsph.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd4heatgas <- stepAIC(cufeetngsph.null.modelrd4heatgas, scope = formula(cufeetngsph.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd5heatgas <- stepAIC(cufeetngsph.null.modelrd5heatgas, scope = formula(cufeetngsph.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd6heatgas <- stepAIC(cufeetngsph.null.modelrd6heatgas, scope = formula(cufeetngsph.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd7heatgas <- stepAIC(cufeetngsph.null.modelrd7heatgas, scope = formula(cufeetngsph.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd8heatgas <- stepAIC(cufeetngsph.null.modelrd8heatgas, scope = formula(cufeetngsph.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd9heatgas <- stepAIC(cufeetngsph.null.modelrd9heatgas, scope = formula(cufeetngsph.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd10heatgas <- stepAIC(cufeetngsph.null.modelrd10heatgas, scope = formula(cufeetngsph.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd11heatgas <- stepAIC(cufeetngsph.null.modelrd11heatgas, scope = formula(cufeetngsph.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd12heatgas <- stepAIC(cufeetngsph.null.modelrd12heatgas, scope = formula(cufeetngsph.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd13heatgas <- stepAIC(cufeetngsph.null.modelrd13heatgas, scope = formula(cufeetngsph.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd14heatgas <- stepAIC(cufeetngsph.null.modelrd14heatgas, scope = formula(cufeetngsph.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd15heatgas <- stepAIC(cufeetngsph.null.modelrd15heatgas, scope = formula(cufeetngsph.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd16heatgas <- stepAIC(cufeetngsph.null.modelrd16heatgas, scope = formula(cufeetngsph.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd17heatgas <- stepAIC(cufeetngsph.null.modelrd17heatgas, scope = formula(cufeetngsph.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd18heatgas <- stepAIC(cufeetngsph.null.modelrd18heatgas, scope = formula(cufeetngsph.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd19heatgas <- stepAIC(cufeetngsph.null.modelrd19heatgas, scope = formula(cufeetngsph.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd20heatgas <- stepAIC(cufeetngsph.null.modelrd20heatgas, scope = formula(cufeetngsph.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd21heatgas <- stepAIC(cufeetngsph.null.modelrd21heatgas, scope = formula(cufeetngsph.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd22heatgas <- stepAIC(cufeetngsph.null.modelrd22heatgas, scope = formula(cufeetngsph.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd23heatgas <- stepAIC(cufeetngsph.null.modelrd23heatgas, scope = formula(cufeetngsph.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd24heatgas <- stepAIC(cufeetngsph.null.modelrd24heatgas, scope = formula(cufeetngsph.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd25heatgas <- stepAIC(cufeetngsph.null.modelrd25heatgas, scope = formula(cufeetngsph.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd26heatgas <- stepAIC(cufeetngsph.null.modelrd26heatgas, scope = formula(cufeetngsph.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngsph.step.modelrd27heatgas <- stepAIC(cufeetngsph.null.modelrd27heatgas, scope = formula(cufeetngsph.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd1heatgas <- stepAIC(cufeetngwth.null.modelrd1heatgas, scope = formula(cufeetngwth.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd2heatgas <- stepAIC(cufeetngwth.null.modelrd2heatgas, scope = formula(cufeetngwth.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd3heatgas <- stepAIC(cufeetngwth.null.modelrd3heatgas, scope = formula(cufeetngwth.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd4heatgas <- stepAIC(cufeetngwth.null.modelrd4heatgas, scope = formula(cufeetngwth.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd5heatgas <- stepAIC(cufeetngwth.null.modelrd5heatgas, scope = formula(cufeetngwth.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd6heatgas <- stepAIC(cufeetngwth.null.modelrd6heatgas, scope = formula(cufeetngwth.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd7heatgas <- stepAIC(cufeetngwth.null.modelrd7heatgas, scope = formula(cufeetngwth.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd8heatgas <- stepAIC(cufeetngwth.null.modelrd8heatgas, scope = formula(cufeetngwth.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd9heatgas <- stepAIC(cufeetngwth.null.modelrd9heatgas, scope = formula(cufeetngwth.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd10heatgas <- stepAIC(cufeetngwth.null.modelrd10heatgas, scope = formula(cufeetngwth.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd11heatgas <- stepAIC(cufeetngwth.null.modelrd11heatgas, scope = formula(cufeetngwth.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd12heatgas <- stepAIC(cufeetngwth.null.modelrd12heatgas, scope = formula(cufeetngwth.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd13heatgas <- stepAIC(cufeetngwth.null.modelrd13heatgas, scope = formula(cufeetngwth.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd14heatgas <- stepAIC(cufeetngwth.null.modelrd14heatgas, scope = formula(cufeetngwth.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd15heatgas <- stepAIC(cufeetngwth.null.modelrd15heatgas, scope = formula(cufeetngwth.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd16heatgas <- stepAIC(cufeetngwth.null.modelrd16heatgas, scope = formula(cufeetngwth.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd17heatgas <- stepAIC(cufeetngwth.null.modelrd17heatgas, scope = formula(cufeetngwth.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd18heatgas <- stepAIC(cufeetngwth.null.modelrd18heatgas, scope = formula(cufeetngwth.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd19heatgas <- stepAIC(cufeetngwth.null.modelrd19heatgas, scope = formula(cufeetngwth.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd20heatgas <- stepAIC(cufeetngwth.null.modelrd20heatgas, scope = formula(cufeetngwth.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd21heatgas <- stepAIC(cufeetngwth.null.modelrd21heatgas, scope = formula(cufeetngwth.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd22heatgas <- stepAIC(cufeetngwth.null.modelrd22heatgas, scope = formula(cufeetngwth.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd23heatgas <- stepAIC(cufeetngwth.null.modelrd23heatgas, scope = formula(cufeetngwth.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd24heatgas <- stepAIC(cufeetngwth.null.modelrd24heatgas, scope = formula(cufeetngwth.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd25heatgas <- stepAIC(cufeetngwth.null.modelrd25heatgas, scope = formula(cufeetngwth.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd26heatgas <- stepAIC(cufeetngwth.null.modelrd26heatgas, scope = formula(cufeetngwth.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
cufeetngwth.step.modelrd27heatgas <- stepAIC(cufeetngwth.null.modelrd27heatgas, scope = formula(cufeetngwth.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd1heatgas <- stepAIC(kwh.null.modelrd1heatgas, scope = formula(kwh.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd2heatgas <- stepAIC(kwh.null.modelrd2heatgas, scope = formula(kwh.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd3heatgas <- stepAIC(kwh.null.modelrd3heatgas, scope = formula(kwh.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd4heatgas <- stepAIC(kwh.null.modelrd4heatgas, scope = formula(kwh.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd5heatgas <- stepAIC(kwh.null.modelrd5heatgas, scope = formula(kwh.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd6heatgas <- stepAIC(kwh.null.modelrd6heatgas, scope = formula(kwh.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd7heatgas <- stepAIC(kwh.null.modelrd7heatgas, scope = formula(kwh.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd8heatgas <- stepAIC(kwh.null.modelrd8heatgas, scope = formula(kwh.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd9heatgas <- stepAIC(kwh.null.modelrd9heatgas, scope = formula(kwh.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd10heatgas <- stepAIC(kwh.null.modelrd10heatgas, scope = formula(kwh.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd11heatgas <- stepAIC(kwh.null.modelrd11heatgas, scope = formula(kwh.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd12heatgas <- stepAIC(kwh.null.modelrd12heatgas, scope = formula(kwh.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd13heatgas <- stepAIC(kwh.null.modelrd13heatgas, scope = formula(kwh.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd14heatgas <- stepAIC(kwh.null.modelrd14heatgas, scope = formula(kwh.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd15heatgas <- stepAIC(kwh.null.modelrd15heatgas, scope = formula(kwh.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd16heatgas <- stepAIC(kwh.null.modelrd16heatgas, scope = formula(kwh.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd17heatgas <- stepAIC(kwh.null.modelrd17heatgas, scope = formula(kwh.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd18heatgas <- stepAIC(kwh.null.modelrd18heatgas, scope = formula(kwh.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd19heatgas <- stepAIC(kwh.null.modelrd19heatgas, scope = formula(kwh.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd20heatgas <- stepAIC(kwh.null.modelrd20heatgas, scope = formula(kwh.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd21heatgas <- stepAIC(kwh.null.modelrd21heatgas, scope = formula(kwh.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd22heatgas <- stepAIC(kwh.null.modelrd22heatgas, scope = formula(kwh.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd23heatgas <- stepAIC(kwh.null.modelrd23heatgas, scope = formula(kwh.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd24heatgas <- stepAIC(kwh.null.modelrd24heatgas, scope = formula(kwh.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd25heatgas <- stepAIC(kwh.null.modelrd25heatgas, scope = formula(kwh.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd26heatgas <- stepAIC(kwh.null.modelrd26heatgas, scope = formula(kwh.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd27heatgas <- stepAIC(kwh.null.modelrd27heatgas, scope = formula(kwh.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd1heatgas <- stepAIC(kwhoth.null.modelrd1heatgas, scope = formula(kwhoth.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd2heatgas <- stepAIC(kwhoth.null.modelrd2heatgas, scope = formula(kwhoth.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd3heatgas <- stepAIC(kwhoth.null.modelrd3heatgas, scope = formula(kwhoth.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd4heatgas <- stepAIC(kwhoth.null.modelrd4heatgas, scope = formula(kwhoth.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd5heatgas <- stepAIC(kwhoth.null.modelrd5heatgas, scope = formula(kwhoth.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd6heatgas <- stepAIC(kwhoth.null.modelrd6heatgas, scope = formula(kwhoth.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd7heatgas <- stepAIC(kwhoth.null.modelrd7heatgas, scope = formula(kwhoth.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd8heatgas <- stepAIC(kwhoth.null.modelrd8heatgas, scope = formula(kwhoth.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd9heatgas <- stepAIC(kwhoth.null.modelrd9heatgas, scope = formula(kwhoth.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd10heatgas <- stepAIC(kwhoth.null.modelrd10heatgas, scope = formula(kwhoth.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd11heatgas <- stepAIC(kwhoth.null.modelrd11heatgas, scope = formula(kwhoth.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd12heatgas <- stepAIC(kwhoth.null.modelrd12heatgas, scope = formula(kwhoth.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd13heatgas <- stepAIC(kwhoth.null.modelrd13heatgas, scope = formula(kwhoth.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd14heatgas <- stepAIC(kwhoth.null.modelrd14heatgas, scope = formula(kwhoth.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd15heatgas <- stepAIC(kwhoth.null.modelrd15heatgas, scope = formula(kwhoth.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd16heatgas <- stepAIC(kwhoth.null.modelrd16heatgas, scope = formula(kwhoth.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd17heatgas <- stepAIC(kwhoth.null.modelrd17heatgas, scope = formula(kwhoth.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd18heatgas <- stepAIC(kwhoth.null.modelrd18heatgas, scope = formula(kwhoth.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd19heatgas <- stepAIC(kwhoth.null.modelrd19heatgas, scope = formula(kwhoth.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd20heatgas <- stepAIC(kwhoth.null.modelrd20heatgas, scope = formula(kwhoth.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd21heatgas <- stepAIC(kwhoth.null.modelrd21heatgas, scope = formula(kwhoth.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd22heatgas <- stepAIC(kwhoth.null.modelrd22heatgas, scope = formula(kwhoth.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd23heatgas <- stepAIC(kwhoth.null.modelrd23heatgas, scope = formula(kwhoth.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd24heatgas <- stepAIC(kwhoth.null.modelrd24heatgas, scope = formula(kwhoth.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd25heatgas <- stepAIC(kwhoth.null.modelrd25heatgas, scope = formula(kwhoth.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd26heatgas <- stepAIC(kwhoth.null.modelrd26heatgas, scope = formula(kwhoth.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd27heatgas <- stepAIC(kwhoth.null.modelrd27heatgas, scope = formula(kwhoth.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd1heatgas <- stepAIC(kwhsph.null.modelrd1heatgas, scope = formula(kwhsph.full.modelrd1heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd2heatgas <- stepAIC(kwhsph.null.modelrd2heatgas, scope = formula(kwhsph.full.modelrd2heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd3heatgas <- stepAIC(kwhsph.null.modelrd3heatgas, scope = formula(kwhsph.full.modelrd3heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd4heatgas <- stepAIC(kwhsph.null.modelrd4heatgas, scope = formula(kwhsph.full.modelrd4heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd5heatgas <- stepAIC(kwhsph.null.modelrd5heatgas, scope = formula(kwhsph.full.modelrd5heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd6heatgas <- stepAIC(kwhsph.null.modelrd6heatgas, scope = formula(kwhsph.full.modelrd6heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd7heatgas <- stepAIC(kwhsph.null.modelrd7heatgas, scope = formula(kwhsph.full.modelrd7heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd8heatgas <- stepAIC(kwhsph.null.modelrd8heatgas, scope = formula(kwhsph.full.modelrd8heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd9heatgas <- stepAIC(kwhsph.null.modelrd9heatgas, scope = formula(kwhsph.full.modelrd9heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd10heatgas <- stepAIC(kwhsph.null.modelrd10heatgas, scope = formula(kwhsph.full.modelrd10heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd11heatgas <- stepAIC(kwhsph.null.modelrd11heatgas, scope = formula(kwhsph.full.modelrd11heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd12heatgas <- stepAIC(kwhsph.null.modelrd12heatgas, scope = formula(kwhsph.full.modelrd12heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd13heatgas <- stepAIC(kwhsph.null.modelrd13heatgas, scope = formula(kwhsph.full.modelrd13heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd14heatgas <- stepAIC(kwhsph.null.modelrd14heatgas, scope = formula(kwhsph.full.modelrd14heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd15heatgas <- stepAIC(kwhsph.null.modelrd15heatgas, scope = formula(kwhsph.full.modelrd15heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd16heatgas <- stepAIC(kwhsph.null.modelrd16heatgas, scope = formula(kwhsph.full.modelrd16heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd17heatgas <- stepAIC(kwhsph.null.modelrd17heatgas, scope = formula(kwhsph.full.modelrd17heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd18heatgas <- stepAIC(kwhsph.null.modelrd18heatgas, scope = formula(kwhsph.full.modelrd18heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd19heatgas <- stepAIC(kwhsph.null.modelrd19heatgas, scope = formula(kwhsph.full.modelrd19heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd20heatgas <- stepAIC(kwhsph.null.modelrd20heatgas, scope = formula(kwhsph.full.modelrd20heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd21heatgas <- stepAIC(kwhsph.null.modelrd21heatgas, scope = formula(kwhsph.full.modelrd21heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd22heatgas <- stepAIC(kwhsph.null.modelrd22heatgas, scope = formula(kwhsph.full.modelrd22heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd23heatgas <- stepAIC(kwhsph.null.modelrd23heatgas, scope = formula(kwhsph.full.modelrd23heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd24heatgas <- stepAIC(kwhsph.null.modelrd24heatgas, scope = formula(kwhsph.full.modelrd24heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd25heatgas <- stepAIC(kwhsph.null.modelrd25heatgas, scope = formula(kwhsph.full.modelrd25heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd26heatgas <- stepAIC(kwhsph.null.modelrd26heatgas, scope = formula(kwhsph.full.modelrd26heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd27heatgas <- stepAIC(kwhsph.null.modelrd27heatgas, scope = formula(kwhsph.full.modelrd27heatgas),  direction = "forward", weights = recs$NWEIGHT)
kwhwth.step.modelrd1heatgas <- kwhwth.full.modelrd1heatgas
kwhwth.step.modelrd2heatgas <- kwhwth.full.modelrd2heatgas
kwhwth.step.modelrd3heatgas <- kwhwth.full.modelrd3heatgas
kwhwth.step.modelrd4heatgas <- kwhwth.full.modelrd4heatgas
kwhwth.step.modelrd5heatgas <- kwhwth.full.modelrd5heatgas
kwhwth.step.modelrd6heatgas <- kwhwth.full.modelrd6heatgas
kwhwth.step.modelrd7heatgas <- kwhwth.full.modelrd7heatgas
kwhwth.step.modelrd8heatgas <- kwhwth.full.modelrd8heatgas
kwhwth.step.modelrd9heatgas <- kwhwth.full.modelrd9heatgas
kwhwth.step.modelrd10heatgas <- kwhwth.full.modelrd10heatgas
kwhwth.step.modelrd11heatgas <- kwhwth.full.modelrd11heatgas
kwhwth.step.modelrd12heatgas <- kwhwth.full.modelrd12heatgas
kwhwth.step.modelrd13heatgas <- kwhwth.full.modelrd13heatgas
kwhwth.step.modelrd14heatgas <- kwhwth.full.modelrd14heatgas
kwhwth.step.modelrd15heatgas <- kwhwth.full.modelrd15heatgas
kwhwth.step.modelrd16heatgas <- kwhwth.full.modelrd16heatgas
kwhwth.step.modelrd17heatgas <- kwhwth.full.modelrd17heatgas
kwhwth.step.modelrd18heatgas <- kwhwth.full.modelrd18heatgas
kwhwth.step.modelrd19heatgas <- kwhwth.full.modelrd19heatgas
kwhwth.step.modelrd20heatgas <- kwhwth.full.modelrd20heatgas
kwhwth.step.modelrd21heatgas <- kwhwth.full.modelrd21heatgas
kwhwth.step.modelrd22heatgas <- kwhwth.full.modelrd22heatgas
kwhwth.step.modelrd23heatgas <- kwhwth.full.modelrd23heatgas
kwhwth.step.modelrd24heatgas <- kwhwth.full.modelrd24heatgas
kwhwth.step.modelrd25heatgas <- kwhwth.full.modelrd25heatgas
kwhwth.step.modelrd26heatgas <- kwhwth.full.modelrd26heatgas
kwhwth.step.modelrd27heatgas <- kwhwth.full.modelrd27heatgas
kwh.step.modelrd1heatkwh <- stepAIC(kwh.null.modelrd1heatkwh, scope = formula(kwh.full.modelrd1heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd2heatkwh <- stepAIC(kwh.null.modelrd2heatkwh, scope = formula(kwh.full.modelrd2heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd3heatkwh <- stepAIC(kwh.null.modelrd3heatkwh, scope = formula(kwh.full.modelrd3heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd4heatkwh <- stepAIC(kwh.null.modelrd4heatkwh, scope = formula(kwh.full.modelrd4heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd5heatkwh <- stepAIC(kwh.null.modelrd5heatkwh, scope = formula(kwh.full.modelrd5heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd6heatkwh <- stepAIC(kwh.null.modelrd6heatkwh, scope = formula(kwh.full.modelrd6heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd7heatkwh <- stepAIC(kwh.null.modelrd7heatkwh, scope = formula(kwh.full.modelrd7heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd8heatkwh <- stepAIC(kwh.null.modelrd8heatkwh, scope = formula(kwh.full.modelrd8heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd9heatkwh <- stepAIC(kwh.null.modelrd9heatkwh, scope = formula(kwh.full.modelrd9heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd10heatkwh <- stepAIC(kwh.null.modelrd10heatkwh, scope = formula(kwh.full.modelrd10heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd11heatkwh <- stepAIC(kwh.null.modelrd11heatkwh, scope = formula(kwh.full.modelrd11heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd12heatkwh <- stepAIC(kwh.null.modelrd12heatkwh, scope = formula(kwh.full.modelrd12heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd13heatkwh <- stepAIC(kwh.null.modelrd13heatkwh, scope = formula(kwh.full.modelrd13heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd14heatkwh <- stepAIC(kwh.null.modelrd14heatkwh, scope = formula(kwh.full.modelrd14heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd15heatkwh <- stepAIC(kwh.null.modelrd15heatkwh, scope = formula(kwh.full.modelrd15heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd16heatkwh <- stepAIC(kwh.null.modelrd16heatkwh, scope = formula(kwh.full.modelrd16heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd17heatkwh <- stepAIC(kwh.null.modelrd17heatkwh, scope = formula(kwh.full.modelrd17heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd18heatkwh <- stepAIC(kwh.null.modelrd18heatkwh, scope = formula(kwh.full.modelrd18heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd19heatkwh <- stepAIC(kwh.null.modelrd19heatkwh, scope = formula(kwh.full.modelrd19heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd20heatkwh <- stepAIC(kwh.null.modelrd20heatkwh, scope = formula(kwh.full.modelrd20heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd21heatkwh <- stepAIC(kwh.null.modelrd21heatkwh, scope = formula(kwh.full.modelrd21heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd22heatkwh <- stepAIC(kwh.null.modelrd22heatkwh, scope = formula(kwh.full.modelrd22heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd23heatkwh <- stepAIC(kwh.null.modelrd23heatkwh, scope = formula(kwh.full.modelrd23heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd24heatkwh <- stepAIC(kwh.null.modelrd24heatkwh, scope = formula(kwh.full.modelrd24heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd25heatkwh <- stepAIC(kwh.null.modelrd25heatkwh, scope = formula(kwh.full.modelrd25heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd26heatkwh <- stepAIC(kwh.null.modelrd26heatkwh, scope = formula(kwh.full.modelrd26heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd27heatkwh <- stepAIC(kwh.null.modelrd27heatkwh, scope = formula(kwh.full.modelrd27heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd1heatkwh <- stepAIC(kwhoth.null.modelrd1heatkwh, scope = formula(kwhoth.full.modelrd1heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd2heatkwh <- stepAIC(kwhoth.null.modelrd2heatkwh, scope = formula(kwhoth.full.modelrd2heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd3heatkwh <- stepAIC(kwhoth.null.modelrd3heatkwh, scope = formula(kwhoth.full.modelrd3heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd4heatkwh <- stepAIC(kwhoth.null.modelrd4heatkwh, scope = formula(kwhoth.full.modelrd4heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd5heatkwh <- stepAIC(kwhoth.null.modelrd5heatkwh, scope = formula(kwhoth.full.modelrd5heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd6heatkwh <- stepAIC(kwhoth.null.modelrd6heatkwh, scope = formula(kwhoth.full.modelrd6heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd7heatkwh <- stepAIC(kwhoth.null.modelrd7heatkwh, scope = formula(kwhoth.full.modelrd7heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd8heatkwh <- stepAIC(kwhoth.null.modelrd8heatkwh, scope = formula(kwhoth.full.modelrd8heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd9heatkwh <- stepAIC(kwhoth.null.modelrd9heatkwh, scope = formula(kwhoth.full.modelrd9heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd10heatkwh <- stepAIC(kwhoth.null.modelrd10heatkwh, scope = formula(kwhoth.full.modelrd10heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd11heatkwh <- stepAIC(kwhoth.null.modelrd11heatkwh, scope = formula(kwhoth.full.modelrd11heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd12heatkwh <- stepAIC(kwhoth.null.modelrd12heatkwh, scope = formula(kwhoth.full.modelrd12heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd13heatkwh <- stepAIC(kwhoth.null.modelrd13heatkwh, scope = formula(kwhoth.full.modelrd13heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd14heatkwh <- stepAIC(kwhoth.null.modelrd14heatkwh, scope = formula(kwhoth.full.modelrd14heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd15heatkwh <- stepAIC(kwhoth.null.modelrd15heatkwh, scope = formula(kwhoth.full.modelrd15heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd16heatkwh <- stepAIC(kwhoth.null.modelrd16heatkwh, scope = formula(kwhoth.full.modelrd16heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd17heatkwh <- stepAIC(kwhoth.null.modelrd17heatkwh, scope = formula(kwhoth.full.modelrd17heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd18heatkwh <- stepAIC(kwhoth.null.modelrd18heatkwh, scope = formula(kwhoth.full.modelrd18heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd19heatkwh <- stepAIC(kwhoth.null.modelrd19heatkwh, scope = formula(kwhoth.full.modelrd19heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd20heatkwh <- stepAIC(kwhoth.null.modelrd20heatkwh, scope = formula(kwhoth.full.modelrd20heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd21heatkwh <- stepAIC(kwhoth.null.modelrd21heatkwh, scope = formula(kwhoth.full.modelrd21heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd22heatkwh <- stepAIC(kwhoth.null.modelrd22heatkwh, scope = formula(kwhoth.full.modelrd22heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd23heatkwh <- stepAIC(kwhoth.null.modelrd23heatkwh, scope = formula(kwhoth.full.modelrd23heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd24heatkwh <- stepAIC(kwhoth.null.modelrd24heatkwh, scope = formula(kwhoth.full.modelrd24heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd25heatkwh <- stepAIC(kwhoth.null.modelrd25heatkwh, scope = formula(kwhoth.full.modelrd25heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd26heatkwh <- stepAIC(kwhoth.null.modelrd26heatkwh, scope = formula(kwhoth.full.modelrd26heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd27heatkwh <- stepAIC(kwhoth.null.modelrd27heatkwh, scope = formula(kwhoth.full.modelrd27heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd1heatkwh <- stepAIC(kwhsph.null.modelrd1heatkwh, scope = formula(kwhsph.full.modelrd1heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd2heatkwh <- stepAIC(kwhsph.null.modelrd2heatkwh, scope = formula(kwhsph.full.modelrd2heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd3heatkwh <- stepAIC(kwhsph.null.modelrd3heatkwh, scope = formula(kwhsph.full.modelrd3heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd4heatkwh <- stepAIC(kwhsph.null.modelrd4heatkwh, scope = formula(kwhsph.full.modelrd4heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd5heatkwh <- stepAIC(kwhsph.null.modelrd5heatkwh, scope = formula(kwhsph.full.modelrd5heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd6heatkwh <- stepAIC(kwhsph.null.modelrd6heatkwh, scope = formula(kwhsph.full.modelrd6heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd7heatkwh <- stepAIC(kwhsph.null.modelrd7heatkwh, scope = formula(kwhsph.full.modelrd7heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd8heatkwh <- stepAIC(kwhsph.null.modelrd8heatkwh, scope = formula(kwhsph.full.modelrd8heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd9heatkwh <- stepAIC(kwhsph.null.modelrd9heatkwh, scope = formula(kwhsph.full.modelrd9heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd10heatkwh <- stepAIC(kwhsph.null.modelrd10heatkwh, scope = formula(kwhsph.full.modelrd10heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd11heatkwh <- stepAIC(kwhsph.null.modelrd11heatkwh, scope = formula(kwhsph.full.modelrd11heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd12heatkwh <- stepAIC(kwhsph.null.modelrd12heatkwh, scope = formula(kwhsph.full.modelrd12heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd13heatkwh <- stepAIC(kwhsph.null.modelrd13heatkwh, scope = formula(kwhsph.full.modelrd13heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd14heatkwh <- stepAIC(kwhsph.null.modelrd14heatkwh, scope = formula(kwhsph.full.modelrd14heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd15heatkwh <- stepAIC(kwhsph.null.modelrd15heatkwh, scope = formula(kwhsph.full.modelrd15heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd16heatkwh <- stepAIC(kwhsph.null.modelrd16heatkwh, scope = formula(kwhsph.full.modelrd16heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd17heatkwh <- stepAIC(kwhsph.null.modelrd17heatkwh, scope = formula(kwhsph.full.modelrd17heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd18heatkwh <- stepAIC(kwhsph.null.modelrd18heatkwh, scope = formula(kwhsph.full.modelrd18heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd19heatkwh <- stepAIC(kwhsph.null.modelrd19heatkwh, scope = formula(kwhsph.full.modelrd19heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd20heatkwh <- stepAIC(kwhsph.null.modelrd20heatkwh, scope = formula(kwhsph.full.modelrd20heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd21heatkwh <- stepAIC(kwhsph.null.modelrd21heatkwh, scope = formula(kwhsph.full.modelrd21heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd22heatkwh <- stepAIC(kwhsph.null.modelrd22heatkwh, scope = formula(kwhsph.full.modelrd22heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd23heatkwh <- stepAIC(kwhsph.null.modelrd23heatkwh, scope = formula(kwhsph.full.modelrd23heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd24heatkwh <- stepAIC(kwhsph.null.modelrd24heatkwh, scope = formula(kwhsph.full.modelrd24heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd25heatkwh <- stepAIC(kwhsph.null.modelrd25heatkwh, scope = formula(kwhsph.full.modelrd25heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd26heatkwh <- stepAIC(kwhsph.null.modelrd26heatkwh, scope = formula(kwhsph.full.modelrd26heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd27heatkwh <- stepAIC(kwhsph.null.modelrd27heatkwh, scope = formula(kwhsph.full.modelrd27heatkwh),  direction = "forward", weights = recs$NWEIGHT)
kwhwth.step.modelrd1heatkwh <- kwhwth.full.modelrd1heatkwh
kwhwth.step.modelrd2heatkwh <- kwhwth.full.modelrd2heatkwh
kwhwth.step.modelrd3heatkwh <- kwhwth.full.modelrd3heatkwh
kwhwth.step.modelrd4heatkwh <- kwhwth.full.modelrd4heatkwh
kwhwth.step.modelrd5heatkwh <- kwhwth.full.modelrd5heatkwh
kwhwth.step.modelrd6heatkwh <- kwhwth.full.modelrd6heatkwh
kwhwth.step.modelrd7heatkwh <- kwhwth.full.modelrd7heatkwh
kwhwth.step.modelrd8heatkwh <- kwhwth.full.modelrd8heatkwh
kwhwth.step.modelrd9heatkwh <- kwhwth.full.modelrd9heatkwh
kwhwth.step.modelrd10heatkwh <- kwhwth.full.modelrd10heatkwh
kwhwth.step.modelrd11heatkwh <- kwhwth.full.modelrd11heatkwh
kwhwth.step.modelrd12heatkwh <- kwhwth.full.modelrd12heatkwh
kwhwth.step.modelrd13heatkwh <- kwhwth.full.modelrd13heatkwh
kwhwth.step.modelrd14heatkwh <- kwhwth.full.modelrd14heatkwh
kwhwth.step.modelrd15heatkwh <- kwhwth.full.modelrd15heatkwh
kwhwth.step.modelrd16heatkwh <- kwhwth.full.modelrd16heatkwh
kwhwth.step.modelrd17heatkwh <- kwhwth.full.modelrd17heatkwh
kwhwth.step.modelrd18heatkwh <- kwhwth.full.modelrd18heatkwh
kwhwth.step.modelrd19heatkwh <- kwhwth.full.modelrd19heatkwh
kwhwth.step.modelrd20heatkwh <- kwhwth.full.modelrd20heatkwh
kwhwth.step.modelrd21heatkwh <- kwhwth.full.modelrd21heatkwh
kwhwth.step.modelrd22heatkwh <- kwhwth.full.modelrd22heatkwh
kwhwth.step.modelrd23heatkwh <- kwhwth.full.modelrd23heatkwh
kwhwth.step.modelrd24heatkwh <- kwhwth.full.modelrd24heatkwh
kwhwth.step.modelrd25heatkwh <- kwhwth.full.modelrd25heatkwh
kwhwth.step.modelrd26heatkwh <- kwhwth.full.modelrd26heatkwh
kwhwth.step.modelrd27heatkwh <- kwhwth.full.modelrd27heatkwh
btuother.step.modelrd1heatoth <- stepAIC(btuother.null.modelrd1heatoth, scope = formula(btuother.full.modelrd1heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd2heatoth <- stepAIC(btuother.null.modelrd2heatoth, scope = formula(btuother.full.modelrd2heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd3heatoth <- stepAIC(btuother.null.modelrd3heatoth, scope = formula(btuother.full.modelrd3heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd4heatoth <- stepAIC(btuother.null.modelrd4heatoth, scope = formula(btuother.full.modelrd4heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd5heatoth <- stepAIC(btuother.null.modelrd5heatoth, scope = formula(btuother.full.modelrd5heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd6heatoth <- stepAIC(btuother.null.modelrd6heatoth, scope = formula(btuother.full.modelrd6heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd7heatoth <- stepAIC(btuother.null.modelrd7heatoth, scope = formula(btuother.full.modelrd7heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd8heatoth <- stepAIC(btuother.null.modelrd8heatoth, scope = formula(btuother.full.modelrd8heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd9heatoth <- stepAIC(btuother.null.modelrd9heatoth, scope = formula(btuother.full.modelrd9heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd10heatoth <- stepAIC(btuother.null.modelrd10heatoth, scope = formula(btuother.full.modelrd10heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd11heatoth <- stepAIC(btuother.null.modelrd11heatoth, scope = formula(btuother.full.modelrd11heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd12heatoth <- stepAIC(btuother.null.modelrd12heatoth, scope = formula(btuother.full.modelrd12heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd13heatoth <- stepAIC(btuother.null.modelrd13heatoth, scope = formula(btuother.full.modelrd13heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd14heatoth <- stepAIC(btuother.null.modelrd14heatoth, scope = formula(btuother.full.modelrd14heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd15heatoth <- stepAIC(btuother.null.modelrd15heatoth, scope = formula(btuother.full.modelrd15heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd16heatoth <- stepAIC(btuother.null.modelrd16heatoth, scope = formula(btuother.full.modelrd16heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd17heatoth <- stepAIC(btuother.null.modelrd17heatoth, scope = formula(btuother.full.modelrd17heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd18heatoth <- stepAIC(btuother.null.modelrd18heatoth, scope = formula(btuother.full.modelrd18heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd19heatoth <- stepAIC(btuother.null.modelrd19heatoth, scope = formula(btuother.full.modelrd19heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd20heatoth <- stepAIC(btuother.null.modelrd20heatoth, scope = formula(btuother.full.modelrd20heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd21heatoth <- stepAIC(btuother.null.modelrd21heatoth, scope = formula(btuother.full.modelrd21heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd22heatoth <- stepAIC(btuother.null.modelrd22heatoth, scope = formula(btuother.full.modelrd22heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd23heatoth <- stepAIC(btuother.null.modelrd23heatoth, scope = formula(btuother.full.modelrd23heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd24heatoth <- stepAIC(btuother.null.modelrd24heatoth, scope = formula(btuother.full.modelrd24heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd25heatoth <- stepAIC(btuother.null.modelrd25heatoth, scope = formula(btuother.full.modelrd25heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd26heatoth <- stepAIC(btuother.null.modelrd26heatoth, scope = formula(btuother.full.modelrd26heatoth),  direction = "forward", weights = recs$NWEIGHT)
btuother.step.modelrd27heatoth <- stepAIC(btuother.null.modelrd27heatoth, scope = formula(btuother.full.modelrd27heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd1heatoth <- stepAIC(kwh.null.modelrd1heatoth, scope = formula(kwh.full.modelrd1heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd2heatoth <- stepAIC(kwh.null.modelrd2heatoth, scope = formula(kwh.full.modelrd2heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd3heatoth <- stepAIC(kwh.null.modelrd3heatoth, scope = formula(kwh.full.modelrd3heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd4heatoth <- stepAIC(kwh.null.modelrd4heatoth, scope = formula(kwh.full.modelrd4heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd5heatoth <- stepAIC(kwh.null.modelrd5heatoth, scope = formula(kwh.full.modelrd5heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd6heatoth <- stepAIC(kwh.null.modelrd6heatoth, scope = formula(kwh.full.modelrd6heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd7heatoth <- stepAIC(kwh.null.modelrd7heatoth, scope = formula(kwh.full.modelrd7heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd8heatoth <- stepAIC(kwh.null.modelrd8heatoth, scope = formula(kwh.full.modelrd8heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd9heatoth <- stepAIC(kwh.null.modelrd9heatoth, scope = formula(kwh.full.modelrd9heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd10heatoth <- stepAIC(kwh.null.modelrd10heatoth, scope = formula(kwh.full.modelrd10heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd11heatoth <- stepAIC(kwh.null.modelrd11heatoth, scope = formula(kwh.full.modelrd11heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd12heatoth <- stepAIC(kwh.null.modelrd12heatoth, scope = formula(kwh.full.modelrd12heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd13heatoth <- stepAIC(kwh.null.modelrd13heatoth, scope = formula(kwh.full.modelrd13heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd14heatoth <- stepAIC(kwh.null.modelrd14heatoth, scope = formula(kwh.full.modelrd14heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd15heatoth <- stepAIC(kwh.null.modelrd15heatoth, scope = formula(kwh.full.modelrd15heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd16heatoth <- stepAIC(kwh.null.modelrd16heatoth, scope = formula(kwh.full.modelrd16heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd17heatoth <- stepAIC(kwh.null.modelrd17heatoth, scope = formula(kwh.full.modelrd17heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd18heatoth <- stepAIC(kwh.null.modelrd18heatoth, scope = formula(kwh.full.modelrd18heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd19heatoth <- stepAIC(kwh.null.modelrd19heatoth, scope = formula(kwh.full.modelrd19heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd20heatoth <- stepAIC(kwh.null.modelrd20heatoth, scope = formula(kwh.full.modelrd20heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd21heatoth <- stepAIC(kwh.null.modelrd21heatoth, scope = formula(kwh.full.modelrd21heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd22heatoth <- stepAIC(kwh.null.modelrd22heatoth, scope = formula(kwh.full.modelrd22heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd23heatoth <- stepAIC(kwh.null.modelrd23heatoth, scope = formula(kwh.full.modelrd23heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd24heatoth <- stepAIC(kwh.null.modelrd24heatoth, scope = formula(kwh.full.modelrd24heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd25heatoth <- stepAIC(kwh.null.modelrd25heatoth, scope = formula(kwh.full.modelrd25heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd26heatoth <- stepAIC(kwh.null.modelrd26heatoth, scope = formula(kwh.full.modelrd26heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.modelrd27heatoth <- stepAIC(kwh.null.modelrd27heatoth, scope = formula(kwh.full.modelrd27heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd1heatoth <- stepAIC(kwhoth.null.modelrd1heatoth, scope = formula(kwhoth.full.modelrd1heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd2heatoth <- stepAIC(kwhoth.null.modelrd2heatoth, scope = formula(kwhoth.full.modelrd2heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd3heatoth <- stepAIC(kwhoth.null.modelrd3heatoth, scope = formula(kwhoth.full.modelrd3heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd4heatoth <- stepAIC(kwhoth.null.modelrd4heatoth, scope = formula(kwhoth.full.modelrd4heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd5heatoth <- stepAIC(kwhoth.null.modelrd5heatoth, scope = formula(kwhoth.full.modelrd5heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd6heatoth <- stepAIC(kwhoth.null.modelrd6heatoth, scope = formula(kwhoth.full.modelrd6heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd7heatoth <- stepAIC(kwhoth.null.modelrd7heatoth, scope = formula(kwhoth.full.modelrd7heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd8heatoth <- stepAIC(kwhoth.null.modelrd8heatoth, scope = formula(kwhoth.full.modelrd8heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd9heatoth <- stepAIC(kwhoth.null.modelrd9heatoth, scope = formula(kwhoth.full.modelrd9heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd10heatoth <- stepAIC(kwhoth.null.modelrd10heatoth, scope = formula(kwhoth.full.modelrd10heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd11heatoth <- stepAIC(kwhoth.null.modelrd11heatoth, scope = formula(kwhoth.full.modelrd11heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd12heatoth <- stepAIC(kwhoth.null.modelrd12heatoth, scope = formula(kwhoth.full.modelrd12heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd13heatoth <- stepAIC(kwhoth.null.modelrd13heatoth, scope = formula(kwhoth.full.modelrd13heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd14heatoth <- stepAIC(kwhoth.null.modelrd14heatoth, scope = formula(kwhoth.full.modelrd14heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd15heatoth <- stepAIC(kwhoth.null.modelrd15heatoth, scope = formula(kwhoth.full.modelrd15heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd16heatoth <- stepAIC(kwhoth.null.modelrd16heatoth, scope = formula(kwhoth.full.modelrd16heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd17heatoth <- stepAIC(kwhoth.null.modelrd17heatoth, scope = formula(kwhoth.full.modelrd17heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd18heatoth <- stepAIC(kwhoth.null.modelrd18heatoth, scope = formula(kwhoth.full.modelrd18heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd19heatoth <- stepAIC(kwhoth.null.modelrd19heatoth, scope = formula(kwhoth.full.modelrd19heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd20heatoth <- stepAIC(kwhoth.null.modelrd20heatoth, scope = formula(kwhoth.full.modelrd20heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd21heatoth <- stepAIC(kwhoth.null.modelrd21heatoth, scope = formula(kwhoth.full.modelrd21heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd22heatoth <- stepAIC(kwhoth.null.modelrd22heatoth, scope = formula(kwhoth.full.modelrd22heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd23heatoth <- stepAIC(kwhoth.null.modelrd23heatoth, scope = formula(kwhoth.full.modelrd23heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd24heatoth <- stepAIC(kwhoth.null.modelrd24heatoth, scope = formula(kwhoth.full.modelrd24heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd25heatoth <- stepAIC(kwhoth.null.modelrd25heatoth, scope = formula(kwhoth.full.modelrd25heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd26heatoth <- stepAIC(kwhoth.null.modelrd26heatoth, scope = formula(kwhoth.full.modelrd26heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhoth.step.modelrd27heatoth <- stepAIC(kwhoth.null.modelrd27heatoth, scope = formula(kwhoth.full.modelrd27heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd1heatoth <- stepAIC(kwhsph.null.modelrd1heatoth, scope = formula(kwhsph.full.modelrd1heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd2heatoth <- stepAIC(kwhsph.null.modelrd2heatoth, scope = formula(kwhsph.full.modelrd2heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd3heatoth <- stepAIC(kwhsph.null.modelrd3heatoth, scope = formula(kwhsph.full.modelrd3heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd4heatoth <- stepAIC(kwhsph.null.modelrd4heatoth, scope = formula(kwhsph.full.modelrd4heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd5heatoth <- stepAIC(kwhsph.null.modelrd5heatoth, scope = formula(kwhsph.full.modelrd5heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd6heatoth <- stepAIC(kwhsph.null.modelrd6heatoth, scope = formula(kwhsph.full.modelrd6heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd7heatoth <- stepAIC(kwhsph.null.modelrd7heatoth, scope = formula(kwhsph.full.modelrd7heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd8heatoth <- stepAIC(kwhsph.null.modelrd8heatoth, scope = formula(kwhsph.full.modelrd8heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd9heatoth <- stepAIC(kwhsph.null.modelrd9heatoth, scope = formula(kwhsph.full.modelrd9heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd10heatoth <- stepAIC(kwhsph.null.modelrd10heatoth, scope = formula(kwhsph.full.modelrd10heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd11heatoth <- stepAIC(kwhsph.null.modelrd11heatoth, scope = formula(kwhsph.full.modelrd11heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd12heatoth <- stepAIC(kwhsph.null.modelrd12heatoth, scope = formula(kwhsph.full.modelrd12heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd13heatoth <- stepAIC(kwhsph.null.modelrd13heatoth, scope = formula(kwhsph.full.modelrd13heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd14heatoth <- stepAIC(kwhsph.null.modelrd14heatoth, scope = formula(kwhsph.full.modelrd14heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd15heatoth <- stepAIC(kwhsph.null.modelrd15heatoth, scope = formula(kwhsph.full.modelrd15heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd16heatoth <- stepAIC(kwhsph.null.modelrd16heatoth, scope = formula(kwhsph.full.modelrd16heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd17heatoth <- stepAIC(kwhsph.null.modelrd17heatoth, scope = formula(kwhsph.full.modelrd17heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd18heatoth <- stepAIC(kwhsph.null.modelrd18heatoth, scope = formula(kwhsph.full.modelrd18heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd19heatoth <- stepAIC(kwhsph.null.modelrd19heatoth, scope = formula(kwhsph.full.modelrd19heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd20heatoth <- stepAIC(kwhsph.null.modelrd20heatoth, scope = formula(kwhsph.full.modelrd20heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd21heatoth <- stepAIC(kwhsph.null.modelrd21heatoth, scope = formula(kwhsph.full.modelrd21heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd22heatoth <- stepAIC(kwhsph.null.modelrd22heatoth, scope = formula(kwhsph.full.modelrd22heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd23heatoth <- stepAIC(kwhsph.null.modelrd23heatoth, scope = formula(kwhsph.full.modelrd23heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd24heatoth <- stepAIC(kwhsph.null.modelrd24heatoth, scope = formula(kwhsph.full.modelrd24heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd25heatoth <- stepAIC(kwhsph.null.modelrd25heatoth, scope = formula(kwhsph.full.modelrd25heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd26heatoth <- stepAIC(kwhsph.null.modelrd26heatoth, scope = formula(kwhsph.full.modelrd26heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhsph.step.modelrd27heatoth <- stepAIC(kwhsph.null.modelrd27heatoth, scope = formula(kwhsph.full.modelrd27heatoth),  direction = "forward", weights = recs$NWEIGHT)
kwhwth.step.modelrd1heatoth <- kwhwth.full.modelrd1heatoth
kwhwth.step.modelrd2heatoth <- kwhwth.full.modelrd2heatoth
kwhwth.step.modelrd3heatoth <- kwhwth.full.modelrd3heatoth
kwhwth.step.modelrd4heatoth <- kwhwth.full.modelrd4heatoth
kwhwth.step.modelrd5heatoth <- kwhwth.full.modelrd5heatoth
kwhwth.step.modelrd6heatoth <- kwhwth.full.modelrd6heatoth
kwhwth.step.modelrd7heatoth <- kwhwth.full.modelrd7heatoth
kwhwth.step.modelrd8heatoth <- kwhwth.full.modelrd8heatoth
kwhwth.step.modelrd9heatoth <- kwhwth.full.modelrd9heatoth
kwhwth.step.modelrd10heatoth <- kwhwth.full.modelrd10heatoth
kwhwth.step.modelrd11heatoth <- kwhwth.full.modelrd11heatoth
kwhwth.step.modelrd12heatoth <- kwhwth.full.modelrd12heatoth
kwhwth.step.modelrd13heatoth <- kwhwth.full.modelrd13heatoth
kwhwth.step.modelrd14heatoth <- kwhwth.full.modelrd14heatoth
kwhwth.step.modelrd15heatoth <- kwhwth.full.modelrd15heatoth
kwhwth.step.modelrd16heatoth <- kwhwth.full.modelrd16heatoth
kwhwth.step.modelrd17heatoth <- kwhwth.full.modelrd17heatoth
kwhwth.step.modelrd18heatoth <- kwhwth.full.modelrd18heatoth
kwhwth.step.modelrd19heatoth <- kwhwth.full.modelrd19heatoth
kwhwth.step.modelrd20heatoth <- kwhwth.full.modelrd20heatoth
kwhwth.step.modelrd21heatoth <- kwhwth.full.modelrd21heatoth
kwhwth.step.modelrd22heatoth <- kwhwth.full.modelrd22heatoth
kwhwth.step.modelrd23heatoth <- kwhwth.full.modelrd23heatoth
kwhwth.step.modelrd24heatoth <- kwhwth.full.modelrd24heatoth
kwhwth.step.modelrd25heatoth <- kwhwth.full.modelrd25heatoth
kwhwth.step.modelrd26heatoth <- kwhwth.full.modelrd26heatoth
kwhwth.step.modelrd27heatoth <- kwhwth.full.modelrd27heatoth


### Joining Data Frames


## Best solution from: https://adairama.wordpress.com/2017/11/22/how-to-merge-multiple-datasets-in-r-based-on-row-names/


### Writing the merge code for all the step models

## First, create simple dataframes of all step models with just betas
var1 = data.frame("cufeetng.rd1heatgas" = summary(cufeetng.step.modelrd1heatgas)$coefficients[, 1])
var2 = data.frame("cufeetng.rd2heatgas" = summary(cufeetng.step.modelrd2heatgas)$coefficients[, 1])
var3 = data.frame("cufeetng.rd3heatgas" = summary(cufeetng.step.modelrd3heatgas)$coefficients[, 1])
var4 = data.frame("cufeetng.rd4heatgas" = summary(cufeetng.step.modelrd4heatgas)$coefficients[, 1])
var5 = data.frame("cufeetng.rd5heatgas" = summary(cufeetng.step.modelrd5heatgas)$coefficients[, 1])
var6 = data.frame("cufeetng.rd6heatgas" = summary(cufeetng.step.modelrd6heatgas)$coefficients[, 1])
var7 = data.frame("cufeetng.rd7heatgas" = summary(cufeetng.step.modelrd7heatgas)$coefficients[, 1])
var8 = data.frame("cufeetng.rd8heatgas" = summary(cufeetng.step.modelrd8heatgas)$coefficients[, 1])
var9 = data.frame("cufeetng.rd9heatgas" = summary(cufeetng.step.modelrd9heatgas)$coefficients[, 1])
var10 = data.frame("cufeetng.rd10heatgas" = summary(cufeetng.step.modelrd10heatgas)$coefficients[, 1])
var11 = data.frame("cufeetng.rd11heatgas" = summary(cufeetng.step.modelrd11heatgas)$coefficients[, 1])
var12 = data.frame("cufeetng.rd12heatgas" = summary(cufeetng.step.modelrd12heatgas)$coefficients[, 1])
var13 = data.frame("cufeetng.rd13heatgas" = summary(cufeetng.step.modelrd13heatgas)$coefficients[, 1])
var14 = data.frame("cufeetng.rd14heatgas" = summary(cufeetng.step.modelrd14heatgas)$coefficients[, 1])
var15 = data.frame("cufeetng.rd15heatgas" = summary(cufeetng.step.modelrd15heatgas)$coefficients[, 1])
var16 = data.frame("cufeetng.rd16heatgas" = summary(cufeetng.step.modelrd16heatgas)$coefficients[, 1])
var17 = data.frame("cufeetng.rd17heatgas" = summary(cufeetng.step.modelrd17heatgas)$coefficients[, 1])
var18 = data.frame("cufeetng.rd18heatgas" = summary(cufeetng.step.modelrd18heatgas)$coefficients[, 1])
var19 = data.frame("cufeetng.rd19heatgas" = summary(cufeetng.step.modelrd19heatgas)$coefficients[, 1])
var20 = data.frame("cufeetng.rd20heatgas" = summary(cufeetng.step.modelrd20heatgas)$coefficients[, 1])
var21 = data.frame("cufeetng.rd21heatgas" = summary(cufeetng.step.modelrd21heatgas)$coefficients[, 1])
var22 = data.frame("cufeetng.rd22heatgas" = summary(cufeetng.step.modelrd22heatgas)$coefficients[, 1])
var23 = data.frame("cufeetng.rd23heatgas" = summary(cufeetng.step.modelrd23heatgas)$coefficients[, 1])
var24 = data.frame("cufeetng.rd24heatgas" = summary(cufeetng.step.modelrd24heatgas)$coefficients[, 1])
var25 = data.frame("cufeetng.rd25heatgas" = summary(cufeetng.step.modelrd25heatgas)$coefficients[, 1])
var26 = data.frame("cufeetng.rd26heatgas" = summary(cufeetng.step.modelrd26heatgas)$coefficients[, 1])
var27 = data.frame("cufeetng.rd27heatgas" = summary(cufeetng.step.modelrd27heatgas)$coefficients[, 1])
var28 = data.frame("cufeetngsph.rd1heatgas" = summary(cufeetngsph.step.modelrd1heatgas)$coefficients[, 1])
var29 = data.frame("cufeetngsph.rd2heatgas" = summary(cufeetngsph.step.modelrd2heatgas)$coefficients[, 1])
var30 = data.frame("cufeetngsph.rd3heatgas" = summary(cufeetngsph.step.modelrd3heatgas)$coefficients[, 1])
var31 = data.frame("cufeetngsph.rd4heatgas" = summary(cufeetngsph.step.modelrd4heatgas)$coefficients[, 1])
var32 = data.frame("cufeetngsph.rd5heatgas" = summary(cufeetngsph.step.modelrd5heatgas)$coefficients[, 1])
var33 = data.frame("cufeetngsph.rd6heatgas" = summary(cufeetngsph.step.modelrd6heatgas)$coefficients[, 1])
var34 = data.frame("cufeetngsph.rd7heatgas" = summary(cufeetngsph.step.modelrd7heatgas)$coefficients[, 1])
var35 = data.frame("cufeetngsph.rd8heatgas" = summary(cufeetngsph.step.modelrd8heatgas)$coefficients[, 1])
var36 = data.frame("cufeetngsph.rd9heatgas" = summary(cufeetngsph.step.modelrd9heatgas)$coefficients[, 1])
var37 = data.frame("cufeetngsph.rd10heatgas" = summary(cufeetngsph.step.modelrd10heatgas)$coefficients[, 1])
var38 = data.frame("cufeetngsph.rd11heatgas" = summary(cufeetngsph.step.modelrd11heatgas)$coefficients[, 1])
var39 = data.frame("cufeetngsph.rd12heatgas" = summary(cufeetngsph.step.modelrd12heatgas)$coefficients[, 1])
var40 = data.frame("cufeetngsph.rd13heatgas" = summary(cufeetngsph.step.modelrd13heatgas)$coefficients[, 1])
var41 = data.frame("cufeetngsph.rd14heatgas" = summary(cufeetngsph.step.modelrd14heatgas)$coefficients[, 1])
var42 = data.frame("cufeetngsph.rd15heatgas" = summary(cufeetngsph.step.modelrd15heatgas)$coefficients[, 1])
var43 = data.frame("cufeetngsph.rd16heatgas" = summary(cufeetngsph.step.modelrd16heatgas)$coefficients[, 1])
var44 = data.frame("cufeetngsph.rd17heatgas" = summary(cufeetngsph.step.modelrd17heatgas)$coefficients[, 1])
var45 = data.frame("cufeetngsph.rd18heatgas" = summary(cufeetngsph.step.modelrd18heatgas)$coefficients[, 1])
var46 = data.frame("cufeetngsph.rd19heatgas" = summary(cufeetngsph.step.modelrd19heatgas)$coefficients[, 1])
var47 = data.frame("cufeetngsph.rd20heatgas" = summary(cufeetngsph.step.modelrd20heatgas)$coefficients[, 1])
var48 = data.frame("cufeetngsph.rd21heatgas" = summary(cufeetngsph.step.modelrd21heatgas)$coefficients[, 1])
var49 = data.frame("cufeetngsph.rd22heatgas" = summary(cufeetngsph.step.modelrd22heatgas)$coefficients[, 1])
var50 = data.frame("cufeetngsph.rd23heatgas" = summary(cufeetngsph.step.modelrd23heatgas)$coefficients[, 1])
var51 = data.frame("cufeetngsph.rd24heatgas" = summary(cufeetngsph.step.modelrd24heatgas)$coefficients[, 1])
var52 = data.frame("cufeetngsph.rd25heatgas" = summary(cufeetngsph.step.modelrd25heatgas)$coefficients[, 1])
var53 = data.frame("cufeetngsph.rd26heatgas" = summary(cufeetngsph.step.modelrd26heatgas)$coefficients[, 1])
var54 = data.frame("cufeetngsph.rd27heatgas" = summary(cufeetngsph.step.modelrd27heatgas)$coefficients[, 1])
var55 = data.frame("cufeetngwth.rd1heatgas" = summary(cufeetngwth.step.modelrd1heatgas)$coefficients[, 1])
var56 = data.frame("cufeetngwth.rd2heatgas" = summary(cufeetngwth.step.modelrd2heatgas)$coefficients[, 1])
var57 = data.frame("cufeetngwth.rd3heatgas" = summary(cufeetngwth.step.modelrd3heatgas)$coefficients[, 1])
var58 = data.frame("cufeetngwth.rd4heatgas" = summary(cufeetngwth.step.modelrd4heatgas)$coefficients[, 1])
var59 = data.frame("cufeetngwth.rd5heatgas" = summary(cufeetngwth.step.modelrd5heatgas)$coefficients[, 1])
var60 = data.frame("cufeetngwth.rd6heatgas" = summary(cufeetngwth.step.modelrd6heatgas)$coefficients[, 1])
var61 = data.frame("cufeetngwth.rd7heatgas" = summary(cufeetngwth.step.modelrd7heatgas)$coefficients[, 1])
var62 = data.frame("cufeetngwth.rd8heatgas" = summary(cufeetngwth.step.modelrd8heatgas)$coefficients[, 1])
var63 = data.frame("cufeetngwth.rd9heatgas" = summary(cufeetngwth.step.modelrd9heatgas)$coefficients[, 1])
var64 = data.frame("cufeetngwth.rd10heatgas" = summary(cufeetngwth.step.modelrd10heatgas)$coefficients[, 1])
var65 = data.frame("cufeetngwth.rd11heatgas" = summary(cufeetngwth.step.modelrd11heatgas)$coefficients[, 1])
var66 = data.frame("cufeetngwth.rd12heatgas" = summary(cufeetngwth.step.modelrd12heatgas)$coefficients[, 1])
var67 = data.frame("cufeetngwth.rd13heatgas" = summary(cufeetngwth.step.modelrd13heatgas)$coefficients[, 1])
var68 = data.frame("cufeetngwth.rd14heatgas" = summary(cufeetngwth.step.modelrd14heatgas)$coefficients[, 1])
var69 = data.frame("cufeetngwth.rd15heatgas" = summary(cufeetngwth.step.modelrd15heatgas)$coefficients[, 1])
var70 = data.frame("cufeetngwth.rd16heatgas" = summary(cufeetngwth.step.modelrd16heatgas)$coefficients[, 1])
var71 = data.frame("cufeetngwth.rd17heatgas" = summary(cufeetngwth.step.modelrd17heatgas)$coefficients[, 1])
var72 = data.frame("cufeetngwth.rd18heatgas" = summary(cufeetngwth.step.modelrd18heatgas)$coefficients[, 1])
var73 = data.frame("cufeetngwth.rd19heatgas" = summary(cufeetngwth.step.modelrd19heatgas)$coefficients[, 1])
var74 = data.frame("cufeetngwth.rd20heatgas" = summary(cufeetngwth.step.modelrd20heatgas)$coefficients[, 1])
var75 = data.frame("cufeetngwth.rd21heatgas" = summary(cufeetngwth.step.modelrd21heatgas)$coefficients[, 1])
var76 = data.frame("cufeetngwth.rd22heatgas" = summary(cufeetngwth.step.modelrd22heatgas)$coefficients[, 1])
var77 = data.frame("cufeetngwth.rd23heatgas" = summary(cufeetngwth.step.modelrd23heatgas)$coefficients[, 1])
var78 = data.frame("cufeetngwth.rd24heatgas" = summary(cufeetngwth.step.modelrd24heatgas)$coefficients[, 1])
var79 = data.frame("cufeetngwth.rd25heatgas" = summary(cufeetngwth.step.modelrd25heatgas)$coefficients[, 1])
var80 = data.frame("cufeetngwth.rd26heatgas" = summary(cufeetngwth.step.modelrd26heatgas)$coefficients[, 1])
var81 = data.frame("cufeetngwth.rd27heatgas" = summary(cufeetngwth.step.modelrd27heatgas)$coefficients[, 1])
var82 = data.frame("kwh.rd1heatgas" = summary(kwh.step.modelrd1heatgas)$coefficients[, 1])
var83 = data.frame("kwh.rd2heatgas" = summary(kwh.step.modelrd2heatgas)$coefficients[, 1])
var84 = data.frame("kwh.rd3heatgas" = summary(kwh.step.modelrd3heatgas)$coefficients[, 1])
var85 = data.frame("kwh.rd4heatgas" = summary(kwh.step.modelrd4heatgas)$coefficients[, 1])
var86 = data.frame("kwh.rd5heatgas" = summary(kwh.step.modelrd5heatgas)$coefficients[, 1])
var87 = data.frame("kwh.rd6heatgas" = summary(kwh.step.modelrd6heatgas)$coefficients[, 1])
var88 = data.frame("kwh.rd7heatgas" = summary(kwh.step.modelrd7heatgas)$coefficients[, 1])
var89 = data.frame("kwh.rd8heatgas" = summary(kwh.step.modelrd8heatgas)$coefficients[, 1])
var90 = data.frame("kwh.rd9heatgas" = summary(kwh.step.modelrd9heatgas)$coefficients[, 1])
var91 = data.frame("kwh.rd10heatgas" = summary(kwh.step.modelrd10heatgas)$coefficients[, 1])
var92 = data.frame("kwh.rd11heatgas" = summary(kwh.step.modelrd11heatgas)$coefficients[, 1])
var93 = data.frame("kwh.rd12heatgas" = summary(kwh.step.modelrd12heatgas)$coefficients[, 1])
var94 = data.frame("kwh.rd13heatgas" = summary(kwh.step.modelrd13heatgas)$coefficients[, 1])
var95 = data.frame("kwh.rd14heatgas" = summary(kwh.step.modelrd14heatgas)$coefficients[, 1])
var96 = data.frame("kwh.rd15heatgas" = summary(kwh.step.modelrd15heatgas)$coefficients[, 1])
var97 = data.frame("kwh.rd16heatgas" = summary(kwh.step.modelrd16heatgas)$coefficients[, 1])
var98 = data.frame("kwh.rd17heatgas" = summary(kwh.step.modelrd17heatgas)$coefficients[, 1])
var99 = data.frame("kwh.rd18heatgas" = summary(kwh.step.modelrd18heatgas)$coefficients[, 1])
var100 = data.frame("kwh.rd19heatgas" = summary(kwh.step.modelrd19heatgas)$coefficients[, 1])
var101 = data.frame("kwh.rd20heatgas" = summary(kwh.step.modelrd20heatgas)$coefficients[, 1])
var102 = data.frame("kwh.rd21heatgas" = summary(kwh.step.modelrd21heatgas)$coefficients[, 1])
var103 = data.frame("kwh.rd22heatgas" = summary(kwh.step.modelrd22heatgas)$coefficients[, 1])
var104 = data.frame("kwh.rd23heatgas" = summary(kwh.step.modelrd23heatgas)$coefficients[, 1])
var105 = data.frame("kwh.rd24heatgas" = summary(kwh.step.modelrd24heatgas)$coefficients[, 1])
var106 = data.frame("kwh.rd25heatgas" = summary(kwh.step.modelrd25heatgas)$coefficients[, 1])
var107 = data.frame("kwh.rd26heatgas" = summary(kwh.step.modelrd26heatgas)$coefficients[, 1])
var108 = data.frame("kwh.rd27heatgas" = summary(kwh.step.modelrd27heatgas)$coefficients[, 1])
var109 = data.frame("kwhoth.rd1heatgas" = summary(kwhoth.step.modelrd1heatgas)$coefficients[, 1])
var110 = data.frame("kwhoth.rd2heatgas" = summary(kwhoth.step.modelrd2heatgas)$coefficients[, 1])
var111 = data.frame("kwhoth.rd3heatgas" = summary(kwhoth.step.modelrd3heatgas)$coefficients[, 1])
var112 = data.frame("kwhoth.rd4heatgas" = summary(kwhoth.step.modelrd4heatgas)$coefficients[, 1])
var113 = data.frame("kwhoth.rd5heatgas" = summary(kwhoth.step.modelrd5heatgas)$coefficients[, 1])
var114 = data.frame("kwhoth.rd6heatgas" = summary(kwhoth.step.modelrd6heatgas)$coefficients[, 1])
var115 = data.frame("kwhoth.rd7heatgas" = summary(kwhoth.step.modelrd7heatgas)$coefficients[, 1])
var116 = data.frame("kwhoth.rd8heatgas" = summary(kwhoth.step.modelrd8heatgas)$coefficients[, 1])
var117 = data.frame("kwhoth.rd9heatgas" = summary(kwhoth.step.modelrd9heatgas)$coefficients[, 1])
var118 = data.frame("kwhoth.rd10heatgas" = summary(kwhoth.step.modelrd10heatgas)$coefficients[, 1])
var119 = data.frame("kwhoth.rd11heatgas" = summary(kwhoth.step.modelrd11heatgas)$coefficients[, 1])
var120 = data.frame("kwhoth.rd12heatgas" = summary(kwhoth.step.modelrd12heatgas)$coefficients[, 1])
var121 = data.frame("kwhoth.rd13heatgas" = summary(kwhoth.step.modelrd13heatgas)$coefficients[, 1])
var122 = data.frame("kwhoth.rd14heatgas" = summary(kwhoth.step.modelrd14heatgas)$coefficients[, 1])
var123 = data.frame("kwhoth.rd15heatgas" = summary(kwhoth.step.modelrd15heatgas)$coefficients[, 1])
var124 = data.frame("kwhoth.rd16heatgas" = summary(kwhoth.step.modelrd16heatgas)$coefficients[, 1])
var125 = data.frame("kwhoth.rd17heatgas" = summary(kwhoth.step.modelrd17heatgas)$coefficients[, 1])
var126 = data.frame("kwhoth.rd18heatgas" = summary(kwhoth.step.modelrd18heatgas)$coefficients[, 1])
var127 = data.frame("kwhoth.rd19heatgas" = summary(kwhoth.step.modelrd19heatgas)$coefficients[, 1])
var128 = data.frame("kwhoth.rd20heatgas" = summary(kwhoth.step.modelrd20heatgas)$coefficients[, 1])
var129 = data.frame("kwhoth.rd21heatgas" = summary(kwhoth.step.modelrd21heatgas)$coefficients[, 1])
var130 = data.frame("kwhoth.rd22heatgas" = summary(kwhoth.step.modelrd22heatgas)$coefficients[, 1])
var131 = data.frame("kwhoth.rd23heatgas" = summary(kwhoth.step.modelrd23heatgas)$coefficients[, 1])
var132 = data.frame("kwhoth.rd24heatgas" = summary(kwhoth.step.modelrd24heatgas)$coefficients[, 1])
var133 = data.frame("kwhoth.rd25heatgas" = summary(kwhoth.step.modelrd25heatgas)$coefficients[, 1])
var134 = data.frame("kwhoth.rd26heatgas" = summary(kwhoth.step.modelrd26heatgas)$coefficients[, 1])
var135 = data.frame("kwhoth.rd27heatgas" = summary(kwhoth.step.modelrd27heatgas)$coefficients[, 1])
var136 = data.frame("kwhsph.rd1heatgas" = summary(kwhsph.step.modelrd1heatgas)$coefficients[, 1])
var137 = data.frame("kwhsph.rd2heatgas" = summary(kwhsph.step.modelrd2heatgas)$coefficients[, 1])
var138 = data.frame("kwhsph.rd3heatgas" = summary(kwhsph.step.modelrd3heatgas)$coefficients[, 1])
var139 = data.frame("kwhsph.rd4heatgas" = summary(kwhsph.step.modelrd4heatgas)$coefficients[, 1])
var140 = data.frame("kwhsph.rd5heatgas" = summary(kwhsph.step.modelrd5heatgas)$coefficients[, 1])
var141 = data.frame("kwhsph.rd6heatgas" = summary(kwhsph.step.modelrd6heatgas)$coefficients[, 1])
var142 = data.frame("kwhsph.rd7heatgas" = summary(kwhsph.step.modelrd7heatgas)$coefficients[, 1])
var143 = data.frame("kwhsph.rd8heatgas" = summary(kwhsph.step.modelrd8heatgas)$coefficients[, 1])
var144 = data.frame("kwhsph.rd9heatgas" = summary(kwhsph.step.modelrd9heatgas)$coefficients[, 1])
var145 = data.frame("kwhsph.rd10heatgas" = summary(kwhsph.step.modelrd10heatgas)$coefficients[, 1])
var146 = data.frame("kwhsph.rd11heatgas" = summary(kwhsph.step.modelrd11heatgas)$coefficients[, 1])
var147 = data.frame("kwhsph.rd12heatgas" = summary(kwhsph.step.modelrd12heatgas)$coefficients[, 1])
var148 = data.frame("kwhsph.rd13heatgas" = summary(kwhsph.step.modelrd13heatgas)$coefficients[, 1])
var149 = data.frame("kwhsph.rd14heatgas" = summary(kwhsph.step.modelrd14heatgas)$coefficients[, 1])
var150 = data.frame("kwhsph.rd15heatgas" = summary(kwhsph.step.modelrd15heatgas)$coefficients[, 1])
var151 = data.frame("kwhsph.rd16heatgas" = summary(kwhsph.step.modelrd16heatgas)$coefficients[, 1])
var152 = data.frame("kwhsph.rd17heatgas" = summary(kwhsph.step.modelrd17heatgas)$coefficients[, 1])
var153 = data.frame("kwhsph.rd18heatgas" = summary(kwhsph.step.modelrd18heatgas)$coefficients[, 1])
var154 = data.frame("kwhsph.rd19heatgas" = summary(kwhsph.step.modelrd19heatgas)$coefficients[, 1])
var155 = data.frame("kwhsph.rd20heatgas" = summary(kwhsph.step.modelrd20heatgas)$coefficients[, 1])
var156 = data.frame("kwhsph.rd21heatgas" = summary(kwhsph.step.modelrd21heatgas)$coefficients[, 1])
var157 = data.frame("kwhsph.rd22heatgas" = summary(kwhsph.step.modelrd22heatgas)$coefficients[, 1])
var158 = data.frame("kwhsph.rd23heatgas" = summary(kwhsph.step.modelrd23heatgas)$coefficients[, 1])
var159 = data.frame("kwhsph.rd24heatgas" = summary(kwhsph.step.modelrd24heatgas)$coefficients[, 1])
var160 = data.frame("kwhsph.rd25heatgas" = summary(kwhsph.step.modelrd25heatgas)$coefficients[, 1])
var161 = data.frame("kwhsph.rd26heatgas" = summary(kwhsph.step.modelrd26heatgas)$coefficients[, 1])
var162 = data.frame("kwhsph.rd27heatgas" = summary(kwhsph.step.modelrd27heatgas)$coefficients[, 1])
var163 = data.frame("kwh.rd1heatkwh" = summary(kwh.step.modelrd1heatkwh)$coefficients[, 1])
var164 = data.frame("kwh.rd2heatkwh" = summary(kwh.step.modelrd2heatkwh)$coefficients[, 1])
var165 = data.frame("kwh.rd3heatkwh" = summary(kwh.step.modelrd3heatkwh)$coefficients[, 1])
var166 = data.frame("kwh.rd4heatkwh" = summary(kwh.step.modelrd4heatkwh)$coefficients[, 1])
var167 = data.frame("kwh.rd5heatkwh" = summary(kwh.step.modelrd5heatkwh)$coefficients[, 1])
var168 = data.frame("kwh.rd6heatkwh" = summary(kwh.step.modelrd6heatkwh)$coefficients[, 1])
var169 = data.frame("kwh.rd7heatkwh" = summary(kwh.step.modelrd7heatkwh)$coefficients[, 1])
var170 = data.frame("kwh.rd8heatkwh" = summary(kwh.step.modelrd8heatkwh)$coefficients[, 1])
var171 = data.frame("kwh.rd9heatkwh" = summary(kwh.step.modelrd9heatkwh)$coefficients[, 1])
var172 = data.frame("kwh.rd10heatkwh" = summary(kwh.step.modelrd10heatkwh)$coefficients[, 1])
var173 = data.frame("kwh.rd11heatkwh" = summary(kwh.step.modelrd11heatkwh)$coefficients[, 1])
var174 = data.frame("kwh.rd12heatkwh" = summary(kwh.step.modelrd12heatkwh)$coefficients[, 1])
var175 = data.frame("kwh.rd13heatkwh" = summary(kwh.step.modelrd13heatkwh)$coefficients[, 1])
var176 = data.frame("kwh.rd14heatkwh" = summary(kwh.step.modelrd14heatkwh)$coefficients[, 1])
var177 = data.frame("kwh.rd15heatkwh" = summary(kwh.step.modelrd15heatkwh)$coefficients[, 1])
var178 = data.frame("kwh.rd16heatkwh" = summary(kwh.step.modelrd16heatkwh)$coefficients[, 1])
var179 = data.frame("kwh.rd17heatkwh" = summary(kwh.step.modelrd17heatkwh)$coefficients[, 1])
var180 = data.frame("kwh.rd18heatkwh" = summary(kwh.step.modelrd18heatkwh)$coefficients[, 1])
var181 = data.frame("kwh.rd19heatkwh" = summary(kwh.step.modelrd19heatkwh)$coefficients[, 1])
var182 = data.frame("kwh.rd20heatkwh" = summary(kwh.step.modelrd20heatkwh)$coefficients[, 1])
var183 = data.frame("kwh.rd21heatkwh" = summary(kwh.step.modelrd21heatkwh)$coefficients[, 1])
var184 = data.frame("kwh.rd22heatkwh" = summary(kwh.step.modelrd22heatkwh)$coefficients[, 1])
var185 = data.frame("kwh.rd23heatkwh" = summary(kwh.step.modelrd23heatkwh)$coefficients[, 1])
var186 = data.frame("kwh.rd24heatkwh" = summary(kwh.step.modelrd24heatkwh)$coefficients[, 1])
var187 = data.frame("kwh.rd25heatkwh" = summary(kwh.step.modelrd25heatkwh)$coefficients[, 1])
var188 = data.frame("kwh.rd26heatkwh" = summary(kwh.step.modelrd26heatkwh)$coefficients[, 1])
var189 = data.frame("kwh.rd27heatkwh" = summary(kwh.step.modelrd27heatkwh)$coefficients[, 1])
var190 = data.frame("kwhoth.rd1heatkwh" = summary(kwhoth.step.modelrd1heatkwh)$coefficients[, 1])
var191 = data.frame("kwhoth.rd2heatkwh" = summary(kwhoth.step.modelrd2heatkwh)$coefficients[, 1])
var192 = data.frame("kwhoth.rd3heatkwh" = summary(kwhoth.step.modelrd3heatkwh)$coefficients[, 1])
var193 = data.frame("kwhoth.rd4heatkwh" = summary(kwhoth.step.modelrd4heatkwh)$coefficients[, 1])
var194 = data.frame("kwhoth.rd5heatkwh" = summary(kwhoth.step.modelrd5heatkwh)$coefficients[, 1])
var195 = data.frame("kwhoth.rd6heatkwh" = summary(kwhoth.step.modelrd6heatkwh)$coefficients[, 1])
var196 = data.frame("kwhoth.rd7heatkwh" = summary(kwhoth.step.modelrd7heatkwh)$coefficients[, 1])
var197 = data.frame("kwhoth.rd8heatkwh" = summary(kwhoth.step.modelrd8heatkwh)$coefficients[, 1])
var198 = data.frame("kwhoth.rd9heatkwh" = summary(kwhoth.step.modelrd9heatkwh)$coefficients[, 1])
var199 = data.frame("kwhoth.rd10heatkwh" = summary(kwhoth.step.modelrd10heatkwh)$coefficients[, 1])
var200 = data.frame("kwhoth.rd11heatkwh" = summary(kwhoth.step.modelrd11heatkwh)$coefficients[, 1])
var201 = data.frame("kwhoth.rd12heatkwh" = summary(kwhoth.step.modelrd12heatkwh)$coefficients[, 1])
var202 = data.frame("kwhoth.rd13heatkwh" = summary(kwhoth.step.modelrd13heatkwh)$coefficients[, 1])
var203 = data.frame("kwhoth.rd14heatkwh" = summary(kwhoth.step.modelrd14heatkwh)$coefficients[, 1])
var204 = data.frame("kwhoth.rd15heatkwh" = summary(kwhoth.step.modelrd15heatkwh)$coefficients[, 1])
var205 = data.frame("kwhoth.rd16heatkwh" = summary(kwhoth.step.modelrd16heatkwh)$coefficients[, 1])
var206 = data.frame("kwhoth.rd17heatkwh" = summary(kwhoth.step.modelrd17heatkwh)$coefficients[, 1])
var207 = data.frame("kwhoth.rd18heatkwh" = summary(kwhoth.step.modelrd18heatkwh)$coefficients[, 1])
var208 = data.frame("kwhoth.rd19heatkwh" = summary(kwhoth.step.modelrd19heatkwh)$coefficients[, 1])
var209 = data.frame("kwhoth.rd20heatkwh" = summary(kwhoth.step.modelrd20heatkwh)$coefficients[, 1])
var210 = data.frame("kwhoth.rd21heatkwh" = summary(kwhoth.step.modelrd21heatkwh)$coefficients[, 1])
var211 = data.frame("kwhoth.rd22heatkwh" = summary(kwhoth.step.modelrd22heatkwh)$coefficients[, 1])
var212 = data.frame("kwhoth.rd23heatkwh" = summary(kwhoth.step.modelrd23heatkwh)$coefficients[, 1])
var213 = data.frame("kwhoth.rd24heatkwh" = summary(kwhoth.step.modelrd24heatkwh)$coefficients[, 1])
var214 = data.frame("kwhoth.rd25heatkwh" = summary(kwhoth.step.modelrd25heatkwh)$coefficients[, 1])
var215 = data.frame("kwhoth.rd26heatkwh" = summary(kwhoth.step.modelrd26heatkwh)$coefficients[, 1])
var216 = data.frame("kwhoth.rd27heatkwh" = summary(kwhoth.step.modelrd27heatkwh)$coefficients[, 1])
var217 = data.frame("kwhsph.rd1heatkwh" = summary(kwhsph.step.modelrd1heatkwh)$coefficients[, 1])
var218 = data.frame("kwhsph.rd2heatkwh" = summary(kwhsph.step.modelrd2heatkwh)$coefficients[, 1])
var219 = data.frame("kwhsph.rd3heatkwh" = summary(kwhsph.step.modelrd3heatkwh)$coefficients[, 1])
var220 = data.frame("kwhsph.rd4heatkwh" = summary(kwhsph.step.modelrd4heatkwh)$coefficients[, 1])
var221 = data.frame("kwhsph.rd5heatkwh" = summary(kwhsph.step.modelrd5heatkwh)$coefficients[, 1])
var222 = data.frame("kwhsph.rd6heatkwh" = summary(kwhsph.step.modelrd6heatkwh)$coefficients[, 1])
var223 = data.frame("kwhsph.rd7heatkwh" = summary(kwhsph.step.modelrd7heatkwh)$coefficients[, 1])
var224 = data.frame("kwhsph.rd8heatkwh" = summary(kwhsph.step.modelrd8heatkwh)$coefficients[, 1])
var225 = data.frame("kwhsph.rd9heatkwh" = summary(kwhsph.step.modelrd9heatkwh)$coefficients[, 1])
var226 = data.frame("kwhsph.rd10heatkwh" = summary(kwhsph.step.modelrd10heatkwh)$coefficients[, 1])
var227 = data.frame("kwhsph.rd11heatkwh" = summary(kwhsph.step.modelrd11heatkwh)$coefficients[, 1])
var228 = data.frame("kwhsph.rd12heatkwh" = summary(kwhsph.step.modelrd12heatkwh)$coefficients[, 1])
var229 = data.frame("kwhsph.rd13heatkwh" = summary(kwhsph.step.modelrd13heatkwh)$coefficients[, 1])
var230 = data.frame("kwhsph.rd14heatkwh" = summary(kwhsph.step.modelrd14heatkwh)$coefficients[, 1])
var231 = data.frame("kwhsph.rd15heatkwh" = summary(kwhsph.step.modelrd15heatkwh)$coefficients[, 1])
var232 = data.frame("kwhsph.rd16heatkwh" = summary(kwhsph.step.modelrd16heatkwh)$coefficients[, 1])
var233 = data.frame("kwhsph.rd17heatkwh" = summary(kwhsph.step.modelrd17heatkwh)$coefficients[, 1])
var234 = data.frame("kwhsph.rd18heatkwh" = summary(kwhsph.step.modelrd18heatkwh)$coefficients[, 1])
var235 = data.frame("kwhsph.rd19heatkwh" = summary(kwhsph.step.modelrd19heatkwh)$coefficients[, 1])
var236 = data.frame("kwhsph.rd20heatkwh" = summary(kwhsph.step.modelrd20heatkwh)$coefficients[, 1])
var237 = data.frame("kwhsph.rd21heatkwh" = summary(kwhsph.step.modelrd21heatkwh)$coefficients[, 1])
var238 = data.frame("kwhsph.rd22heatkwh" = summary(kwhsph.step.modelrd22heatkwh)$coefficients[, 1])
var239 = data.frame("kwhsph.rd23heatkwh" = summary(kwhsph.step.modelrd23heatkwh)$coefficients[, 1])
var240 = data.frame("kwhsph.rd24heatkwh" = summary(kwhsph.step.modelrd24heatkwh)$coefficients[, 1])
var241 = data.frame("kwhsph.rd25heatkwh" = summary(kwhsph.step.modelrd25heatkwh)$coefficients[, 1])
var242 = data.frame("kwhsph.rd26heatkwh" = summary(kwhsph.step.modelrd26heatkwh)$coefficients[, 1])
var243 = data.frame("kwhsph.rd27heatkwh" = summary(kwhsph.step.modelrd27heatkwh)$coefficients[, 1])
var244 = data.frame("btuother.rd1heatoth" = summary(btuother.step.modelrd1heatoth)$coefficients[, 1])
var245 = data.frame("btuother.rd2heatoth" = summary(btuother.step.modelrd2heatoth)$coefficients[, 1])
var246 = data.frame("btuother.rd3heatoth" = summary(btuother.step.modelrd3heatoth)$coefficients[, 1])
var247 = data.frame("btuother.rd4heatoth" = summary(btuother.step.modelrd4heatoth)$coefficients[, 1])
var248 = data.frame("btuother.rd5heatoth" = summary(btuother.step.modelrd5heatoth)$coefficients[, 1])
var249 = data.frame("btuother.rd6heatoth" = summary(btuother.step.modelrd6heatoth)$coefficients[, 1])
var250 = data.frame("btuother.rd7heatoth" = summary(btuother.step.modelrd7heatoth)$coefficients[, 1])
var251 = data.frame("btuother.rd8heatoth" = summary(btuother.step.modelrd8heatoth)$coefficients[, 1])
var252 = data.frame("btuother.rd9heatoth" = summary(btuother.step.modelrd9heatoth)$coefficients[, 1])
var253 = data.frame("btuother.rd10heatoth" = summary(btuother.step.modelrd10heatoth)$coefficients[, 1])
var254 = data.frame("btuother.rd11heatoth" = summary(btuother.step.modelrd11heatoth)$coefficients[, 1])
var255 = data.frame("btuother.rd12heatoth" = summary(btuother.step.modelrd12heatoth)$coefficients[, 1])
var256 = data.frame("btuother.rd13heatoth" = summary(btuother.step.modelrd13heatoth)$coefficients[, 1])
var257 = data.frame("btuother.rd14heatoth" = summary(btuother.step.modelrd14heatoth)$coefficients[, 1])
var258 = data.frame("btuother.rd15heatoth" = summary(btuother.step.modelrd15heatoth)$coefficients[, 1])
var259 = data.frame("btuother.rd16heatoth" = summary(btuother.step.modelrd16heatoth)$coefficients[, 1])
var260 = data.frame("btuother.rd17heatoth" = summary(btuother.step.modelrd17heatoth)$coefficients[, 1])
var261 = data.frame("btuother.rd18heatoth" = summary(btuother.step.modelrd18heatoth)$coefficients[, 1])
var262 = data.frame("btuother.rd19heatoth" = summary(btuother.step.modelrd19heatoth)$coefficients[, 1])
var263 = data.frame("btuother.rd20heatoth" = summary(btuother.step.modelrd20heatoth)$coefficients[, 1])
var264 = data.frame("btuother.rd21heatoth" = summary(btuother.step.modelrd21heatoth)$coefficients[, 1])
var265 = data.frame("btuother.rd22heatoth" = summary(btuother.step.modelrd22heatoth)$coefficients[, 1])
var266 = data.frame("btuother.rd23heatoth" = summary(btuother.step.modelrd23heatoth)$coefficients[, 1])
var267 = data.frame("btuother.rd24heatoth" = summary(btuother.step.modelrd24heatoth)$coefficients[, 1])
var268 = data.frame("btuother.rd25heatoth" = summary(btuother.step.modelrd25heatoth)$coefficients[, 1])
var269 = data.frame("btuother.rd26heatoth" = summary(btuother.step.modelrd26heatoth)$coefficients[, 1])
var270 = data.frame("btuother.rd27heatoth" = summary(btuother.step.modelrd27heatoth)$coefficients[, 1])
var271 = data.frame("kwh.rd1heatoth" = summary(kwh.step.modelrd1heatoth)$coefficients[, 1])
var272 = data.frame("kwh.rd2heatoth" = summary(kwh.step.modelrd2heatoth)$coefficients[, 1])
var273 = data.frame("kwh.rd3heatoth" = summary(kwh.step.modelrd3heatoth)$coefficients[, 1])
var274 = data.frame("kwh.rd4heatoth" = summary(kwh.step.modelrd4heatoth)$coefficients[, 1])
var275 = data.frame("kwh.rd5heatoth" = summary(kwh.step.modelrd5heatoth)$coefficients[, 1])
var276 = data.frame("kwh.rd6heatoth" = summary(kwh.step.modelrd6heatoth)$coefficients[, 1])
var277 = data.frame("kwh.rd7heatoth" = summary(kwh.step.modelrd7heatoth)$coefficients[, 1])
var278 = data.frame("kwh.rd8heatoth" = summary(kwh.step.modelrd8heatoth)$coefficients[, 1])
var279 = data.frame("kwh.rd9heatoth" = summary(kwh.step.modelrd9heatoth)$coefficients[, 1])
var280 = data.frame("kwh.rd10heatoth" = summary(kwh.step.modelrd10heatoth)$coefficients[, 1])
var281 = data.frame("kwh.rd11heatoth" = summary(kwh.step.modelrd11heatoth)$coefficients[, 1])
var282 = data.frame("kwh.rd12heatoth" = summary(kwh.step.modelrd12heatoth)$coefficients[, 1])
var283 = data.frame("kwh.rd13heatoth" = summary(kwh.step.modelrd13heatoth)$coefficients[, 1])
var284 = data.frame("kwh.rd14heatoth" = summary(kwh.step.modelrd14heatoth)$coefficients[, 1])
var285 = data.frame("kwh.rd15heatoth" = summary(kwh.step.modelrd15heatoth)$coefficients[, 1])
var286 = data.frame("kwh.rd16heatoth" = summary(kwh.step.modelrd16heatoth)$coefficients[, 1])
var287 = data.frame("kwh.rd17heatoth" = summary(kwh.step.modelrd17heatoth)$coefficients[, 1])
var288 = data.frame("kwh.rd18heatoth" = summary(kwh.step.modelrd18heatoth)$coefficients[, 1])
var289 = data.frame("kwh.rd19heatoth" = summary(kwh.step.modelrd19heatoth)$coefficients[, 1])
var290 = data.frame("kwh.rd20heatoth" = summary(kwh.step.modelrd20heatoth)$coefficients[, 1])
var291 = data.frame("kwh.rd21heatoth" = summary(kwh.step.modelrd21heatoth)$coefficients[, 1])
var292 = data.frame("kwh.rd22heatoth" = summary(kwh.step.modelrd22heatoth)$coefficients[, 1])
var293 = data.frame("kwh.rd23heatoth" = summary(kwh.step.modelrd23heatoth)$coefficients[, 1])
var294 = data.frame("kwh.rd24heatoth" = summary(kwh.step.modelrd24heatoth)$coefficients[, 1])
var295 = data.frame("kwh.rd25heatoth" = summary(kwh.step.modelrd25heatoth)$coefficients[, 1])
var296 = data.frame("kwh.rd26heatoth" = summary(kwh.step.modelrd26heatoth)$coefficients[, 1])
var297 = data.frame("kwh.rd27heatoth" = summary(kwh.step.modelrd27heatoth)$coefficients[, 1])
var298 = data.frame("kwhoth.rd1heatoth" = summary(kwhoth.step.modelrd1heatoth)$coefficients[, 1])
var299 = data.frame("kwhoth.rd2heatoth" = summary(kwhoth.step.modelrd2heatoth)$coefficients[, 1])
var300 = data.frame("kwhoth.rd3heatoth" = summary(kwhoth.step.modelrd3heatoth)$coefficients[, 1])
var301 = data.frame("kwhoth.rd4heatoth" = summary(kwhoth.step.modelrd4heatoth)$coefficients[, 1])
var302 = data.frame("kwhoth.rd5heatoth" = summary(kwhoth.step.modelrd5heatoth)$coefficients[, 1])
var303 = data.frame("kwhoth.rd6heatoth" = summary(kwhoth.step.modelrd6heatoth)$coefficients[, 1])
var304 = data.frame("kwhoth.rd7heatoth" = summary(kwhoth.step.modelrd7heatoth)$coefficients[, 1])
var305 = data.frame("kwhoth.rd8heatoth" = summary(kwhoth.step.modelrd8heatoth)$coefficients[, 1])
var306 = data.frame("kwhoth.rd9heatoth" = summary(kwhoth.step.modelrd9heatoth)$coefficients[, 1])
var307 = data.frame("kwhoth.rd10heatoth" = summary(kwhoth.step.modelrd10heatoth)$coefficients[, 1])
var308 = data.frame("kwhoth.rd11heatoth" = summary(kwhoth.step.modelrd11heatoth)$coefficients[, 1])
var309 = data.frame("kwhoth.rd12heatoth" = summary(kwhoth.step.modelrd12heatoth)$coefficients[, 1])
var310 = data.frame("kwhoth.rd13heatoth" = summary(kwhoth.step.modelrd13heatoth)$coefficients[, 1])
var311 = data.frame("kwhoth.rd14heatoth" = summary(kwhoth.step.modelrd14heatoth)$coefficients[, 1])
var312 = data.frame("kwhoth.rd15heatoth" = summary(kwhoth.step.modelrd15heatoth)$coefficients[, 1])
var313 = data.frame("kwhoth.rd16heatoth" = summary(kwhoth.step.modelrd16heatoth)$coefficients[, 1])
var314 = data.frame("kwhoth.rd17heatoth" = summary(kwhoth.step.modelrd17heatoth)$coefficients[, 1])
var315 = data.frame("kwhoth.rd18heatoth" = summary(kwhoth.step.modelrd18heatoth)$coefficients[, 1])
var316 = data.frame("kwhoth.rd19heatoth" = summary(kwhoth.step.modelrd19heatoth)$coefficients[, 1])
var317 = data.frame("kwhoth.rd20heatoth" = summary(kwhoth.step.modelrd20heatoth)$coefficients[, 1])
var318 = data.frame("kwhoth.rd21heatoth" = summary(kwhoth.step.modelrd21heatoth)$coefficients[, 1])
var319 = data.frame("kwhoth.rd22heatoth" = summary(kwhoth.step.modelrd22heatoth)$coefficients[, 1])
var320 = data.frame("kwhoth.rd23heatoth" = summary(kwhoth.step.modelrd23heatoth)$coefficients[, 1])
var321 = data.frame("kwhoth.rd24heatoth" = summary(kwhoth.step.modelrd24heatoth)$coefficients[, 1])
var322 = data.frame("kwhoth.rd25heatoth" = summary(kwhoth.step.modelrd25heatoth)$coefficients[, 1])
var323 = data.frame("kwhoth.rd26heatoth" = summary(kwhoth.step.modelrd26heatoth)$coefficients[, 1])
var324 = data.frame("kwhoth.rd27heatoth" = summary(kwhoth.step.modelrd27heatoth)$coefficients[, 1])
var325 = data.frame("kwhsph.rd1heatoth" = summary(kwhsph.step.modelrd1heatoth)$coefficients[, 1])
var326 = data.frame("kwhsph.rd2heatoth" = summary(kwhsph.step.modelrd2heatoth)$coefficients[, 1])
var327 = data.frame("kwhsph.rd3heatoth" = summary(kwhsph.step.modelrd3heatoth)$coefficients[, 1])
var328 = data.frame("kwhsph.rd4heatoth" = summary(kwhsph.step.modelrd4heatoth)$coefficients[, 1])
var329 = data.frame("kwhsph.rd5heatoth" = summary(kwhsph.step.modelrd5heatoth)$coefficients[, 1])
var330 = data.frame("kwhsph.rd6heatoth" = summary(kwhsph.step.modelrd6heatoth)$coefficients[, 1])
var331 = data.frame("kwhsph.rd7heatoth" = summary(kwhsph.step.modelrd7heatoth)$coefficients[, 1])
var332 = data.frame("kwhsph.rd8heatoth" = summary(kwhsph.step.modelrd8heatoth)$coefficients[, 1])
var333 = data.frame("kwhsph.rd9heatoth" = summary(kwhsph.step.modelrd9heatoth)$coefficients[, 1])
var334 = data.frame("kwhsph.rd10heatoth" = summary(kwhsph.step.modelrd10heatoth)$coefficients[, 1])
var335 = data.frame("kwhsph.rd11heatoth" = summary(kwhsph.step.modelrd11heatoth)$coefficients[, 1])
var336 = data.frame("kwhsph.rd12heatoth" = summary(kwhsph.step.modelrd12heatoth)$coefficients[, 1])
var337 = data.frame("kwhsph.rd13heatoth" = summary(kwhsph.step.modelrd13heatoth)$coefficients[, 1])
var338 = data.frame("kwhsph.rd14heatoth" = summary(kwhsph.step.modelrd14heatoth)$coefficients[, 1])
var339 = data.frame("kwhsph.rd15heatoth" = summary(kwhsph.step.modelrd15heatoth)$coefficients[, 1])
var340 = data.frame("kwhsph.rd16heatoth" = summary(kwhsph.step.modelrd16heatoth)$coefficients[, 1])
var341 = data.frame("kwhsph.rd17heatoth" = summary(kwhsph.step.modelrd17heatoth)$coefficients[, 1])
var342 = data.frame("kwhsph.rd18heatoth" = summary(kwhsph.step.modelrd18heatoth)$coefficients[, 1])
var343 = data.frame("kwhsph.rd19heatoth" = summary(kwhsph.step.modelrd19heatoth)$coefficients[, 1])
var344 = data.frame("kwhsph.rd20heatoth" = summary(kwhsph.step.modelrd20heatoth)$coefficients[, 1])
var345 = data.frame("kwhsph.rd21heatoth" = summary(kwhsph.step.modelrd21heatoth)$coefficients[, 1])
var346 = data.frame("kwhsph.rd22heatoth" = summary(kwhsph.step.modelrd22heatoth)$coefficients[, 1])
var347 = data.frame("kwhsph.rd23heatoth" = summary(kwhsph.step.modelrd23heatoth)$coefficients[, 1])
var348 = data.frame("kwhsph.rd24heatoth" = summary(kwhsph.step.modelrd24heatoth)$coefficients[, 1])
var349 = data.frame("kwhsph.rd25heatoth" = summary(kwhsph.step.modelrd25heatoth)$coefficients[, 1])
var350 = data.frame("kwhsph.rd26heatoth" = summary(kwhsph.step.modelrd26heatoth)$coefficients[, 1])
var351 = data.frame("kwhsph.rd27heatoth" = summary(kwhsph.step.modelrd27heatoth)$coefficients[, 1])
var352 = data.frame("kwhcol.rd1heatany" = summary(kwhcol.step.modelrd1heatany)$coefficients[, 1])
var353 = data.frame("kwhcol.rd2heatany" = summary(kwhcol.step.modelrd2heatany)$coefficients[, 1])
var354 = data.frame("kwhcol.rd3heatany" = summary(kwhcol.step.modelrd3heatany)$coefficients[, 1])
var355 = data.frame("kwhcol.rd4heatany" = summary(kwhcol.step.modelrd4heatany)$coefficients[, 1])
var356 = data.frame("kwhcol.rd5heatany" = summary(kwhcol.step.modelrd5heatany)$coefficients[, 1])
var357 = data.frame("kwhcol.rd6heatany" = summary(kwhcol.step.modelrd6heatany)$coefficients[, 1])
var358 = data.frame("kwhcol.rd7heatany" = summary(kwhcol.step.modelrd7heatany)$coefficients[, 1])
var359 = data.frame("kwhcol.rd8heatany" = summary(kwhcol.step.modelrd8heatany)$coefficients[, 1])
var360 = data.frame("kwhcol.rd9heatany" = summary(kwhcol.step.modelrd9heatany)$coefficients[, 1])
var361 = data.frame("kwhcol.rd10heatany" = summary(kwhcol.step.modelrd10heatany)$coefficients[, 1])
var362 = data.frame("kwhcol.rd11heatany" = summary(kwhcol.step.modelrd11heatany)$coefficients[, 1])
var363 = data.frame("kwhcol.rd12heatany" = summary(kwhcol.step.modelrd12heatany)$coefficients[, 1])
var364 = data.frame("kwhcol.rd13heatany" = summary(kwhcol.step.modelrd13heatany)$coefficients[, 1])
var365 = data.frame("kwhcol.rd14heatany" = summary(kwhcol.step.modelrd14heatany)$coefficients[, 1])
var366 = data.frame("kwhcol.rd15heatany" = summary(kwhcol.step.modelrd15heatany)$coefficients[, 1])
var367 = data.frame("kwhcol.rd16heatany" = summary(kwhcol.step.modelrd16heatany)$coefficients[, 1])
var368 = data.frame("kwhcol.rd17heatany" = summary(kwhcol.step.modelrd17heatany)$coefficients[, 1])
var369 = data.frame("kwhcol.rd18heatany" = summary(kwhcol.step.modelrd18heatany)$coefficients[, 1])
var370 = data.frame("kwhcol.rd19heatany" = summary(kwhcol.step.modelrd19heatany)$coefficients[, 1])
var371 = data.frame("kwhcol.rd20heatany" = summary(kwhcol.step.modelrd20heatany)$coefficients[, 1])
var372 = data.frame("kwhcol.rd21heatany" = summary(kwhcol.step.modelrd21heatany)$coefficients[, 1])
var373 = data.frame("kwhcol.rd22heatany" = summary(kwhcol.step.modelrd22heatany)$coefficients[, 1])
var374 = data.frame("kwhcol.rd23heatany" = summary(kwhcol.step.modelrd23heatany)$coefficients[, 1])
var375 = data.frame("kwhcol.rd24heatany" = summary(kwhcol.step.modelrd24heatany)$coefficients[, 1])
var376 = data.frame("kwhcol.rd25heatany" = summary(kwhcol.step.modelrd25heatany)$coefficients[, 1])
var377 = data.frame("kwhcol.rd26heatany" = summary(kwhcol.step.modelrd26heatany)$coefficients[, 1])
var378 = data.frame("kwhcol.rd27heatany" = summary(kwhcol.step.modelrd27heatany)$coefficients[, 1])
var379 = data.frame("btulp.rd1heatany" = summary(btulp.step.modelrd1heatany)$coefficients[, 1])
var380 = data.frame("btulp.rd2heatany" = summary(btulp.step.modelrd2heatany)$coefficients[, 1])
var381 = data.frame("btulp.rd3heatany" = summary(btulp.step.modelrd3heatany)$coefficients[, 1])
var382 = data.frame("btulp.rd4heatany" = summary(btulp.step.modelrd4heatany)$coefficients[, 1])
var383 = data.frame("btulp.rd5heatany" = summary(btulp.step.modelrd5heatany)$coefficients[, 1])
var384 = data.frame("btulp.rd6heatany" = summary(btulp.step.modelrd6heatany)$coefficients[, 1])
var385 = data.frame("btulp.rd7heatany" = summary(btulp.step.modelrd7heatany)$coefficients[, 1])
var386 = data.frame("btulp.rd8heatany" = summary(btulp.step.modelrd8heatany)$coefficients[, 1])
var387 = data.frame("btulp.rd9heatany" = summary(btulp.step.modelrd9heatany)$coefficients[, 1])
var388 = data.frame("btulp.rd10heatany" = summary(btulp.step.modelrd10heatany)$coefficients[, 1])
var389 = data.frame("btulp.rd11heatany" = summary(btulp.step.modelrd11heatany)$coefficients[, 1])
var390 = data.frame("btulp.rd12heatany" = summary(btulp.step.modelrd12heatany)$coefficients[, 1])
var391 = data.frame("btulp.rd13heatany" = summary(btulp.step.modelrd13heatany)$coefficients[, 1])
var392 = data.frame("btulp.rd14heatany" = summary(btulp.step.modelrd14heatany)$coefficients[, 1])
var393 = data.frame("btulp.rd15heatany" = summary(btulp.step.modelrd15heatany)$coefficients[, 1])
var394 = data.frame("btulp.rd16heatany" = summary(btulp.step.modelrd16heatany)$coefficients[, 1])
var395 = data.frame("btulp.rd17heatany" = summary(btulp.step.modelrd17heatany)$coefficients[, 1])
var396 = data.frame("btulp.rd18heatany" = summary(btulp.step.modelrd18heatany)$coefficients[, 1])
var397 = data.frame("btulp.rd19heatany" = summary(btulp.step.modelrd19heatany)$coefficients[, 1])
var398 = data.frame("btulp.rd20heatany" = summary(btulp.step.modelrd20heatany)$coefficients[, 1])
var399 = data.frame("btulp.rd21heatany" = summary(btulp.step.modelrd21heatany)$coefficients[, 1])
var400 = data.frame("btulp.rd22heatany" = summary(btulp.step.modelrd22heatany)$coefficients[, 1])
var401 = data.frame("btulp.rd23heatany" = summary(btulp.step.modelrd23heatany)$coefficients[, 1])
var402 = data.frame("btulp.rd24heatany" = summary(btulp.step.modelrd24heatany)$coefficients[, 1])
var403 = data.frame("btulp.rd25heatany" = summary(btulp.step.modelrd25heatany)$coefficients[, 1])
var404 = data.frame("btulp.rd26heatany" = summary(btulp.step.modelrd26heatany)$coefficients[, 1])
var405 = data.frame("btulp.rd27heatany" = summary(btulp.step.modelrd27heatany)$coefficients[, 1])
var406 = data.frame("kwhwth.rd1heatgas" = summary(kwhwth.step.modelrd1heatgas)$coefficients[, 1])
var407 = data.frame("kwhwth.rd2heatgas" = summary(kwhwth.step.modelrd2heatgas)$coefficients[, 1])
var408 = data.frame("kwhwth.rd3heatgas" = summary(kwhwth.step.modelrd3heatgas)$coefficients[, 1])
var409 = data.frame("kwhwth.rd4heatgas" = summary(kwhwth.step.modelrd4heatgas)$coefficients[, 1])
var410 = data.frame("kwhwth.rd5heatgas" = summary(kwhwth.step.modelrd5heatgas)$coefficients[, 1])
var411 = data.frame("kwhwth.rd6heatgas" = summary(kwhwth.step.modelrd6heatgas)$coefficients[, 1])
var412 = data.frame("kwhwth.rd7heatgas" = summary(kwhwth.step.modelrd7heatgas)$coefficients[, 1])
var413 = data.frame("kwhwth.rd8heatgas" = summary(kwhwth.step.modelrd8heatgas)$coefficients[, 1])
var414 = data.frame("kwhwth.rd9heatgas" = summary(kwhwth.step.modelrd9heatgas)$coefficients[, 1])
var415 = data.frame("kwhwth.rd10heatgas" = summary(kwhwth.step.modelrd10heatgas)$coefficients[, 1])
var416 = data.frame("kwhwth.rd11heatgas" = summary(kwhwth.step.modelrd11heatgas)$coefficients[, 1])
var417 = data.frame("kwhwth.rd12heatgas" = summary(kwhwth.step.modelrd12heatgas)$coefficients[, 1])
var418 = data.frame("kwhwth.rd13heatgas" = summary(kwhwth.step.modelrd13heatgas)$coefficients[, 1])
var419 = data.frame("kwhwth.rd14heatgas" = summary(kwhwth.step.modelrd14heatgas)$coefficients[, 1])
var420 = data.frame("kwhwth.rd15heatgas" = summary(kwhwth.step.modelrd15heatgas)$coefficients[, 1])
var421 = data.frame("kwhwth.rd16heatgas" = summary(kwhwth.step.modelrd16heatgas)$coefficients[, 1])
var422 = data.frame("kwhwth.rd17heatgas" = summary(kwhwth.step.modelrd17heatgas)$coefficients[, 1])
var423 = data.frame("kwhwth.rd18heatgas" = summary(kwhwth.step.modelrd18heatgas)$coefficients[, 1])
var424 = data.frame("kwhwth.rd19heatgas" = summary(kwhwth.step.modelrd19heatgas)$coefficients[, 1])
var425 = data.frame("kwhwth.rd20heatgas" = summary(kwhwth.step.modelrd20heatgas)$coefficients[, 1])
var426 = data.frame("kwhwth.rd21heatgas" = summary(kwhwth.step.modelrd21heatgas)$coefficients[, 1])
var427 = data.frame("kwhwth.rd22heatgas" = summary(kwhwth.step.modelrd22heatgas)$coefficients[, 1])
var428 = data.frame("kwhwth.rd23heatgas" = summary(kwhwth.step.modelrd23heatgas)$coefficients[, 1])
var429 = data.frame("kwhwth.rd24heatgas" = summary(kwhwth.step.modelrd24heatgas)$coefficients[, 1])
var430 = data.frame("kwhwth.rd25heatgas" = summary(kwhwth.step.modelrd25heatgas)$coefficients[, 1])
var431 = data.frame("kwhwth.rd26heatgas" = summary(kwhwth.step.modelrd26heatgas)$coefficients[, 1])
var432 = data.frame("kwhwth.rd27heatgas" = summary(kwhwth.step.modelrd27heatgas)$coefficients[, 1])
var433 = data.frame("kwhwth.rd1heatkwh" = summary(kwhwth.step.modelrd1heatkwh)$coefficients[, 1])
var434 = data.frame("kwhwth.rd2heatkwh" = summary(kwhwth.step.modelrd2heatkwh)$coefficients[, 1])
var435 = data.frame("kwhwth.rd3heatkwh" = summary(kwhwth.step.modelrd3heatkwh)$coefficients[, 1])
var436 = data.frame("kwhwth.rd4heatkwh" = summary(kwhwth.step.modelrd4heatkwh)$coefficients[, 1])
var437 = data.frame("kwhwth.rd5heatkwh" = summary(kwhwth.step.modelrd5heatkwh)$coefficients[, 1])
var438 = data.frame("kwhwth.rd6heatkwh" = summary(kwhwth.step.modelrd6heatkwh)$coefficients[, 1])
var439 = data.frame("kwhwth.rd7heatkwh" = summary(kwhwth.step.modelrd7heatkwh)$coefficients[, 1])
var440 = data.frame("kwhwth.rd8heatkwh" = summary(kwhwth.step.modelrd8heatkwh)$coefficients[, 1])
var441 = data.frame("kwhwth.rd9heatkwh" = summary(kwhwth.step.modelrd9heatkwh)$coefficients[, 1])
var442 = data.frame("kwhwth.rd10heatkwh" = summary(kwhwth.step.modelrd10heatkwh)$coefficients[, 1])
var443 = data.frame("kwhwth.rd11heatkwh" = summary(kwhwth.step.modelrd11heatkwh)$coefficients[, 1])
var444 = data.frame("kwhwth.rd12heatkwh" = summary(kwhwth.step.modelrd12heatkwh)$coefficients[, 1])
var445 = data.frame("kwhwth.rd13heatkwh" = summary(kwhwth.step.modelrd13heatkwh)$coefficients[, 1])
var446 = data.frame("kwhwth.rd14heatkwh" = summary(kwhwth.step.modelrd14heatkwh)$coefficients[, 1])
var447 = data.frame("kwhwth.rd15heatkwh" = summary(kwhwth.step.modelrd15heatkwh)$coefficients[, 1])
var448 = data.frame("kwhwth.rd16heatkwh" = summary(kwhwth.step.modelrd16heatkwh)$coefficients[, 1])
var449 = data.frame("kwhwth.rd17heatkwh" = summary(kwhwth.step.modelrd17heatkwh)$coefficients[, 1])
var450 = data.frame("kwhwth.rd18heatkwh" = summary(kwhwth.step.modelrd18heatkwh)$coefficients[, 1])
var451 = data.frame("kwhwth.rd19heatkwh" = summary(kwhwth.step.modelrd19heatkwh)$coefficients[, 1])
var452 = data.frame("kwhwth.rd20heatkwh" = summary(kwhwth.step.modelrd20heatkwh)$coefficients[, 1])
var453 = data.frame("kwhwth.rd21heatkwh" = summary(kwhwth.step.modelrd21heatkwh)$coefficients[, 1])
var454 = data.frame("kwhwth.rd22heatkwh" = summary(kwhwth.step.modelrd22heatkwh)$coefficients[, 1])
var455 = data.frame("kwhwth.rd23heatkwh" = summary(kwhwth.step.modelrd23heatkwh)$coefficients[, 1])
var456 = data.frame("kwhwth.rd24heatkwh" = summary(kwhwth.step.modelrd24heatkwh)$coefficients[, 1])
var457 = data.frame("kwhwth.rd25heatkwh" = summary(kwhwth.step.modelrd25heatkwh)$coefficients[, 1])
var458 = data.frame("kwhwth.rd26heatkwh" = summary(kwhwth.step.modelrd26heatkwh)$coefficients[, 1])
var459 = data.frame("kwhwth.rd27heatkwh" = summary(kwhwth.step.modelrd27heatkwh)$coefficients[, 1])
var460 = data.frame(kwhwth.rd1heatoth = summary(kwhwth.step.modelrd1heatoth)$coefficients[, 1])
var461 = data.frame(kwhwth.rd2heatoth = summary(kwhwth.step.modelrd2heatoth)$coefficients[, 1])
var462 = data.frame(kwhwth.rd3heatoth = summary(kwhwth.step.modelrd3heatoth)$coefficients[, 1])
var463 = data.frame(kwhwth.rd4heatoth = summary(kwhwth.step.modelrd4heatoth)$coefficients[, 1])
var464 = data.frame(kwhwth.rd5heatoth = summary(kwhwth.step.modelrd5heatoth)$coefficients[, 1])
var465 = data.frame(kwhwth.rd6heatoth = summary(kwhwth.step.modelrd6heatoth)$coefficients[, 1])
var466 = data.frame(kwhwth.rd7heatoth = summary(kwhwth.step.modelrd7heatoth)$coefficients[, 1])
var467 = data.frame(kwhwth.rd8heatoth = summary(kwhwth.step.modelrd8heatoth)$coefficients[, 1])
var468 = data.frame(kwhwth.rd9heatoth = summary(kwhwth.step.modelrd9heatoth)$coefficients[, 1])
var469 = data.frame(kwhwth.rd10heatoth = summary(kwhwth.step.modelrd10heatoth)$coefficients[, 1])
var470 = data.frame(kwhwth.rd11heatoth = summary(kwhwth.step.modelrd11heatoth)$coefficients[, 1])
var471 = data.frame(kwhwth.rd12heatoth = summary(kwhwth.step.modelrd12heatoth)$coefficients[, 1])
var472 = data.frame(kwhwth.rd13heatoth = summary(kwhwth.step.modelrd13heatoth)$coefficients[, 1])
var473 = data.frame(kwhwth.rd14heatoth = summary(kwhwth.step.modelrd14heatoth)$coefficients[, 1])
var474 = data.frame(kwhwth.rd15heatoth = summary(kwhwth.step.modelrd15heatoth)$coefficients[, 1])
var475 = data.frame(kwhwth.rd16heatoth = summary(kwhwth.step.modelrd16heatoth)$coefficients[, 1])
var476 = data.frame(kwhwth.rd17heatoth = summary(kwhwth.step.modelrd17heatoth)$coefficients[, 1])
var477 = data.frame(kwhwth.rd18heatoth = summary(kwhwth.step.modelrd18heatoth)$coefficients[, 1])
var478 = data.frame(kwhwth.rd19heatoth = summary(kwhwth.step.modelrd19heatoth)$coefficients[, 1])
var479 = data.frame(kwhwth.rd20heatoth = summary(kwhwth.step.modelrd20heatoth)$coefficients[, 1])
var480 = data.frame(kwhwth.rd21heatoth = summary(kwhwth.step.modelrd21heatoth)$coefficients[, 1])
var481 = data.frame(kwhwth.rd22heatoth = summary(kwhwth.step.modelrd22heatoth)$coefficients[, 1])
var482 = data.frame(kwhwth.rd23heatoth = summary(kwhwth.step.modelrd23heatoth)$coefficients[, 1])
var483 = data.frame(kwhwth.rd24heatoth = summary(kwhwth.step.modelrd24heatoth)$coefficients[, 1])
var484 = data.frame(kwhwth.rd25heatoth = summary(kwhwth.step.modelrd25heatoth)$coefficients[, 1])
var485 = data.frame(kwhwth.rd26heatoth = summary(kwhwth.step.modelrd26heatoth)$coefficients[, 1])
var486 = data.frame(kwhwth.rd27heatoth = summary(kwhwth.step.modelrd27heatoth)$coefficients[, 1])


 
## Now write the function to join the data frames created above

multimerge <- function (mylist) {
  ## mimics a recursive merge or full outer join
  
  unames <- unique(unlist(lapply(mylist, rownames)))
  
  n <- length(unames)
  
  out <- lapply(mylist, function(df) {
    
    tmp <- matrix(nr = n, nc = ncol(df), dimnames = list(unames,colnames(df)))
    tmp[rownames(df), ] <- as.matrix(df)
    rm(df); gc()
    
    return(tmp)
  })
  
  stopifnot( all( sapply(out, function(x) identical(rownames(x), unames)) ) )
  
  bigout <- do.call(cbind, out)
  colnames(bigout) <- paste(rep(names(mylist), sapply(mylist, ncol)), unlist(sapply(mylist, colnames)), sep = "_")
  return(bigout)
}

out <- multimerge( list (
  var1,
  var2,
  var3,
  var4,
  var5,
  var6,
  var7,
  var8,
  var9,
  var10,
  var11,
  var12,
  var13,
  var14,
  var15,
  var16,
  var17,
  var18,
  var19,
  var20,
  var21,
  var22,
  var23,
  var24,
  var25,
  var26,
  var27,
  var28,
  var29,
  var30,
  var31,
  var32,
  var33,
  var34,
  var35,
  var36,
  var37,
  var38,
  var39,
  var40,
  var41,
  var42,
  var43,
  var44,
  var45,
  var46,
  var47,
  var48,
  var49,
  var50,
  var51,
  var52,
  var53,
  var54,
  var55,
  var56,
  var57,
  var58,
  var59,
  var60,
  var61,
  var62,
  var63,
  var64,
  var65,
  var66,
  var67,
  var68,
  var69,
  var70,
  var71,
  var72,
  var73,
  var74,
  var75,
  var76,
  var77,
  var78,
  var79,
  var80,
  var81,
  var82,
  var83,
  var84,
  var85,
  var86,
  var87,
  var88,
  var89,
  var90,
  var91,
  var92,
  var93,
  var94,
  var95,
  var96,
  var97,
  var98,
  var99,
  var100,
  var101,
  var102,
  var103,
  var104,
  var105,
  var106,
  var107,
  var108,
  var109,
  var110,
  var111,
  var112,
  var113,
  var114,
  var115,
  var116,
  var117,
  var118,
  var119,
  var120,
  var121,
  var122,
  var123,
  var124,
  var125,
  var126,
  var127,
  var128,
  var129,
  var130,
  var131,
  var132,
  var133,
  var134,
  var135,
  var136,
  var137,
  var138,
  var139,
  var140,
  var141,
  var142,
  var143,
  var144,
  var145,
  var146,
  var147,
  var148,
  var149,
  var150,
  var151,
  var152,
  var153,
  var154,
  var155,
  var156,
  var157,
  var158,
  var159,
  var160,
  var161,
  var162,
  var163,
  var164,
  var165,
  var166,
  var167,
  var168,
  var169,
  var170,
  var171,
  var172,
  var173,
  var174,
  var175,
  var176,
  var177,
  var178,
  var179,
  var180,
  var181,
  var182,
  var183,
  var184,
  var185,
  var186,
  var187,
  var188,
  var189,
  var190,
  var191,
  var192,
  var193,
  var194,
  var195,
  var196,
  var197,
  var198,
  var199,
  var200,
  var201,
  var202,
  var203,
  var204,
  var205,
  var206,
  var207,
  var208,
  var209,
  var210,
  var211,
  var212,
  var213,
  var214,
  var215,
  var216,
  var217,
  var218,
  var219,
  var220,
  var221,
  var222,
  var223,
  var224,
  var225,
  var226,
  var227,
  var228,
  var229,
  var230,
  var231,
  var232,
  var233,
  var234,
  var235,
  var236,
  var237,
  var238,
  var239,
  var240,
  var241,
  var242,
  var243,
  var244,
  var245,
  var246,
  var247,
  var248,
  var249,
  var250,
  var251,
  var252,
  var253,
  var254,
  var255,
  var256,
  var257,
  var258,
  var259,
  var260,
  var261,
  var262,
  var263,
  var264,
  var265,
  var266,
  var267,
  var268,
  var269,
  var270,
  var271,
  var272,
  var273,
  var274,
  var275,
  var276,
  var277,
  var278,
  var279,
  var280,
  var281,
  var282,
  var283,
  var284,
  var285,
  var286,
  var287,
  var288,
  var289,
  var290,
  var291,
  var292,
  var293,
  var294,
  var295,
  var296,
  var297,
  var298,
  var299,
  var300,
  var301,
  var302,
  var303,
  var304,
  var305,
  var306,
  var307,
  var308,
  var309,
  var310,
  var311,
  var312,
  var313,
  var314,
  var315,
  var316,
  var317,
  var318,
  var319,
  var320,
  var321,
  var322,
  var323,
  var324,
  var325,
  var326,
  var327,
  var328,
  var329,
  var330,
  var331,
  var332,
  var333,
  var334,
  var335,
  var336,
  var337,
  var338,
  var339,
  var340,
  var341,
  var342,
  var343,
  var344,
  var345,
  var346,
  var347,
  var348,
  var349,
  var350,
  var351,
  var352,
  var353,
  var354,
  var355,
  var356,
  var357,
  var358,
  var359,
  var360,
  var361,
  var362,
  var363,
  var364,
  var365,
  var366,
  var367,
  var368,
  var369,
  var370,
  var371,
  var372,
  var373,
  var374,
  var375,
  var376,
  var377,
  var378,
  var379,
  var380,
  var381,
  var382,
  var383,
  var384,
  var385,
  var386,
  var387,
  var388,
  var389,
  var390,
  var391,
  var392,
  var393,
  var394,
  var395,
  var396,
  var397,
  var398,
  var399,
  var400,
  var401,
  var402,
  var403,
  var404,
  var405,
  var406,
  var407,
  var408,
  var409,
  var410,
  var411,
  var412,
  var413,
  var414,
  var415,
  var416,
  var417,
  var418,
  var419,
  var420,
  var421,
  var422,
  var423,
  var424,
  var425,
  var426,
  var427,
  var428,
  var429,
  var430,
  var431,
  var432,
  var433,
  var434,
  var435,
  var436,
  var437,
  var438,
  var439,
  var440,
  var441,
  var442,
  var443,
  var444,
  var445,
  var446,
  var447,
  var448,
  var449,
  var450,
  var451,
  var452,
  var453,
  var454,
  var455,
  var456,
  var457,
  var458,
  var459,
  var460,
  var461,
  var462,
  var463,
  var464,
  var465,
  var466,
  var467,
  var468,
  var469,
  var470,
  var471,
  var472,
  var473,
  var474,
  var475,
  var476,
  var477,
  var478,
  var479,
  var480,
  var481,
  var482,
  var483,
  var484,
  var485,
  var486
) )
colnames(out) <- gsub("^_", "", colnames(out))







# Create vectors with Sample Size (N) and Adjusted R squared
cufeetng.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd1heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd1heatgas)$sigma)
cufeetng.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd2heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd2heatgas)$sigma)
cufeetng.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd3heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd3heatgas)$sigma)
cufeetng.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd4heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd4heatgas)$sigma)
cufeetng.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd5heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd5heatgas)$sigma)
cufeetng.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd6heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd6heatgas)$sigma)
cufeetng.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd7heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd7heatgas)$sigma)
cufeetng.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd8heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd8heatgas)$sigma)
cufeetng.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd9heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd9heatgas)$sigma)
cufeetng.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd10heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd10heatgas)$sigma)
cufeetng.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd11heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd11heatgas)$sigma)
cufeetng.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd12heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd12heatgas)$sigma)
cufeetng.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd13heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd13heatgas)$sigma)
cufeetng.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd14heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd14heatgas)$sigma)
cufeetng.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd15heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd15heatgas)$sigma)
cufeetng.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd16heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd16heatgas)$sigma)
cufeetng.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd17heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd17heatgas)$sigma)
cufeetng.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd18heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd18heatgas)$sigma)
cufeetng.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd19heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd19heatgas)$sigma)
cufeetng.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd20heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd20heatgas)$sigma)
cufeetng.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd21heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd21heatgas)$sigma)
cufeetng.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd22heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd22heatgas)$sigma)
cufeetng.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd23heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd23heatgas)$sigma)
cufeetng.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd24heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd24heatgas)$sigma)
cufeetng.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd25heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd25heatgas)$sigma)
cufeetng.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd26heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd26heatgas)$sigma)
cufeetng.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(cufeetng.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetng.step.modelrd27heatgas)$df, "Sigma" = summary(cufeetng.step.modelrd27heatgas)$sigma)
cufeetngsph.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd1heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd1heatgas)$sigma)
cufeetngsph.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd2heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd2heatgas)$sigma)
cufeetngsph.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd3heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd3heatgas)$sigma)
cufeetngsph.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd4heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd4heatgas)$sigma)
cufeetngsph.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd5heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd5heatgas)$sigma)
cufeetngsph.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd6heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd6heatgas)$sigma)
cufeetngsph.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd7heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd7heatgas)$sigma)
cufeetngsph.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd8heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd8heatgas)$sigma)
cufeetngsph.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd9heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd9heatgas)$sigma)
cufeetngsph.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd10heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd10heatgas)$sigma)
cufeetngsph.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd11heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd11heatgas)$sigma)
cufeetngsph.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd12heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd12heatgas)$sigma)
cufeetngsph.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd13heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd13heatgas)$sigma)
cufeetngsph.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd14heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd14heatgas)$sigma)
cufeetngsph.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd15heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd15heatgas)$sigma)
cufeetngsph.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd16heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd16heatgas)$sigma)
cufeetngsph.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd17heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd17heatgas)$sigma)
cufeetngsph.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd18heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd18heatgas)$sigma)
cufeetngsph.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd19heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd19heatgas)$sigma)
cufeetngsph.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd20heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd20heatgas)$sigma)
cufeetngsph.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd21heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd21heatgas)$sigma)
cufeetngsph.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd22heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd22heatgas)$sigma)
cufeetngsph.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd23heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd23heatgas)$sigma)
cufeetngsph.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd24heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd24heatgas)$sigma)
cufeetngsph.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd25heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd25heatgas)$sigma)
cufeetngsph.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd26heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd26heatgas)$sigma)
cufeetngsph.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(cufeetngsph.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngsph.step.modelrd27heatgas)$df, "Sigma" = summary(cufeetngsph.step.modelrd27heatgas)$sigma)
cufeetngwth.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd1heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd1heatgas)$sigma)
cufeetngwth.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd2heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd2heatgas)$sigma)
cufeetngwth.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd3heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd3heatgas)$sigma)
cufeetngwth.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd4heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd4heatgas)$sigma)
cufeetngwth.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd5heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd5heatgas)$sigma)
cufeetngwth.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd6heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd6heatgas)$sigma)
cufeetngwth.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd7heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd7heatgas)$sigma)
cufeetngwth.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd8heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd8heatgas)$sigma)
cufeetngwth.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd9heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd9heatgas)$sigma)
cufeetngwth.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd10heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd10heatgas)$sigma)
cufeetngwth.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd11heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd11heatgas)$sigma)
cufeetngwth.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd12heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd12heatgas)$sigma)
cufeetngwth.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd13heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd13heatgas)$sigma)
cufeetngwth.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd14heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd14heatgas)$sigma)
cufeetngwth.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd15heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd15heatgas)$sigma)
cufeetngwth.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd16heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd16heatgas)$sigma)
cufeetngwth.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd17heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd17heatgas)$sigma)
cufeetngwth.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd18heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd18heatgas)$sigma)
cufeetngwth.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd19heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd19heatgas)$sigma)
cufeetngwth.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd20heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd20heatgas)$sigma)
cufeetngwth.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd21heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd21heatgas)$sigma)
cufeetngwth.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd22heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd22heatgas)$sigma)
cufeetngwth.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd23heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd23heatgas)$sigma)
cufeetngwth.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd24heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd24heatgas)$sigma)
cufeetngwth.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd25heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd25heatgas)$sigma)
cufeetngwth.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd26heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd26heatgas)$sigma)
cufeetngwth.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(cufeetngwth.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(cufeetngwth.step.modelrd27heatgas)$df, "Sigma" = summary(cufeetngwth.step.modelrd27heatgas)$sigma)
kwh.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(kwh.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd1heatgas)$df, "Sigma" = summary(kwh.step.modelrd1heatgas)$sigma)
kwh.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(kwh.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd2heatgas)$df, "Sigma" = summary(kwh.step.modelrd2heatgas)$sigma)
kwh.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(kwh.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd3heatgas)$df, "Sigma" = summary(kwh.step.modelrd3heatgas)$sigma)
kwh.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(kwh.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd4heatgas)$df, "Sigma" = summary(kwh.step.modelrd4heatgas)$sigma)
kwh.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(kwh.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd5heatgas)$df, "Sigma" = summary(kwh.step.modelrd5heatgas)$sigma)
kwh.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(kwh.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd6heatgas)$df, "Sigma" = summary(kwh.step.modelrd6heatgas)$sigma)
kwh.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(kwh.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd7heatgas)$df, "Sigma" = summary(kwh.step.modelrd7heatgas)$sigma)
kwh.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(kwh.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd8heatgas)$df, "Sigma" = summary(kwh.step.modelrd8heatgas)$sigma)
kwh.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(kwh.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd9heatgas)$df, "Sigma" = summary(kwh.step.modelrd9heatgas)$sigma)
kwh.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(kwh.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd10heatgas)$df, "Sigma" = summary(kwh.step.modelrd10heatgas)$sigma)
kwh.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(kwh.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd11heatgas)$df, "Sigma" = summary(kwh.step.modelrd11heatgas)$sigma)
kwh.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(kwh.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd12heatgas)$df, "Sigma" = summary(kwh.step.modelrd12heatgas)$sigma)
kwh.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(kwh.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd13heatgas)$df, "Sigma" = summary(kwh.step.modelrd13heatgas)$sigma)
kwh.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(kwh.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd14heatgas)$df, "Sigma" = summary(kwh.step.modelrd14heatgas)$sigma)
kwh.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(kwh.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd15heatgas)$df, "Sigma" = summary(kwh.step.modelrd15heatgas)$sigma)
kwh.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(kwh.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd16heatgas)$df, "Sigma" = summary(kwh.step.modelrd16heatgas)$sigma)
kwh.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(kwh.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd17heatgas)$df, "Sigma" = summary(kwh.step.modelrd17heatgas)$sigma)
kwh.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(kwh.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd18heatgas)$df, "Sigma" = summary(kwh.step.modelrd18heatgas)$sigma)
kwh.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(kwh.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd19heatgas)$df, "Sigma" = summary(kwh.step.modelrd19heatgas)$sigma)
kwh.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(kwh.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd20heatgas)$df, "Sigma" = summary(kwh.step.modelrd20heatgas)$sigma)
kwh.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(kwh.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd21heatgas)$df, "Sigma" = summary(kwh.step.modelrd21heatgas)$sigma)
kwh.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(kwh.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd22heatgas)$df, "Sigma" = summary(kwh.step.modelrd22heatgas)$sigma)
kwh.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(kwh.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd23heatgas)$df, "Sigma" = summary(kwh.step.modelrd23heatgas)$sigma)
kwh.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(kwh.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd24heatgas)$df, "Sigma" = summary(kwh.step.modelrd24heatgas)$sigma)
kwh.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(kwh.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd25heatgas)$df, "Sigma" = summary(kwh.step.modelrd25heatgas)$sigma)
kwh.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(kwh.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd26heatgas)$df, "Sigma" = summary(kwh.step.modelrd26heatgas)$sigma)
kwh.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(kwh.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd27heatgas)$df, "Sigma" = summary(kwh.step.modelrd27heatgas)$sigma)
kwhoth.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd1heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd1heatgas)$sigma)
kwhoth.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd2heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd2heatgas)$sigma)
kwhoth.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd3heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd3heatgas)$sigma)
kwhoth.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd4heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd4heatgas)$sigma)
kwhoth.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd5heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd5heatgas)$sigma)
kwhoth.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd6heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd6heatgas)$sigma)
kwhoth.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd7heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd7heatgas)$sigma)
kwhoth.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd8heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd8heatgas)$sigma)
kwhoth.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd9heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd9heatgas)$sigma)
kwhoth.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd10heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd10heatgas)$sigma)
kwhoth.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd11heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd11heatgas)$sigma)
kwhoth.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd12heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd12heatgas)$sigma)
kwhoth.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd13heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd13heatgas)$sigma)
kwhoth.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd14heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd14heatgas)$sigma)
kwhoth.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd15heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd15heatgas)$sigma)
kwhoth.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd16heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd16heatgas)$sigma)
kwhoth.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd17heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd17heatgas)$sigma)
kwhoth.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd18heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd18heatgas)$sigma)
kwhoth.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd19heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd19heatgas)$sigma)
kwhoth.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd20heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd20heatgas)$sigma)
kwhoth.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd21heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd21heatgas)$sigma)
kwhoth.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd22heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd22heatgas)$sigma)
kwhoth.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd23heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd23heatgas)$sigma)
kwhoth.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd24heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd24heatgas)$sigma)
kwhoth.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd25heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd25heatgas)$sigma)
kwhoth.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd26heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd26heatgas)$sigma)
kwhoth.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(kwhoth.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd27heatgas)$df, "Sigma" = summary(kwhoth.step.modelrd27heatgas)$sigma)
kwhsph.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd1heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd1heatgas)$sigma)
kwhsph.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd2heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd2heatgas)$sigma)
kwhsph.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd3heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd3heatgas)$sigma)
kwhsph.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd4heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd4heatgas)$sigma)
kwhsph.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd5heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd5heatgas)$sigma)
kwhsph.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd6heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd6heatgas)$sigma)
kwhsph.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd7heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd7heatgas)$sigma)
kwhsph.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd8heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd8heatgas)$sigma)
kwhsph.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd9heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd9heatgas)$sigma)
kwhsph.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd10heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd10heatgas)$sigma)
kwhsph.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd11heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd11heatgas)$sigma)
kwhsph.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd12heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd12heatgas)$sigma)
kwhsph.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd13heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd13heatgas)$sigma)
kwhsph.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd14heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd14heatgas)$sigma)
kwhsph.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd15heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd15heatgas)$sigma)
kwhsph.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd16heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd16heatgas)$sigma)
kwhsph.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd17heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd17heatgas)$sigma)
kwhsph.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd18heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd18heatgas)$sigma)
kwhsph.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd19heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd19heatgas)$sigma)
kwhsph.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd20heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd20heatgas)$sigma)
kwhsph.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd21heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd21heatgas)$sigma)
kwhsph.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd22heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd22heatgas)$sigma)
kwhsph.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd23heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd23heatgas)$sigma)
kwhsph.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd24heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd24heatgas)$sigma)
kwhsph.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd25heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd25heatgas)$sigma)
kwhsph.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd26heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd26heatgas)$sigma)
kwhsph.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(kwhsph.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd27heatgas)$df, "Sigma" = summary(kwhsph.step.modelrd27heatgas)$sigma)
kwh.rd1heatkwh <- c("N" = nrow(dfrd1heatkwh), "AdjR2" = round(summary(kwh.step.modelrd1heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd1heatkwh)$df, "Sigma" = summary(kwh.step.modelrd1heatkwh)$sigma)
kwh.rd2heatkwh <- c("N" = nrow(dfrd2heatkwh), "AdjR2" = round(summary(kwh.step.modelrd2heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd2heatkwh)$df, "Sigma" = summary(kwh.step.modelrd2heatkwh)$sigma)
kwh.rd3heatkwh <- c("N" = nrow(dfrd3heatkwh), "AdjR2" = round(summary(kwh.step.modelrd3heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd3heatkwh)$df, "Sigma" = summary(kwh.step.modelrd3heatkwh)$sigma)
kwh.rd4heatkwh <- c("N" = nrow(dfrd4heatkwh), "AdjR2" = round(summary(kwh.step.modelrd4heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd4heatkwh)$df, "Sigma" = summary(kwh.step.modelrd4heatkwh)$sigma)
kwh.rd5heatkwh <- c("N" = nrow(dfrd5heatkwh), "AdjR2" = round(summary(kwh.step.modelrd5heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd5heatkwh)$df, "Sigma" = summary(kwh.step.modelrd5heatkwh)$sigma)
kwh.rd6heatkwh <- c("N" = nrow(dfrd6heatkwh), "AdjR2" = round(summary(kwh.step.modelrd6heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd6heatkwh)$df, "Sigma" = summary(kwh.step.modelrd6heatkwh)$sigma)
kwh.rd7heatkwh <- c("N" = nrow(dfrd7heatkwh), "AdjR2" = round(summary(kwh.step.modelrd7heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd7heatkwh)$df, "Sigma" = summary(kwh.step.modelrd7heatkwh)$sigma)
kwh.rd8heatkwh <- c("N" = nrow(dfrd8heatkwh), "AdjR2" = round(summary(kwh.step.modelrd8heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd8heatkwh)$df, "Sigma" = summary(kwh.step.modelrd8heatkwh)$sigma)
kwh.rd9heatkwh <- c("N" = nrow(dfrd9heatkwh), "AdjR2" = round(summary(kwh.step.modelrd9heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd9heatkwh)$df, "Sigma" = summary(kwh.step.modelrd9heatkwh)$sigma)
kwh.rd10heatkwh <- c("N" = nrow(dfrd10heatkwh), "AdjR2" = round(summary(kwh.step.modelrd10heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd10heatkwh)$df, "Sigma" = summary(kwh.step.modelrd10heatkwh)$sigma)
kwh.rd11heatkwh <- c("N" = nrow(dfrd11heatkwh), "AdjR2" = round(summary(kwh.step.modelrd11heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd11heatkwh)$df, "Sigma" = summary(kwh.step.modelrd11heatkwh)$sigma)
kwh.rd12heatkwh <- c("N" = nrow(dfrd12heatkwh), "AdjR2" = round(summary(kwh.step.modelrd12heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd12heatkwh)$df, "Sigma" = summary(kwh.step.modelrd12heatkwh)$sigma)
kwh.rd13heatkwh <- c("N" = nrow(dfrd13heatkwh), "AdjR2" = round(summary(kwh.step.modelrd13heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd13heatkwh)$df, "Sigma" = summary(kwh.step.modelrd13heatkwh)$sigma)
kwh.rd14heatkwh <- c("N" = nrow(dfrd14heatkwh), "AdjR2" = round(summary(kwh.step.modelrd14heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd14heatkwh)$df, "Sigma" = summary(kwh.step.modelrd14heatkwh)$sigma)
kwh.rd15heatkwh <- c("N" = nrow(dfrd15heatkwh), "AdjR2" = round(summary(kwh.step.modelrd15heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd15heatkwh)$df, "Sigma" = summary(kwh.step.modelrd15heatkwh)$sigma)
kwh.rd16heatkwh <- c("N" = nrow(dfrd16heatkwh), "AdjR2" = round(summary(kwh.step.modelrd16heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd16heatkwh)$df, "Sigma" = summary(kwh.step.modelrd16heatkwh)$sigma)
kwh.rd17heatkwh <- c("N" = nrow(dfrd17heatkwh), "AdjR2" = round(summary(kwh.step.modelrd17heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd17heatkwh)$df, "Sigma" = summary(kwh.step.modelrd17heatkwh)$sigma)
kwh.rd18heatkwh <- c("N" = nrow(dfrd18heatkwh), "AdjR2" = round(summary(kwh.step.modelrd18heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd18heatkwh)$df, "Sigma" = summary(kwh.step.modelrd18heatkwh)$sigma)
kwh.rd19heatkwh <- c("N" = nrow(dfrd19heatkwh), "AdjR2" = round(summary(kwh.step.modelrd19heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd19heatkwh)$df, "Sigma" = summary(kwh.step.modelrd19heatkwh)$sigma)
kwh.rd20heatkwh <- c("N" = nrow(dfrd20heatkwh), "AdjR2" = round(summary(kwh.step.modelrd20heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd20heatkwh)$df, "Sigma" = summary(kwh.step.modelrd20heatkwh)$sigma)
kwh.rd21heatkwh <- c("N" = nrow(dfrd21heatkwh), "AdjR2" = round(summary(kwh.step.modelrd21heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd21heatkwh)$df, "Sigma" = summary(kwh.step.modelrd21heatkwh)$sigma)
kwh.rd22heatkwh <- c("N" = nrow(dfrd22heatkwh), "AdjR2" = round(summary(kwh.step.modelrd22heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd22heatkwh)$df, "Sigma" = summary(kwh.step.modelrd22heatkwh)$sigma)
kwh.rd23heatkwh <- c("N" = nrow(dfrd23heatkwh), "AdjR2" = round(summary(kwh.step.modelrd23heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd23heatkwh)$df, "Sigma" = summary(kwh.step.modelrd23heatkwh)$sigma)
kwh.rd24heatkwh <- c("N" = nrow(dfrd24heatkwh), "AdjR2" = round(summary(kwh.step.modelrd24heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd24heatkwh)$df, "Sigma" = summary(kwh.step.modelrd24heatkwh)$sigma)
kwh.rd25heatkwh <- c("N" = nrow(dfrd25heatkwh), "AdjR2" = round(summary(kwh.step.modelrd25heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd25heatkwh)$df, "Sigma" = summary(kwh.step.modelrd25heatkwh)$sigma)
kwh.rd26heatkwh <- c("N" = nrow(dfrd26heatkwh), "AdjR2" = round(summary(kwh.step.modelrd26heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd26heatkwh)$df, "Sigma" = summary(kwh.step.modelrd26heatkwh)$sigma)
kwh.rd27heatkwh <- c("N" = nrow(dfrd27heatkwh), "AdjR2" = round(summary(kwh.step.modelrd27heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd27heatkwh)$df, "Sigma" = summary(kwh.step.modelrd27heatkwh)$sigma)
kwhoth.rd1heatkwh <- c("N" = nrow(dfrd1heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd1heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd1heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd1heatkwh)$sigma)
kwhoth.rd2heatkwh <- c("N" = nrow(dfrd2heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd2heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd2heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd2heatkwh)$sigma)
kwhoth.rd3heatkwh <- c("N" = nrow(dfrd3heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd3heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd3heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd3heatkwh)$sigma)
kwhoth.rd4heatkwh <- c("N" = nrow(dfrd4heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd4heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd4heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd4heatkwh)$sigma)
kwhoth.rd5heatkwh <- c("N" = nrow(dfrd5heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd5heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd5heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd5heatkwh)$sigma)
kwhoth.rd6heatkwh <- c("N" = nrow(dfrd6heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd6heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd6heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd6heatkwh)$sigma)
kwhoth.rd7heatkwh <- c("N" = nrow(dfrd7heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd7heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd7heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd7heatkwh)$sigma)
kwhoth.rd8heatkwh <- c("N" = nrow(dfrd8heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd8heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd8heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd8heatkwh)$sigma)
kwhoth.rd9heatkwh <- c("N" = nrow(dfrd9heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd9heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd9heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd9heatkwh)$sigma)
kwhoth.rd10heatkwh <- c("N" = nrow(dfrd10heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd10heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd10heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd10heatkwh)$sigma)
kwhoth.rd11heatkwh <- c("N" = nrow(dfrd11heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd11heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd11heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd11heatkwh)$sigma)
kwhoth.rd12heatkwh <- c("N" = nrow(dfrd12heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd12heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd12heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd12heatkwh)$sigma)
kwhoth.rd13heatkwh <- c("N" = nrow(dfrd13heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd13heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd13heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd13heatkwh)$sigma)
kwhoth.rd14heatkwh <- c("N" = nrow(dfrd14heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd14heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd14heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd14heatkwh)$sigma)
kwhoth.rd15heatkwh <- c("N" = nrow(dfrd15heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd15heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd15heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd15heatkwh)$sigma)
kwhoth.rd16heatkwh <- c("N" = nrow(dfrd16heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd16heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd16heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd16heatkwh)$sigma)
kwhoth.rd17heatkwh <- c("N" = nrow(dfrd17heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd17heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd17heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd17heatkwh)$sigma)
kwhoth.rd18heatkwh <- c("N" = nrow(dfrd18heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd18heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd18heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd18heatkwh)$sigma)
kwhoth.rd19heatkwh <- c("N" = nrow(dfrd19heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd19heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd19heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd19heatkwh)$sigma)
kwhoth.rd20heatkwh <- c("N" = nrow(dfrd20heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd20heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd20heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd20heatkwh)$sigma)
kwhoth.rd21heatkwh <- c("N" = nrow(dfrd21heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd21heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd21heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd21heatkwh)$sigma)
kwhoth.rd22heatkwh <- c("N" = nrow(dfrd22heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd22heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd22heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd22heatkwh)$sigma)
kwhoth.rd23heatkwh <- c("N" = nrow(dfrd23heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd23heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd23heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd23heatkwh)$sigma)
kwhoth.rd24heatkwh <- c("N" = nrow(dfrd24heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd24heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd24heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd24heatkwh)$sigma)
kwhoth.rd25heatkwh <- c("N" = nrow(dfrd25heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd25heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd25heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd25heatkwh)$sigma)
kwhoth.rd26heatkwh <- c("N" = nrow(dfrd26heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd26heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd26heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd26heatkwh)$sigma)
kwhoth.rd27heatkwh <- c("N" = nrow(dfrd27heatkwh), "AdjR2" = round(summary(kwhoth.step.modelrd27heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd27heatkwh)$df, "Sigma" = summary(kwhoth.step.modelrd27heatkwh)$sigma)
kwhsph.rd1heatkwh <- c("N" = nrow(dfrd1heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd1heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd1heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd1heatkwh)$sigma)
kwhsph.rd2heatkwh <- c("N" = nrow(dfrd2heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd2heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd2heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd2heatkwh)$sigma)
kwhsph.rd3heatkwh <- c("N" = nrow(dfrd3heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd3heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd3heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd3heatkwh)$sigma)
kwhsph.rd4heatkwh <- c("N" = nrow(dfrd4heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd4heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd4heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd4heatkwh)$sigma)
kwhsph.rd5heatkwh <- c("N" = nrow(dfrd5heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd5heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd5heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd5heatkwh)$sigma)
kwhsph.rd6heatkwh <- c("N" = nrow(dfrd6heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd6heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd6heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd6heatkwh)$sigma)
kwhsph.rd7heatkwh <- c("N" = nrow(dfrd7heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd7heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd7heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd7heatkwh)$sigma)
kwhsph.rd8heatkwh <- c("N" = nrow(dfrd8heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd8heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd8heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd8heatkwh)$sigma)
kwhsph.rd9heatkwh <- c("N" = nrow(dfrd9heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd9heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd9heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd9heatkwh)$sigma)
kwhsph.rd10heatkwh <- c("N" = nrow(dfrd10heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd10heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd10heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd10heatkwh)$sigma)
kwhsph.rd11heatkwh <- c("N" = nrow(dfrd11heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd11heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd11heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd11heatkwh)$sigma)
kwhsph.rd12heatkwh <- c("N" = nrow(dfrd12heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd12heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd12heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd12heatkwh)$sigma)
kwhsph.rd13heatkwh <- c("N" = nrow(dfrd13heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd13heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd13heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd13heatkwh)$sigma)
kwhsph.rd14heatkwh <- c("N" = nrow(dfrd14heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd14heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd14heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd14heatkwh)$sigma)
kwhsph.rd15heatkwh <- c("N" = nrow(dfrd15heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd15heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd15heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd15heatkwh)$sigma)
kwhsph.rd16heatkwh <- c("N" = nrow(dfrd16heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd16heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd16heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd16heatkwh)$sigma)
kwhsph.rd17heatkwh <- c("N" = nrow(dfrd17heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd17heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd17heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd17heatkwh)$sigma)
kwhsph.rd18heatkwh <- c("N" = nrow(dfrd18heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd18heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd18heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd18heatkwh)$sigma)
kwhsph.rd19heatkwh <- c("N" = nrow(dfrd19heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd19heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd19heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd19heatkwh)$sigma)
kwhsph.rd20heatkwh <- c("N" = nrow(dfrd20heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd20heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd20heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd20heatkwh)$sigma)
kwhsph.rd21heatkwh <- c("N" = nrow(dfrd21heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd21heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd21heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd21heatkwh)$sigma)
kwhsph.rd22heatkwh <- c("N" = nrow(dfrd22heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd22heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd22heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd22heatkwh)$sigma)
kwhsph.rd23heatkwh <- c("N" = nrow(dfrd23heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd23heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd23heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd23heatkwh)$sigma)
kwhsph.rd24heatkwh <- c("N" = nrow(dfrd24heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd24heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd24heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd24heatkwh)$sigma)
kwhsph.rd25heatkwh <- c("N" = nrow(dfrd25heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd25heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd25heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd25heatkwh)$sigma)
kwhsph.rd26heatkwh <- c("N" = nrow(dfrd26heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd26heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd26heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd26heatkwh)$sigma)
kwhsph.rd27heatkwh <- c("N" = nrow(dfrd27heatkwh), "AdjR2" = round(summary(kwhsph.step.modelrd27heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd27heatkwh)$df, "Sigma" = summary(kwhsph.step.modelrd27heatkwh)$sigma)
btuother.rd1heatoth <- c("N" = nrow(dfrd1heatoth), "AdjR2" = round(summary(btuother.step.modelrd1heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd1heatoth)$df, "Sigma" = summary(btuother.step.modelrd1heatoth)$sigma)
btuother.rd2heatoth <- c("N" = nrow(dfrd2heatoth), "AdjR2" = round(summary(btuother.step.modelrd2heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd2heatoth)$df, "Sigma" = summary(btuother.step.modelrd2heatoth)$sigma)
btuother.rd3heatoth <- c("N" = nrow(dfrd3heatoth), "AdjR2" = round(summary(btuother.step.modelrd3heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd3heatoth)$df, "Sigma" = summary(btuother.step.modelrd3heatoth)$sigma)
btuother.rd4heatoth <- c("N" = nrow(dfrd4heatoth), "AdjR2" = round(summary(btuother.step.modelrd4heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd4heatoth)$df, "Sigma" = summary(btuother.step.modelrd4heatoth)$sigma)
btuother.rd5heatoth <- c("N" = nrow(dfrd5heatoth), "AdjR2" = round(summary(btuother.step.modelrd5heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd5heatoth)$df, "Sigma" = summary(btuother.step.modelrd5heatoth)$sigma)
btuother.rd6heatoth <- c("N" = nrow(dfrd6heatoth), "AdjR2" = round(summary(btuother.step.modelrd6heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd6heatoth)$df, "Sigma" = summary(btuother.step.modelrd6heatoth)$sigma)
btuother.rd7heatoth <- c("N" = nrow(dfrd7heatoth), "AdjR2" = round(summary(btuother.step.modelrd7heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd7heatoth)$df, "Sigma" = summary(btuother.step.modelrd7heatoth)$sigma)
btuother.rd8heatoth <- c("N" = nrow(dfrd8heatoth), "AdjR2" = round(summary(btuother.step.modelrd8heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd8heatoth)$df, "Sigma" = summary(btuother.step.modelrd8heatoth)$sigma)
btuother.rd9heatoth <- c("N" = nrow(dfrd9heatoth), "AdjR2" = round(summary(btuother.step.modelrd9heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd9heatoth)$df, "Sigma" = summary(btuother.step.modelrd9heatoth)$sigma)
btuother.rd10heatoth <- c("N" = nrow(dfrd10heatoth), "AdjR2" = round(summary(btuother.step.modelrd10heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd10heatoth)$df, "Sigma" = summary(btuother.step.modelrd10heatoth)$sigma)
btuother.rd11heatoth <- c("N" = nrow(dfrd11heatoth), "AdjR2" = round(summary(btuother.step.modelrd11heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd11heatoth)$df, "Sigma" = summary(btuother.step.modelrd11heatoth)$sigma)
btuother.rd12heatoth <- c("N" = nrow(dfrd12heatoth), "AdjR2" = round(summary(btuother.step.modelrd12heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd12heatoth)$df, "Sigma" = summary(btuother.step.modelrd12heatoth)$sigma)
btuother.rd13heatoth <- c("N" = nrow(dfrd13heatoth), "AdjR2" = round(summary(btuother.step.modelrd13heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd13heatoth)$df, "Sigma" = summary(btuother.step.modelrd13heatoth)$sigma)
btuother.rd14heatoth <- c("N" = nrow(dfrd14heatoth), "AdjR2" = round(summary(btuother.step.modelrd14heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd14heatoth)$df, "Sigma" = summary(btuother.step.modelrd14heatoth)$sigma)
btuother.rd15heatoth <- c("N" = nrow(dfrd15heatoth), "AdjR2" = round(summary(btuother.step.modelrd15heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd15heatoth)$df, "Sigma" = summary(btuother.step.modelrd15heatoth)$sigma)
btuother.rd16heatoth <- c("N" = nrow(dfrd16heatoth), "AdjR2" = round(summary(btuother.step.modelrd16heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd16heatoth)$df, "Sigma" = summary(btuother.step.modelrd16heatoth)$sigma)
btuother.rd17heatoth <- c("N" = nrow(dfrd17heatoth), "AdjR2" = round(summary(btuother.step.modelrd17heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd17heatoth)$df, "Sigma" = summary(btuother.step.modelrd17heatoth)$sigma)
btuother.rd18heatoth <- c("N" = nrow(dfrd18heatoth), "AdjR2" = round(summary(btuother.step.modelrd18heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd18heatoth)$df, "Sigma" = summary(btuother.step.modelrd18heatoth)$sigma)
btuother.rd19heatoth <- c("N" = nrow(dfrd19heatoth), "AdjR2" = round(summary(btuother.step.modelrd19heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd19heatoth)$df, "Sigma" = summary(btuother.step.modelrd19heatoth)$sigma)
btuother.rd20heatoth <- c("N" = nrow(dfrd20heatoth), "AdjR2" = round(summary(btuother.step.modelrd20heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd20heatoth)$df, "Sigma" = summary(btuother.step.modelrd20heatoth)$sigma)
btuother.rd21heatoth <- c("N" = nrow(dfrd21heatoth), "AdjR2" = round(summary(btuother.step.modelrd21heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd21heatoth)$df, "Sigma" = summary(btuother.step.modelrd21heatoth)$sigma)
btuother.rd22heatoth <- c("N" = nrow(dfrd22heatoth), "AdjR2" = round(summary(btuother.step.modelrd22heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd22heatoth)$df, "Sigma" = summary(btuother.step.modelrd22heatoth)$sigma)
btuother.rd23heatoth <- c("N" = nrow(dfrd23heatoth), "AdjR2" = round(summary(btuother.step.modelrd23heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd23heatoth)$df, "Sigma" = summary(btuother.step.modelrd23heatoth)$sigma)
btuother.rd24heatoth <- c("N" = nrow(dfrd24heatoth), "AdjR2" = round(summary(btuother.step.modelrd24heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd24heatoth)$df, "Sigma" = summary(btuother.step.modelrd24heatoth)$sigma)
btuother.rd25heatoth <- c("N" = nrow(dfrd25heatoth), "AdjR2" = round(summary(btuother.step.modelrd25heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd25heatoth)$df, "Sigma" = summary(btuother.step.modelrd25heatoth)$sigma)
btuother.rd26heatoth <- c("N" = nrow(dfrd26heatoth), "AdjR2" = round(summary(btuother.step.modelrd26heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd26heatoth)$df, "Sigma" = summary(btuother.step.modelrd26heatoth)$sigma)
btuother.rd27heatoth <- c("N" = nrow(dfrd27heatoth), "AdjR2" = round(summary(btuother.step.modelrd27heatoth)$adj.r.squared, digits = 3), "DF" = summary(btuother.step.modelrd27heatoth)$df, "Sigma" = summary(btuother.step.modelrd27heatoth)$sigma)
kwh.rd1heatoth <- c("N" = nrow(dfrd1heatoth), "AdjR2" = round(summary(kwh.step.modelrd1heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd1heatoth)$df, "Sigma" = summary(kwh.step.modelrd1heatoth)$sigma)
kwh.rd2heatoth <- c("N" = nrow(dfrd2heatoth), "AdjR2" = round(summary(kwh.step.modelrd2heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd2heatoth)$df, "Sigma" = summary(kwh.step.modelrd2heatoth)$sigma)
kwh.rd3heatoth <- c("N" = nrow(dfrd3heatoth), "AdjR2" = round(summary(kwh.step.modelrd3heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd3heatoth)$df, "Sigma" = summary(kwh.step.modelrd3heatoth)$sigma)
kwh.rd4heatoth <- c("N" = nrow(dfrd4heatoth), "AdjR2" = round(summary(kwh.step.modelrd4heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd4heatoth)$df, "Sigma" = summary(kwh.step.modelrd4heatoth)$sigma)
kwh.rd5heatoth <- c("N" = nrow(dfrd5heatoth), "AdjR2" = round(summary(kwh.step.modelrd5heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd5heatoth)$df, "Sigma" = summary(kwh.step.modelrd5heatoth)$sigma)
kwh.rd6heatoth <- c("N" = nrow(dfrd6heatoth), "AdjR2" = round(summary(kwh.step.modelrd6heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd6heatoth)$df, "Sigma" = summary(kwh.step.modelrd6heatoth)$sigma)
kwh.rd7heatoth <- c("N" = nrow(dfrd7heatoth), "AdjR2" = round(summary(kwh.step.modelrd7heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd7heatoth)$df, "Sigma" = summary(kwh.step.modelrd7heatoth)$sigma)
kwh.rd8heatoth <- c("N" = nrow(dfrd8heatoth), "AdjR2" = round(summary(kwh.step.modelrd8heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd8heatoth)$df, "Sigma" = summary(kwh.step.modelrd8heatoth)$sigma)
kwh.rd9heatoth <- c("N" = nrow(dfrd9heatoth), "AdjR2" = round(summary(kwh.step.modelrd9heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd9heatoth)$df, "Sigma" = summary(kwh.step.modelrd9heatoth)$sigma)
kwh.rd10heatoth <- c("N" = nrow(dfrd10heatoth), "AdjR2" = round(summary(kwh.step.modelrd10heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd10heatoth)$df, "Sigma" = summary(kwh.step.modelrd10heatoth)$sigma)
kwh.rd11heatoth <- c("N" = nrow(dfrd11heatoth), "AdjR2" = round(summary(kwh.step.modelrd11heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd11heatoth)$df, "Sigma" = summary(kwh.step.modelrd11heatoth)$sigma)
kwh.rd12heatoth <- c("N" = nrow(dfrd12heatoth), "AdjR2" = round(summary(kwh.step.modelrd12heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd12heatoth)$df, "Sigma" = summary(kwh.step.modelrd12heatoth)$sigma)
kwh.rd13heatoth <- c("N" = nrow(dfrd13heatoth), "AdjR2" = round(summary(kwh.step.modelrd13heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd13heatoth)$df, "Sigma" = summary(kwh.step.modelrd13heatoth)$sigma)
kwh.rd14heatoth <- c("N" = nrow(dfrd14heatoth), "AdjR2" = round(summary(kwh.step.modelrd14heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd14heatoth)$df, "Sigma" = summary(kwh.step.modelrd14heatoth)$sigma)
kwh.rd15heatoth <- c("N" = nrow(dfrd15heatoth), "AdjR2" = round(summary(kwh.step.modelrd15heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd15heatoth)$df, "Sigma" = summary(kwh.step.modelrd15heatoth)$sigma)
kwh.rd16heatoth <- c("N" = nrow(dfrd16heatoth), "AdjR2" = round(summary(kwh.step.modelrd16heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd16heatoth)$df, "Sigma" = summary(kwh.step.modelrd16heatoth)$sigma)
kwh.rd17heatoth <- c("N" = nrow(dfrd17heatoth), "AdjR2" = round(summary(kwh.step.modelrd17heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd17heatoth)$df, "Sigma" = summary(kwh.step.modelrd17heatoth)$sigma)
kwh.rd18heatoth <- c("N" = nrow(dfrd18heatoth), "AdjR2" = round(summary(kwh.step.modelrd18heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd18heatoth)$df, "Sigma" = summary(kwh.step.modelrd18heatoth)$sigma)
kwh.rd19heatoth <- c("N" = nrow(dfrd19heatoth), "AdjR2" = round(summary(kwh.step.modelrd19heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd19heatoth)$df, "Sigma" = summary(kwh.step.modelrd19heatoth)$sigma)
kwh.rd20heatoth <- c("N" = nrow(dfrd20heatoth), "AdjR2" = round(summary(kwh.step.modelrd20heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd20heatoth)$df, "Sigma" = summary(kwh.step.modelrd20heatoth)$sigma)
kwh.rd21heatoth <- c("N" = nrow(dfrd21heatoth), "AdjR2" = round(summary(kwh.step.modelrd21heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd21heatoth)$df, "Sigma" = summary(kwh.step.modelrd21heatoth)$sigma)
kwh.rd22heatoth <- c("N" = nrow(dfrd22heatoth), "AdjR2" = round(summary(kwh.step.modelrd22heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd22heatoth)$df, "Sigma" = summary(kwh.step.modelrd22heatoth)$sigma)
kwh.rd23heatoth <- c("N" = nrow(dfrd23heatoth), "AdjR2" = round(summary(kwh.step.modelrd23heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd23heatoth)$df, "Sigma" = summary(kwh.step.modelrd23heatoth)$sigma)
kwh.rd24heatoth <- c("N" = nrow(dfrd24heatoth), "AdjR2" = round(summary(kwh.step.modelrd24heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd24heatoth)$df, "Sigma" = summary(kwh.step.modelrd24heatoth)$sigma)
kwh.rd25heatoth <- c("N" = nrow(dfrd25heatoth), "AdjR2" = round(summary(kwh.step.modelrd25heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd25heatoth)$df, "Sigma" = summary(kwh.step.modelrd25heatoth)$sigma)
kwh.rd26heatoth <- c("N" = nrow(dfrd26heatoth), "AdjR2" = round(summary(kwh.step.modelrd26heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd26heatoth)$df, "Sigma" = summary(kwh.step.modelrd26heatoth)$sigma)
kwh.rd27heatoth <- c("N" = nrow(dfrd27heatoth), "AdjR2" = round(summary(kwh.step.modelrd27heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.modelrd27heatoth)$df, "Sigma" = summary(kwh.step.modelrd27heatoth)$sigma)
kwhoth.rd1heatoth <- c("N" = nrow(dfrd1heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd1heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd1heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd1heatoth)$sigma)
kwhoth.rd2heatoth <- c("N" = nrow(dfrd2heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd2heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd2heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd2heatoth)$sigma)
kwhoth.rd3heatoth <- c("N" = nrow(dfrd3heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd3heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd3heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd3heatoth)$sigma)
kwhoth.rd4heatoth <- c("N" = nrow(dfrd4heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd4heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd4heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd4heatoth)$sigma)
kwhoth.rd5heatoth <- c("N" = nrow(dfrd5heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd5heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd5heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd5heatoth)$sigma)
kwhoth.rd6heatoth <- c("N" = nrow(dfrd6heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd6heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd6heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd6heatoth)$sigma)
kwhoth.rd7heatoth <- c("N" = nrow(dfrd7heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd7heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd7heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd7heatoth)$sigma)
kwhoth.rd8heatoth <- c("N" = nrow(dfrd8heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd8heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd8heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd8heatoth)$sigma)
kwhoth.rd9heatoth <- c("N" = nrow(dfrd9heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd9heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd9heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd9heatoth)$sigma)
kwhoth.rd10heatoth <- c("N" = nrow(dfrd10heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd10heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd10heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd10heatoth)$sigma)
kwhoth.rd11heatoth <- c("N" = nrow(dfrd11heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd11heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd11heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd11heatoth)$sigma)
kwhoth.rd12heatoth <- c("N" = nrow(dfrd12heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd12heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd12heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd12heatoth)$sigma)
kwhoth.rd13heatoth <- c("N" = nrow(dfrd13heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd13heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd13heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd13heatoth)$sigma)
kwhoth.rd14heatoth <- c("N" = nrow(dfrd14heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd14heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd14heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd14heatoth)$sigma)
kwhoth.rd15heatoth <- c("N" = nrow(dfrd15heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd15heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd15heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd15heatoth)$sigma)
kwhoth.rd16heatoth <- c("N" = nrow(dfrd16heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd16heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd16heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd16heatoth)$sigma)
kwhoth.rd17heatoth <- c("N" = nrow(dfrd17heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd17heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd17heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd17heatoth)$sigma)
kwhoth.rd18heatoth <- c("N" = nrow(dfrd18heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd18heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd18heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd18heatoth)$sigma)
kwhoth.rd19heatoth <- c("N" = nrow(dfrd19heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd19heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd19heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd19heatoth)$sigma)
kwhoth.rd20heatoth <- c("N" = nrow(dfrd20heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd20heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd20heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd20heatoth)$sigma)
kwhoth.rd21heatoth <- c("N" = nrow(dfrd21heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd21heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd21heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd21heatoth)$sigma)
kwhoth.rd22heatoth <- c("N" = nrow(dfrd22heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd22heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd22heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd22heatoth)$sigma)
kwhoth.rd23heatoth <- c("N" = nrow(dfrd23heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd23heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd23heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd23heatoth)$sigma)
kwhoth.rd24heatoth <- c("N" = nrow(dfrd24heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd24heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd24heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd24heatoth)$sigma)
kwhoth.rd25heatoth <- c("N" = nrow(dfrd25heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd25heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd25heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd25heatoth)$sigma)
kwhoth.rd26heatoth <- c("N" = nrow(dfrd26heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd26heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd26heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd26heatoth)$sigma)
kwhoth.rd27heatoth <- c("N" = nrow(dfrd27heatoth), "AdjR2" = round(summary(kwhoth.step.modelrd27heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhoth.step.modelrd27heatoth)$df, "Sigma" = summary(kwhoth.step.modelrd27heatoth)$sigma)
kwhsph.rd1heatoth <- c("N" = nrow(dfrd1heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd1heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd1heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd1heatoth)$sigma)
kwhsph.rd2heatoth <- c("N" = nrow(dfrd2heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd2heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd2heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd2heatoth)$sigma)
kwhsph.rd3heatoth <- c("N" = nrow(dfrd3heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd3heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd3heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd3heatoth)$sigma)
kwhsph.rd4heatoth <- c("N" = nrow(dfrd4heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd4heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd4heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd4heatoth)$sigma)
kwhsph.rd5heatoth <- c("N" = nrow(dfrd5heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd5heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd5heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd5heatoth)$sigma)
kwhsph.rd6heatoth <- c("N" = nrow(dfrd6heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd6heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd6heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd6heatoth)$sigma)
kwhsph.rd7heatoth <- c("N" = nrow(dfrd7heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd7heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd7heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd7heatoth)$sigma)
kwhsph.rd8heatoth <- c("N" = nrow(dfrd8heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd8heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd8heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd8heatoth)$sigma)
kwhsph.rd9heatoth <- c("N" = nrow(dfrd9heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd9heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd9heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd9heatoth)$sigma)
kwhsph.rd10heatoth <- c("N" = nrow(dfrd10heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd10heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd10heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd10heatoth)$sigma)
kwhsph.rd11heatoth <- c("N" = nrow(dfrd11heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd11heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd11heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd11heatoth)$sigma)
kwhsph.rd12heatoth <- c("N" = nrow(dfrd12heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd12heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd12heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd12heatoth)$sigma)
kwhsph.rd13heatoth <- c("N" = nrow(dfrd13heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd13heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd13heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd13heatoth)$sigma)
kwhsph.rd14heatoth <- c("N" = nrow(dfrd14heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd14heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd14heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd14heatoth)$sigma)
kwhsph.rd15heatoth <- c("N" = nrow(dfrd15heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd15heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd15heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd15heatoth)$sigma)
kwhsph.rd16heatoth <- c("N" = nrow(dfrd16heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd16heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd16heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd16heatoth)$sigma)
kwhsph.rd17heatoth <- c("N" = nrow(dfrd17heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd17heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd17heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd17heatoth)$sigma)
kwhsph.rd18heatoth <- c("N" = nrow(dfrd18heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd18heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd18heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd18heatoth)$sigma)
kwhsph.rd19heatoth <- c("N" = nrow(dfrd19heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd19heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd19heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd19heatoth)$sigma)
kwhsph.rd20heatoth <- c("N" = nrow(dfrd20heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd20heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd20heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd20heatoth)$sigma)
kwhsph.rd21heatoth <- c("N" = nrow(dfrd21heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd21heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd21heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd21heatoth)$sigma)
kwhsph.rd22heatoth <- c("N" = nrow(dfrd22heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd22heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd22heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd22heatoth)$sigma)
kwhsph.rd23heatoth <- c("N" = nrow(dfrd23heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd23heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd23heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd23heatoth)$sigma)
kwhsph.rd24heatoth <- c("N" = nrow(dfrd24heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd24heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd24heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd24heatoth)$sigma)
kwhsph.rd25heatoth <- c("N" = nrow(dfrd25heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd25heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd25heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd25heatoth)$sigma)
kwhsph.rd26heatoth <- c("N" = nrow(dfrd26heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd26heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd26heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd26heatoth)$sigma)
kwhsph.rd27heatoth <- c("N" = nrow(dfrd27heatoth), "AdjR2" = round(summary(kwhsph.step.modelrd27heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhsph.step.modelrd27heatoth)$df, "Sigma" = summary(kwhsph.step.modelrd27heatoth)$sigma)
kwhcol.rd1heatany <- c("N" = nrow(dfrd1heatany), "AdjR2" = round(summary(kwhcol.step.modelrd1heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd1heatany)$df, "Sigma" = summary(kwhcol.step.modelrd1heatany)$sigma)
kwhcol.rd2heatany <- c("N" = nrow(dfrd2heatany), "AdjR2" = round(summary(kwhcol.step.modelrd2heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd2heatany)$df, "Sigma" = summary(kwhcol.step.modelrd2heatany)$sigma)
kwhcol.rd3heatany <- c("N" = nrow(dfrd3heatany), "AdjR2" = round(summary(kwhcol.step.modelrd3heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd3heatany)$df, "Sigma" = summary(kwhcol.step.modelrd3heatany)$sigma)
kwhcol.rd4heatany <- c("N" = nrow(dfrd4heatany), "AdjR2" = round(summary(kwhcol.step.modelrd4heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd4heatany)$df, "Sigma" = summary(kwhcol.step.modelrd4heatany)$sigma)
kwhcol.rd5heatany <- c("N" = nrow(dfrd5heatany), "AdjR2" = round(summary(kwhcol.step.modelrd5heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd5heatany)$df, "Sigma" = summary(kwhcol.step.modelrd5heatany)$sigma)
kwhcol.rd6heatany <- c("N" = nrow(dfrd6heatany), "AdjR2" = round(summary(kwhcol.step.modelrd6heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd6heatany)$df, "Sigma" = summary(kwhcol.step.modelrd6heatany)$sigma)
kwhcol.rd7heatany <- c("N" = nrow(dfrd7heatany), "AdjR2" = round(summary(kwhcol.step.modelrd7heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd7heatany)$df, "Sigma" = summary(kwhcol.step.modelrd7heatany)$sigma)
kwhcol.rd8heatany <- c("N" = nrow(dfrd8heatany), "AdjR2" = round(summary(kwhcol.step.modelrd8heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd8heatany)$df, "Sigma" = summary(kwhcol.step.modelrd8heatany)$sigma)
kwhcol.rd9heatany <- c("N" = nrow(dfrd9heatany), "AdjR2" = round(summary(kwhcol.step.modelrd9heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd9heatany)$df, "Sigma" = summary(kwhcol.step.modelrd9heatany)$sigma)
kwhcol.rd10heatany <- c("N" = nrow(dfrd10heatany), "AdjR2" = round(summary(kwhcol.step.modelrd10heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd10heatany)$df, "Sigma" = summary(kwhcol.step.modelrd10heatany)$sigma)
kwhcol.rd11heatany <- c("N" = nrow(dfrd11heatany), "AdjR2" = round(summary(kwhcol.step.modelrd11heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd11heatany)$df, "Sigma" = summary(kwhcol.step.modelrd11heatany)$sigma)
kwhcol.rd12heatany <- c("N" = nrow(dfrd12heatany), "AdjR2" = round(summary(kwhcol.step.modelrd12heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd12heatany)$df, "Sigma" = summary(kwhcol.step.modelrd12heatany)$sigma)
kwhcol.rd13heatany <- c("N" = nrow(dfrd13heatany), "AdjR2" = round(summary(kwhcol.step.modelrd13heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd13heatany)$df, "Sigma" = summary(kwhcol.step.modelrd13heatany)$sigma)
kwhcol.rd14heatany <- c("N" = nrow(dfrd14heatany), "AdjR2" = round(summary(kwhcol.step.modelrd14heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd14heatany)$df, "Sigma" = summary(kwhcol.step.modelrd14heatany)$sigma)
kwhcol.rd15heatany <- c("N" = nrow(dfrd15heatany), "AdjR2" = round(summary(kwhcol.step.modelrd15heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd15heatany)$df, "Sigma" = summary(kwhcol.step.modelrd15heatany)$sigma)
kwhcol.rd16heatany <- c("N" = nrow(dfrd16heatany), "AdjR2" = round(summary(kwhcol.step.modelrd16heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd16heatany)$df, "Sigma" = summary(kwhcol.step.modelrd16heatany)$sigma)
kwhcol.rd17heatany <- c("N" = nrow(dfrd17heatany), "AdjR2" = round(summary(kwhcol.step.modelrd17heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd17heatany)$df, "Sigma" = summary(kwhcol.step.modelrd17heatany)$sigma)
kwhcol.rd18heatany <- c("N" = nrow(dfrd18heatany), "AdjR2" = round(summary(kwhcol.step.modelrd18heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd18heatany)$df, "Sigma" = summary(kwhcol.step.modelrd18heatany)$sigma)
kwhcol.rd19heatany <- c("N" = nrow(dfrd19heatany), "AdjR2" = round(summary(kwhcol.step.modelrd19heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd19heatany)$df, "Sigma" = summary(kwhcol.step.modelrd19heatany)$sigma)
kwhcol.rd20heatany <- c("N" = nrow(dfrd20heatany), "AdjR2" = round(summary(kwhcol.step.modelrd20heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd20heatany)$df, "Sigma" = summary(kwhcol.step.modelrd20heatany)$sigma)
kwhcol.rd21heatany <- c("N" = nrow(dfrd21heatany), "AdjR2" = round(summary(kwhcol.step.modelrd21heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd21heatany)$df, "Sigma" = summary(kwhcol.step.modelrd21heatany)$sigma)
kwhcol.rd22heatany <- c("N" = nrow(dfrd22heatany), "AdjR2" = round(summary(kwhcol.step.modelrd22heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd22heatany)$df, "Sigma" = summary(kwhcol.step.modelrd22heatany)$sigma)
kwhcol.rd23heatany <- c("N" = nrow(dfrd23heatany), "AdjR2" = round(summary(kwhcol.step.modelrd23heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd23heatany)$df, "Sigma" = summary(kwhcol.step.modelrd23heatany)$sigma)
kwhcol.rd24heatany <- c("N" = nrow(dfrd24heatany), "AdjR2" = round(summary(kwhcol.step.modelrd24heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd24heatany)$df, "Sigma" = summary(kwhcol.step.modelrd24heatany)$sigma)
kwhcol.rd25heatany <- c("N" = nrow(dfrd25heatany), "AdjR2" = round(summary(kwhcol.step.modelrd25heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd25heatany)$df, "Sigma" = summary(kwhcol.step.modelrd25heatany)$sigma)
kwhcol.rd26heatany <- c("N" = nrow(dfrd26heatany), "AdjR2" = round(summary(kwhcol.step.modelrd26heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd26heatany)$df, "Sigma" = summary(kwhcol.step.modelrd26heatany)$sigma)
kwhcol.rd27heatany <- c("N" = nrow(dfrd27heatany), "AdjR2" = round(summary(kwhcol.step.modelrd27heatany)$adj.r.squared, digits = 3), "DF" = summary(kwhcol.step.modelrd27heatany)$df, "Sigma" = summary(kwhcol.step.modelrd27heatany)$sigma)
btulp.rd1heatany <- c("N" = nrow(dfrd1heatany), "AdjR2" = round(summary(btulp.step.modelrd1heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd1heatany)$df, "Sigma" = summary(btulp.step.modelrd1heatany)$sigma)
btulp.rd2heatany <- c("N" = nrow(dfrd2heatany), "AdjR2" = round(summary(btulp.step.modelrd2heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd2heatany)$df, "Sigma" = summary(btulp.step.modelrd2heatany)$sigma)
btulp.rd3heatany <- c("N" = nrow(dfrd3heatany), "AdjR2" = round(summary(btulp.step.modelrd3heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd3heatany)$df, "Sigma" = summary(btulp.step.modelrd3heatany)$sigma)
btulp.rd4heatany <- c("N" = nrow(dfrd4heatany), "AdjR2" = round(summary(btulp.step.modelrd4heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd4heatany)$df, "Sigma" = summary(btulp.step.modelrd4heatany)$sigma)
btulp.rd5heatany <- c("N" = nrow(dfrd5heatany), "AdjR2" = round(summary(btulp.step.modelrd5heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd5heatany)$df, "Sigma" = summary(btulp.step.modelrd5heatany)$sigma)
btulp.rd6heatany <- c("N" = nrow(dfrd6heatany), "AdjR2" = round(summary(btulp.step.modelrd6heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd6heatany)$df, "Sigma" = summary(btulp.step.modelrd6heatany)$sigma)
btulp.rd7heatany <- c("N" = nrow(dfrd7heatany), "AdjR2" = round(summary(btulp.step.modelrd7heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd7heatany)$df, "Sigma" = summary(btulp.step.modelrd7heatany)$sigma)
btulp.rd8heatany <- c("N" = nrow(dfrd8heatany), "AdjR2" = round(summary(btulp.step.modelrd8heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd8heatany)$df, "Sigma" = summary(btulp.step.modelrd8heatany)$sigma)
btulp.rd9heatany <- c("N" = nrow(dfrd9heatany), "AdjR2" = round(summary(btulp.step.modelrd9heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd9heatany)$df, "Sigma" = summary(btulp.step.modelrd9heatany)$sigma)
btulp.rd10heatany <- c("N" = nrow(dfrd10heatany), "AdjR2" = round(summary(btulp.step.modelrd10heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd10heatany)$df, "Sigma" = summary(btulp.step.modelrd10heatany)$sigma)
btulp.rd11heatany <- c("N" = nrow(dfrd11heatany), "AdjR2" = round(summary(btulp.step.modelrd11heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd11heatany)$df, "Sigma" = summary(btulp.step.modelrd11heatany)$sigma)
btulp.rd12heatany <- c("N" = nrow(dfrd12heatany), "AdjR2" = round(summary(btulp.step.modelrd12heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd12heatany)$df, "Sigma" = summary(btulp.step.modelrd12heatany)$sigma)
btulp.rd13heatany <- c("N" = nrow(dfrd13heatany), "AdjR2" = round(summary(btulp.step.modelrd13heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd13heatany)$df, "Sigma" = summary(btulp.step.modelrd13heatany)$sigma)
btulp.rd14heatany <- c("N" = nrow(dfrd14heatany), "AdjR2" = round(summary(btulp.step.modelrd14heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd14heatany)$df, "Sigma" = summary(btulp.step.modelrd14heatany)$sigma)
btulp.rd15heatany <- c("N" = nrow(dfrd15heatany), "AdjR2" = round(summary(btulp.step.modelrd15heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd15heatany)$df, "Sigma" = summary(btulp.step.modelrd15heatany)$sigma)
btulp.rd16heatany <- c("N" = nrow(dfrd16heatany), "AdjR2" = round(summary(btulp.step.modelrd16heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd16heatany)$df, "Sigma" = summary(btulp.step.modelrd16heatany)$sigma)
btulp.rd17heatany <- c("N" = nrow(dfrd17heatany), "AdjR2" = round(summary(btulp.step.modelrd17heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd17heatany)$df, "Sigma" = summary(btulp.step.modelrd17heatany)$sigma)
btulp.rd18heatany <- c("N" = nrow(dfrd18heatany), "AdjR2" = round(summary(btulp.step.modelrd18heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd18heatany)$df, "Sigma" = summary(btulp.step.modelrd18heatany)$sigma)
btulp.rd19heatany <- c("N" = nrow(dfrd19heatany), "AdjR2" = round(summary(btulp.step.modelrd19heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd19heatany)$df, "Sigma" = summary(btulp.step.modelrd19heatany)$sigma)
btulp.rd20heatany <- c("N" = nrow(dfrd20heatany), "AdjR2" = round(summary(btulp.step.modelrd20heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd20heatany)$df, "Sigma" = summary(btulp.step.modelrd20heatany)$sigma)
btulp.rd21heatany <- c("N" = nrow(dfrd21heatany), "AdjR2" = round(summary(btulp.step.modelrd21heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd21heatany)$df, "Sigma" = summary(btulp.step.modelrd21heatany)$sigma)
btulp.rd22heatany <- c("N" = nrow(dfrd22heatany), "AdjR2" = round(summary(btulp.step.modelrd22heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd22heatany)$df, "Sigma" = summary(btulp.step.modelrd22heatany)$sigma)
btulp.rd23heatany <- c("N" = nrow(dfrd23heatany), "AdjR2" = round(summary(btulp.step.modelrd23heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd23heatany)$df, "Sigma" = summary(btulp.step.modelrd23heatany)$sigma)
btulp.rd24heatany <- c("N" = nrow(dfrd24heatany), "AdjR2" = round(summary(btulp.step.modelrd24heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd24heatany)$df, "Sigma" = summary(btulp.step.modelrd24heatany)$sigma)
btulp.rd25heatany <- c("N" = nrow(dfrd25heatany), "AdjR2" = round(summary(btulp.step.modelrd25heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd25heatany)$df, "Sigma" = summary(btulp.step.modelrd25heatany)$sigma)
btulp.rd26heatany <- c("N" = nrow(dfrd26heatany), "AdjR2" = round(summary(btulp.step.modelrd26heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd26heatany)$df, "Sigma" = summary(btulp.step.modelrd26heatany)$sigma)
btulp.rd27heatany <- c("N" = nrow(dfrd27heatany), "AdjR2" = round(summary(btulp.step.modelrd27heatany)$adj.r.squared, digits = 3), "DF" = summary(btulp.step.modelrd27heatany)$df, "Sigma" = summary(btulp.step.modelrd27heatany)$sigma)
kwhwth.rd1heatgas <- c("N" = nrow(dfrd1heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd1heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd1heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd1heatgas)$sigma)
kwhwth.rd2heatgas <- c("N" = nrow(dfrd2heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd2heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd2heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd2heatgas)$sigma)
kwhwth.rd3heatgas <- c("N" = nrow(dfrd3heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd3heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd3heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd3heatgas)$sigma)
kwhwth.rd4heatgas <- c("N" = nrow(dfrd4heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd4heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd4heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd4heatgas)$sigma)
kwhwth.rd5heatgas <- c("N" = nrow(dfrd5heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd5heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd5heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd5heatgas)$sigma)
kwhwth.rd6heatgas <- c("N" = nrow(dfrd6heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd6heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd6heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd6heatgas)$sigma)
kwhwth.rd7heatgas <- c("N" = nrow(dfrd7heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd7heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd7heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd7heatgas)$sigma)
kwhwth.rd8heatgas <- c("N" = nrow(dfrd8heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd8heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd8heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd8heatgas)$sigma)
kwhwth.rd9heatgas <- c("N" = nrow(dfrd9heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd9heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd9heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd9heatgas)$sigma)
kwhwth.rd10heatgas <- c("N" = nrow(dfrd10heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd10heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd10heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd10heatgas)$sigma)
kwhwth.rd11heatgas <- c("N" = nrow(dfrd11heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd11heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd11heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd11heatgas)$sigma)
kwhwth.rd12heatgas <- c("N" = nrow(dfrd12heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd12heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd12heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd12heatgas)$sigma)
kwhwth.rd13heatgas <- c("N" = nrow(dfrd13heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd13heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd13heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd13heatgas)$sigma)
kwhwth.rd14heatgas <- c("N" = nrow(dfrd14heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd14heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd14heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd14heatgas)$sigma)
kwhwth.rd15heatgas <- c("N" = nrow(dfrd15heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd15heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd15heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd15heatgas)$sigma)
kwhwth.rd16heatgas <- c("N" = nrow(dfrd16heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd16heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd16heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd16heatgas)$sigma)
kwhwth.rd17heatgas <- c("N" = nrow(dfrd17heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd17heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd17heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd17heatgas)$sigma)
kwhwth.rd18heatgas <- c("N" = nrow(dfrd18heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd18heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd18heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd18heatgas)$sigma)
kwhwth.rd19heatgas <- c("N" = nrow(dfrd19heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd19heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd19heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd19heatgas)$sigma)
kwhwth.rd20heatgas <- c("N" = nrow(dfrd20heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd20heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd20heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd20heatgas)$sigma)
kwhwth.rd21heatgas <- c("N" = nrow(dfrd21heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd21heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd21heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd21heatgas)$sigma)
kwhwth.rd22heatgas <- c("N" = nrow(dfrd22heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd22heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd22heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd22heatgas)$sigma)
kwhwth.rd23heatgas <- c("N" = nrow(dfrd23heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd23heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd23heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd23heatgas)$sigma)
kwhwth.rd24heatgas <- c("N" = nrow(dfrd24heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd24heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd24heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd24heatgas)$sigma)
kwhwth.rd25heatgas <- c("N" = nrow(dfrd25heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd25heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd25heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd25heatgas)$sigma)
kwhwth.rd26heatgas <- c("N" = nrow(dfrd26heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd26heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd26heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd26heatgas)$sigma)
kwhwth.rd27heatgas <- c("N" = nrow(dfrd27heatgas), "AdjR2" = round(summary(kwhwth.step.modelrd27heatgas)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd27heatgas)$df, "Sigma" = summary(kwhwth.step.modelrd27heatgas)$sigma)
kwhwth.rd1heatkwh <- c("N" = nrow(dfrd1heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd1heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd1heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd1heatkwh)$sigma)
kwhwth.rd2heatkwh <- c("N" = nrow(dfrd2heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd2heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd2heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd2heatkwh)$sigma)
kwhwth.rd3heatkwh <- c("N" = nrow(dfrd3heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd3heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd3heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd3heatkwh)$sigma)
kwhwth.rd4heatkwh <- c("N" = nrow(dfrd4heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd4heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd4heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd4heatkwh)$sigma)
kwhwth.rd5heatkwh <- c("N" = nrow(dfrd5heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd5heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd5heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd5heatkwh)$sigma)
kwhwth.rd6heatkwh <- c("N" = nrow(dfrd6heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd6heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd6heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd6heatkwh)$sigma)
kwhwth.rd7heatkwh <- c("N" = nrow(dfrd7heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd7heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd7heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd7heatkwh)$sigma)
kwhwth.rd8heatkwh <- c("N" = nrow(dfrd8heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd8heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd8heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd8heatkwh)$sigma)
kwhwth.rd9heatkwh <- c("N" = nrow(dfrd9heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd9heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd9heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd9heatkwh)$sigma)
kwhwth.rd10heatkwh <- c("N" = nrow(dfrd10heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd10heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd10heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd10heatkwh)$sigma)
kwhwth.rd11heatkwh <- c("N" = nrow(dfrd11heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd11heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd11heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd11heatkwh)$sigma)
kwhwth.rd12heatkwh <- c("N" = nrow(dfrd12heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd12heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd12heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd12heatkwh)$sigma)
kwhwth.rd13heatkwh <- c("N" = nrow(dfrd13heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd13heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd13heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd13heatkwh)$sigma)
kwhwth.rd14heatkwh <- c("N" = nrow(dfrd14heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd14heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd14heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd14heatkwh)$sigma)
kwhwth.rd15heatkwh <- c("N" = nrow(dfrd15heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd15heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd15heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd15heatkwh)$sigma)
kwhwth.rd16heatkwh <- c("N" = nrow(dfrd16heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd16heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd16heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd16heatkwh)$sigma)
kwhwth.rd17heatkwh <- c("N" = nrow(dfrd17heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd17heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd17heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd17heatkwh)$sigma)
kwhwth.rd18heatkwh <- c("N" = nrow(dfrd18heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd18heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd18heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd18heatkwh)$sigma)
kwhwth.rd19heatkwh <- c("N" = nrow(dfrd19heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd19heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd19heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd19heatkwh)$sigma)
kwhwth.rd20heatkwh <- c("N" = nrow(dfrd20heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd20heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd20heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd20heatkwh)$sigma)
kwhwth.rd21heatkwh <- c("N" = nrow(dfrd21heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd21heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd21heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd21heatkwh)$sigma)
kwhwth.rd22heatkwh <- c("N" = nrow(dfrd22heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd22heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd22heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd22heatkwh)$sigma)
kwhwth.rd23heatkwh <- c("N" = nrow(dfrd23heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd23heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd23heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd23heatkwh)$sigma)
kwhwth.rd24heatkwh <- c("N" = nrow(dfrd24heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd24heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd24heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd24heatkwh)$sigma)
kwhwth.rd25heatkwh <- c("N" = nrow(dfrd25heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd25heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd25heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd25heatkwh)$sigma)
kwhwth.rd26heatkwh <- c("N" = nrow(dfrd26heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd26heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd26heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd26heatkwh)$sigma)
kwhwth.rd27heatkwh <- c("N" = nrow(dfrd27heatkwh), "AdjR2" = round(summary(kwhwth.step.modelrd27heatkwh)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd27heatkwh)$df, "Sigma" = summary(kwhwth.step.modelrd27heatkwh)$sigma)
kwhwth.rd1heatoth <- c("N" = nrow(dfrd1heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd1heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd1heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd1heatoth)$sigma)
kwhwth.rd2heatoth <- c("N" = nrow(dfrd2heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd2heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd2heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd2heatoth)$sigma)
kwhwth.rd3heatoth <- c("N" = nrow(dfrd3heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd3heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd3heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd3heatoth)$sigma)
kwhwth.rd4heatoth <- c("N" = nrow(dfrd4heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd4heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd4heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd4heatoth)$sigma)
kwhwth.rd5heatoth <- c("N" = nrow(dfrd5heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd5heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd5heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd5heatoth)$sigma)
kwhwth.rd6heatoth <- c("N" = nrow(dfrd6heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd6heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd6heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd6heatoth)$sigma)
kwhwth.rd7heatoth <- c("N" = nrow(dfrd7heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd7heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd7heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd7heatoth)$sigma)
kwhwth.rd8heatoth <- c("N" = nrow(dfrd8heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd8heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd8heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd8heatoth)$sigma)
kwhwth.rd9heatoth <- c("N" = nrow(dfrd9heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd9heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd9heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd9heatoth)$sigma)
kwhwth.rd10heatoth <- c("N" = nrow(dfrd10heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd10heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd10heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd10heatoth)$sigma)
kwhwth.rd11heatoth <- c("N" = nrow(dfrd11heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd11heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd11heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd11heatoth)$sigma)
kwhwth.rd12heatoth <- c("N" = nrow(dfrd12heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd12heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd12heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd12heatoth)$sigma)
kwhwth.rd13heatoth <- c("N" = nrow(dfrd13heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd13heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd13heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd13heatoth)$sigma)
kwhwth.rd14heatoth <- c("N" = nrow(dfrd14heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd14heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd14heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd14heatoth)$sigma)
kwhwth.rd15heatoth <- c("N" = nrow(dfrd15heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd15heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd15heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd15heatoth)$sigma)
kwhwth.rd16heatoth <- c("N" = nrow(dfrd16heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd16heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd16heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd16heatoth)$sigma)
kwhwth.rd17heatoth <- c("N" = nrow(dfrd17heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd17heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd17heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd17heatoth)$sigma)
kwhwth.rd18heatoth <- c("N" = nrow(dfrd18heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd18heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd18heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd18heatoth)$sigma)
kwhwth.rd19heatoth <- c("N" = nrow(dfrd19heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd19heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd19heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd19heatoth)$sigma)
kwhwth.rd20heatoth <- c("N" = nrow(dfrd20heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd20heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd20heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd20heatoth)$sigma)
kwhwth.rd21heatoth <- c("N" = nrow(dfrd21heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd21heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd21heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd21heatoth)$sigma)
kwhwth.rd22heatoth <- c("N" = nrow(dfrd22heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd22heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd22heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd22heatoth)$sigma)
kwhwth.rd23heatoth <- c("N" = nrow(dfrd23heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd23heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd23heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd23heatoth)$sigma)
kwhwth.rd24heatoth <- c("N" = nrow(dfrd24heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd24heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd24heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd24heatoth)$sigma)
kwhwth.rd25heatoth <- c("N" = nrow(dfrd25heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd25heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd25heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd25heatoth)$sigma)
kwhwth.rd26heatoth <- c("N" = nrow(dfrd26heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd26heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd26heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd26heatoth)$sigma)
kwhwth.rd27heatoth <- c("N" = nrow(dfrd27heatoth), "AdjR2" = round(summary(kwhwth.step.modelrd27heatoth)$adj.r.squared, digits = 3), "DF" = summary(kwhwth.step.modelrd27heatoth)$df, "Sigma" = summary(kwhwth.step.modelrd27heatoth)$sigma)

# Create data frame containing all the stats vectors
statsdf <- data.frame(
  cufeetng.rd1heatgas,
  cufeetng.rd2heatgas,
  cufeetng.rd3heatgas,
  cufeetng.rd4heatgas,
  cufeetng.rd5heatgas,
  cufeetng.rd6heatgas,
  cufeetng.rd7heatgas,
  cufeetng.rd8heatgas,
  cufeetng.rd9heatgas,
  cufeetng.rd10heatgas,
  cufeetng.rd11heatgas,
  cufeetng.rd12heatgas,
  cufeetng.rd13heatgas,
  cufeetng.rd14heatgas,
  cufeetng.rd15heatgas,
  cufeetng.rd16heatgas,
  cufeetng.rd17heatgas,
  cufeetng.rd18heatgas,
  cufeetng.rd19heatgas,
  cufeetng.rd20heatgas,
  cufeetng.rd21heatgas,
  cufeetng.rd22heatgas,
  cufeetng.rd23heatgas,
  cufeetng.rd24heatgas,
  cufeetng.rd25heatgas,
  cufeetng.rd26heatgas,
  cufeetng.rd27heatgas,
  cufeetngsph.rd1heatgas,
  cufeetngsph.rd2heatgas,
  cufeetngsph.rd3heatgas,
  cufeetngsph.rd4heatgas,
  cufeetngsph.rd5heatgas,
  cufeetngsph.rd6heatgas,
  cufeetngsph.rd7heatgas,
  cufeetngsph.rd8heatgas,
  cufeetngsph.rd9heatgas,
  cufeetngsph.rd10heatgas,
  cufeetngsph.rd11heatgas,
  cufeetngsph.rd12heatgas,
  cufeetngsph.rd13heatgas,
  cufeetngsph.rd14heatgas,
  cufeetngsph.rd15heatgas,
  cufeetngsph.rd16heatgas,
  cufeetngsph.rd17heatgas,
  cufeetngsph.rd18heatgas,
  cufeetngsph.rd19heatgas,
  cufeetngsph.rd20heatgas,
  cufeetngsph.rd21heatgas,
  cufeetngsph.rd22heatgas,
  cufeetngsph.rd23heatgas,
  cufeetngsph.rd24heatgas,
  cufeetngsph.rd25heatgas,
  cufeetngsph.rd26heatgas,
  cufeetngsph.rd27heatgas,
  cufeetngwth.rd1heatgas,
  cufeetngwth.rd2heatgas,
  cufeetngwth.rd3heatgas,
  cufeetngwth.rd4heatgas,
  cufeetngwth.rd5heatgas,
  cufeetngwth.rd6heatgas,
  cufeetngwth.rd7heatgas,
  cufeetngwth.rd8heatgas,
  cufeetngwth.rd9heatgas,
  cufeetngwth.rd10heatgas,
  cufeetngwth.rd11heatgas,
  cufeetngwth.rd12heatgas,
  cufeetngwth.rd13heatgas,
  cufeetngwth.rd14heatgas,
  cufeetngwth.rd15heatgas,
  cufeetngwth.rd16heatgas,
  cufeetngwth.rd17heatgas,
  cufeetngwth.rd18heatgas,
  cufeetngwth.rd19heatgas,
  cufeetngwth.rd20heatgas,
  cufeetngwth.rd21heatgas,
  cufeetngwth.rd22heatgas,
  cufeetngwth.rd23heatgas,
  cufeetngwth.rd24heatgas,
  cufeetngwth.rd25heatgas,
  cufeetngwth.rd26heatgas,
  cufeetngwth.rd27heatgas,
  kwh.rd1heatgas,
  kwh.rd2heatgas,
  kwh.rd3heatgas,
  kwh.rd4heatgas,
  kwh.rd5heatgas,
  kwh.rd6heatgas,
  kwh.rd7heatgas,
  kwh.rd8heatgas,
  kwh.rd9heatgas,
  kwh.rd10heatgas,
  kwh.rd11heatgas,
  kwh.rd12heatgas,
  kwh.rd13heatgas,
  kwh.rd14heatgas,
  kwh.rd15heatgas,
  kwh.rd16heatgas,
  kwh.rd17heatgas,
  kwh.rd18heatgas,
  kwh.rd19heatgas,
  kwh.rd20heatgas,
  kwh.rd21heatgas,
  kwh.rd22heatgas,
  kwh.rd23heatgas,
  kwh.rd24heatgas,
  kwh.rd25heatgas,
  kwh.rd26heatgas,
  kwh.rd27heatgas,
  kwhoth.rd1heatgas,
  kwhoth.rd2heatgas,
  kwhoth.rd3heatgas,
  kwhoth.rd4heatgas,
  kwhoth.rd5heatgas,
  kwhoth.rd6heatgas,
  kwhoth.rd7heatgas,
  kwhoth.rd8heatgas,
  kwhoth.rd9heatgas,
  kwhoth.rd10heatgas,
  kwhoth.rd11heatgas,
  kwhoth.rd12heatgas,
  kwhoth.rd13heatgas,
  kwhoth.rd14heatgas,
  kwhoth.rd15heatgas,
  kwhoth.rd16heatgas,
  kwhoth.rd17heatgas,
  kwhoth.rd18heatgas,
  kwhoth.rd19heatgas,
  kwhoth.rd20heatgas,
  kwhoth.rd21heatgas,
  kwhoth.rd22heatgas,
  kwhoth.rd23heatgas,
  kwhoth.rd24heatgas,
  kwhoth.rd25heatgas,
  kwhoth.rd26heatgas,
  kwhoth.rd27heatgas,
  kwhsph.rd1heatgas,
  kwhsph.rd2heatgas,
  kwhsph.rd3heatgas,
  kwhsph.rd4heatgas,
  kwhsph.rd5heatgas,
  kwhsph.rd6heatgas,
  kwhsph.rd7heatgas,
  kwhsph.rd8heatgas,
  kwhsph.rd9heatgas,
  kwhsph.rd10heatgas,
  kwhsph.rd11heatgas,
  kwhsph.rd12heatgas,
  kwhsph.rd13heatgas,
  kwhsph.rd14heatgas,
  kwhsph.rd15heatgas,
  kwhsph.rd16heatgas,
  kwhsph.rd17heatgas,
  kwhsph.rd18heatgas,
  kwhsph.rd19heatgas,
  kwhsph.rd20heatgas,
  kwhsph.rd21heatgas,
  kwhsph.rd22heatgas,
  kwhsph.rd23heatgas,
  kwhsph.rd24heatgas,
  kwhsph.rd25heatgas,
  kwhsph.rd26heatgas,
  kwhsph.rd27heatgas,
  kwh.rd1heatkwh,
  kwh.rd2heatkwh,
  kwh.rd3heatkwh,
  kwh.rd4heatkwh,
  kwh.rd5heatkwh,
  kwh.rd6heatkwh,
  kwh.rd7heatkwh,
  kwh.rd8heatkwh,
  kwh.rd9heatkwh,
  kwh.rd10heatkwh,
  kwh.rd11heatkwh,
  kwh.rd12heatkwh,
  kwh.rd13heatkwh,
  kwh.rd14heatkwh,
  kwh.rd15heatkwh,
  kwh.rd16heatkwh,
  kwh.rd17heatkwh,
  kwh.rd18heatkwh,
  kwh.rd19heatkwh,
  kwh.rd20heatkwh,
  kwh.rd21heatkwh,
  kwh.rd22heatkwh,
  kwh.rd23heatkwh,
  kwh.rd24heatkwh,
  kwh.rd25heatkwh,
  kwh.rd26heatkwh,
  kwh.rd27heatkwh,
  kwhoth.rd1heatkwh,
  kwhoth.rd2heatkwh,
  kwhoth.rd3heatkwh,
  kwhoth.rd4heatkwh,
  kwhoth.rd5heatkwh,
  kwhoth.rd6heatkwh,
  kwhoth.rd7heatkwh,
  kwhoth.rd8heatkwh,
  kwhoth.rd9heatkwh,
  kwhoth.rd10heatkwh,
  kwhoth.rd11heatkwh,
  kwhoth.rd12heatkwh,
  kwhoth.rd13heatkwh,
  kwhoth.rd14heatkwh,
  kwhoth.rd15heatkwh,
  kwhoth.rd16heatkwh,
  kwhoth.rd17heatkwh,
  kwhoth.rd18heatkwh,
  kwhoth.rd19heatkwh,
  kwhoth.rd20heatkwh,
  kwhoth.rd21heatkwh,
  kwhoth.rd22heatkwh,
  kwhoth.rd23heatkwh,
  kwhoth.rd24heatkwh,
  kwhoth.rd25heatkwh,
  kwhoth.rd26heatkwh,
  kwhoth.rd27heatkwh,
  kwhsph.rd1heatkwh,
  kwhsph.rd2heatkwh,
  kwhsph.rd3heatkwh,
  kwhsph.rd4heatkwh,
  kwhsph.rd5heatkwh,
  kwhsph.rd6heatkwh,
  kwhsph.rd7heatkwh,
  kwhsph.rd8heatkwh,
  kwhsph.rd9heatkwh,
  kwhsph.rd10heatkwh,
  kwhsph.rd11heatkwh,
  kwhsph.rd12heatkwh,
  kwhsph.rd13heatkwh,
  kwhsph.rd14heatkwh,
  kwhsph.rd15heatkwh,
  kwhsph.rd16heatkwh,
  kwhsph.rd17heatkwh,
  kwhsph.rd18heatkwh,
  kwhsph.rd19heatkwh,
  kwhsph.rd20heatkwh,
  kwhsph.rd21heatkwh,
  kwhsph.rd22heatkwh,
  kwhsph.rd23heatkwh,
  kwhsph.rd24heatkwh,
  kwhsph.rd25heatkwh,
  kwhsph.rd26heatkwh,
  kwhsph.rd27heatkwh,
  btuother.rd1heatoth,
  btuother.rd2heatoth,
  btuother.rd3heatoth,
  btuother.rd4heatoth,
  btuother.rd5heatoth,
  btuother.rd6heatoth,
  btuother.rd7heatoth,
  btuother.rd8heatoth,
  btuother.rd9heatoth,
  btuother.rd10heatoth,
  btuother.rd11heatoth,
  btuother.rd12heatoth,
  btuother.rd13heatoth,
  btuother.rd14heatoth,
  btuother.rd15heatoth,
  btuother.rd16heatoth,
  btuother.rd17heatoth,
  btuother.rd18heatoth,
  btuother.rd19heatoth,
  btuother.rd20heatoth,
  btuother.rd21heatoth,
  btuother.rd22heatoth,
  btuother.rd23heatoth,
  btuother.rd24heatoth,
  btuother.rd25heatoth,
  btuother.rd26heatoth,
  btuother.rd27heatoth,
  kwh.rd1heatoth,
  kwh.rd2heatoth,
  kwh.rd3heatoth,
  kwh.rd4heatoth,
  kwh.rd5heatoth,
  kwh.rd6heatoth,
  kwh.rd7heatoth,
  kwh.rd8heatoth,
  kwh.rd9heatoth,
  kwh.rd10heatoth,
  kwh.rd11heatoth,
  kwh.rd12heatoth,
  kwh.rd13heatoth,
  kwh.rd14heatoth,
  kwh.rd15heatoth,
  kwh.rd16heatoth,
  kwh.rd17heatoth,
  kwh.rd18heatoth,
  kwh.rd19heatoth,
  kwh.rd20heatoth,
  kwh.rd21heatoth,
  kwh.rd22heatoth,
  kwh.rd23heatoth,
  kwh.rd24heatoth,
  kwh.rd25heatoth,
  kwh.rd26heatoth,
  kwh.rd27heatoth,
  kwhoth.rd1heatoth,
  kwhoth.rd2heatoth,
  kwhoth.rd3heatoth,
  kwhoth.rd4heatoth,
  kwhoth.rd5heatoth,
  kwhoth.rd6heatoth,
  kwhoth.rd7heatoth,
  kwhoth.rd8heatoth,
  kwhoth.rd9heatoth,
  kwhoth.rd10heatoth,
  kwhoth.rd11heatoth,
  kwhoth.rd12heatoth,
  kwhoth.rd13heatoth,
  kwhoth.rd14heatoth,
  kwhoth.rd15heatoth,
  kwhoth.rd16heatoth,
  kwhoth.rd17heatoth,
  kwhoth.rd18heatoth,
  kwhoth.rd19heatoth,
  kwhoth.rd20heatoth,
  kwhoth.rd21heatoth,
  kwhoth.rd22heatoth,
  kwhoth.rd23heatoth,
  kwhoth.rd24heatoth,
  kwhoth.rd25heatoth,
  kwhoth.rd26heatoth,
  kwhoth.rd27heatoth,
  kwhsph.rd1heatoth,
  kwhsph.rd2heatoth,
  kwhsph.rd3heatoth,
  kwhsph.rd4heatoth,
  kwhsph.rd5heatoth,
  kwhsph.rd6heatoth,
  kwhsph.rd7heatoth,
  kwhsph.rd8heatoth,
  kwhsph.rd9heatoth,
  kwhsph.rd10heatoth,
  kwhsph.rd11heatoth,
  kwhsph.rd12heatoth,
  kwhsph.rd13heatoth,
  kwhsph.rd14heatoth,
  kwhsph.rd15heatoth,
  kwhsph.rd16heatoth,
  kwhsph.rd17heatoth,
  kwhsph.rd18heatoth,
  kwhsph.rd19heatoth,
  kwhsph.rd20heatoth,
  kwhsph.rd21heatoth,
  kwhsph.rd22heatoth,
  kwhsph.rd23heatoth,
  kwhsph.rd24heatoth,
  kwhsph.rd25heatoth,
  kwhsph.rd26heatoth,
  kwhsph.rd27heatoth,
  kwhcol.rd1heatany,
  kwhcol.rd2heatany,
  kwhcol.rd3heatany,
  kwhcol.rd4heatany,
  kwhcol.rd5heatany,
  kwhcol.rd6heatany,
  kwhcol.rd7heatany,
  kwhcol.rd8heatany,
  kwhcol.rd9heatany,
  kwhcol.rd10heatany,
  kwhcol.rd11heatany,
  kwhcol.rd12heatany,
  kwhcol.rd13heatany,
  kwhcol.rd14heatany,
  kwhcol.rd15heatany,
  kwhcol.rd16heatany,
  kwhcol.rd17heatany,
  kwhcol.rd18heatany,
  kwhcol.rd19heatany,
  kwhcol.rd20heatany,
  kwhcol.rd21heatany,
  kwhcol.rd22heatany,
  kwhcol.rd23heatany,
  kwhcol.rd24heatany,
  kwhcol.rd25heatany,
  kwhcol.rd26heatany,
  kwhcol.rd27heatany,
  btulp.rd1heatany,
  btulp.rd2heatany,
  btulp.rd3heatany,
  btulp.rd4heatany,
  btulp.rd5heatany,
  btulp.rd6heatany,
  btulp.rd7heatany,
  btulp.rd8heatany,
  btulp.rd9heatany,
  btulp.rd10heatany,
  btulp.rd11heatany,
  btulp.rd12heatany,
  btulp.rd13heatany,
  btulp.rd14heatany,
  btulp.rd15heatany,
  btulp.rd16heatany,
  btulp.rd17heatany,
  btulp.rd18heatany,
  btulp.rd19heatany,
  btulp.rd20heatany,
  btulp.rd21heatany,
  btulp.rd22heatany,
  btulp.rd23heatany,
  btulp.rd24heatany,
  btulp.rd25heatany,
  btulp.rd26heatany,
  btulp.rd27heatany,
  kwhwth.rd1heatgas,
  kwhwth.rd2heatgas,
  kwhwth.rd3heatgas,
  kwhwth.rd4heatgas,
  kwhwth.rd5heatgas,
  kwhwth.rd6heatgas,
  kwhwth.rd7heatgas,
  kwhwth.rd8heatgas,
  kwhwth.rd9heatgas,
  kwhwth.rd10heatgas,
  kwhwth.rd11heatgas,
  kwhwth.rd12heatgas,
  kwhwth.rd13heatgas,
  kwhwth.rd14heatgas,
  kwhwth.rd15heatgas,
  kwhwth.rd16heatgas,
  kwhwth.rd17heatgas,
  kwhwth.rd18heatgas,
  kwhwth.rd19heatgas,
  kwhwth.rd20heatgas,
  kwhwth.rd21heatgas,
  kwhwth.rd22heatgas,
  kwhwth.rd23heatgas,
  kwhwth.rd24heatgas,
  kwhwth.rd25heatgas,
  kwhwth.rd26heatgas,
  kwhwth.rd27heatgas,
  kwhwth.rd1heatkwh,
  kwhwth.rd2heatkwh,
  kwhwth.rd3heatkwh,
  kwhwth.rd4heatkwh,
  kwhwth.rd5heatkwh,
  kwhwth.rd6heatkwh,
  kwhwth.rd7heatkwh,
  kwhwth.rd8heatkwh,
  kwhwth.rd9heatkwh,
  kwhwth.rd10heatkwh,
  kwhwth.rd11heatkwh,
  kwhwth.rd12heatkwh,
  kwhwth.rd13heatkwh,
  kwhwth.rd14heatkwh,
  kwhwth.rd15heatkwh,
  kwhwth.rd16heatkwh,
  kwhwth.rd17heatkwh,
  kwhwth.rd18heatkwh,
  kwhwth.rd19heatkwh,
  kwhwth.rd20heatkwh,
  kwhwth.rd21heatkwh,
  kwhwth.rd22heatkwh,
  kwhwth.rd23heatkwh,
  kwhwth.rd24heatkwh,
  kwhwth.rd25heatkwh,
  kwhwth.rd26heatkwh,
  kwhwth.rd27heatkwh,
  kwhwth.rd1heatoth,
  kwhwth.rd2heatoth,
  kwhwth.rd3heatoth,
  kwhwth.rd4heatoth,
  kwhwth.rd5heatoth,
  kwhwth.rd6heatoth,
  kwhwth.rd7heatoth,
  kwhwth.rd8heatoth,
  kwhwth.rd9heatoth,
  kwhwth.rd10heatoth,
  kwhwth.rd11heatoth,
  kwhwth.rd12heatoth,
  kwhwth.rd13heatoth,
  kwhwth.rd14heatoth,
  kwhwth.rd15heatoth,
  kwhwth.rd16heatoth,
  kwhwth.rd17heatoth,
  kwhwth.rd18heatoth,
  kwhwth.rd19heatoth,
  kwhwth.rd20heatoth,
  kwhwth.rd21heatoth,
  kwhwth.rd22heatoth,
  kwhwth.rd23heatoth,
  kwhwth.rd24heatoth,
  kwhwth.rd25heatoth,
  kwhwth.rd26heatoth,
  kwhwth.rd27heatoth
  )




## Joining the Stats and Betas into a single FINAL RESULTS FILE!!

finalresults <- merge(t(statsdf), t(out), by=0, all=T)
rownames(finalresults) <- finalresults$Row.names; finalresults$Row.names <- NULL
#View(finalresults)

#Use this to add a csv file to your working directory
write.csv(finalresults, file = "finalresults3.csv", na="")

#clearing variables from memory: rm(list = ls()), and control l
#rm(list = ls())
