### Loading packages
library(MASS)
library(readr)



##IMPORTANT INSTRUCTIONS

# Download recs2009_public.csv from github (it is already cleaned)

# Or clean the data youself by:

# 1) Download recs2009_public.csv from the RECS website:
# 2) In MS Excel or compatible program remove all -2, -8, -9 values (find and replace all)

# 3)This will show your working directory. 
    getwd()
# 4) By default, Rstudio chooses your user account's root directory. 
# Use this function to change your working directory to uplaod the RECS dataset. 
# setwd(dir) 
# This is also the directory RStudio will use to export your results files

# Set working directory to Google File Stream ecodatalab folder
    
setwd("/Volumes/GoogleDrive/My\ Drive/ecodatalab/econometrics/recs2009/playground")

# Load the RECS dataset from Google File Stream ecodatalab folder
recs <- read.csv("/Volumes/GoogleDrive/My\ Drive/ecodatalab/econometrics/recs2009/playground/recs2009_public.csv")

# Load Census Data
census <- read.csv("/Volumes/GoogleDrive/My\ Drive/ecodatalab/econometrics/recs2009/playground/2017_1YR_STATES_CLEAN.csv")
 
# Load Census Locations Mapping
census_locations <- read.csv("/Volumes/GoogleDrive/My\ Drive/ecodatalab/econometrics/recs2009/playground/census_locations.csv")


######### RECODING VARIABLES AND CLEANING DATA ########

#VARIABLES PRECEDED BY C_ ARE FROM US CENSUS

HHSIZE<-recs$NHSLDMEM
C_HHSIZE<-census$SIZE

LNHHSIZE<-log(HHSIZE)
C_LNHHSIZE<-log(C_HHSIZE)


WHITE = recs$Householder_Race
WHITE[recs$Householder_Race != 1] = 0
C_WHITE<-census$WHITE

BLACK = recs$Householder_Race
BLACK[recs$Householder_Race != 2] = 0
BLACK[recs$Householder_Race == 2] = 1
C_BLACK<-census$AFAM

ASIAN = recs$Householder_Race
ASIAN[recs$Householder_Race != 4] = 0
ASIAN[recs$Householder_Race == 4] = 1
C_ASIAN<-census$ASIAN

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
C_DEGREE<-census$DEGREE

ROOMS <- recs$TOTROOMS
C_ROOMS <- census$ROOMS

LNROOMS <- log(ROOMS)
C_LNROOMS <- log(C_ROOMS)
  

AVGINCOME = recs$MONEYPY
AVGINCOME[recs$MONEYPY == 1] = 1250
AVGINCOME[recs$MONEYPY == 2] = (2500+4999)/2
AVGINCOME[recs$MONEYPY == 3] = (5000+7499)/2
AVGINCOME[recs$MONEYPY == 4] = (7500+9999)/2
AVGINCOME[recs$MONEYPY == 5] = (10000+14999)/2
AVGINCOME[recs$MONEYPY == 6] = (15000+19999)/2
AVGINCOME[recs$MONEYPY == 7] = (20000+24999)/2
AVGINCOME[recs$MONEYPY == 8] = (25000+29999)/2
AVGINCOME[recs$MONEYPY == 9] = (30000+34999)/2
AVGINCOME[recs$MONEYPY == 10] = (35000+39999)/2
AVGINCOME[recs$MONEYPY == 11] = (40000+44999)/2
AVGINCOME[recs$MONEYPY == 12] = (45000+49999)/2
AVGINCOME[recs$MONEYPY == 13] = (50000+54999)/2
AVGINCOME[recs$MONEYPY == 14] = (55000+59999)/2
AVGINCOME[recs$MONEYPY == 15] = (60000+64999)/2
AVGINCOME[recs$MONEYPY == 16] = (65000+69999)/2
AVGINCOME[recs$MONEYPY == 17] = (70000+74999)/2
AVGINCOME[recs$MONEYPY == 18] = (75000+79999)/2
AVGINCOME[recs$MONEYPY == 19] = (80000+84999)/2
AVGINCOME[recs$MONEYPY == 20] = (85000+89999)/2
AVGINCOME[recs$MONEYPY == 21] = (90000+94999)/2
AVGINCOME[recs$MONEYPY == 22] = (95000+99999)/2
AVGINCOME[recs$MONEYPY == 23] = (100000+119999)/2
AVGINCOME[recs$MONEYPY == 24] = 150000
C_AVGINCOME <- census$AVGINCOME

LNAVGINCOME <- log(AVGINCOME)
C_LNAVGINCOME <- log(C_AVGINCOME)

OWN = recs$KOWNRENT
OWN[OWN == 2 | OWN == 3] = 0
C_OWN <- census$OWN
 

DETACHED = recs$TYPEHUQ
DETACHED[DETACHED != 2] = 0
DETACHED[DETACHED == 2] = 1
C_DETACHED <- census$DETACHED
  
 
STATE <- census$Geo_STUSAB

# CENSUS DIVISIONS IN RECS
#1  New England Census Division (CT, MA, ME, NH, RI, VT)
#2  Middle Atlantic Census Division (NJ, NY, PA)
#3  East North Central Census Division (IL, IN, MI, OH, WI)
#4  West North Central Census Division (IA, KS, MN, MO, ND, NE, SD)
#5  South Atlantic  Census Division (DC, DE, FL, GA, MD, NC, SC, VA, WV)
#6  East South Central Census Division (AL, KY, MS, TN)
#7  West South Central Census Division (AR, LA, OK, TX)
#8  Mountain North Sub-Division (CO, ID, MT, UT, WY)
#9  Mountain South Sub-Division (AZ, NM, NV)
#10 Pacific Census Division (AK, CA, HI, OR, WA)


DIVISION1 = recs$DIVISION
DIVISION1[DIVISION1 != 1] = 0
DIVISION1[DIVISION1 = 1] = 1

DIVISION2 = recs$DIVISION
DIVISION2[DIVISION2 != 2] = 0
DIVISION2[DIVISION2 == 2] = 1

DIVISION3 = recs$DIVISION
DIVISION3[DIVISION3 != 3] = 0
DIVISION3[DIVISION3 == 3] = 1

DIVISION4 = recs$DIVISION
DIVISION4[DIVISION4 != 4] = 0
DIVISION4[DIVISION4 == 4] = 1

DIVISION5 = recs$DIVISION
DIVISION5[DIVISION5 != 5] = 0
DIVISION5[DIVISION5 == 5] = 1

DIVISION6 = recs$DIVISION
DIVISION6[DIVISION6 != 6] = 0
DIVISION6[DIVISION6 == 6] = 1

DIVISION7 = recs$DIVISION
DIVISION7[DIVISION7 != 7] = 0
DIVISION7[DIVISION7 == 7] = 1

DIVISION8 = recs$DIVISION
DIVISION8[DIVISION8 != 8] = 0
DIVISION8[DIVISION8 == 8] = 1

DIVISION9 = recs$DIVISION
DIVISION9[DIVISION9 != 9] = 0
DIVISION9[DIVISION9 == 9] = 1

DIVISION10 = recs$DIVISION
DIVISION10[DIVISION10 != 10] = 0
DIVISION10[DIVISION10 == 10] = 1
 

summary(recs$KWH)

#df1[mapply(is.infinite, df1)] <- NA


#DEPENDENT VARIABLES ___________________________-
 
KWH <- recs$KWH

 
# LOOCATION VARIABLES

CENSUS_DIVISION <- census_locations$DIVISION
CENSUS_STATE <- census_locations$STATE
CENSUS_REGION <- census_locations$REGION
CENSUS_REPORTABLE_DOMAIN <-census_locations$REPORTABLE_DOMAIN


## CENSUS LOCATIONS DATA FRAME
df_census_locations <- data.frame(CENSUS_STATE, CENSUS_DIVISION, CENSUS_REGION, CENSUS_REPORTABLE_DOMAIN)
df_census_locations <- df_census_locations[order(CENSUS_STATE),] 

df_census_data <- data.frame(STATE, C_HHSIZE, C_LNHHSIZE, C_ROOMS, C_WHITE, C_BLACK, C_ASIAN, C_AVGINCOME, C_DEGREE, C_DETACHED, C_LNROOMS, C_LNAVGINCOME, C_OWN)
df_census_data <- df_census_data[order(STATE),] 


## FULL DATA FRAME (USED IN ALL MODELS) 
df1 <- data.frame(KWH, ASIAN,  AVGINCOME,  BLACK,   DEGREE,  DETACHED,  HHSIZE,  OWN,  ROOMS, WHITE , LNHHSIZE,  LNROOMS,  LNAVGINCOME,  KWH, DIVISION1, DIVISION2, DIVISION3, DIVISION4, DIVISION5, DIVISION6, DIVISION7, DIVISION8, DIVISION9, DIVISION10, recs$REPORTABLE_DOMAIN)


# RECS STATES DATAFRAMES

dfAK = data.frame(subset(df1, DIVISION10 == 1))
dfAL = data.frame(subset(df1, DIVISION6 == 1))
dfAR = data.frame(subset(df1, DIVISION7 == 1))
dfAZ = data.frame(subset(df1, DIVISION9 == 1))
dfCA = data.frame(subset(df1, DIVISION10 == 1))
dfCO = data.frame(subset(df1, DIVISION8 == 1))
dfCT = data.frame(subset(df1, DIVISION1 == 1))
dfDC = data.frame(subset(df1, DIVISION5 == 1))
dfDE = data.frame(subset(df1, DIVISION5 == 1))
dfFL = data.frame(subset(df1, DIVISION5 == 1))
dfGA = data.frame(subset(df1, DIVISION5 == 1))
dfHI = data.frame(subset(df1, DIVISION10 == 1))
dfIA = data.frame(subset(df1, DIVISION4 == 1))
dfID = data.frame(subset(df1, DIVISION8 == 1))
dfIL = data.frame(subset(df1, DIVISION3 == 1))
dfIN = data.frame(subset(df1, DIVISION3 == 1))
dfKS = data.frame(subset(df1, DIVISION4 == 1))
dfKY = data.frame(subset(df1, DIVISION6 == 1))
dfLA = data.frame(subset(df1, DIVISION7 == 1))
dfMA = data.frame(subset(df1, DIVISION1 == 1))
dfMD = data.frame(subset(df1, DIVISION5 == 1))
dfME = data.frame(subset(df1, DIVISION1 == 1))
dfMI = data.frame(subset(df1, DIVISION3 == 1))
dfMN = data.frame(subset(df1, DIVISION4 == 1))
dfMO = data.frame(subset(df1, DIVISION4 == 1))
dfMS = data.frame(subset(df1, DIVISION6 == 1))
dfMT = data.frame(subset(df1, DIVISION8 == 1))
dfNC = data.frame(subset(df1, DIVISION5 == 1))
dfND = data.frame(subset(df1, DIVISION4 == 1))
dfNE = data.frame(subset(df1, DIVISION4 == 1))
dfNH = data.frame(subset(df1, DIVISION1 == 1))
dfNJ = data.frame(subset(df1, DIVISION2 == 1))
dfNM = data.frame(subset(df1, DIVISION9 == 1))
dfNV = data.frame(subset(df1, DIVISION9 == 1))
dfNY = data.frame(subset(df1, DIVISION2 == 1))
dfOH = data.frame(subset(df1, DIVISION3 == 1))
dfOK = data.frame(subset(df1, DIVISION7 == 1))
dfOR = data.frame(subset(df1, DIVISION10 == 1))
dfPA = data.frame(subset(df1, DIVISION2 == 1))
dfRI = data.frame(subset(df1, DIVISION1 == 1))
dfSC = data.frame(subset(df1, DIVISION5 == 1))
dfSD = data.frame(subset(df1, DIVISION4 == 1))
dfTN = data.frame(subset(df1, DIVISION6 == 1))
dfTX = data.frame(subset(df1, DIVISION7 == 1))
dfUT = data.frame(subset(df1, DIVISION8 == 1))
dfVA = data.frame(subset(df1, DIVISION5 == 1))
dfVT = data.frame(subset(df1, DIVISION1 == 1))
dfWA = data.frame(subset(df1, DIVISION10 == 1))
dfWI = data.frame(subset(df1, DIVISION3 == 1))
dfWV = data.frame(subset(df1, DIVISION5 == 1))
dfWY = data.frame(subset(df1, DIVISION8 == 1))


##NULL MODELS

kwh.null.dfAK <- lm(KWH ~ 1, data = dfAK)
kwh.null.dfAL <- lm(KWH ~ 1, data = dfAL)
kwh.null.dfAR <- lm(KWH ~ 1, data = dfAR)
kwh.null.dfAZ <- lm(KWH ~ 1, data = dfAZ)
kwh.null.dfCA <- lm(KWH ~ 1, data = dfCA)
kwh.null.dfCO <- lm(KWH ~ 1, data = dfCO)
kwh.null.dfCT <- lm(KWH ~ 1, data = dfCT)
kwh.null.dfDC <- lm(KWH ~ 1, data = dfDC)
kwh.null.dfDE <- lm(KWH ~ 1, data = dfDE)
kwh.null.dfFL <- lm(KWH ~ 1, data = dfFL)
kwh.null.dfGA <- lm(KWH ~ 1, data = dfGA)
kwh.null.dfHI <- lm(KWH ~ 1, data = dfHI)
kwh.null.dfIA <- lm(KWH ~ 1, data = dfIA)
kwh.null.dfID <- lm(KWH ~ 1, data = dfID)
kwh.null.dfIL <- lm(KWH ~ 1, data = dfIL)
kwh.null.dfIN <- lm(KWH ~ 1, data = dfIN)
kwh.null.dfKS <- lm(KWH ~ 1, data = dfKS)
kwh.null.dfKY <- lm(KWH ~ 1, data = dfKY)
kwh.null.dfLA <- lm(KWH ~ 1, data = dfLA)
kwh.null.dfMA <- lm(KWH ~ 1, data = dfMA)
kwh.null.dfMD <- lm(KWH ~ 1, data = dfMD)
kwh.null.dfME <- lm(KWH ~ 1, data = dfME)
kwh.null.dfMI <- lm(KWH ~ 1, data = dfMI)
kwh.null.dfMN <- lm(KWH ~ 1, data = dfMN)
kwh.null.dfMO <- lm(KWH ~ 1, data = dfMO)
kwh.null.dfMS <- lm(KWH ~ 1, data = dfMS)
kwh.null.dfMT <- lm(KWH ~ 1, data = dfMT)
kwh.null.dfNC <- lm(KWH ~ 1, data = dfNC)
kwh.null.dfND <- lm(KWH ~ 1, data = dfND)
kwh.null.dfNE <- lm(KWH ~ 1, data = dfNE)
kwh.null.dfNH <- lm(KWH ~ 1, data = dfNH)
kwh.null.dfNJ <- lm(KWH ~ 1, data = dfNJ)
kwh.null.dfNM <- lm(KWH ~ 1, data = dfNM)
kwh.null.dfNV <- lm(KWH ~ 1, data = dfNV)
kwh.null.dfNY <- lm(KWH ~ 1, data = dfNY)
kwh.null.dfOH <- lm(KWH ~ 1, data = dfOH)
kwh.null.dfOK <- lm(KWH ~ 1, data = dfOK)
kwh.null.dfOR <- lm(KWH ~ 1, data = dfOR)
kwh.null.dfPA <- lm(KWH ~ 1, data = dfPA)
kwh.null.dfRI <- lm(KWH ~ 1, data = dfRI)
kwh.null.dfSC <- lm(KWH ~ 1, data = dfSC)
kwh.null.dfSD <- lm(KWH ~ 1, data = dfSD)
kwh.null.dfTN <- lm(KWH ~ 1, data = dfTN)
kwh.null.dfTX <- lm(KWH ~ 1, data = dfTX)
kwh.null.dfUT <- lm(KWH ~ 1, data = dfUT)
kwh.null.dfVA <- lm(KWH ~ 1, data = dfVA)
kwh.null.dfVT <- lm(KWH ~ 1, data = dfVT)
kwh.null.dfWA <- lm(KWH ~ 1, data = dfWA)
kwh.null.dfWI <- lm(KWH ~ 1, data = dfWI)
kwh.null.dfWV <- lm(KWH ~ 1, data = dfWV)
kwh.null.dfWY <- lm(KWH ~ 1, data = dfWY)


##Full Models

# CREATE MODELS WITH DIVISIONS 1 AND 2 AS A SUBSETS

kwh.full.dfAK <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfAK)
kwh.full.dfAL <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfAL)
kwh.full.dfAR <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfAR)
kwh.full.dfAZ <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfAZ)
kwh.full.dfCA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfCA)
kwh.full.dfCO <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfCO)
kwh.full.dfCT <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfCT)
kwh.full.dfDC <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfDC)
kwh.full.dfDE <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfDE)
kwh.full.dfFL <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfFL)
kwh.full.dfGA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfGA)
kwh.full.dfHI <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfHI)
kwh.full.dfIA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfIA)
kwh.full.dfID <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfID)
kwh.full.dfIL <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfIL)
kwh.full.dfIN <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfIN)
kwh.full.dfKS <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfKS)
kwh.full.dfKY <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfKY)
kwh.full.dfLA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfLA)
kwh.full.dfMA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMA)
kwh.full.dfMD <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMD)
kwh.full.dfME <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfME)
kwh.full.dfMI <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMI)
kwh.full.dfMN <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMN)
kwh.full.dfMO <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMO)
kwh.full.dfMS <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMS)
kwh.full.dfMT <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfMT)
kwh.full.dfNC <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNC)
kwh.full.dfND <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfND)
kwh.full.dfNE <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNE)
kwh.full.dfNH <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNH)
kwh.full.dfNJ <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNJ)
kwh.full.dfNM <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNM)
kwh.full.dfNV <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNV)
kwh.full.dfNY <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfNY)
kwh.full.dfOH <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfOH)
kwh.full.dfOK <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfOK)
kwh.full.dfOR <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfOR)
kwh.full.dfPA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfPA)
kwh.full.dfRI <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfRI)
kwh.full.dfSC <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfSC)
kwh.full.dfSD <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfSD)
kwh.full.dfTN <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfTN)
kwh.full.dfTX <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfTX)
kwh.full.dfUT <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfUT)
kwh.full.dfVA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfVA)
kwh.full.dfVT <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfVT)
kwh.full.dfWA <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfWA)
kwh.full.dfWI <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfWI)
kwh.full.dfWV <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfWV)
kwh.full.dfWY <- lm(KWH ~ AVGINCOME  + DEGREE + DETACHED  + HHSIZE + OWN + ROOMS + WHITE + LNHHSIZE + LNROOMS + LNAVGINCOME, data=dfWY)


# STEP MODELS

kwh.step.dfAK <- stepAIC(kwh.null.dfAK, scope = formula(kwh.full.dfAK),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfAL <- stepAIC(kwh.null.dfAL, scope = formula(kwh.full.dfAL),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfAR <- stepAIC(kwh.null.dfAR, scope = formula(kwh.full.dfAR),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfAZ <- stepAIC(kwh.null.dfAZ, scope = formula(kwh.full.dfAZ),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfCA <- stepAIC(kwh.null.dfCA, scope = formula(kwh.full.dfCA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfCO <- stepAIC(kwh.null.dfCO, scope = formula(kwh.full.dfCO),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfCT <- stepAIC(kwh.null.dfCT, scope = formula(kwh.full.dfCT),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfDC <- stepAIC(kwh.null.dfDC, scope = formula(kwh.full.dfDC),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfDE <- stepAIC(kwh.null.dfDE, scope = formula(kwh.full.dfDE),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfFL <- stepAIC(kwh.null.dfFL, scope = formula(kwh.full.dfFL),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfGA <- stepAIC(kwh.null.dfGA, scope = formula(kwh.full.dfGA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfHI <- stepAIC(kwh.null.dfHI, scope = formula(kwh.full.dfHI),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfIA <- stepAIC(kwh.null.dfIA, scope = formula(kwh.full.dfIA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfID <- stepAIC(kwh.null.dfID, scope = formula(kwh.full.dfID),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfIL <- stepAIC(kwh.null.dfIL, scope = formula(kwh.full.dfIL),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfIN <- stepAIC(kwh.null.dfIN, scope = formula(kwh.full.dfIN),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfKS <- stepAIC(kwh.null.dfKS, scope = formula(kwh.full.dfKS),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfKY <- stepAIC(kwh.null.dfKY, scope = formula(kwh.full.dfKY),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfLA <- stepAIC(kwh.null.dfLA, scope = formula(kwh.full.dfLA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMA <- stepAIC(kwh.null.dfMA, scope = formula(kwh.full.dfMA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMD <- stepAIC(kwh.null.dfMD, scope = formula(kwh.full.dfMD),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfME <- stepAIC(kwh.null.dfME, scope = formula(kwh.full.dfME),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMI <- stepAIC(kwh.null.dfMI, scope = formula(kwh.full.dfMI),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMN <- stepAIC(kwh.null.dfMN, scope = formula(kwh.full.dfMN),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMO <- stepAIC(kwh.null.dfMO, scope = formula(kwh.full.dfMO),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMS <- stepAIC(kwh.null.dfMS, scope = formula(kwh.full.dfMS),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfMT <- stepAIC(kwh.null.dfMT, scope = formula(kwh.full.dfMT),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNC <- stepAIC(kwh.null.dfNC, scope = formula(kwh.full.dfNC),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfND <- stepAIC(kwh.null.dfND, scope = formula(kwh.full.dfND),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNE <- stepAIC(kwh.null.dfNE, scope = formula(kwh.full.dfNE),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNH <- stepAIC(kwh.null.dfNH, scope = formula(kwh.full.dfNH),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNJ <- stepAIC(kwh.null.dfNJ, scope = formula(kwh.full.dfNJ),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNM <- stepAIC(kwh.null.dfNM, scope = formula(kwh.full.dfNM),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNV <- stepAIC(kwh.null.dfNV, scope = formula(kwh.full.dfNV),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfNY <- stepAIC(kwh.null.dfNY, scope = formula(kwh.full.dfNY),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfOH <- stepAIC(kwh.null.dfOH, scope = formula(kwh.full.dfOH),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfOK <- stepAIC(kwh.null.dfOK, scope = formula(kwh.full.dfOK),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfOR <- stepAIC(kwh.null.dfOR, scope = formula(kwh.full.dfOR),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfPA <- stepAIC(kwh.null.dfPA, scope = formula(kwh.full.dfPA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfRI <- stepAIC(kwh.null.dfRI, scope = formula(kwh.full.dfRI),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfSC <- stepAIC(kwh.null.dfSC, scope = formula(kwh.full.dfSC),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfSD <- stepAIC(kwh.null.dfSD, scope = formula(kwh.full.dfSD),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfTN <- stepAIC(kwh.null.dfTN, scope = formula(kwh.full.dfTN),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfTX <- stepAIC(kwh.null.dfTX, scope = formula(kwh.full.dfTX),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfUT <- stepAIC(kwh.null.dfUT, scope = formula(kwh.full.dfUT),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfVA <- stepAIC(kwh.null.dfVA, scope = formula(kwh.full.dfVA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfVT <- stepAIC(kwh.null.dfVT, scope = formula(kwh.full.dfVT),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfWA <- stepAIC(kwh.null.dfWA, scope = formula(kwh.full.dfWA),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfWI <- stepAIC(kwh.null.dfWI, scope = formula(kwh.full.dfWI),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfWV <- stepAIC(kwh.null.dfWV, scope = formula(kwh.full.dfWV),  direction = "forward", weights = recs$NWEIGHT)
kwh.step.dfWY <- stepAIC(kwh.null.dfWY, scope = formula(kwh.full.dfWY),  direction = "forward", weights = recs$NWEIGHT)

### Joining Data Frames
## Best solution from: https://adairama.wordpress.com/2017/11/22/how-to-merge-multiple-datasets-in-r-based-on-row-names/


### Writing the merge code for all the step models

## First, create simple dataframes of all step models with just betas

var1 = data.frame("AK" = summary(kwh.step.dfAK)$coefficients[, 1])
var2 = data.frame("AL" = summary(kwh.step.dfAL)$coefficients[, 1])
var3 = data.frame("AR" = summary(kwh.step.dfAR)$coefficients[, 1])
var4 = data.frame("AZ" = summary(kwh.step.dfAZ)$coefficients[, 1])
var5 = data.frame("CA" = summary(kwh.step.dfCA)$coefficients[, 1])
var6 = data.frame("CO" = summary(kwh.step.dfCO)$coefficients[, 1])
var7 = data.frame("CT" = summary(kwh.step.dfCT)$coefficients[, 1])
var8 = data.frame("DC" = summary(kwh.step.dfDC)$coefficients[, 1])
var9 = data.frame("DE" = summary(kwh.step.dfDE)$coefficients[, 1])
var10 = data.frame("FL" = summary(kwh.step.dfFL)$coefficients[, 1])
var11 = data.frame("GA" = summary(kwh.step.dfGA)$coefficients[, 1])
var12 = data.frame("HI" = summary(kwh.step.dfHI)$coefficients[, 1])
var13 = data.frame("IA" = summary(kwh.step.dfIA)$coefficients[, 1])
var14 = data.frame("ID" = summary(kwh.step.dfID)$coefficients[, 1])
var15 = data.frame("IL" = summary(kwh.step.dfIL)$coefficients[, 1])
var16 = data.frame("IN" = summary(kwh.step.dfIN)$coefficients[, 1])
var17 = data.frame("KS" = summary(kwh.step.dfKS)$coefficients[, 1])
var18 = data.frame("KY" = summary(kwh.step.dfKY)$coefficients[, 1])
var19 = data.frame("LA" = summary(kwh.step.dfLA)$coefficients[, 1])
var20 = data.frame("MA" = summary(kwh.step.dfMA)$coefficients[, 1])
var21 = data.frame("MD" = summary(kwh.step.dfMD)$coefficients[, 1])
var22 = data.frame("ME" = summary(kwh.step.dfME)$coefficients[, 1])
var23 = data.frame("MI" = summary(kwh.step.dfMI)$coefficients[, 1])
var24 = data.frame("MN" = summary(kwh.step.dfMN)$coefficients[, 1])
var25 = data.frame("MO" = summary(kwh.step.dfMO)$coefficients[, 1])
var26 = data.frame("MS" = summary(kwh.step.dfMS)$coefficients[, 1])
var27 = data.frame("MT" = summary(kwh.step.dfMT)$coefficients[, 1])
var28 = data.frame("NC" = summary(kwh.step.dfNC)$coefficients[, 1])
var29 = data.frame("ND" = summary(kwh.step.dfND)$coefficients[, 1])
var30 = data.frame("NE" = summary(kwh.step.dfNE)$coefficients[, 1])
var31 = data.frame("NH" = summary(kwh.step.dfNH)$coefficients[, 1])
var32 = data.frame("NJ" = summary(kwh.step.dfNJ)$coefficients[, 1])
var33 = data.frame("NM" = summary(kwh.step.dfNM)$coefficients[, 1])
var34 = data.frame("NV" = summary(kwh.step.dfNV)$coefficients[, 1])
var35 = data.frame("NY" = summary(kwh.step.dfNY)$coefficients[, 1])
var36 = data.frame("OH" = summary(kwh.step.dfOH)$coefficients[, 1])
var37 = data.frame("OK" = summary(kwh.step.dfOK)$coefficients[, 1])
var38 = data.frame("OR" = summary(kwh.step.dfOR)$coefficients[, 1])
var39 = data.frame("PA" = summary(kwh.step.dfPA)$coefficients[, 1])
var40 = data.frame("RI" = summary(kwh.step.dfRI)$coefficients[, 1])
var41 = data.frame("SC" = summary(kwh.step.dfSC)$coefficients[, 1])
var42 = data.frame("SD" = summary(kwh.step.dfSD)$coefficients[, 1])
var43 = data.frame("TN" = summary(kwh.step.dfTN)$coefficients[, 1])
var44 = data.frame("TX" = summary(kwh.step.dfTX)$coefficients[, 1])
var45 = data.frame("UT" = summary(kwh.step.dfUT)$coefficients[, 1])
var46 = data.frame("VA" = summary(kwh.step.dfVA)$coefficients[, 1])
var47 = data.frame("VT" = summary(kwh.step.dfVT)$coefficients[, 1])
var48 = data.frame("WA" = summary(kwh.step.dfWA)$coefficients[, 1])
var49 = data.frame("WI" = summary(kwh.step.dfWI)$coefficients[, 1])
var50 = data.frame("WV" = summary(kwh.step.dfWV)$coefficients[, 1])
var51 = data.frame("WY" = summary(kwh.step.dfWY)$coefficients[, 1])
 
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
  var51
) )
colnames(out) <- gsub("^_", "", colnames(out))

View(out)

# Create vectors with Sample Size (N), Adjusted R squared, Degrees of Freedom and Sigma

AK <- c("N" = nrow(dfAK), "AdjR2" = round(summary(kwh.step.dfAK)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfAK)$df, "Sigma" = summary(kwh.step.dfAK)$sigma)
AL <- c("N" = nrow(dfAL), "AdjR2" = round(summary(kwh.step.dfAL)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfAL)$df, "Sigma" = summary(kwh.step.dfAL)$sigma)
AR <- c("N" = nrow(dfAR), "AdjR2" = round(summary(kwh.step.dfAR)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfAR)$df, "Sigma" = summary(kwh.step.dfAR)$sigma)
AZ <- c("N" = nrow(dfAZ), "AdjR2" = round(summary(kwh.step.dfAZ)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfAZ)$df, "Sigma" = summary(kwh.step.dfAZ)$sigma)
CA <- c("N" = nrow(dfCA), "AdjR2" = round(summary(kwh.step.dfCA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfCA)$df, "Sigma" = summary(kwh.step.dfCA)$sigma)
CO <- c("N" = nrow(dfCO), "AdjR2" = round(summary(kwh.step.dfCO)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfCO)$df, "Sigma" = summary(kwh.step.dfCO)$sigma)
CT <- c("N" = nrow(dfCT), "AdjR2" = round(summary(kwh.step.dfCT)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfCT)$df, "Sigma" = summary(kwh.step.dfCT)$sigma)
DC <- c("N" = nrow(dfDC), "AdjR2" = round(summary(kwh.step.dfDC)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfDC)$df, "Sigma" = summary(kwh.step.dfDC)$sigma)
DE <- c("N" = nrow(dfDE), "AdjR2" = round(summary(kwh.step.dfDE)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfDE)$df, "Sigma" = summary(kwh.step.dfDE)$sigma)
FL <- c("N" = nrow(dfFL), "AdjR2" = round(summary(kwh.step.dfFL)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfFL)$df, "Sigma" = summary(kwh.step.dfFL)$sigma)
GA <- c("N" = nrow(dfGA), "AdjR2" = round(summary(kwh.step.dfGA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfGA)$df, "Sigma" = summary(kwh.step.dfGA)$sigma)
HI <- c("N" = nrow(dfHI), "AdjR2" = round(summary(kwh.step.dfHI)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfHI)$df, "Sigma" = summary(kwh.step.dfHI)$sigma)
IA <- c("N" = nrow(dfIA), "AdjR2" = round(summary(kwh.step.dfIA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfIA)$df, "Sigma" = summary(kwh.step.dfIA)$sigma)
ID <- c("N" = nrow(dfID), "AdjR2" = round(summary(kwh.step.dfID)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfID)$df, "Sigma" = summary(kwh.step.dfID)$sigma)
IL <- c("N" = nrow(dfIL), "AdjR2" = round(summary(kwh.step.dfIL)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfIL)$df, "Sigma" = summary(kwh.step.dfIL)$sigma)
IN <- c("N" = nrow(dfIN), "AdjR2" = round(summary(kwh.step.dfIN)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfIN)$df, "Sigma" = summary(kwh.step.dfIN)$sigma)
KS <- c("N" = nrow(dfKS), "AdjR2" = round(summary(kwh.step.dfKS)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfKS)$df, "Sigma" = summary(kwh.step.dfKS)$sigma)
KY <- c("N" = nrow(dfKY), "AdjR2" = round(summary(kwh.step.dfKY)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfKY)$df, "Sigma" = summary(kwh.step.dfKY)$sigma)
LA <- c("N" = nrow(dfLA), "AdjR2" = round(summary(kwh.step.dfLA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfLA)$df, "Sigma" = summary(kwh.step.dfLA)$sigma)
MA <- c("N" = nrow(dfMA), "AdjR2" = round(summary(kwh.step.dfMA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMA)$df, "Sigma" = summary(kwh.step.dfMA)$sigma)
MD <- c("N" = nrow(dfMD), "AdjR2" = round(summary(kwh.step.dfMD)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMD)$df, "Sigma" = summary(kwh.step.dfMD)$sigma)
ME <- c("N" = nrow(dfME), "AdjR2" = round(summary(kwh.step.dfME)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfME)$df, "Sigma" = summary(kwh.step.dfME)$sigma)
MI <- c("N" = nrow(dfMI), "AdjR2" = round(summary(kwh.step.dfMI)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMI)$df, "Sigma" = summary(kwh.step.dfMI)$sigma)
MN <- c("N" = nrow(dfMN), "AdjR2" = round(summary(kwh.step.dfMN)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMN)$df, "Sigma" = summary(kwh.step.dfMN)$sigma)
MO <- c("N" = nrow(dfMO), "AdjR2" = round(summary(kwh.step.dfMO)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMO)$df, "Sigma" = summary(kwh.step.dfMO)$sigma)
MS <- c("N" = nrow(dfMS), "AdjR2" = round(summary(kwh.step.dfMS)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMS)$df, "Sigma" = summary(kwh.step.dfMS)$sigma)
MT <- c("N" = nrow(dfMT), "AdjR2" = round(summary(kwh.step.dfMT)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfMT)$df, "Sigma" = summary(kwh.step.dfMT)$sigma)
NC <- c("N" = nrow(dfNC), "AdjR2" = round(summary(kwh.step.dfNC)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNC)$df, "Sigma" = summary(kwh.step.dfNC)$sigma)
ND <- c("N" = nrow(dfND), "AdjR2" = round(summary(kwh.step.dfND)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfND)$df, "Sigma" = summary(kwh.step.dfND)$sigma)
NE <- c("N" = nrow(dfNE), "AdjR2" = round(summary(kwh.step.dfNE)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNE)$df, "Sigma" = summary(kwh.step.dfNE)$sigma)
NH <- c("N" = nrow(dfNH), "AdjR2" = round(summary(kwh.step.dfNH)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNH)$df, "Sigma" = summary(kwh.step.dfNH)$sigma)
NJ <- c("N" = nrow(dfNJ), "AdjR2" = round(summary(kwh.step.dfNJ)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNJ)$df, "Sigma" = summary(kwh.step.dfNJ)$sigma)
NM <- c("N" = nrow(dfNM), "AdjR2" = round(summary(kwh.step.dfNM)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNM)$df, "Sigma" = summary(kwh.step.dfNM)$sigma)
NV <- c("N" = nrow(dfNV), "AdjR2" = round(summary(kwh.step.dfNV)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNV)$df, "Sigma" = summary(kwh.step.dfNV)$sigma)
NY <- c("N" = nrow(dfNY), "AdjR2" = round(summary(kwh.step.dfNY)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfNY)$df, "Sigma" = summary(kwh.step.dfNY)$sigma)
OH <- c("N" = nrow(dfOH), "AdjR2" = round(summary(kwh.step.dfOH)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfOH)$df, "Sigma" = summary(kwh.step.dfOH)$sigma)
OK <- c("N" = nrow(dfOK), "AdjR2" = round(summary(kwh.step.dfOK)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfOK)$df, "Sigma" = summary(kwh.step.dfOK)$sigma)
OR <- c("N" = nrow(dfOR), "AdjR2" = round(summary(kwh.step.dfOR)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfOR)$df, "Sigma" = summary(kwh.step.dfOR)$sigma)
PA <- c("N" = nrow(dfPA), "AdjR2" = round(summary(kwh.step.dfPA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfPA)$df, "Sigma" = summary(kwh.step.dfPA)$sigma)
RI <- c("N" = nrow(dfRI), "AdjR2" = round(summary(kwh.step.dfRI)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfRI)$df, "Sigma" = summary(kwh.step.dfRI)$sigma)
SC <- c("N" = nrow(dfSC), "AdjR2" = round(summary(kwh.step.dfSC)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfSC)$df, "Sigma" = summary(kwh.step.dfSC)$sigma)
SD <- c("N" = nrow(dfSD), "AdjR2" = round(summary(kwh.step.dfSD)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfSD)$df, "Sigma" = summary(kwh.step.dfSD)$sigma)
TN <- c("N" = nrow(dfTN), "AdjR2" = round(summary(kwh.step.dfTN)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfTN)$df, "Sigma" = summary(kwh.step.dfTN)$sigma)
TX <- c("N" = nrow(dfTX), "AdjR2" = round(summary(kwh.step.dfTX)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfTX)$df, "Sigma" = summary(kwh.step.dfTX)$sigma)
UT <- c("N" = nrow(dfUT), "AdjR2" = round(summary(kwh.step.dfUT)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfUT)$df, "Sigma" = summary(kwh.step.dfUT)$sigma)
VA <- c("N" = nrow(dfVA), "AdjR2" = round(summary(kwh.step.dfVA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfVA)$df, "Sigma" = summary(kwh.step.dfVA)$sigma)
VT <- c("N" = nrow(dfVT), "AdjR2" = round(summary(kwh.step.dfVT)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfVT)$df, "Sigma" = summary(kwh.step.dfVT)$sigma)
WA <- c("N" = nrow(dfWA), "AdjR2" = round(summary(kwh.step.dfWA)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfWA)$df, "Sigma" = summary(kwh.step.dfWA)$sigma)
WI <- c("N" = nrow(dfWI), "AdjR2" = round(summary(kwh.step.dfWI)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfWI)$df, "Sigma" = summary(kwh.step.dfWI)$sigma)
WV <- c("N" = nrow(dfWV), "AdjR2" = round(summary(kwh.step.dfWV)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfWV)$df, "Sigma" = summary(kwh.step.dfWV)$sigma)
WY <- c("N" = nrow(dfWY), "AdjR2" = round(summary(kwh.step.dfWY)$adj.r.squared, digits = 3), "DF" = summary(kwh.step.dfWY)$df, "Sigma" = summary(kwh.step.dfWY)$sigma)

# Create data frame containing all the stats vectors
statsdf <- data.frame(
  AK,
  AL,
  AR,
  AZ,
  CA,
  CO,
  CT,
  DC,
  DE,
  FL,
  GA,
  HI,
  IA,
  ID,
  IL,
  IN,
  KS,
  KY,
  LA,
  MA,
  MD,
  ME,
  MI,
  MN,
  MO,
  MS,
  MT,
  NC,
  ND,
  NE,
  NH,
  NJ,
  NM,
  NV,
  NY,
  OH,
  OK,
  OR,
  PA,
  RI,
  SC,
  SD,
  TN,
  TX,
  UT,
  VA,
  VT,
  WA,
  WI,
  WV,
  WY
  )


## Joining the Stats and Betas into a single FINAL RESULTS FILE!!

finalresults <- merge(t(statsdf), t(out), by=0, all=T)
rownames(finalresults) <- finalresults$Row.names; finalresults$Row.names <- NULL
View(finalresults)



## Join Census Data and Census Locations

# First write data frames 


joined <-cbind(df_census_locations, df_census_data, finalresults)
colnames(joined)[colnames(joined) == '(Intercept)'] <- 'INTERCEPT'
joined[is.na(joined)] <- 0


## CALCULATE KWH AND APPEND IT TO JOINED

joined$kwh <- round(joined$INTERCEPT +  joined$DEGREE * joined$C_DEGREE + joined$ROOMS * joined$C_ROOMS + joined$OWN * joined$C_OWN + joined$AVGINCOME * joined$C_AVGINCOME + joined$LNAVGINCOME * joined$C_LNAVGINCOME, digits = 0)
joined$state <- joined$STATE

View(joined)

#Use this to add a csv file to your working directory
write.csv(joined, file = "joined_recs_results.csv", na="")



#clearing variables from memory: rm(list = ls()), and control l
#rm(list = ls())
