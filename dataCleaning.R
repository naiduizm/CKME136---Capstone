options(java.parameters = "-Xmx4g") 

####################################################################
#Import dataset. Convert from SAS to CSV and then read it into R
######################################################################

#data <- read.xport("C:/Users/tanna_000/Documents/CKME136/LLCP2017.XPT") #read SAS file
  data2017 <- read.csv("C:/Users/tanna_000/Documents/CKME136/2017.csv", na.strings="NA",header=TRUE)
  df_data <- data.frame(data2017) #store imported data as a data frame


##########################################################################
#Cleaning data. Missing values, converting to factor and re-factoring
###########################################################################

##1.Count number of missing values
  options(scipen = 999)
  missData2 <- apply(df_data, 2, function(col)sum(is.na(col))/length(col))
  View(missData2)

##2 Remove columns with more than 50% missing data
  
  selDataset <- df_data[, -which(colMeans(is.na(df_data)) >= 0.5)]
  selDataset <- data.frame(selDataset)



##3: Create variables for the following variables based on corresponding conditions

#3.1. ALCDAY5

#1=Days per week (new variable = alc30Week)
  selDataset$alc30Week<-NA
  selDataset$alc30Week[which(selDataset$ALCDAY5>=101 & selDataset$ALCDAY5<=107)] <- 1
  
  #2 = Days in past 30 days(new variable = alc30Week)
  selDataset$alc30Week[which(selDataset$ALCDAY5>=201 & selDataset$ALCDAY5<=230)] <- 2
  
  #convert into factor variable
  selDataset$alc30Week<-as.factor(selDataset$alc30Week)
  
  #drop unused levels
  selDataset$alc30Week<-droplevels(selDataset$alc30Week)

#3.2. FRUIT2


#1=Times per week (new variable = fruitIntake)
  selDataset$fruitIntake<-NA
  selDataset$fruitIntake[which(selDataset$FRUIT2>=101 & selDataset$FRUIT2<=199)] <- 1
  
  #2 = Times in days(new variable = fruitIntake)
  selDataset$fruitIntake[which(selDataset$FRUIT2>=201 & selDataset$FRUIT2<=299)] <- 2
  
  #3 = Times per month/year(new variable = fruitIntake)
  selDataset$fruitIntake[which(selDataset$FRUIT2>=301 & selDataset$FRUIT2<=399)] <- 3
  
  #4 = Less than once a month (new variable = fruitIntake)
  selDataset$fruitIntake[which(selDataset$FRUIT2==300)] <- 4
  
  #convert into factor variable
  selDataset$fruitIntake<-as.factor(selDataset$fruitIntake)
  
  #drop unused levels
  selDataset$fruitIntake<-droplevels(selDataset$fruitIntake)


#3.3. FRUITJU2

  #1 = Times per week (new variable = fruitJuIntake)
  selDataset$fruitJuIntake<-NA
  selDataset$fruitJuIntake[which(selDataset$FRUITJU2>=101 & selDataset$FRUITJU2<=199)] <- 1
  
  #2 = Times in days(new variable = fruitJuIntake)
  selDataset$fruitJuIntake[which(selDataset$FRUITJU2>=201 & selDataset$FRUITJU2<=299)] <- 2
  
  #3 = Times per month/year(new variable = fruitJuIntake)
  selDataset$fruitJuIntake[which(selDataset$FRUITJU2>=301 & selDataset$FRUITJU2<=399)] <- 3
  
  #4 = Less than once a month (new variable = fruitJuIntake)
  selDataset$fruitJuIntake[which(selDataset$FRUITJU2==300)] <- 4
  
  #convert into factor variable
  selDataset$fruitJuIntake<-as.factor(selDataset$fruitJuIntake)
  
  #drop unused levels
  selDataset$fruitJuIntake<-droplevels(selDataset$fruitJuIntake)

#3.4. FVGREEN1
  #1 = Times per week (new variable = greenIntake)
  selDataset$greenIntake<-NA
  selDataset$greenIntake[which(selDataset$FVGREEN1>=101 & selDataset$FVGREEN1<=199)] <- 1
  
  #2 = Times in days(new variable = greenIntake)
  selDataset$greenIntake[which(selDataset$FVGREEN1>=201 & selDataset$FVGREEN1<=299)] <- 2
  
  #3 = Times per month/year(new variable = greenIntake)
  selDataset$greenIntake[which(selDataset$FVGREEN1>=301 & selDataset$FVGREEN1<=399)] <- 3
  
  #4 = Less than once a month (new variable = greenIntake)
  selDataset$greenIntake[which(selDataset$FVGREEN1==300)] <- 4
  
  #convert into factor variable
  selDataset$greenIntake<-as.factor(selDataset$greenIntake)
  
  #drop unused levels
  selDataset$greenIntake<-droplevels(selDataset$greenIntake)

#3.5. FRENCHF1
  #1 = Times per week (new variable = fryIntake)
  selDataset$fryIntake<-NA
  selDataset$fryIntake[which(selDataset$FRENCHF1>=101 & selDataset$FRENCHF1<=199)] <- 1
  
  #2 = Times in days(new variable = fryIntake)
  selDataset$fryIntake[which(selDataset$FRENCHF1>=201 & selDataset$FRENCHF1<=299)] <- 2
  
  #3 = Times per month/year(new variable = fryIntake)
  selDataset$fryIntake[which(selDataset$FRENCHF1>=301 & selDataset$FRENCHF1<=399)] <- 3
  
  #4 = Less than once a month (new variable = fryIntake)
  selDataset$fryIntake[which(selDataset$FRENCHF1==300)] <- 4
  
  #convert into factor variable
  selDataset$fryIntake<-as.factor(selDataset$fryIntake)
  
  #drop unused levels
  selDataset$fryIntake<-droplevels(selDataset$fryIntake)

#3.6. POTATOE1
  #1 = Times per week (new variable = potatoIntake)
  selDataset$potatoIntake<-NA
  selDataset$potatoIntake[which(selDataset$POTATOE1>=101 & selDataset$POTATOE1<=199)] <- 1
  
  #2 = Times in days(new variable = potatoIntake)
  selDataset$potatoIntake[which(selDataset$POTATOE1>=201 & selDataset$POTATOE1<=299)] <- 2
  
  #3 = Times per month/year(new variable = potatoIntake)
  selDataset$potatoIntake[which(selDataset$POTATOE1>=301 & selDataset$POTATOE1<=399)] <- 3
  
  #4 = Less than once a month (new variable = potatoIntake)
  selDataset$potatoIntake[which(selDataset$POTATOE1==300)] <- 4
  
  #convert into factor variable
  selDataset$potatoIntake<-as.factor(selDataset$potatoIntake)
  
  #drop unused levels
  selDataset$potatoIntake<-droplevels(selDataset$potatoIntake)


#3.7. VEGETAB2
  #1 = Times per week (new variable = vegIntake)
  selDataset$vegIntake<-NA
  selDataset$vegIntake[which(selDataset$VEGETAB2>=101 & selDataset$VEGETAB2<=199)] <- 1
  
  #2 = Times in days(new variable = vegIntake)
  selDataset$vegIntake[which(selDataset$VEGETAB2>=201 & selDataset$VEGETAB2<=299)] <- 2
  
  #3 = Times per month/year(new variable = vegIntake)
  selDataset$vegIntake[which(selDataset$VEGETAB2>=301 & selDataset$VEGETAB2<=399)] <- 3
  
  #4 = Less than once a month (new variable = vegIntake)
  selDataset$vegIntake[which(selDataset$VEGETAB2==300)] <- 4
  
  #convert into factor variable
  selDataset$vegIntake<-as.factor(selDataset$vegIntake)
  
  #drop unused levels
  selDataset$vegIntake<-droplevels(selDataset$vegIntake)


#3.8. EXEROFT1
  #1 = Times per week (new variable = exWeekMonth)
  selDataset$exWeekMonth<-NA
  selDataset$exWeekMonth[which(selDataset$EXEROFT1>=101 & selDataset$EXEROFT1<=199)] <- 1
  
  #2 = Times in days(new variable = exWeekMonth)
  selDataset$exWeekMonth[which(selDataset$EXEROFT1>=201 & selDataset$EXEROFT1<=299)] <- 2
  
  #convert into factor variable
  selDataset$exWeekMonth<-as.factor(selDataset$exWeekMonth)
  
  #drop unused levels
  selDataset$exWeekMonth<-droplevels(selDataset$exWeekMonth)



#3.9. EXERHMM1
  #1 = Times per week (new variable = exMinHour)
  selDataset$exMinHour<-NA
  selDataset$exMinHour[which(selDataset$EXERHMM1>=1 & selDataset$EXERHMM1<=759)] <- 1
  
  #2 = Times in days(new variable = exMinHour)
  selDataset$exMinHour[which(selDataset$EXERHMM1>=800 & selDataset$EXERHMM1<=959)] <- 2
  
  #convert into factor variable
  selDataset$exMinHour<-as.factor(selDataset$exMinHour)
  
  #drop unused levels
  selDataset$exMinHour<-droplevels(selDataset$exMinHour)

#3.10. STRENGTH
  #1 = Times per week (new variable = strengthWeekMonth)
      selDataset$strengthWeekMonth<-NA
  selDataset$strengthWeekMonth[which(selDataset$STRENGTH>=101 & selDataset$STRENGTH<=199)] <- 1
  
  #2 = Times in days(new variable = exMinHour)
  selDataset$strengthWeekMonth[which(selDataset$STRENGTH>=201 & selDataset$STRENGTH<=299)] <- 2
  
  #convert into factor variable
  selDataset$strengthWeekMonth<-as.factor(selDataset$strengthWeekMonth) 




##4: Re-encode values equal labeled as "Don't know/Not sure" as NA. Re-encode values equal to None or never as 0 
  selDataset$LANDLINE[selDataset$LANDLINE == 7] <-NA
  selDataset$LANDLINE[selDataset$LANDLINE == 9] <-NA
  
  selDataset$GENHLTH[selDataset$GENHLTH == 7] <-NA
  selDataset$GENHLTH[selDataset$GENHLTH == 9] <-NA
  
  selDataset$HLTHPLN1[selDataset$HLTHPLN1 == 7] <-NA
  selDataset$HLTHPLN1[selDataset$HLTHPLN1 == 9] <-NA
  
  selDataset$PERSDOC2[selDataset$PERSDOC2 == 7] <-NA
  selDataset$PERSDOC2[selDataset$PERSDOC2 == 9] <-NA
  
  selDataset$MEDCOST[selDataset$MEDCOST == 7] <-NA
  selDataset$MEDCOST[selDataset$MEDCOST == 9] <-NA
  
  selDataset$CHECKUP1[selDataset$CHECKUP1 == 7] <-NA
  selDataset$CHECKUP1[selDataset$CHECKUP1 == 9] <-NA
  
  selDataset$BPHIGH4[selDataset$BPHIGH4 == 7] <-NA
  selDataset$BPHIGH4[selDataset$BPHIGH4 == 9] <-NA
  
  selDataset$CHOLCHK1[selDataset$CHOLCHK1 == 7] <-NA
  selDataset$CHOLCHK1[selDataset$CHOLCHK1 == 9] <-NA
  
  selDataset$TOLDHI2[selDataset$TOLDHI2 == 7] <-NA
  selDataset$TOLDHI2[selDataset$TOLDHI2 == 9] <-NA
  
  selDataset$CVDINFR4[selDataset$CVDINFR4 == 7] <-NA
  selDataset$CVDINFR4[selDataset$CVDINFR4 == 9] <-NA
  
  selDataset$CVDCRHD4[selDataset$CVDCRHD4 == 7] <-NA
  selDataset$CVDCRHD4[selDataset$CVDCRHD4 == 9] <-NA
  
  selDataset$CVDSTRK3[selDataset$CVDSTRK3 == 7] <-NA
  selDataset$CVDSTRK3[selDataset$CVDSTRK3 == 9] <-NA
  
  selDataset$ASTHMA3[selDataset$ASTHMA3 == 7] <-NA
  selDataset$ASTHMA3[selDataset$ASTHMA3 == 9] <-NA
  
  selDataset$CHCSCNCR[selDataset$CHCSCNCR == 7] <-NA
  selDataset$CHCSCNCR[selDataset$CHCSCNCR == 9] <-NA
  
  selDataset$CHCOCNCR[selDataset$CHCOCNCR == 7] <-NA
  selDataset$CHCOCNCR[selDataset$CHCOCNCR == 9] <-NA
  
  selDataset$CHCCOPD1[selDataset$CHCCOPD1 == 7] <-NA
  selDataset$CHCCOPD1[selDataset$CHCCOPD1 == 9] <-NA
  
  selDataset$HAVARTH3[selDataset$HAVARTH3 == 7] <-NA
  selDataset$HAVARTH3[selDataset$HAVARTH3 == 9] <-NA
  
  selDataset$ADDEPEV2[selDataset$ADDEPEV2 == 7] <-NA
  selDataset$ADDEPEV2[selDataset$ADDEPEV2 == 9] <-NA
  
  selDataset$CHCKIDNY[selDataset$CHCKIDNY == 7] <-NA
  selDataset$CHCKIDNY[selDataset$CHCKIDNY == 9] <-NA
  
  selDataset$DIABETE3[selDataset$DIABETE3 == 7] <-NA
  selDataset$DIABETE3[selDataset$DIABETE3 == 9] <-NA
  
  selDataset$SEX[selDataset$SEX == 9] <-NA
  selDataset$MARITAL[selDataset$MARITAL == 9] <-NA
  selDataset$EDUCA[selDataset$EDUCA == 9] <-NA
  
  selDataset$RENTHOM1[selDataset$RENTHOM1 == 7] <-NA
  selDataset$RENTHOM1[selDataset$RENTHOM1 == 9] <-NA
  
  selDataset$VETERAN3[selDataset$VETERAN3 == 7] <-NA
  selDataset$VETERAN3[selDataset$VETERAN3 == 9] <-NA
  
  selDataset$EMPLOY1[selDataset$EMPLOY1 == 9] <-NA
  
  selDataset$INCOME2[selDataset$INCOME2 == 77] <-NA
  selDataset$INCOME2[selDataset$INCOME2 == 99] <-NA
  
  selDataset$INTERNET[selDataset$INTERNET == 7] <-NA
  selDataset$INTERNET[selDataset$INTERNET == 9] <-NA
  
  selDataset$DEAF[selDataset$DEAF == 7] <-NA
  selDataset$DEAF[selDataset$DEAF == 9] <-NA
  
  selDataset$BLIND[selDataset$BLIND == 7] <-NA
  selDataset$BLIND[selDataset$BLIND == 9] <-NA
  
  selDataset$DECIDE[selDataset$DECIDE == 7] <-NA
  selDataset$DECIDE[selDataset$DECIDE == 9] <-NA
  
  selDataset$DIFFWALK[selDataset$DIFFWALK == 7] <-NA
  selDataset$DIFFWALK[selDataset$DIFFWALK == 9] <-NA
  
  selDataset$DIFFDRES[selDataset$DIFFDRES == 7] <-NA
  selDataset$DIFFDRES[selDataset$DIFFDRES == 9] <-NA
  
  selDataset$DIFFALON[selDataset$DIFFALON == 7] <-NA
  selDataset$DIFFALON[selDataset$DIFFALON == 9] <-NA
  
  selDataset$SMOKE100[selDataset$SMOKE100 == 7] <-NA
  selDataset$SMOKE100[selDataset$SMOKE100 == 9] <-NA
  
  selDataset$USENOW3[selDataset$USENOW3 == 7] <-NA
  selDataset$USENOW3[selDataset$USENOW3 == 9] <-NA
  
  selDataset$ECIGARET[selDataset$ECIGARET == 7] <-NA
  selDataset$ECIGARET[selDataset$ECIGARET == 9] <-NA
  
  selDataset$EXERANY2[selDataset$EXERANY2 == 7] <-NA
  selDataset$EXERANY2[selDataset$EXERANY2 == 9] <-NA
  
  selDataset$EXRACT11[selDataset$EXRACT11 == 77] <-NA
  selDataset$EXRACT11[selDataset$EXRACT11 == 99] <-NA
  
  selDataset$EXRACT21[selDataset$EXRACT21 == 77] <-NA
  selDataset$EXRACT21[selDataset$EXRACT21 == 99] <-NA
  
  selDataset$SEATBELT[selDataset$SEATBELT == 7] <-NA
  selDataset$SEATBELT[selDataset$SEATBELT == 9] <-NA
  
  selDataset$FLUSHOT6[selDataset$FLUSHOT6 == 7] <-NA
  selDataset$FLUSHOT6[selDataset$FLUSHOT6 == 9] <-NA
  
  selDataset$PNEUVAC3[selDataset$PNEUVAC3 == 7] <-NA
  selDataset$PNEUVAC3[selDataset$PNEUVAC3 == 9] <-NA
  
  selDataset$SHINGLE2[selDataset$SHINGLE2 == 7] <-NA
  selDataset$SHINGLE2[selDataset$SHINGLE2 == 9] <-NA
  
  selDataset$HIVTST6[selDataset$HIVTST6 == 7] <-NA
  selDataset$HIVTST6[selDataset$HIVTST6 == 9] <-NA
  
  selDataset$HIVRISK5[selDataset$HIVRISK5 == 7] <-NA
  selDataset$HIVRISK5[selDataset$HIVRISK5 == 9] <-NA
  
  selDataset$PDIABTST[selDataset$PDIABTST == 7] <-NA
  selDataset$PDIABTST[selDataset$PDIABTST == 9] <-NA
  
  selDataset$PREDIAB1[selDataset$PREDIAB1 == 7] <-NA
  selDataset$PREDIAB1[selDataset$PREDIAB1 == 9] <-NA
  
  selDataset$X_CHISPNC[selDataset$X_CHISPNC == 9] <-NA
  
  selDataset$X_RFHLTH[selDataset$X_RFHLTH == 9] <-NA
  selDataset$X_PHYS14D[selDataset$X_PHYS14D == 9] <-NA
  
  selDataset$X_MENT14D[selDataset$X_MENT14D == 9] <-NA
  selDataset$X_HCVU651[selDataset$X_HCVU651 == 9] <-NA
  
  selDataset$X_RFHYPE5[selDataset$X_RFHYPE5 == 9] <-NA
  selDataset$X_CHOLCH1[selDataset$X_CHOLCH1 == 9] <-NA  
  selDataset$X_LTASTH1[selDataset$X_LTASTH1 == 9] <-NA
  selDataset$X_CASTHM1[selDataset$X_CASTHM1 == 9] <-NA
  
  selDataset$X_ASTHMS1[selDataset$X_ASTHMS1 == 9] <-NA
  selDataset$X_LMTACT1[selDataset$X_LMTACT1 == 9] <-NA
  selDataset$X_LMTWRK1[selDataset$X_LMTWRK1 == 9] <-NA
  selDataset$X_LMTSCL1[selDataset$X_LMTSCL1 == 9] <-NA
  
  
  selDataset$X_PRACE1[selDataset$X_PRACE1 == 77] <-NA
  selDataset$X_PRACE1[selDataset$X_PRACE1 == 99] <-NA
  
  
  selDataset$X_MRACE1[selDataset$X_MRACE1 == 77] <-NA
  selDataset$X_MRACE1[selDataset$X_MRACE1 == 99] <-NA
  
  selDataset$X_HISPANC[selDataset$X_HISPANC == 9] <-NA
  selDataset$X_RACE[selDataset$X_RACE == 9] <-NA
  
  selDataset$X_AGEG5YR[selDataset$X_AGEG5YR == 14] <-NA
  selDataset$X_AGE65YR[selDataset$X_AGE65YR == 3] <-NA
  
  
  selDataset$X_RFBMI5[selDataset$X_RFBMI5 == 9] <-NA
  selDataset$X_CHLDCNT[selDataset$X_CHLDCNT == 9] <-NA
  selDataset$X_EDUCAG[selDataset$X_EDUCAG == 9] <-NA
  selDataset$X_INCOMG[selDataset$X_INCOMG == 9] <-NA
  selDataset$X_SMOKER3[selDataset$X_SMOKER3 == 9] <-NA
  selDataset$X_RFSMOK3[selDataset$X_RFSMOK3 == 9] <-NA
  selDataset$X_ECIGSTS[selDataset$X_ECIGSTS == 9] <-NA
  selDataset$X_CURECIG[selDataset$X_CURECIG == 9] <-NA
  selDataset$DRNKANY5[selDataset$DRNKANY5 == 9] <-NA
  selDataset$X_RFBING5[selDataset$X_RFBING5 == 9] <-NA
  selDataset$X_RFDRHV5[selDataset$X_RFDRHV5 == 9] <-NA
  selDataset$X_FRTLT1A[selDataset$X_FRTLT1A == 9] <-NA
  selDataset$X_VEGLT1A[selDataset$X_VEGLT1A == 9] <-NA
  
  selDataset$X_TOTINDA[selDataset$X_TOTINDA == 9] <-NA
  selDataset$X_PACAT1[selDataset$X_PACAT1 == 9] <-NA
  selDataset$X_PAINDX1[selDataset$X_PAINDX1 == 9] <-NA
  selDataset$X_PA150R2[selDataset$X_PA150R2 == 9] <-NA
  selDataset$X_PA300R2[selDataset$X_PA300R2 == 9] <-NA
  selDataset$X_PA30021[selDataset$X_PA30021 == 9] <-NA
  selDataset$X_PASTRNG[selDataset$X_PASTRNG == 9] <-NA
  selDataset$X_PAREC1[selDataset$X_PAREC1 == 9] <-NA
  
  selDataset$X_PASTAE1[selDataset$X_PASTAE1 == 9] <-NA
  selDataset$X_RFSEAT2[selDataset$X_RFSEAT2 == 9] <-NA
  selDataset$X_RFSEAT3[selDataset$X_RFSEAT3 == 9] <-NA
  selDataset$X_AIDTST3[selDataset$X_AIDTST3 == 9] <-NA
  
  selDataset$PHYSHLTH[selDataset$PHYSHLTH == 77] <-NA
  selDataset$PHYSHLTH[selDataset$PHYSHLTH == 99] <-NA
  selDataset$PHYSHLTH[selDataset$PHYSHLTH == 88] <-0
  
  selDataset$MENTHLTH[selDataset$MENTHLTH == 77] <-NA
  selDataset$MENTHLTH[selDataset$MENTHLTH == 99] <-NA
  selDataset$MENTHLTH[selDataset$MENTHLTH == 88] <-0
  
  selDataset$POORHLTH[selDataset$POORHLTH == 77] <-NA
  selDataset$POORHLTH[selDataset$POORHLTH == 99] <-NA
  selDataset$POORHLTH[selDataset$POORHLTH == 88] <-0
  
  
  selDataset$CHILDREN[selDataset$CHILDREN == 99] <-NA
  
  selDataset$ALCDAY5[selDataset$ALCDAY5 == 777] <-NA
  selDataset$ALCDAY5[selDataset$ALCDAY5 == 999] <-NA
  selDataset$ALCDAY5[selDataset$ALCDAY5 == 888] <-0
  
  selDataset$FRUIT2[selDataset$FRUIT2 == 777] <-NA
  selDataset$FRUIT2[selDataset$FRUIT2 == 999] <-NA
  selDataset$FRUIT2[selDataset$FRUIT2 == 555] <-0
  
  selDataset$FRUITJU2[selDataset$FRUITJU2 == 777] <-NA
  selDataset$FRUITJU2[selDataset$FRUITJU2 == 999] <-NA
  selDataset$FRUITJU2[selDataset$FRUITJU2 == 555] <-0
  
  selDataset$FVGREEN1[selDataset$FVGREEN1 == 777] <-NA
  selDataset$FVGREEN1[selDataset$FVGREEN1 == 999] <-NA
  selDataset$FVGREEN1[selDataset$FVGREEN1 == 555] <-0
  
  selDataset$FRENCHF1[selDataset$FRENCHF1 == 777] <-NA
  selDataset$FRENCHF1[selDataset$FRENCHF1 == 999] <-NA
  selDataset$FRENCHF1[selDataset$FRENCHF1 == 555] <-0
  
  
  selDataset$POTATOE1[selDataset$POTATOE1 == 777] <-NA
  selDataset$POTATOE1[selDataset$POTATOE1 == 999] <-NA
  selDataset$POTATOE1[selDataset$POTATOE1 == 555] <-0
  
  
  selDataset$VEGETAB2[selDataset$VEGETAB2 == 777] <-NA
  selDataset$VEGETAB2[selDataset$VEGETAB2 == 999] <-NA
  selDataset$VEGETAB2[selDataset$VEGETAB2 == 555] <-0
  
  selDataset$EXEROFT1[selDataset$EXEROFT1 == 777] <-NA
  selDataset$EXEROFT1[selDataset$EXEROFT1 == 999] <-NA
  
  selDataset$EXERHMM1[selDataset$EXERHMM1 == 999] <-NA
  
  selDataset$STRENGTH[selDataset$STRENGTH == 777] <-NA
  selDataset$STRENGTH[selDataset$STRENGTH == 999] <-NA
  selDataset$STRENGTH[selDataset$STRENGTH == 888] <-NA
  
  
  selDataset$DROCDY3_[selDataset$DROCDY3_ == 900] <-NA
  
  selDataset$X_DRNKWEK[selDataset$X_DRNKWEK == 99900] <-NA
  
  selDataset$MAXVO2_[selDataset$MAXVO2_== 99900] <-NA
  selDataset$FC60_[selDataset$FC60_ == 99900] <-NA
  
  selDataset$PAFREQ1_[selDataset$PAFREQ1_ == 99000] <-NA
  selDataset$STRFREQ_[selDataset$STRFREQ_ == 99000] <-NA
  
  
  #9. Re-factor target variable values of 2(No) to 0
  selDataset$ADDEPEV2[selDataset$ADDEPEV2==2]<-0
  
  ##5: Discard variables not useful for analysis  
  
  selDataset<-subset(selDataset,select = -c(X,IDATE,SEQNO,X_PSU,WEIGHT2,HEIGHT3,DLYOTHER,X_STSTR,X_STRWT,X_RAWRAKE,X_WT2RAKE,X_DUALCOR,X_LLCPWT2,X_LLCPWT))

  

##6. Factorize variables that are categorical using VAR# from spreadsheet
  
  cols <- c(1:14,18:41,43:53,61:62,65,67:103,105,110:119,121,123,136:139,142,147,148,161:181)
  selDataset[cols] <- lapply(selDataset[cols], factor) 
  selDataset<-subset(selDataset,select = -c(X_MISFRT1,X_MISVEG1,X_FRTRES1,  X_VEGRES1,X_FRUITE1,X_VEGETE1,PAMISS1_))
  
  

  #7. Re-factor EXRACT11 as it has too many categories
  #num of resondents for this variable
  exract11<-table(selDataset$EXRACT11)
  exract11<-sort(exract11,decreasing = TRUE)
  View(exract11)
  #select top 60% of categories sorted by frequency
  selExract11<-head(exract11,n=45)
  View(selExract11)
  
  
  ##Group the other levels/categories not in top 60% as other
  
  selDataset$EXRACT11[selDataset$EXRACT11==1 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==3 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==4 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==12 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==17 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==21 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==26 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==27 & !is.na(selDataset$EXRACT11)]<-46
  
  
  selDataset$EXRACT11[selDataset$EXRACT11==29 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==30 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==32 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==33 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==35 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==36 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==39 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==41 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==42 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==43 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==44 & !is.na(selDataset$EXRACT11)]<-46
  
  
  selDataset$EXRACT11[selDataset$EXRACT11==45 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==46 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==47 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==50 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==53 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==55 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==56 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==59 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==62 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==65 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==66 & !is.na(selDataset$EXRACT11)]<-46
  
  
  selDataset$EXRACT11[selDataset$EXRACT11==68 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==70 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==77 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==78 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==79 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==80 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==81 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==82 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==83 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==84 & !is.na(selDataset$EXRACT11)]<-46
  
  
  selDataset$EXRACT11[selDataset$EXRACT11==85 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==86 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==87 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==88 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==90 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==91 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==92 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==93 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==94 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==95 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==96 & !is.na(selDataset$EXRACT11)]<-46
  selDataset$EXRACT11[selDataset$EXRACT11==97 & !is.na(selDataset$EXRACT11)]<-46
  
  
  
  #8. Re-factor EXRACT21 as it has too many categories
  #num of resondents for this variable
  exract21<-table(selDataset$EXRACT21)
  exract21<-sort(exract21,decreasing = TRUE)
  View(exract21)
  #select top 60% of categories sorted by frequency
  selExract21<-head(exract21,n=45)
  View(selExract21)
  
  
  ##Group the other levels/categories not in top 60% as other
  
  selDataset$EXRACT21[selDataset$EXRACT21==1 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==3 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==4 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==7 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==9 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==17 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==21 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==23 & !is.na(selDataset$EXRACT21)]<-46
  
  
  selDataset$EXRACT21[selDataset$EXRACT21==27 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==29 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==30 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==32 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==33 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==35 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==38 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==39 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==41 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==42 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==43 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==44 & !is.na(selDataset$EXRACT21)]<-46
  
  
  selDataset$EXRACT21[selDataset$EXRACT21==45 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==46 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==47 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==50 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT11==53 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==55 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==56 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==59 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==62 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==65 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==66 & !is.na(selDataset$EXRACT21)]<-46
  
  
  selDataset$EXRACT21[selDataset$EXRACT21==68 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==70 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==77 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==78 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==79 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==80 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==81 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==82 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==83 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==84 & !is.na(selDataset$EXRACT21)]<-46
  
  
  selDataset$EXRACT21[selDataset$EXRACT21==85 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==86 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==87 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==88 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==90 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==91 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==92 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==93 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==94 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==95 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==96 & !is.na(selDataset$EXRACT21)]<-46
  selDataset$EXRACT21[selDataset$EXRACT21==97 & !is.na(selDataset$EXRACT21)]<-46


  
  
  ##10. Drop usused levels from all categorical variables
  selDataset$X_STATE<-droplevels(selDataset$X_STATE)
  selDataset$FMONTH<-droplevels(selDataset$FMONTH)
  selDataset$IMONTH<-droplevels(selDataset$IMONTH)
  selDataset$IDAY<-droplevels(selDataset$IDAY)
  selDataset$IYEAR<-droplevels(selDataset$IYEAR)
  selDataset$DISPCODE<-droplevels(selDataset$DISPCODE)
  selDataset$SAFETIME<-droplevels(selDataset$SAFETIME)
  selDataset$CTELNUM1<-droplevels(selDataset$CTELNUM1)
  selDataset$CELLFON5<-droplevels(selDataset$CELLFON5)
  
  selDataset$CADULT<-droplevels(selDataset$CADULT)
  selDataset$PVTRESD3<-droplevels(selDataset$PVTRESD3)
  selDataset$CSTATE1<-droplevels(selDataset$CSTATE1)
  selDataset$LANDLINE<-droplevels(selDataset$LANDLINE)
  selDataset$GENHLTH<-droplevels(selDataset$GENHLTH)
  selDataset$HLTHPLN1<-droplevels(selDataset$HLTHPLN1)
  selDataset$PERSDOC2<-droplevels(selDataset$PERSDOC2)
  selDataset$MEDCOST<-droplevels(selDataset$MEDCOST)
  selDataset$CHECKUP1<-droplevels(selDataset$CHECKUP1)
  selDataset$BPHIGH4<-droplevels(selDataset$BPHIGH4)
  selDataset$CHOLCHK1<-droplevels(selDataset$CHOLCHK1)
  
  selDataset$TOLDHI2<-droplevels(selDataset$TOLDHI2)
  selDataset$CVDINFR4<-droplevels(selDataset$CVDINFR4)
  selDataset$CVDCRHD4<-droplevels(selDataset$CVDCRHD4)
  selDataset$CVDSTRK3<-droplevels(selDataset$CVDSTRK3)
  selDataset$ASTHMA3<-droplevels(selDataset$ASTHMA3)
  selDataset$CHCSCNCR<-droplevels(selDataset$CHCSCNCR)
  selDataset$CHCOCNCR<-droplevels(selDataset$CHCOCNCR)
  selDataset$CHCCOPD1<-droplevels(selDataset$CHCCOPD1)
  selDataset$HAVARTH3<-droplevels(selDataset$HAVARTH3)
  selDataset$ADDEPEV2<-droplevels(selDataset$ADDEPEV2)
  selDataset$CHCKIDNY<-droplevels(selDataset$CHCKIDNY)
  selDataset$DIABETE3<-droplevels(selDataset$DIABETE3)
  
  
  selDataset$SEX<-droplevels(selDataset$SEX)
  selDataset$MARITAL<-droplevels(selDataset$MARITAL)
  selDataset$EDUCA<-droplevels(selDataset$EDUCA)
  selDataset$RENTHOM1<-droplevels(selDataset$RENTHOM1)
  selDataset$VETERAN3<-droplevels(selDataset$VETERAN3)
  selDataset$EMPLOY1<-droplevels(selDataset$EMPLOY1)
  selDataset$INCOME2<-droplevels(selDataset$INCOME2)
  selDataset$INTERNET<-droplevels(selDataset$INTERNET)
  selDataset$DEAF<-droplevels(selDataset$DEAF)
  selDataset$BLIND<-droplevels(selDataset$BLIND)
  selDataset$DECIDE<-droplevels(selDataset$DECIDE)
  selDataset$DIFFWALK<-droplevels(selDataset$DIFFWALK)
  selDataset$DIFFDRES<-droplevels(selDataset$DIFFDRES)
  selDataset$DIFFALON<-droplevels(selDataset$DIFFALON)
  selDataset$SMOKE100<-droplevels(selDataset$SMOKE100)
  
  
  selDataset$USENOW3<-droplevels(selDataset$USENOW3)
  selDataset$ECIGARET<-droplevels(selDataset$ECIGARET)
  selDataset$EXERANY2<-droplevels(selDataset$EXERANY2)
  selDataset$EXRACT11<-droplevels(selDataset$EXRACT11)
  selDataset$EXRACT21<-droplevels(selDataset$EXRACT21)
  selDataset$SEATBELT<-droplevels(selDataset$SEATBELT)
  selDataset$FLUSHOT6<-droplevels(selDataset$FLUSHOT6)
  selDataset$PNEUVAC3<-droplevels(selDataset$PNEUVAC3)
  selDataset$SHINGLE2<-droplevels(selDataset$SHINGLE2)
  selDataset$HIVTST6<-droplevels(selDataset$HIVTST6)
  selDataset$HIVRISK5<-droplevels(selDataset$HIVRISK5)
  selDataset$PDIABTST<-droplevels(selDataset$PDIABTST)
  selDataset$PREDIAB1<-droplevels(selDataset$PREDIAB1)
  
  
  selDataset$QSTVER<-droplevels(selDataset$QSTVER)
  selDataset$QSTLANG<-droplevels(selDataset$QSTLANG)
  selDataset$X_IMPRACE<-droplevels(selDataset$X_IMPRACE)
  selDataset$X_CHISPNC<-droplevels(selDataset$X_CHISPNC)
  selDataset$X_DUALUSE<-droplevels(selDataset$X_DUALUSE)
  selDataset$X_RFHLTH<-droplevels(selDataset$X_RFHLTH)
  selDataset$X_PHYS14D<-droplevels(selDataset$X_PHYS14D)
  selDataset$X_MENT14D<-droplevels(selDataset$X_MENT14D)
  selDataset$X_HCVU651<-droplevels(selDataset$X_HCVU651)
  selDataset$X_RFHYPE5<-droplevels(selDataset$X_RFHYPE5)
  selDataset$X_CHOLCH1<-droplevels(selDataset$X_CHOLCH1)
  selDataset$X_RFCHOL1<-droplevels(selDataset$X_RFCHOL1)
  selDataset$X_MICHD<-droplevels(selDataset$X_MICHD)
  selDataset$X_LTASTH1<-droplevels(selDataset$X_LTASTH1)
  selDataset$X_CASTHM1<-droplevels(selDataset$X_CASTHM1)
  selDataset$X_ASTHMS1<-droplevels(selDataset$X_ASTHMS1)
  selDataset$X_DRDXAR1<-droplevels(selDataset$X_DRDXAR1) 
  selDataset$X_LMTACT1<-droplevels(selDataset$X_LMTACT1)
  selDataset$X_LMTWRK1<-droplevels(selDataset$X_LMTWRK1)
  selDataset$X_LMTSCL1<-droplevels(selDataset$X_LMTSCL1)
  selDataset$X_PRACE1<-droplevels(selDataset$X_PRACE1)
  selDataset$X_MRACE1<-droplevels(selDataset$X_MRACE1)
  selDataset$X_HISPANC<-droplevels(selDataset$X_HISPANC)
  selDataset$X_RACE<-droplevels(selDataset$X_RACE)
  
  selDataset$X_RACEG21<-droplevels(selDataset$X_RACEG21)
  selDataset$X_RACEGR3<-droplevels(selDataset$X_RACEGR3)
  selDataset$X_RACE_G1<-droplevels(selDataset$X_RACE_G1)
  selDataset$X_AGEG5YR<-droplevels(selDataset$X_AGEG5YR)
  selDataset$X_AGE65YR<-droplevels(selDataset$X_AGE65YR)
  selDataset$X_AGE_G<-droplevels(selDataset$X_AGE_G)
  selDataset$X_BMI5CAT<-droplevels(selDataset$X_BMI5CAT)
  selDataset$X_RFBMI5<-droplevels(selDataset$X_RFBMI5)
  selDataset$X_CHLDCNT<-droplevels(selDataset$X_CHLDCNT)
  selDataset$X_EDUCAG<-droplevels(selDataset$X_EDUCAG)
  selDataset$X_INCOMG<-droplevels(selDataset$X_INCOMG)
  selDataset$X_SMOKER3<-droplevels(selDataset$X_SMOKER3)
  selDataset$X_RFSMOK3<-droplevels(selDataset$X_RFSMOK3)
  selDataset$X_CURECIG<-droplevels(selDataset$X_CURECIG)
  selDataset$DRNKANY5<-droplevels(selDataset$DRNKANY5)
  selDataset$X_RFBING5<-droplevels(selDataset$X_RFBING5)
  selDataset$X_RFDRHV5<-droplevels(selDataset$X_RFDRHV5)
  selDataset$X_FRTLT1A<-droplevels(selDataset$X_FRTLT1A)
  
  
  
  selDataset$X_VEGLT1A<-droplevels(selDataset$X_VEGLT1A) 
  selDataset$X_FRT16A<-droplevels(selDataset$X_FRT16A)
  selDataset$X_VEG23A<-droplevels(selDataset$X_VEG23A)
  selDataset$X_TOTINDA<-droplevels(selDataset$X_TOTINDA)
  selDataset$ACTIN11_<-droplevels(selDataset$ACTIN11_)
  selDataset$ACTIN21_<-droplevels(selDataset$ACTIN21_)
  selDataset$X_PACAT1<-droplevels(selDataset$X_PACAT1)
  selDataset$X_PAINDX1<-droplevels(selDataset$X_PAINDX1) 
  selDataset$X_PA150R2<-droplevels(selDataset$X_PA150R2)
  
  selDataset$X_PA300R2<-droplevels(selDataset$X_PA300R2) 
  selDataset$X_PA30021<-droplevels(selDataset$X_PA30021)
  selDataset$X_PASTRNG<-droplevels(selDataset$X_PASTRNG) 
  selDataset$X_PAREC1<-droplevels(selDataset$X_PAREC1)
  selDataset$X_PASTAE1<-droplevels(selDataset$X_PASTAE1) 
  selDataset$X_RFSEAT2<-droplevels(selDataset$X_RFSEAT2)
  selDataset$X_RFSEAT3<-droplevels(selDataset$X_RFSEAT3)
  selDataset$X_AIDTST3<-droplevels(selDataset$X_AIDTST3)
  selDataset$alc30Week<-droplevels(selDataset$alc30Week)
  selDataset$fruitIntake<-droplevels(selDataset$fruitIntake)
  selDataset$fruitJuIntake<-droplevels(selDataset$fruitJuIntake)
  selDataset$greenIntake<-droplevels(selDataset$greenIntake)
  selDataset$fryIntake<-droplevels(selDataset$fryIntake)
  selDataset$potatoIntake<-droplevels(selDataset$potatoIntake)
  selDataset$vegIntake<-droplevels(selDataset$vegIntake)
  selDataset$exWeekMonth<-droplevels(selDataset$exWeekMonth)
  selDataset$exMinHour<-droplevels(selDataset$exMinHour)
  selDataset$strengthWeekMonth<-droplevels(selDataset$strengthWeekMonth)
  
  
  
  
  
  
  
  #########################################################################################
  
  #Outlier Analysis
  
  ######################################################################################
  
  #store continuous  and categorical data in seperate variable for analysis
  library(dplyr)
  continuous <-select_if(selDataset, is.numeric)
  categorical<-select_if(selDataset,is.factor)
  continuous <- data.frame(continuous)
  categorical<-data.frame(categorical)
  View(summary(continuous))
  View(summary(categorical))
  
  #function to impute NA for continuous variable with median
  impute <- function(data, type) {
    for (i in which(sapply(data, is.numeric))) {
      data[is.na(data[, i]), i] <- type(data[, i],  na.rm = TRUE)
    }
    return(data)}
  
  continuous <- impute(continuous,median) #perform imputation
  sum(is.na(continuous))  #check for # of NA values for continuous data
  
  
  
  #impute NA in categorical variable with mode
  getmode <- function(v) {
    uniqv <- na.omit(unique(v))
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  categorical$SAFETIME[is.na(categorical$SAFETIME)]<-getmode(categorical$SAFETIME) #should remove since only one level
  categorical$CTELNUM1[is.na(categorical$CTELNUM1)]<-getmode(categorical$CTELNUM1)#should remove since only one level
  categorical$CELLFON5[is.na(categorical$CELLFON5)]<-getmode(categorical$CELLFON5)#should remove since only one level
  categorical<-subset(categorical,select = -c(SAFETIME,CTELNUM1,CTELNUM1))
  
  
  
  categorical$CADULT[is.na(categorical$CADULT)]<-getmode(categorical$CADULT)
  categorical$PVTRESD3[is.na(categorical$PVTRESD3)]<-getmode(categorical$PVTRESD3)
  categorical$CSTATE1[is.na(categorical$CSTATE1)]<-getmode(categorical$CSTATE1)
  categorical$LANDLINE[is.na(categorical$LANDLINE)]<-getmode(categorical$LANDLINE)
  categorical$GENHLTH[is.na(categorical$GENHLTH)]<-getmode(categorical$GENHLTH)
  categorical$HLTHPLN1[is.na(categorical$HLTHPLN1)]<-getmode(categorical$HLTHPLN1)
  categorical$PERSDOC2[is.na(categorical$PERSDOC2)]<-getmode(categorical$PERSDOC2)
  categorical$MEDCOST[is.na(categorical$MEDCOST)]<-getmode(categorical$MEDCOST)
  
  
  categorical$CHECKUP1[is.na(categorical$CHECKUP1)]<-getmode(categorical$CHECKUP1)
  categorical$BPHIGH4[is.na(categorical$BPHIGH4)]<-getmode(categorical$BPHIGH4)
  categorical$CHOLCHK1[is.na(categorical$CHOLCHK1)]<-getmode(categorical$CHOLCHK1)
  categorical$TOLDHI2[is.na(categorical$TOLDHI2)]<-getmode(categorical$TOLDHI2)
  categorical$CVDINFR4[is.na(categorical$CVDINFR4)]<-getmode(categorical$CVDINFR4)
  categorical$CVDCRHD4[is.na(categorical$CVDCRHD4)]<-getmode(categorical$CVDCRHD4)
  categorical$CVDSTRK3[is.na(categorical$CVDSTRK3)]<-getmode(categorical$CVDSTRK3)
  categorical$ASTHMA3[is.na(categorical$ASTHMA3)]<-getmode(categorical$ASTHMA3)
  categorical$CHCSCNCR[is.na(categorical$CHCSCNCR)]<-getmode(categorical$CHCSCNCR)
  categorical$CHCOCNCR[is.na(categorical$CHCOCNCR)]<-getmode(categorical$CHCOCNCR)
  categorical$CHCCOPD1[is.na(categorical$CHCCOPD1)]<-getmode(categorical$CHCCOPD1)
  categorical$HAVARTH3[is.na(categorical$HAVARTH3)]<-getmode(categorical$HAVARTH3)
  categorical$ADDEPEV2[is.na(categorical$ADDEPEV2)]<-getmode(categorical$ADDEPEV2)
  
  categorical$CHCKIDNY[is.na(categorical$CHCKIDNY)]<-getmode(categorical$CHCKIDNY)
  categorical$DIABETE3[is.na(categorical$DIABETE3)]<-getmode(categorical$DIABETE3)
  categorical$SEX[is.na(categorical$SEX)]<-getmode(categorical$SEX)
  categorical$MARITAL[is.na(categorical$MARITAL)]<-getmode(categorical$MARITAL)
  categorical$EDUCA[is.na(categorical$EDUCA)]<-getmode(categorical$EDUCA)
  categorical$RENTHOM1[is.na(categorical$RENTHOM1)]<-getmode(categorical$RENTHOM1)
  
  categorical$VETERAN3[is.na(categorical$VETERAN3)]<-getmode(categorical$VETERAN3)
  categorical$EMPLOY1[is.na(categorical$EMPLOY1)]<-getmode(categorical$EMPLOY1)
  categorical$INCOME2[is.na(categorical$INCOME2)]<-getmode(categorical$INCOME2)
  categorical$INTERNET[is.na(categorical$INTERNET)]<-getmode(categorical$INTERNET)
  categorical$DEAF[is.na(categorical$DEAF)]<-getmode(categorical$DEAF)
  
  categorical$BLIND[is.na(categorical$BLIND)]<-getmode(categorical$BLIND)
  categorical$DECIDE[is.na(categorical$DECIDE)]<-getmode(categorical$DECIDE)
  categorical$DIFFWALK[is.na(categorical$DIFFWALK)]<-getmode(categorical$DIFFWALK)
  categorical$DIFFDRES[is.na(categorical$DIFFDRES)]<-getmode(categorical$DIFFDRES)
  categorical$DIFFALON[is.na(categorical$DIFFALON)]<-getmode(categorical$DIFFALON)
  categorical$SMOKE100[is.na(categorical$SMOKE100)]<-getmode(categorical$SMOKE100)
  categorical$USENOW3[is.na(categorical$USENOW3)]<-getmode(categorical$USENOW3)
  categorical$ECIGARET[is.na(categorical$ECIGARET)]<-getmode(categorical$ECIGARET)
  categorical$EXERANY2[is.na(categorical$EXERANY2)]<-getmode(categorical$EXERANY2)
  categorical$EXRACT11[is.na(categorical$EXRACT11)]<-getmode(categorical$EXRACT11)
  categorical$EXRACT21[is.na(categorical$EXRACT21)]<-getmode(categorical$EXRACT21)
  categorical$SEATBELT[is.na(categorical$SEATBELT)]<-getmode(categorical$SEATBELT)
  categorical$FLUSHOT6[is.na(categorical$FLUSHOT6)]<-getmode(categorical$FLUSHOT6)
  categorical$PNEUVAC3[is.na(categorical$PNEUVAC3)]<-getmode(categorical$PNEUVAC3)
  categorical$SHINGLE2[is.na(categorical$SHINGLE2)]<-getmode(categorical$SHINGLE2)
  categorical$HIVTST6[is.na(categorical$HIVTST6)]<-getmode(categorical$HIVTST6)
  categorical$HIVRISK5[is.na(categorical$HIVRISK5)]<-getmode(categorical$HIVRISK5)
  categorical$PDIABTST[is.na(categorical$PDIABTST)]<-getmode(categorical$PDIABTST)
  categorical$PREDIAB1[is.na(categorical$PREDIAB1)]<-getmode(categorical$PREDIAB1)
  categorical$QSTVER[is.na(categorical$QSTVER)]<-getmode(categorical$QSTVER)
  categorical$QSTLANG[is.na(categorical$QSTLANG)]<-getmode(categorical$QSTLANG)
  categorical$X_IMPRACE[is.na(categorical$X_IMPRACE)]<-getmode(categorical$X_IMPRACE)
  categorical$X_CHISPNC[is.na(categorical$X_CHISPNC)]<-getmode(categorical$X_CHISPNC)
  categorical$X_DUALUSE[is.na(categorical$X_DUALUSE)]<-getmode(categorical$X_DUALUSE)
  categorical$X_RFHLTH[is.na(categorical$X_RFHLTH)]<-getmode(categorical$X_RFHLTH)
  categorical$X_PHYS14D[is.na(categorical$X_PHYS14D)]<-getmode(categorical$X_PHYS14D)
  categorical$X_MENT14D[is.na(categorical$X_MENT14D)]<-getmode(categorical$X_MENT14D)
  categorical$X_HCVU651[is.na(categorical$X_HCVU651)]<-getmode(categorical$X_HCVU651)
  categorical$X_RFHYPE5[is.na(categorical$X_RFHYPE5)]<-getmode(categorical$X_RFHYPE5)
  categorical$X_CHOLCH1[is.na(categorical$X_CHOLCH1)]<-getmode(categorical$X_CHOLCH1)
  categorical$X_RFCHOL1[is.na(categorical$X_RFCHOL1)]<-getmode(categorical$X_RFCHOL1)
  
  categorical$X_MICHD[is.na(categorical$X_MICHD)]<-getmode(categorical$X_MICHD)
  categorical$X_LTASTH1[is.na(categorical$X_LTASTH1)]<-getmode(categorical$X_LTASTH1)
  categorical$X_CASTHM1[is.na(categorical$X_CASTHM1)]<-getmode(categorical$X_CASTHM1)
  categorical$X_ASTHMS1[is.na(categorical$X_ASTHMS1)]<-getmode(categorical$X_ASTHMS1)
  categorical$X_DRDXAR1[is.na(categorical$X_DRDXAR1)]<-getmode(categorical$X_DRDXAR1)
  categorical$X_LMTACT1[is.na(categorical$X_LMTACT1)]<-getmode(categorical$X_LMTACT1)
  categorical$X_LMTWRK1[is.na(categorical$X_LMTWRK1)]<-getmode(categorical$X_LMTWRK1)
  categorical$X_LMTSCL1[is.na(categorical$X_LMTSCL1)]<-getmode(categorical$X_LMTSCL1)
  categorical$X_PRACE1[is.na(categorical$X_PRACE1)]<-getmode(categorical$X_PRACE1)
  categorical$X_MRACE1[is.na(categorical$X_MRACE1)]<-getmode(categorical$X_MRACE1)
  categorical$X_HISPANC[is.na(categorical$X_HISPANC)]<-getmode(categorical$X_HISPANC)
  categorical$X_RACE[is.na(categorical$X_RACE)]<-getmode(categorical$X_RACE)
  categorical$X_RACEG21[is.na(categorical$X_RACEG21)]<-getmode(categorical$X_RACEG21)
  categorical$X_RACEGR3[is.na(categorical$X_RACEGR3)]<-getmode(categorical$X_RACEGR3)
  
  categorical$X_RACE_G1[is.na(categorical$X_RACE_G1)]<-getmode(categorical$X_RACE_G1)
  categorical$X_AGEG5YR[is.na(categorical$X_AGEG5YR)]<-getmode(categorical$X_AGEG5YR)
  categorical$X_AGE65YR[is.na(categorical$X_AGE65YR)]<-getmode(categorical$X_AGE65YR)
  categorical$X_AGE_G[is.na(categorical$X_AGE_G)]<-getmode(categorical$X_AGE_G)
  categorical$X_BMI5CAT[is.na(categorical$X_BMI5CAT)]<-getmode(categorical$X_BMI5CAT)
  categorical$X_RFBMI5[is.na(categorical$X_RFBMI5)]<-getmode(categorical$X_RFBMI5)
  categorical$X_CHLDCNT[is.na(categorical$X_CHLDCNT)]<-getmode(categorical$X_CHLDCNT)
  categorical$X_EDUCAG[is.na(categorical$X_EDUCAG)]<-getmode(categorical$X_EDUCAG)
  categorical$X_INCOMG[is.na(categorical$X_INCOMG)]<-getmode(categorical$X_INCOMG)
  categorical$X_SMOKER3[is.na(categorical$X_SMOKER3)]<-getmode(categorical$X_SMOKER3)
  categorical$X_RFSMOK3[is.na(categorical$X_RFSMOK3)]<-getmode(categorical$X_RFSMOK3)
  categorical$X_ECIGSTS[is.na(categorical$X_ECIGSTS)]<-getmode(categorical$X_ECIGSTS)
  categorical$X_CURECIG[is.na(categorical$X_CURECIG)]<-getmode(categorical$X_CURECIG)
  categorical$DRNKANY5[is.na(categorical$DRNKANY5)]<-getmode(categorical$DRNKANY5)
  categorical$X_RFBING5[is.na(categorical$X_RFBING5)]<-getmode(categorical$X_RFBING5)
  categorical$X_RFDRHV5[is.na(categorical$X_RFDRHV5)]<-getmode(categorical$X_RFDRHV5)
  categorical$X_FRTLT1A[is.na(categorical$X_FRTLT1A)]<-getmode(categorical$X_FRTLT1A)
  categorical$X_VEGLT1A[is.na(categorical$X_VEGLT1A)]<-getmode(categorical$X_VEGLT1A)
  
  categorical$X_FRT16A[is.na(categorical$X_FRT16A)]<-getmode(categorical$X_FRT16A)
  categorical$X_VEG23A[is.na(categorical$X_VEG23A)]<-getmode(categorical$X_VEG23A)
  categorical$X_TOTINDA[is.na(categorical$X_TOTINDA)]<-getmode(categorical$X_TOTINDA)
  categorical$ACTIN11_[is.na(categorical$ACTIN11_)]<-getmode(categorical$ACTIN11_)
  categorical$ACTIN21_[is.na(categorical$ACTIN21_)]<-getmode(categorical$ACTIN21_)
  categorical$X_PACAT1[is.na(categorical$X_PACAT1)]<-getmode(categorical$X_PACAT1)
  categorical$X_PAINDX1[is.na(categorical$X_PAINDX1)]<-getmode(categorical$X_PAINDX1)
  categorical$X_PA150R2[is.na(categorical$X_PA150R2)]<-getmode(categorical$X_PA150R2)
  categorical$X_PA300R2[is.na(categorical$X_PA300R2)]<-getmode(categorical$X_PA300R2)
  categorical$X_PA30021[is.na(categorical$X_PA30021)]<-getmode(categorical$X_PA30021)
  categorical$X_PASTRNG[is.na(categorical$X_PASTRNG)]<-getmode(categorical$X_PASTRNG)
  categorical$X_PAREC1[is.na(categorical$X_PAREC1)]<-getmode(categorical$X_PAREC1)
  categorical$X_PASTAE1[is.na(categorical$X_PASTAE1)]<-getmode(categorical$X_PASTAE1)
  categorical$X_RFSEAT2[is.na(categorical$X_RFSEAT2)]<-getmode(categorical$X_RFSEAT2)
  categorical$X_RFSEAT3[is.na(categorical$X_RFSEAT3)]<-getmode(categorical$X_RFSEAT3)
  
  categorical$X_AIDTST3[is.na(categorical$X_AIDTST3)]<-getmode(categorical$X_AIDTST3)
  categorical$alc30Week[is.na(categorical$alc30Week)]<-getmode(categorical$alc30Week)
  categorical$fruitIntake[is.na(categorical$fruitIntake)]<-getmode(categorical$fruitIntake)
  categorical$fruitJuIntake[is.na(categorical$fruitJuIntake)]<-getmode(categorical$fruitJuIntake)
  categorical$greenIntake[is.na(categorical$greenIntake)]<-getmode(categorical$greenIntake)
  categorical$fryIntake[is.na(categorical$fryIntake)]<-getmode(categorical$fryIntake)
  
  categorical$potatoIntake[is.na(categorical$potatoIntake)]<-getmode(categorical$potatoIntake)
  categorical$vegIntake[is.na(categorical$vegIntake)]<-getmode(categorical$vegIntake)
  categorical$exWeekMonth[is.na(categorical$exWeekMonth)]<-getmode(categorical$exWeekMonth)
  categorical$exMinHour[is.na(categorical$exMinHour)]<-getmode(categorical$exMinHour)
  categorical$strengthWeekMonth[is.na(categorical$strengthWeekMonth)]<-getmode(categorical$strengthWeekMonth)

  
  sum(is.na(categorical))  #check for # of NA values
  
  #merge the two dataframes of numerical and categorical data
  finalData <- cbind(categorical,continuous)
  finalData<-finalData[,order(colnames(finalData))] #order column names alphabetically
  
  
  #create two samples of 10k
  set.seed(1)
  finalData1<- finalData[sample(nrow(finalData), 100000), ]
  finalData2<- finalData[sample(nrow(finalData), 10000), ]
  structImp<-capture.output(str(finalData1,list.len=ncol(finalData1))) 
  
  
  
  
  
  
  
  
  
  
  ################################################################################################333
  
  
  #outlier for continuous variable

  FindOutliers <- function(data) {
    lowerq = quantile(data)[2]
    upperq = quantile(data)[4]
    iqr = upperq - lowerq #Or use IQR(data)
    # we identify extreme outliers
    extreme.threshold.upper = (iqr * 3) + upperq
    extreme.threshold.lower = lowerq - (iqr * 3)
    result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
  }  
  pos <- lapply(continuous, FindOutliers)
  outlierCount <- data.frame(lengths(pos))
  
  options(scipen = 999)
  
  for(i in 1:length(outlierCount$lengths.pos.)){
    outlierCount$lengths.pos.[i] <- outlierCount$lengths.pos.[i]/450016
  }

  #find and treat outliers for those columns that have more than 10% containing outliers
  outlierCandidate <- outlierCount[outlierCount$lengths.pos. > .1 , ]
  hist(continuous$MENTHLTH)
  boxplot(continuous$MENTHLTH)
  sort(boxplot.stats(continuous$MENTHLTH)$out,decreasing = TRUE)
  #for all values equal to or greater to 6, impute with median 
  continuous$MENTHLTH[which(continuous$MENTHLTH >= 6)]<-0
  
  
   

  
  
  
  
  
  
 
  

#####################################################################################
 
   #Impute missing values
  
######################################################################################3
  set.seed(1)
  impDataset<- selDataset[sample(nrow(selDataset), 10000), ]
  structImp<-capture.output(str(impDataset,list.len=ncol(impDataset))) 
  View(struct)
  library(mice)
  library(missForest)
  start.time <- Sys.time()
  
  data.imputed_rf<-missForest(impDataset, xtrue = impDataset)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  #  data.imputedMice <- mice(impDataset ,meth='rf', ntree=10)
  



