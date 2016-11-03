#### ControlFile ###

AvailableData<- c("LengthData","CatchData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBAR','CatchMSY','LBSPR')
 
 Fish<- NULL
 
 Fish$SciName<- 'B. vetula'
 
 Fish$CommName<- 'Queen triggerfish'
### Life History ###
 
 DefaultSD<- 0.05
 
 Fish$LHITol<- 0.1 # The average % deviation from LHI allowed
 
 Fish$vbk<- 0.30                      ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<- 0.05
 
 Fish$Linf<- 41.50                       ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- -0.6
 
 Fish$WeightA <-0.05679647
 
 Fish$WeightB<- 2.75
 
 Fish$AgeSD <- NA
 
 
 Fish$M<-    0.42796
 
 
 Fish$MvK<-1.426533 #Ratio of M versus K
 
 Fish$MaxAge<- 7 #Max age is age at thich only 1% of population are left. Can also be set manually if needed
 
 Fish$MortalityError<- 0.1                 ## test error, can change to accomodate more error
 
 Fish$Mat50<- 23.69438                      ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<- 27.0116                              ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<- NA
 
 Fish$VBSD<- 0.05
 
 Fish$VBErrorSlope<- 0.1
 
 Fish$res<- 'Medium'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- 21                     ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
