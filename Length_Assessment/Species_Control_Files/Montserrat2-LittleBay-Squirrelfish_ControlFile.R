#### ControlFile ###

AvailableData<- c("LengthData","CatchData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBAR','CatchMSY','LBSPR')
 
 Fish<- NULL
 
 Fish$SciName<- 'H. adscensionis'
 
 Fish$CommName<- 'Squirrelfish'
### Life History ###
 
 DefaultSD<- 0.05
 
 Fish$LHITol<- 0.1 # The average % deviation from LHI allowed
 
 Fish$vbk<- 0.34                     ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<- 0.05
 
 Fish$Linf<- 52.23                       ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- -.4
 
 Fish$WeightA <-0.242209
 
 Fish$WeightB<- 2.365
 
 
 Fish$MaxAge<- 8.4
 
 Fish$AgeSD<-0.5
 Fish$M<-   0.356634794
 
 
 Fish$MvK<-1.048926 #Ratio of M versus K
 
 Fish$MaxAge<- 8.4#Max age is age at thich only 1% of population are left. Can also be set manually if needed
 
 Fish$MortalityError<- 0.1                 ## test error, can change to accomodate more error
 
 Fish$Mat50<- 29.12935582                    ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<- 33.20746564                             ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<- NA
 
 Fish$VBSD<- 0.05
 
 Fish$VBErrorSlope<- 0.1
 
 Fish$res<- 'Medium'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- NULL                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
