#### ControlFile ###

AvailableData<- c("LengthData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBSPR','LBAR')
 
 Fish<- NULL
 
 Fish$SciName<- 'L. vivanus'
 
 Fish$CommName<- 'Silk Snapper'
### Life History ###
 
 DefaultSD<- 0.05
 
 Fish$LHITol<- 0.1 # The average % deviation from LHI allowed
 
 Fish$vbk<- 0.09                    ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<- 0.05
 
 Fish$Linf<- 78.1                       ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- -2.309
 
 Fish$WeightA <-0.0373
 
 Fish$WeightB<- 2.7812

 Fish$AgeSD <- NA
 
 
 Fish$MaxAge<- 9
 
 
 Fish$M<-   0.23
 
 
 Fish$MvK<-2.56 #Ratio of M versus K
 
 Fish$MaxAge<- 9 #Max age is age at thich only 1% of population are left. Can also be set manually if needed
 
 Fish$MortalityError<- 0.1                 ## test error, can change to accomodate more error
 
 Fish$Mat50<- 41.8                  ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<-     41.8                        ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<- NA
 
 Fish$VBSD<- 0.05
 
 Fish$VBErrorSlope<- 0.1
 
 Fish$res<- 'Low'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- NULL                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
