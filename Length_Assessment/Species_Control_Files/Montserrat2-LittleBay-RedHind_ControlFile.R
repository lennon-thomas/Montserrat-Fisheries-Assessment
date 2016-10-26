#### ControlFile ###

AvailableData<- c("LengthData","CatchData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBAR','CatchMSY','LBSPR')
 
 Fish<- NULL
 
 Fish$SciName<- 'E. guttas'
 
 Fish$CommName<- 'Red hind'

### Life History ###
 
 DefaultSD<- 0.05
 
 Fish$LHITol<- 0.1 # The average % deviation from LHI allowed
 
 Fish$vbk<- 0.0859                       ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<- 0.05
 
 Fish$Linf<- 55.78                        ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- -3.817
 
 Fish$WeightA <- 0.01338 #From D. Viana's summary of lit, "Lobster Life History..."
 
 Fish$WeightB<- 3.0402#From D. Viana's summary of lit, "Lobster Life History..."
 
                
Fish$AgeSD<-0.5
 
 Fish$M<-    .171185
 

 
 Fish$MvK<- 1.992841 #Ratio of M versus K
 
 Fish$MaxAge<- 17.5
 
 Fish$MortalityError<- 0.1                 ## test error, can change to accomodate more error
 
 Fish$Mat50<- 30.90                      ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<- 35.22                              ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<- NA
 
 Fish$VBSD<- 0.05
 
 Fish$VBErrorSlope<- 0.1
 
 Fish$res<- 'Medium'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- 22                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
