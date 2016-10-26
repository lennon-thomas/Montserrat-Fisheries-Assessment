#### ControlFile ###

AvailableData<- c("LengthData","CatchData")

# Assessments<- c('CatchCurve')

 Assessments<- c('LBAR','CatchMSY','LBSPR')
 
 Fish<- NULL
 
 Fish$SciName<- 'A chirurgus'
 
 Fish$CommName<- 'Doctorfish'

### Life History ###
 
 DefaultSD<- 0.05
 
 Fish$LHITol<- 0.1 # The average % deviation from LHI allowed
 
 Fish$vbk<- 0.08                      ##DO:according to Informe final   0.21625    error 0.004M and 0.06H
 
 Fish$LengthError<- 0.05
 
 Fish$Linf<- 36.69                        ##41.9M and 40.14H averaged from Norahs values (used averages = 41.02) used our equation to determine Linf for tail length (average = 25.15 and H = 24.57)
 
 Fish$t0<- -4.307
 
 Fish$WeightA <- 0.00406 #From D. Viana's summary of lit, "Lobster Life History..."
 
 Fish$WeightB<- 3.533#From D. Viana's summary of lit, "Lobster Life History..."
 
 
 Fish$MaxAge<- 27
 
 #Fish$M<- 0.17                   ## average mortality according to Matt Kay and literature review, Hearn 2008 use 0.348M and 0.378H (average = 0.363)
 
 Fish$M<-    .111
 
 # Fish$M<- 0.166  
 
 Fish$AgeSD<-0.5
 
Fish$MvK<- 1.357 #Ratio of M versus K
 
 #Fish$MaxAge<- log(.05)/-Fish$M #Max age is age at thich only 1% of population are left. Can also be set manually if needed
 
 Fish$MortalityError<- 0.1                ## test error, can change to accomodate more error
 
 Fish$Mat50<- 21.21                      ## 24.05 according to Toral and Hearn (tail only = 13.96)
 
 Fish$Mat95<- 24.18                              ## 26 according to Toral and Hearn (tail only = 15.24)
 
 Fish$PLD<- NA
 
 Fish$VBSD<- 0.05
 
 Fish$VBErrorSlope<- 0.1
 
 Fish$res<- 'Low'
 
 ### Fleet Parameters ###
 
 Fleet<- NULL
 
 Fleet$MinSizeCaught<- 13                        ##26 cm minimum landing size for Galapagos      (was 20 before?) (tail = 15.24) 
 
 Fleet$MaxSizeCaught<- Fish$Linf-0.5
 
