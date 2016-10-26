





LBSPR_SingleSpeciesAssessmentfun<- function(CatchatLength,AssessDir,CurrentDir,LengthBins,Year,EstimatedM,Fish)
{
  Output<- as.data.frame(matrix(NA,nrow=1,ncol=9))
  
  colnames(Output)<- c('Year','Method','SampleSize','Value','LowerCI','UpperCI','SD','Metric','Flag')
  
  Flag<- 'None'
  # Year=Years[y]
  Details<- as.data.frame(matrix(NA,nrow=1,ncol=4))
  
  colnames(Details)<- c('Year','FvM','SelL50','SelL95')
  #Modified from the version sent by Sarah V. in Sep 2013
  ##############################
  # Read in Assumed Parameters #
  ##############################
  
  SpeciesName <- Species
  # assumedLinf  <- 1.2*max(CatchatLength) 
  assumedLinf  <- Fish$Linf
  
  M<- Fish$M
  
  if (is.numeric(EstimatedM)==T)
  {
    M<- EstimatedM
  }	
  
  assumedMK  <- Fish$MvK
  
  # assumedMK  <- M/Fish$vbk
  
  
  genM    <-   0.1 # as.numeric(as.character(SpFile[4, 1 +1]))
  genLinf		<- 1
  genLinfCV	<- Fish$LengthError
  gent0		<- Fish$t0
  MatType		<- 'Logistic'
  MatL50		<- Fish$Mat50  
  MatL95		<- Fish$Mat95
  Wbeta		<- Fish$WeightB
  Walpha		<- Fish$WeightA
  genK    <- genM/assumedMK
  # minLen  <- Fleet$MinSizeCaught
  minLen  <- min(CatchatLength,na.rm=T)
  maxLen  <- 1.2 * max(CatchatLength,na.rm=T)
  # maxLen  <- (Fleet$MaxSizeCaught)
  # maxLen  <- 1.1*max(CatchatLength)    
  #################################################################
  # Convert Data received to appropriate format        
  #################################################################
  
  LengthClasses <- seq(floor(minLen),ceiling(maxLen),by = LengthBins)
  LengthMids <- seq(LengthClasses[1] +((LengthClasses[2]-LengthClasses[1])/2), by=(LengthClasses[2]-LengthClasses[1]), length=length(LengthClasses)-1)
  # LenFreq <- hist(CatchatLength,breaks=seq(floor(minLen),ceiling(maxLen),by=LengthBins),plot=FALSE,right=F)$counts
  LenFreq<- DanHist(CatchatLength,seq(floor(minLen),ceiling(maxLen),by=LengthBins))$Frequency
  
  # DanHist(CatchatLength,seq(minLen,maxLen+LengthBins,by=LengthBins))
  
  LenProp <- as.vector(LenFreq/sum(LenFreq))
  
  ########################################
  # Set final params to pass to ADMB file#    
  ########################################
  
  MK <- assumedMK
  LinfTest <- assumedLinf
  LinfCV <- genLinfCV
  PercLeft <- 0.01
  NumAgClass <- 100
  L50 <-  MatL50
  L95 <-  MatL95
  
  ###########################################
  # Set working directory and run assessmemt#    
  ###########################################
  
  #MNew Assess Dir for each method tested
  setwd(AssessDir)
  WriteDat(MK, LinfTest, LinfCV, PercLeft, NumAgClass, LengthMids, LengthClasses, LenFreq, LenProp, L50, L95, AssessDir) 
  
  #Find best Starting Values
  LengthComp <- cbind(LengthMids, LenFreq)
  # Write Pin
  FirstLen <- LengthComp[min(which(LengthComp[,2] > 0)),1]
  # Find best starting values
  Vals1  <- c(FirstLen/LinfTest, 0.05, log(1))
  Vals2 <- c(FirstLen/LinfTest+0.1, 0.05, log(0.5))
  Vals3 <- c(FirstLen/LinfTest+0.1, 0.05, log(2.5))
  
  ValMat <- matrix(c(Vals1, Vals2, Vals3), byrow=T, nrow=3)
  
  tempRunAssess <-function (Vals) {
    WritePin(AssessDir, Vals)
    ADMBFile <- paste(AssessDir, "/LBSPR_AssessFun", sep="")
    ModelFailed <- FALSE
    setwd(AssessDir)
    #system(ADMBFile)
    run_admb("LBSPR_AssessFun") 
    # Check Model Failed 
    FileList <- c("admodel.cov", "lbspr_assessfun.cor", "lbspr_assessfun.std")
    allFiles <- list.files(AssessDir)
    if(any(FileList %in% allFiles) == FALSE) ModelFailed <- TRUE
    if (ModelFailed) {
      estSel50 <- NA
      estSel95 <- NA
      estFM <- NA
      ObjVal <- 1E6
      estSPR <- NA
      ModelFit <- NA
    } else {
      estSel50 <- read.table(paste(AssessDir, "/LBSPR_AssessFun.par", sep=""))[1,1] * LinfTest
      estSel95 <- estSel50 + read.table(paste(AssessDir, "/LBSPR_AssessFun.par", sep=""))[2,1] * LinfTest
      estFM <- exp(read.table(paste(AssessDir, "/LBSPR_AssessFun.par", sep=""))[3,1])
      ObjVal <- scan(paste(AssessDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), skip=2, nlines=1, quiet=TRUE)
      estSPR <- scan(paste(AssessDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), skip=3, nlines=1, quiet=TRUE)
      ModelFit <- scan(paste(AssessDir, "/LBSPR_AssessFun.rep", sep=""), what=double(), nlines=1, quiet=TRUE) 
    }
    setwd(CurrentDir)
    Output <- NULL
    Output$est <- c(estSel50, estSel95, estFM, estSPR, ObjVal) 
    Output$fit <- ModelFit
    return(Output)
  }  
  
  DeleteFiles(AssessDir) 
  SaveResults <- matrix(NA, nrow=nrow(ValMat), ncol=5)
  SaveFit <- rep(list(NA), nrow(ValMat))
  for (i in 1:nrow(ValMat)) {
    temp <- tempRunAssess(ValMat[i,]) 
    SaveResults[i,] <- temp$est
    SaveFit[[i]] <- temp$fit
  }
  
  ModelFailed <- FALSE
  if (all(is.na(SaveResults[,1:4]))) ModelFailed <- TRUE
  
  MinInd <- which.min(SaveResults[,5])
  
  ###########################################
  # Unpack parameters#    
  ###########################################
  
  LBSPR_Output <- NULL
  LBSPR_Output$SelL50 <- SaveResults[MinInd, 1]
  LBSPR_Output$SelL95 <- SaveResults[MinInd, 2]
  LBSPR_Output$EstFM <- SaveResults[MinInd, 3]
  LBSPR_Output$EstSPR <- SaveResults[MinInd, 4]
  LBSPR_Output$ObjVal <- SaveResults[MinInd, 5]
  LBSPR_Output$ModelFit <- SaveFit[[MinInd]]
  LBSPR_Output$ModFAILED <- ModelFailed
  
  Output<-NULL
  
  Output$Year<- Year
  
  Output$Method<- 'LBSPR'
  
  Output$Value<- LBSPR_Output$EstSPR
  
  Output$Metric<- 'SPR'
  
  Output$LowerCI<- NA
  
  Output$UpperCI<- NA
  
  Output$SD<- NA
  
  Output$Flag<- paste('ModelFailed is',ModelFailed)
  
  Details <- NULL
  
  Details$Year<- Year
  
  Details$FvM <- LBSPR_Output$EstFM
  
  Details$SelL50<- LBSPR_Output$SelL50
  
  Details$SelL95<- LBSPR_Output$SelL95
  
  # Do Plot function
  DoSinglePlot <- function(ModelFit, LenProb, LenMids, SaveFile=TRUE, FileName, ModFail) {
    
    pdf(file=paste(CurrentDir,'/',FigureFolder,FileName,sep=''))
    par(mfrow=c(1,1))
    Max <- max(LenProb)
    xx <- barplot(LenProb, names.arg=round(LenMids,2), ylim=c(0, Max+Max*0.1))
    if (ModFail == FALSE) lines(xx, ModelFit, lwd=4)
    if (ModFail) text(xx[5], Max*0.8, "MODEL FAILED TO CONVERGE", cex=1.5, pos=4)
    mtext(side=1, "Length Classes", line=3, cex=1.5)
    mtext(side=2, "Proportion", line=2.5, cex=1.5)
    dev.off()
  }
  
  DoSinglePlot(LBSPR_Output$ModelFit,LenProp,LengthMids,SaveFile=TRUE,FileName=paste(Year,' LBSPRModelFit.pdf'),ModelFailed)
  
  
  
  LengthMids[LengthMids>Fish$Linf]<- 0.99*Fish$Linf
  AgeVector<- floor(AgeAtLength(LengthMids,Fish,0))
  
  Ages<- unique(AgeVector[is.na(AgeVector)==F])
  
  Residuals<- LenProp - LBSPR_Output$ModelFit
  
  
  CohortDeviates<- as.data.frame(matrix(NA,nrow=length(Ages),ncol=2))
  
  AgeDeviates<- as.data.frame(matrix(NA,nrow=length(Ages),ncol=2))
  
  
  for (a in 1:length(Ages))
  {
    Where<- (AgeVector==Ages[a] & is.na(AgeVector)==F)
    
    CohortDeviates[a,]<- data.frame(Year-Ages[a],sum(Residuals[Where]))
    
    AgeDeviates[a,]<- data.frame(Ages[a],sum(Residuals[Where]))
    
  }
  
  colnames(CohortDeviates)<- c('Cohort','Residuals')
  
  colnames(AgeDeviates)<- c('Age','Residuals')
  
  
  
  if (ModelFailed== TRUE)
  {
    Output$Year<- Year
    
    Output$Method<- 'LBSPR'
    
    Output$Flag<- paste('ModelFailed is',ModelFailed)
    
  }
  
  return(list(Output=Output,Details=Details,CatchCurveResiduals= CohortDeviates,AgeResiduals= AgeDeviates))
}
















