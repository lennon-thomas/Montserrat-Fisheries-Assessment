#
#Script to take Life Hitory values from individual Spps control files and combine into a sinle life history csv file
#
common<-c(
  ACANCH="Doctorfish",
  ACANCO="BlueTang",
  BALIVE="QueenTriggerfish",
  HOLOAD="Squirrelfish",
  LUTJVI="Silk Snapper",
  SERRGU="Red Hind")


LH <- as.data.frame(matrix(NA,nrow=length(common),ncol=21))

colnames(LH)<-c("sp_id","SciName",	"CommName",	"LHITol",	"vbk",	"LengthError",	"Linf",	"t0",	"WeightA",	"WeightB",	"AgeSD","M",	"MvK","MaxAge",	"MortalityError",	"Mat50",	"Mat95","	PLD",	"VBSD","VBErrorSlope",	"res")

for (i in 1:length(common)){
  
source(paste('./Length_Assessment/Species_Control_Files/Montserrat2-LittleBay-',common[i],'_ControlFile.R',sep=""))

  LH$sp_id[i] <- names(common[i])
  
  LH$SciName[i]	<- Fish$SciName
  
  LH$CommName[i] <- Fish$CommName
  
  LH$LHITol[i]	<- Fish$LHITol
  
  LH$vbk[i]	<- Fish$vbk
  
  LH$LengthError[i]	<- Fish$LHITol
  
  LH$Linf[i] <- Fish$Linf
  
  LH$t0[i] <- Fish$t0	
  
  LH$WeightA[i] <- Fish$WeightA
  
  LH$WeightB[i] <- Fish$WeightB
  
  LH$AgeSD[i] <- Fish$AgeSD	

  LH$M[i] <- Fish$M
  
  LH$MvK[i] <- Fish$MvK	
  
  LH$MaxAge[i] <- Fish$MaxAge	
  
  LH$MortalityError[i] <- Fish$MortalityError
  
  LH$Mat50[i] <- Fish$Mat50
  
  LH$Mat95[i] <- Fish$Mat95
  
  LH$PLD[i] <- Fish$PLD
    
  LH$VBSD[i] <- Fish$VBSD	
  
  LH$VBErrorSlope[i] <- Fish$VBErrorSlope	
  
  LH$res[i] <- Fish$res

}



write.csv(LH, "Length_Assessment/MNI_LH_spread1.csv")
