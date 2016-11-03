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


LH <- as.data.frame(matrix(NA,nrow=length(common),ncol=20))

for (i in 1:length(common)){
  
source(paste('./Length_Assessment/Species_Control_Files/Montserrat2-LittleBay-',common[i],'_ControlFile.R',sep=""))

  
LifeHistory<-melt(Fish,na.rm=T)
LifeHistory<-as.matrix(LifeHistory)
LifeHistory<-t(LifeHistory)
colnames(LifeHistory)<-LifeHistory[2,]
LifeHistory<-LifeHistory[1,]
LifeHistory<-t(LifeHistory)
LifeHistory<-LifeHistory[-2,]
LifeHistory<-as.data.frame(LifeHistory,stringsAsFactors =FALSE)

###LOOK HERE TOOO----
### made RDATA
LH[i,]<-data.frame(t(LifeHistory),stringsAsFactors = FALSE)

LH$sp_id[i] <- names(common[i])

}

colnames(LH)<-c(names(Fish),"sp_id")



write.csv(LH, "MNI_LH_spread1.csv")
