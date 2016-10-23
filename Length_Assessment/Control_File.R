#Final Control File 
#MNI Length Assessment - All Species 
#

##Set Up

rm(list = ls())

pkgs <- c('plyr','readr', 'ggplot2','tidyr', 'R2admb', 'dplyr')

#This will install necessary packages on local machine
load.packages = function(a){
  if(!require(a, character.only = TRUE)){
    install.packages(a)
    library(a, character.only = TRUE)
  }
}

lapply(pkgs, load.packages)

#Source functions to file
sapply(list.files(pattern="[.]R$", path="./Length_Assessment/Functions", full.names=TRUE), source)


#Assessments to run with control file
Assessments <- c('LBAR')

Counter<- 0

#Load and summarize length data
LengthData <- read.csv("./Length_Assessment/all_length.csv") %>%
  filter(Species.ID == "ACANCH")

names(LengthData)
gears<-unique(LengthData$Gear.Type)
levels(gears)


sp_id<-c("ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARVI","SERRGU")

#Generates named vector to link sp_id with common name
common<-c(
  ACANCH="Doctorfish",
  ACANCO="Blue Tang",
  BALIVE="Old wife",
  HOLOAD="Squirrelfish",
  LUTJMA="Mahogany snapper",
  LUTJSY="Lane snapper",
  LUTJVI="Silk snapper",
  SCARCH="Spotlight parrotfish", # this should be Redtail Parrotfish?
  SERRGU="Red hind")

######################################################################################
#LBAR

Counter<- Counter+1

SampleCheck<- CheckLengthSampleSize(LengthData)   

Temp<- LBAR(SampleCheck$ParedData,LagLength=1,Weight=1,IncludeMPA=0,ReserveYr=NA,OutsideBoundYr=NA,Iterations=1,
            BootStrap=0,LifeError=0,Lc=NA)$Output		

StoreAssess<- data.frame(Species,Sites[s],Assessments[a],Temp,stringsAsFactors=F) %>%
  rename(Site=Sites.s.,Assessment=Assessments.a.)

AssessmentResults[[Counter]]<-StoreAssess


######################################################################################
#LBSPR




#######################Plots#############################################

#Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
Surveycolor<-"red"
Fisherycolor <- "lightseagreen"


for (i in 1:length(sp_id)){
  
#Life History Parameters for species[i]  
  Fish<-read.csv("./Length_Assessment/MNI_LH_FINAL.csv")%>%
    filter(Species==sp_id[i]) %>%
    select(-Reference) %>%
    spread(Parameter, Value)
  

  Fish$Mat50<-Fish$m95

#Specifies where to save plots
  FigureFolder<- paste("./Length_Assessment/plots/")
  name="Data Type"
  
  Theme<- theme(plot.background=element_rect(color='white'),
                rect=element_rect(fill='transparent',color=NA),
                text=element_text(size=11,color=FontColor),plot.title = element_text(),
                axis.text.x=element_text(color=FontColor), axis.text.y=element_text(color="black"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background=element_blank(),
                strip.background = element_rect(colour="white", fill="white"),
                axis.line.x = element_line(color="black", size = 0.6),
                axis.line.y = element_line(color="black", size = 0.6))
 
  Species=common[i]

  
  PlotLengthData(LengthData,FigureFolder,Fish,Species,Theme)
}

