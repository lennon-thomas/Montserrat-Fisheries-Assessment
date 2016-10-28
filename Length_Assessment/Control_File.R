#Final Control File 
#MNI Length Assessment - All Species 
#

##Set Up

rm(list = ls())

if (basename(getwd())!='Montserrat-Fisheries-Assessment'){
  print("Set working directory to home folder")
}

pkgs <- c('plyr','readr', 'ggplot2','tidyr', 'R2admb', 'dplyr', 'animation')

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
source("./Length_Assessment/Functions/SubFunctions.R") #Pull in helper functions for assessment modules

############High Level Assessment Controls############

#Assessments to run with control file
Assessments <- c('LBAR')

Counter<- 0

Sites<- c('All')

AssessmentResults<- list()

MonteResults<- list()

Assessment <- 'Doctorfish'

dir.create(Assessment)

NumIterations <- 100

SPRRef <- 0.4

CPUERef <- 0.4

RunAssessments <- TRUE 

NumberOfSpecies <- 1

ReserveYear <- NA

MinSampleSize <- 180

############################Load and summarize length data######################
LengthData <- read.csv("./Length_Assessment/all_length_dates.csv") %>%
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
  SCARCH="Redtail parrotfish", # this should be Redtail Parrotfish?
  SERRGU="Red hind")


#######################Life History#############################################

# Fish<-read.csv("./Length_Assessment/MNI_LH_FINAL.csv")%>%
#   filter(Species==sp_id[1]) %>%
#   select(-Reference) %>%
#   spread(Parameter, Value)
# 
# Fish$AgeMat50<- NA
# 
# Fish$AgeMatSource<- NA
# 
# Fish$MaxAge <- ceiling(-log(0.05)/Fish$M)

#######################Species LH Control File#############################################
source(paste('./Length_Assessment/Species_Control_Files/Montserrat2-LittleBay-Doctorfish_ControlFile.R'))

##Lennon commented these out
# LifeHistory<-read.csv(paste(Assessment,'/',Files[grepl('_LifeHistory',Files)],sep=''), stringsAsFactors = F)
#

LifeHistory<-melt(Fish,na.rm=T)
LifeHistory<-as.matrix(LifeHistory)
LifeHistory<-t(LifeHistory)
colnames(LifeHistory)<-LifeHistory[2,]
LifeHistory<-LifeHistory[1,]
LifeHistory<-t(LifeHistory)
LifeHistory<-LifeHistory[-2,]
LifeHistory<-as.data.frame(LifeHistory,stringsAsFactors =FALSE)


LifeHistory<-data.frame(t(LifeHistory),stringsAsFactors = FALSE)

Fishes <- unique(Fish$CommName)


LifeData<- colnames(LifeHistory)[5:dim(LifeHistory)[2]]

Fish$AgeMat50<- NA

Fish$AgeMatSource<- NA


Sites<- c('All')

AssessmentResults<- list()

MonteResults<- list()



# Run Assessments ---------------------------------------------------------
s=1

# if (RunAssessments==T)
# {
#   
#   for (s in 1:length(Sites))    
#   {

show(Sites[s])

f=1


show(Fishes[f])

Species<- Fishes[f]


AssessmentName <- paste(Assessment,Sites[s],Fishes[f],sep='_')

Directory<- paste(Assessment, "/", Sites[s],'/',Fishes[f],'/',sep='')

if (file.exists(Directory)==F)
{
  dir.create(paste(Assessment, "/", Sites[s],sep=''))
  dir.create( paste(Assessment, "/", Sites[s],'/',Fishes[f],'/',sep=''))
}

# Prepare Fish Object -------------------------------------------------------------
s=1
SpeciesLifeHistory<- LifeHistory[LifeHistory$CommName == Species[s],colnames(LifeHistory) %in% LifeData]
SpeciesLifeHistory<-LifeHistory

SpeciesLifeHistory[,3:18] <- as.numeric(SpeciesLifeHistory[,3:18])

sTaxa<-"Surgeonfish"
HasLifeHistory<- SpeciesLifeHistory[which(is.na(SpeciesLifeHistory)==F)]

HasLife<- colnames(HasLifeHistory)

Fish$CommName <- Species[s]



##########CREATES FOLDERS##########     
FigureFolder<- paste(Directory,'Figures/',sep='')

ResultFolder<- paste(Directory,'Results/',sep='')

if (file.exists(FigureFolder)==F)
{
  dir.create(FigureFolder,recursive=T)
  
  dir.create(ResultFolder,recursive=T)
}



a=1
Fish$LHITol<- 0.99
#       
#       for (a in 1:length(Assessments)) #Loop over possible assessments, store in Assessment results. Many assessments have more detailed outputs than can also be accessed 
#       {

Counter<- Counter+1
#         if (Assessments[a]=='LBAR') #Run LBAR assessment
#         {
#           
SampleCheck<- CheckLengthSampleSize(LengthData)        
#           
#           if (SampleCheck$YearsWithEnoughData>0)
#           {


Temp<- LBAR(SampleCheck$ParedData,LagLength=1,Weight=1,IncludeMPA=0,ReserveYr=NA,OutsideBoundYr=NA,Iterations=1000,
            BootStrap=1,LifeError=0,Lc=18)$Output		

StoreAssess<- data.frame(Species,Sites[s],Assessments[a],Temp,stringsAsFactors=F) %>%
  rename(Site=Sites.s.,Assessment=Assessments.a.)

AssessmentResults[[Counter]]<-StoreAssess
#######################Save Results#############################################

CurrentResults<- ldply(AssessmentResults) %>% subset(Species==Fishes[f] & Site==Sites[s])

write.csv(file=paste(ResultFolder,AssessmentName,'_Results.csv',sep=''),CurrentResults)
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
  

  Fish$Mat50 <- Fish$m95

 
  
  
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

