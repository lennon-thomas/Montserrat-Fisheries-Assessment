#Final Control File 
#MNI Length Assessment - All Species 
#
#Runs an LBAR assessment for each species in the data set
#Require data frame of length and data frame of life history paramters
#

##Set Up----

rm(list = ls())


if (basename(getwd())!='Montserrat-Fisheries-Assessment'){
  print("Set working directory to home folder: Montserrat-Fisheries-Assessment")
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

#Assessments to run with this control file
Assessments <- c('LBAR')

Counter<- 0

#Sites to assess
Sites<- c('All')

AssessmentResults<- list()

MonteResults<- list()

dir.create(Assessment)

NumIterations <- 1000

RunAssessments <- TRUE 

NumberOfSpecies <- 1

ReserveYear <- NA

MinSampleSize <- 110

############################Load and summarize length data######################
 LengthDataAll <- read.csv("./Length_Assessment/all_length_dates.csv", stringsAsFactors = FALSE, strip.white = TRUE)

#Filter out Species with a FISHERY DEPENDENT sample size < MinSampleSize 

LengthDataFiltered<- SelectForLengthSampleSize(LengthDataAll)

names(LengthDataFiltered)

#Prints Species to be included in the analysis (those with sufficient sample size)
LBar_sp<- unique(LengthDataFiltered$Species.ID)
print(LBar_sp)

#Prints gears represented in the selected sample
print(unique(LengthDataFiltered$Gear.Type))


#All species IDs
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
  SCARVI="Redtail parrotfish", 
  SERRGU="Red hind")

#Create Empty dataframes

Temp <- as.data.frame(matrix(NA,nrow=length(LBar_sp),ncol=9))

StoreAssess <- as.data.frame(matrix(NA,nrow=length(LBar_sp),ncol=12))

#Start for loop-----
for (i in 1:length(LBar_sp)){
LengthData<-filter(LengthDataFiltered, Species.ID==LBar_sp[i]) 


#######################Life History#############################################

 Fish<-read.csv("./Length_Assessment/MNI_LH_spread.csv", stringsAsFactors = FALSE, strip.white = TRUE) %>%
   filter(sp_id==LBar_sp[i]) 


 Fish$AgeMat50<- NA
 Fish$AgeMatSource<- NA

 Fish$MaxAge <- ceiling(-log(0.05)/Fish$M)

 Fishes <- unique(Fish$CommName)

 #Sets the mode length from FISHERY DEPENDENT sample as Length at first capture (Lc)
 uniqv <- unique(round(LengthData$Length[LengthData$FisheryDependent==1], digits = 1))
 Lc <- uniqv[which.max(tabulate(match(LengthData$Length[LengthData$FisheryDependent==1], uniqv)))]
 

# Run Assessments ---------------------------------------------------------
s=1

#   for (s in 1:length(Sites))    
#   {

show(Sites[s])

show(Fishes)

##########Creates Folders for Output########## 

Directory<- paste(Assessments, "/", Sites[s],'/',Fishes,'/',sep='')

FigureFolder<- paste('Length_Assessment/Figures/',Directory,sep='')

ResultFolder<- paste('Length_Assessment/Results/',sep='')

  if (!file.exists(FigureFolder))
  {
    dir.create(FigureFolder,recursive=T)
    
    dir.create(ResultFolder,recursive=T)
  }



a=1
Fish$LHITol<- 0.99
#       
#       for (a in 1:length(Assessments)) #Loop over possible assessments, store in Assessment results. Many assessments have more detailed outputs than can also be accessed 
#       {

Counter<- Counter+i
 
#Checks for sufficient sample size with Years of species       
SampleCheck<- CheckLengthSampleSize(LengthData)        
#           
#           if (SampleCheck$YearsWithEnoughData>0)
#           {


Temp[i,]<- LBAR(SampleCheck$ParedData,LagLength=1,Weight=1,IncludeMPA=0,ReserveYr=NA,OutsideBoundYr=NA,Iterations=1000,
            BootStrap=1,LifeError=0,Lc=Lc)$Output		

StoreAssess[i,]<- data.frame(Fishes,Sites[s],Assessments[a],Temp[i,],stringsAsFactors=F) 

}

colnames(StoreAssess) <- c("Species", "Site",	"Assessment","	Year",	"Method",	"SampleSize",	"Value",	"LowerCI","UpperCI",	"SD",	"Metric",	"Flag")

#######################Save Results#############################################


write.csv(file=paste('Length_Assessment/Results/',Assessments,'_Results.csv',sep=''),StoreAssess)


#######################Plot Length Distributions#############################################

#Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
Surveycolor<-"red"
Fisherycolor <- "lightseagreen"

for (j in 1:length(sp_id)){
  
#Life History Parameters for species[i]  
  Fish<-read.csv("./Length_Assessment/Full_Data_Sets/MNI_LH_FINAL.csv")%>%
    filter(Species==sp_id[j]) %>%
    select(-Reference) %>%
    spread(Parameter, Value)
  
  Fish$Mat50 <- Fish$m95


#Specifies where to save plots
  FigureFolder<- paste("./Length_Assessment/Figures/")
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
 
  Species=common[j]

  LengthDataPlot<-filter(LengthDataAll, Species.ID==sp_id[j])
  
  PlotLengthData(LengthDataPlot,FigureFolder,Fish,Species,Theme)
}

