##Length data plot and data prep
rm(list = ls())

pkgs <- c('dplyr','readr', 'ggplot2','tidyr')

#This will install necessary packages on local machine
load.packages = function(a){
  if(!require(a, character.only = TRUE)){
    install.packages(a)
    library(a, character.only = TRUE)
  }
}

lapply(pkgs, load.packages)
 

sapply(list.files(pattern="[.]R$", path="./Length_Assessment/Functions", full.names=TRUE), source)
#source("SubFunctions.R") 
#Data Summary Length Data

data <- read.csv("./Length_Assessment/all_length.csv")
names(data)
gears<-unique(data$Gear.Type)
#days <- length(unique(data$Date))
#boats <- length(unique(data$Vessel.ID..Length))
#area <- length(unique(data$Area.fished))
#unique(df[c("data$Date","data$Vessel.ID..Length")])
#trips <- data.frame(data$Date,data$Vessel.ID..Length)
#unique_trips <- unique(trips)
#nrow(unique_trips)
#nrow(trips)
# species_CN <- unique(lapply(data$COMMON_NAME, tolower))

levels(gears)
#data[data=="POT/LINE"]<-"pots"
#data[data=="lines"]<-"line"
sp_id<-c("ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARVI","SERRGU")

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

# SA data has already been merged in 'all_length.csv'
# 
# SA<-read.csv("Sept_2016/Data/SA_individ_lengths.csv")%>%
#   mutate(Gear.Type="SA")
# nrow(SA)
# SA<-SA[!(SA$Length<3),] ## remove observations that are <3 cm
# i=8
# for (i in 1:length(sp_id)){
#   l<-data%>%
#     filter(Species.ID == sp_id[i])%>%
#     select(Species.ID,Length,Gear.Type)
#      
#   s<-SA%>%
#     filter(Species.ID == sp_id[i])%>%
#     select(Species.ID,Length,Gear.Type)
#     t<-rbind(s,l)%>%
#     mutate(Data.Type=ifelse(Gear.Type=="SA","Survey","Fishery"))%>%
#     write.csv(paste("Sept_2016/Data/",sp_id[i],"_length.csv",sep=""))
# }



#######################Plots#############################################

#Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
Surveycolor<-"red"
Fisherycolor <- "lightseagreen"

# Directory<-("Data")
# Files <- list.files(Directory)

for (i in 1:length(sp_id)){

LengthData <-data

Fish<-read.csv("./Length_Assessment/MNI_LH_FINAL.csv")%>%
  filter(Species==sp_id[i]) %>%
  select(-Reference) %>%
  spread(Parameter, Value)

#source(paste(sp_id[i],"_ControlFile.R",sep=""))
Fish$Mat50<-Fish$m95
#Fish$Linf<-Fish$Linf

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
#Fish<-Fish$common
Species=common[i]
#if (exists('LengthData')) {

  PlotLengthData(LengthData,FigureFolder,Fish,Species,Theme)
}


