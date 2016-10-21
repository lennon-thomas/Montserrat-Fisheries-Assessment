##Length data plot and data prep
rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)
#source("SubFunctions.R") 
#Data Summary Length Data

data <- read.csv("Sept_2016/Data/Montserrat_Species_Length_Composition_Data_July_2016_AS.csv")
names(data)
days <- length(unique(data$Date))
boats <- length(unique(data$Vessel.ID..Length))
area <- length(unique(data$Area.fished))
#unique(df[c("data$Date","data$Vessel.ID..Length")])
trips <- data.frame(data$Date,data$Vessel.ID..Length)
unique_trips <- unique(trips)
nrow(unique_trips)
nrow(trips)
gears<-unique(data$Gear.Type)
species_CN <- unique(lapply(data$COMMON_NAME, tolower))

levels(gears)
data[data=="POT/LINE"]<-"pots"
data[data=="lines"]<-"line"
sp_id<-c("ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARVI","SERRGU")

common<-c(
  ACANCH="Doctorfish",
  ACANCO="Blue Tang",
  BALIVE="Old wife",
  HOLOAD="Squirrelfish",
  LUTJMA="Mahogany snapper",
  LUTJSY="Lane snapper",
  LUTJVI="Silk snapper",
  SCARCH="Spotlight parrotfish",
  SERRGU="Red hind")


SA<-read.csv("Sept_2016/Data/SA_individ_lengths.csv")%>%
  mutate(Gear.Type="SA")
nrow(SA)
SA<-SA[!(SA$Length<3),] ## remove observations that are <3 cm
i=8
for (i in 1:length(sp_id)){
  l<-data%>%
    filter(Species.ID == sp_id[i])%>%
    select(Species.ID,Length,Gear.Type)
     
  s<-SA%>%
    filter(Species.ID == sp_id[i])%>%
    select(Species.ID,Length,Gear.Type)
    t<-rbind(s,l)%>%
    mutate(Data.Type=ifelse(Gear.Type=="SA","Survey","Fishery"))%>%
    write.csv(paste("Sept_2016/Data/",sp_id[i],"_length.csv",sep=""))
}



#######################Plots#############################################

#Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
Surveycolor<-"red"
Fisherycolor <- "lightseagreen"

Directory<-("Data")
Files <- list.files(Directory)

for (i in 1:length(sp_id)){

LengthData <-read.csv(paste("Sept_2016/Data/",sp_id[i],"_length.csv",sep=""))

Fish<-read.csv("Sept_2016/Data/life.csv")%>%
  filter(species==sp_id[i])

#source(paste(sp_id[i],"_ControlFile.R",sep=""))
Fish$Mat50<-Fish$m95
Fish$Linf<-Fish$Linf

FigureFolder<- paste("Sept_2016/plots/")
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


