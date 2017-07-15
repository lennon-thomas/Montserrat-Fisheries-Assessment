#rough draft of FF projections 

#edits for MNI on 7/11/17

rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

setwd("/Users/lennonthomas/Desktop/Montserrat-Fisheries-Assessment/projections")

########################
#Load Files
######################

files<-(list.files(path= "functions",pattern = "*.R"))
 
lapply(paste("functions/",files,sep=""),source)

#source(paste("functions/",files,sep=""))

params<-read.csv("LifeParms_MNI.csv",header=T,stringsAsFactors = FALSE)

paramsSite<-read.csv("SiteParams_MNI.csv",header=T)   

#######################
#Parameter vectors
#######################

#Life history parameters

species<-as.vector(params$Common)              #list of species

species_site<-params$Site           #list of sites per species

species_country<-params$Country     #list of country per species

Linf<-as.vector(params$Linf)                   #BH parameter, asymptotic length

Linf_units<-as.vector(params$LinfUnit1)        #units cm, mm

kpar<-as.vector(params$k)                      #BH parameter growth rate

t0par<-as.vector(params$t0)                    #BH parameter, size at first settlement

wtpar1<-as.vector(params$WeightA)              #weight at length parameter

wtpar2<-as.vector(params$WeightB)             #weight at length parameter

wtunit1<-as.vector(params$WeightUnit1)         #units cm, mm

wtunit2<-as.vector(params$WeightUnit3)         #units g, kg

max_age<-as.vector(round(params$AgeAvg))              #ave max age 

mat_age<-as.vector(round(params$MatAge))              #age at maturity
#M                                  #inst. mortality

#m<-(1-exp(-M))                     #annual natural mortality

mort<-as.vector(params$MortYr)                 #annual natural mortality

pld<-as.vector(params$PLD)                     #pelagic larval duration

moveRange<-as.vector(params$Range)             #maximum distance of range in linear meters

moveRange=as.vector(moveRange/1000)            #convert to km

f<-rep(1,length(species))	          #PLACEHOLDER for vector of fecundity at age

steep<-as.vector(params$Steepness )            #parameter for recruitment

#Site information

site<-as.character(paramsSite$Site)           #list of sites 

country<-as.character(paramsSite$Country)     #list of country per site

TURFarea<-paramsSite$TURF_sqkm  #TURF area in square km

NTZarea<-paramsSite$NTZ_sqkm   #NTZ area in square km

#####################################
#Set up parameters and run Catch-MSY#
######################################
#for (i in 1:length(species)) {
i=4
sp<-species[i]

CatchData<-read.csv(paste(species[i],"/",species[i],"_CatchData.csv",sep = ""))

ggplot(CatchData,aes(x=Year,y=Catch))+
  geom_line()+
  theme_bw()+
  ggtitle(species[i])

st_yr<-CatchData$Year[1]

max_catch<-max(CatchData$Catch)

st_bio<-CatchData$Catch[1]/max_catch

start1<-ifelse (st_bio < 0.5,0.5,
                0.3)

start2<-ifelse(st_bio <0.5, 0.9,
               0.6)
  
st_bio<- as.vector(cbind(start1,start2))

end_bio<-CatchData$Catch[length(CatchData$Catch)]/max_catch

end1<-ifelse(end_bio > 0.5, 0.3, 
             0.01)

end2<- ifelse(end_bio > 0.5, 0.7, 
              0.4)

end_bio<-as.vector(cbind(end1,end2))

# Run Catch MSY function and read in results

res<-params$res[i]

Temp<- CatchMSY(CatchData,1000,0.05,0,1,1,0,0,0,CatchData[1,1],st_bio,NA,NA,end_bio,res,sp)

write.csv(Temp,paste(species[i],"/Results/CatchMSY.csv",sep = ""))

msy_results<-read.csv(paste(species[i],"/Results/Raw_CatchMSY.csv",sep = ""))
 
print (species[i])
#}
#################################################
 #Control parameters                           
################################################

R0 = 1000  

P = 60            #number homogenous patches, differ only in fishing pressure

TRfraction = 1/3       #fraction of TR system to be compared to larger community, no matter how large or small TR system, we look at 2* for relative impact

yearsOA = 100        #years of oopen access

yearsTR = 30         #years of Turf Reserve

OAfrac = msy_results$end_b_K_ratio #open access equilibrium B/B0. This value is read in from the b/k output for 2014 from Catch MSY results


#######################
#Simulation setup
#######################
      
years <- yearsOA+yearsTR                      #total years for equilibrium and simulation          

numMA = P*TRfraction                          #number of managed areas within TR system

numOA = P-numMA
      
####################
#Start simulations
####################


loc = 1 
      #for(loc in 1:nrow(paramsSite)){
           
 numNTZ=round((NTZarea[loc]/(TURFarea[loc]+NTZarea[loc]))*numMA)                            #number of NTZ areas within the managed areas

           if((numNTZ%%2)==0){                                                                       #this sets up structure of area with 1 as managed and 0 as NTZ, with NTZ placed in cemter of managed
             
              MAstructure=c(rep(1,(numMA-numNTZ)/2),rep(0,numNTZ),rep(1,(numMA-numNTZ)/2))
            } else{
              
              MAstructure=c(rep(1,round((numMA-numNTZ)/2)),rep(0,numNTZ),rep(1,round((numMA-numNTZ)/2)-1))
            } 
           
       areaSize=(NTZarea[loc]+TURFarea[loc])/numMA
          
   #identify species at site
          
       species_pointer = which(species_site == site[loc])      #gives vector of locations for the species that belong to this site
        
       numSpecies = length(species_pointer)
      
    
       
#####################################
#Start simulations over species
###################################
ss=4
#  for(ss in 1:numSpecies){
              sp = species_pointer[ss]  #find the location value for each species
                   
                ages<-NA                         #reset all values so nothing caries over from previous species
                len<-NA
                wt<-NA                
                sigmaL<-NA
                sigmaA<-NA
                mat<-NA
                fec<-NA
                surv<-NA
                v1<-NA
                v2<-NA
                v3<-NA        #placeholder for common size limit across species given length or weight
                
 ages<-seq(0,max_age[sp],1)                                 #vector of ages considered
                
 len<-Linf[sp]*(1-exp(-kpar[sp]*(ages-t0par[sp])))          #vector of lengths
                
# Check plot of species age at length

agelength<-as.data.frame(cbind(ages,len))
                
ggplot(agelength,aes(x = ages,y = len))+
  geom_line() +
  theme_bw() +
  ylab ("Length") +
  xlab ("Age")

# Convert length units to match units for wt relationship
    if(Linf_units[sp]!= wtunit1[sp]){
                 
          if(Linf_units[sp] == "cm") len=len*10
                  
            if(Linf_units[sp]=="mm") len=len/10
    }

 wt<-wtpar1[sp] * len ^ wtpar2[sp]                             #vector of mass at age
    
if(wtunit2[sp] =="g") wt = wt/1000                               #convert to kg
              
mat<-c(rep(0,mat_age[sp]),rep(1,max_age[sp]+1-mat_age[sp]))     #vector of to flag mature fish, assuming knife edge maturity
                                                                          #NOTE: this rounds mat age from a conversion from mat length, best to change this to mat length somehow?
fec<-c(rep(1,max_age[sp]+1))
#fec<-c(rep(1,max_age[sp]+1))
surv<-c(1,rep(1-mort[sp],max_age[sp]))
                
standardDist<-moveRange[sp]/areaSize                  #Standardized movement range in #patches from center of patch

sigmaA<-min(standardDist/1.65,20) #Adult movement parameter, sd on standard normal gaussian, limit sigma to 20 for matrix setup 
                #placeholder for PLD to area size equation...
sigmaL<-2                     #Larval dispersal distance parameter, sd on standard normal gaussian
 

###########################
# Assign selectivity by age
###########################

v1 = c(0,rep(1,max_age[sp]))  #vector of selectivity ASSUMING fishing all fish age 1 and up, regardless of size   
       

v2 = c(rep(0,mat_age[sp]),rep(1,max_age[sp]-mat_age[sp]+1))           #this allows each species to reach maturity (add +1 to wait 1 year). Use this to set minimum size (age)
                   
##################################
#Movement
##################################

#Dispersal:common larval pool

DL1 = matrix(1/(P),nrow = P,ncol = P)

#dispersal:gaussian movement. create movement probability matrix, start with distance matrix

area.loc<-seq(-(P-1),2*P,1)

area.cur<-seq(1,P,1)

#create distance matrix
dist<-matrix(NA,nrow=P,ncol=P*3)
                
  	for(i in 1:P) {
      		for(j in 1:(P*3)){
                			dist[i,j]<-area.cur[i]-area.loc[j]
                			}
                		}

#create the movement matrix of probabilities of movement

p.init<-round(exp(-((dist)^2)/(2*(sigmaL^2))),2)

#add matrices on ends to wrap movement and normalize so movement from any one area sums to one

p.all<-matrix(NA,nrow=P,ncol=(3*P))

      for(i in 1:P){
            
        for(j in 1:(3*P)){
          
            	p.all[i,j]<-(p.init[i,j])/sum(p.init[i,])
                		}
                	}
                  
p1<-p.all[,1:P]

p2<-p.all[,(2*P+1):(3*P)]

parea<-p.all[,(P+1):(2*P)]

DL2<-p1+p2+parea


#Adult Movement

p.init.A<-round(exp(-((dist)^2)/(2*(sigmaA^2))),2)

#now add matrices on ends to wrap movement and normalize so movement from any one area sums to one

p.all.A<-matrix(NA,nrow=P,ncol=(3*P))

for(i in 1:P)	{
  
    for(j in 1:(3*P))	{
      
         p.all.A[i,j]<-(p.init.A[i,j])/sum(p.init.A[i,])
                		
         }
                	
  }
                
p1.A<-p.all.A[,1:P]

p2.A<-p.all.A[,(2*P+1):(3*P)]

parea.A<-p.all.A[,(P+1):(2*P)]

DA<-p1.A+p2.A+parea.A

############################
#Find Optimal Harvest Rate#
###########################

#Starting conditions

x = NA

initPop = NA

initPopM = NA

SSB0_area = NA

 B0_area = NA
             
x<-max_age[sp]

initPop = rep(NA,x+1)      

#create initial population structure at unfished equilibrium for isolated population

for(d in 1:(x+1)){  
  
      if (d==1) initPop[d]=R0
     
      if  (d>1 & d<=x) initPop[d]= initPop[(d-1)]*surv[d-1]
     
      if  (d>x) initPop[d]=initPop[(d-1)]*surv[d-1]/(1-surv[d])
                       
      
       } #end ages
                      
SSB0_area = sum(wt*fec*mat*initPop)    #unfished spawning stock biomass for one area

B0_area = sum(wt*initPop)       #unfished biomass for one area

initPopM = matrix(initPop,nrow=x+1,ncol=P)        #initial population matrix, rows represent ages, columns are areas
                  
                  
#############################
#function to find optimized harvest rate for single area (equal in each area for now)
##NOTE, this has to be done here for now, as I don't know how to optimize with a function with multiple arguments 
###############################

#find opt u for v1(selecting age 1 and up)

findopt<-optimize(maxHarvestRate,c(0,1),tol=0.00001)

u1_opt<-findopt$minimum
                  
#probably a much better way to do this, but optimizing for a different v2 (selecting one year after maturity)

findopt<-optimize(maxHarvestRate2,c(0,1))

u2_opt<-min(findopt$minimum,0.99)
                

#now also find an open access that makes sense, such that B/B0 is approximately 0.1, specifically OAfrac from control panel

findOA<-optimize(OAHarvestRate,c(0,1))

OArate1<-findOA$minimum

####and again for v2

findOA<-optimize(OAHarvestRate2,c(0,1))

OArate2<-findOA$minimum
                  
############################
#Scenario Simulation
###########################           
OArate1=.5

setupA = rep(1,P*TRfraction)      #status quo, no NTZ

# setupB = MAstructure              #existing NTZ structure

setupC=c(rep(1,(numMA-0.2*numMA)/2),rep(0,0.2*numMA),rep(1,(numMA-0.2*numMA)/2))   #20% NTZ 
               
setupD=c(rep(1,0.34*numMA),rep(0, (0.38*numMA)),rep(1,0.35*numMA)) ##30%

scenarios = 5

scens=matrix(NA,nrow=scenarios,ncol=P)

catch_lim<-OArate1-(OArate1*.20)

#scens[1,]=c(rep(current_u,(P-P*TRfraction)/2),current_u*setupA,rep(current_u,(P-P*TRfraction)/2))#open access at current harvesst rate
#scens[1,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupA,rep(OArate1,(P-P*TRfraction)/2)) #Assume a Catch Limit that is set to maintain current biomass level

scens[1,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupA,rep(OArate1,(P-P*TRfraction)/2)) # Assume current fishing pressure continues

#scens[2,]=c(rep(catch_lim,(P-P*TRfraction)/2),catch_lim*setupA,rep(catch_lim,(P-P*TRfraction)/2)) #  Catch limit # Reduce harvest by 20%

scens[2,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupC,rep(OArate1,(P-P*TRfraction)/2)) # 20% of total are no take

scens[3,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupD,rep(OArate1,(P-P*TRfraction)/2)) # 30% of total area take

scens[4,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupA,rep(OArate1,(P-P*TRfraction)/2)) #minimum size only

scens[5,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupD,rep(OArate1,(P-P*TRfraction)/2)) # minimum size and 20% no take 
 

         
sel1=matrix(v1,nrow=(max_age[sp]+1),ncol=P,byrow=F)          #build matrix of selectivity by age by area, all fully selected 
sa=matrix(v1,nrow=(max_age[sp]+1),ncol=(P-P*TRfraction)/2,byrow=F)                                                             #build matrix of selectivity by age by area, managed area with size liimits
sb=matrix(v2,nrow=(max_age[sp]+1),ncol=numMA,byrow=F) 
sel2=cbind(sa,sb,sa)

#sb=matrix(v2,nrow=(max_age[sp]+1),ncol=numMA,byrow=F) 

                    
PopBiomass = matrix(NA,nrow=scenarios,ncol=years)

YieldBiomass = matrix(NA,nrow=scenarios,ncol=years)

locPopBiomass = matrix(NA,nrow=scenarios,ncol=years)        #local MA

locYieldBiomass = matrix(NA,nrow=scenarios,ncol=years)      #local MA
            

    
for(scen in 1:scenarios){

 for(tt in 1:years){
                         
    if(tt<=yearsOA) {
                            
      uvec = scens[1,]
                           
       age_v = sel1} else {
                            
         uvec = scens[scen,]
                            
         if(scen < 4) age_v = sel1 else age_v = sel2            #adjust based on scenarios and selectivity per scenario
                           
          }
                          
   if(tt==1) PopSim = initPopM
                         
    Eggs = eggsProduce(PopSim,fec,wt,mat) 
    
                          Settlers = settle(Eggs,DL2)
                          
                          Recruits =recruit(Settlers,R0,SSB0_area,steep[sp])
                          
                          YieldN = yieldNumbers(PopSim,uvec,age_v)
                          
                          PopSim = adultSurvival(PopSim,YieldN,surv)
                          
                          PopSim = adultMove(PopSim,DA)
                          
                          PopSim = ageFish(PopSim,Recruits)
                          
                          PopBiomass[scen,tt] = sum(Biomass(PopSim,wt))
                          
                          YieldBiomass[scen,tt] = sum(yieldBio(YieldN,wt))
                          
                          locPopBiomass[scen,tt] = sum(Biomass(PopSim,wt)[((P-P*TRfraction)/2+1):(((P-P*TRfraction)/2)+numMA)])
                          
                          locYieldBiomass[scen,tt] = sum(yieldBio(YieldN,wt)[((P-P*TRfraction)/2+1):(((P-P*TRfraction)/2)+numMA)])
                          
                          }
                        
                      
                       
                    }#end scenarios
                    
####################                   
# Plot results #                     
###################
png(file=paste(species[sp],"/Figures/projections.png",sep=""))

par( mfrow = c( 1, 2 ), oma = c( 0, 0, 2, 0 ))

colvec=c(1,"darkred","gold","red","turquoise3","purple","orange") 

tstart=100 #start graph at year 40 after most of OA access burn in
                  
matplot(t(PopBiomass[,100:years]/(B0_area*numMA)),main=levels(species)[which(levels(species)==species[sp])],xlab="Years",ylab="Relative Population Biomass",ylim=c(0,1),type="l",col=colvec,lty=1,lwd=2)
                   
matplot(t(YieldBiomass[,100:years]/max(YieldBiomass[,tstart:years])),ylab="Relative Fisheries Yield",xlab="Years",ylim=c(0,1),type="l",col=colvec,lty=1,lwd=2)
              
title(paste("Projection Results:",species[sp], "u=",OArate1))
dev.off   ()               
          
                    
    
     }#end loop over species  
                      
#################
#Plot Legend#
################
       
                     legendtext=NA
                     for(leg in 1:numSpecies)  {
                      if(leg==1)legendtext=levels(species)[which(levels(species)==species[species_pointer[leg]])]
                      else  legendtext=c(legendtext,levels(species)[which(levels(species)==species[species_pointer[leg]])]      )
                      }

                      
                     
                     
#                      windows()
#                      par(mfrow=c(1,1) )
#                       matplot(t(scen3CompareYield[,tstart:years]),main=paste(levels(country)[which(levels(country)==country[loc])],", ",levels(site)[which(levels(site)==site[loc])],", Local dispersal"),ylab="Scaled Scenario 3 Yield",xaxt="n",ylim=c(0,0.6),type="l",col=rainbow(numSpecies),lty=1,lwd=2)
#                      axis(1, at=seq(1,(years-tstart+1),5),labels=seq((tstart-yearsOA),yearsTR,5),las=0)
#                      mtext(side=1,"Years",line=2)
#                      legend("topright",legend=legendtext,col=rainbow(numSpecies),cex=0.8,bty="n",lwd=1.5)
                      
                          
                          
                          
                          
   # }  #end loop over sites 
  

  
      
par(mfrow=c(1,1))
#  windows()
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend("topleft",legend=c("Scenario 1: No management intervention","Scenario2: 20% MR","Scenario 3: 30% MR ","Scenario 4: Size Limit","Scenario 5: Size limit+ 30% MR"),lty=1,col=colvec,cex=1.3,bty="n",lwd=3)  
  
#dev.off()

