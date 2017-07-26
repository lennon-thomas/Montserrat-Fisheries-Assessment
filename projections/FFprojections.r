#rough draft of FF projections 

rm(list = ls())
params<-read.csv("projections/LifeParms_MNI.csv",header=T)
paramsSite<-read.csv("projections/SiteParams_MNI.csv",header=T)      
      
 
 #################################################
 ##Control parameters                           #
 ##                                             #
 R0=1000                                        
 P=60                #number homogenous patches, differ only in fishing pressure
 TRfraction=1/3       #fraction of TR system to be compared to larger community, no matter how large or small TR system, we look at 2* for relative impact
 yearsOA=100
 yearsTR=30
 OAfrac=0.15          #open access equilibrium B/B0
 ##                                             #     
 ##                                             #     
 ##                                             #     
 ################################################
colvec=c(1,"darkred","gold",3,"turquoise3")
 
 
 
 
      
      #######################
      #Parameter vectors
      #######################
      
      ##import species information
      species<-params$Common              #list of species
      species_site<-params$Site           #list of sites per species
      species_country<-params$Country     #list of country per species
      Linf<-params$Linf                   #BH parameter, asymptotic length
      Linf_units<-params$LinfUnit1        #units cm, mm
      kpar<-params$k                      #BH parameter growth rate
      t0par<-params$t0                    #BH parameter, size at first settlement
      wtpar1<-params$WeightA              #weight at length parameter
      wtpar2<-params$WeightB              #weight at length parameter
      wtunit1<-params$WeightUnit1         #units cm, mm
      wtunit2<-params$WeightUnit3         #units g, kg
      max_age<-round(params$AgeAvg)              #ave max age 
      mat_age<-round(params$MatAge)              #age at maturity
      #M                                  #inst. mortality
      #m<-(1-exp(-M))                     #annual natural mortality
      mort<-params$MortYr                 #annual natural mortality
      pld<-params$PLD                     #pelagic larval duration
      moveRange<-params$Range             #maximum distance of range in linear meters
      moveRange=moveRange/1000            #convert to km
      f<-rep(1,length(species))	          #PLACEHOLDER for vector of fecundity at age
      steep<-params$Steepness             #parameter for recruitment
    
      ##import site information
      site<-as.character(paramsSite$Site)           #list of sites 
      country<-as.character(paramsSite$Country)     #list of country per site
      ##note potential to use municipal size as comparison for larger community, but need similar proxy for indo
      #note potential to add in year of NTZ implementation
      TURFarea<-paramsSite$TURF_sqkm  #TURF area in square km
      NTZarea<-paramsSite$NTZ_sqkm   #NTZ area in square km
      
      #######################
      #simulation setup
      #######################
      
      years<-yearsOA+yearsTR                      #total years for equilibrium and simulation          
      numMA=P*TRfraction                          #number of managed areas within TR system
      numOA=P-numMA
      
     
        #functions for sims
     ##############################
       
      #create functions for sims:
      #PopM will be holder for any population matrix of rows=#ageclasses and col=#areas
      eggsProduce<-function(PopM,ff,ww,mm){             ##function of population in each area, fecundity, weight, maturity
              eggProd=(ff*ww*mm)*PopM       #egg production for each age class, each area
              eggSum=colSums(eggProd)   #total egg production by area, vector of length # areas
              return(eggSum)
              }
      
      #calculates larval dispersal among patches, so far can choose between common larval pool (DL1) and gaussian dispersal (DL2)        
      settle<-function(EggV,Disp){
              settlers=EggV%*%Disp   
              return(settlers)
              }
              
      recruit<-function(setV,rec,ssb,steepness){        
              recruits=(0.8*rec*steepness*setV)/(0.2*ssb*(1.0 -steepness)+(steepness-0.2)*setV)
              return(recruits)}
              
      yieldNumbers<-function(PopM,uVec,vv){       #(pop, given vector of harvest rates for that sim, vv=selectivity)
              Nums=vv*(t(uVec*t(PopM)))
              return(Nums)
              }
                              
      yieldBio<-function(yieldN,ww){
                yieldage=yieldN*ww
                yB=colSums(yieldage)
                return(yB)
                }
                                  
               
      adultSurvival<-function(PopM,yieldN,survivals){
                fished=PopM-yieldN 
                fished[which(fished<0)]=0
                survives=(fished)*survivals
                return(survives)
                }
                
      adultMove<-function(PopM,move){
                adultPop=PopM[-1,]
                movedPop=adultPop%*%move
                addRecruits=rbind(PopM[1,],movedPop)
                return(addRecruits)
                }
                                  
      ageFish<-function(PopM,recruits){
                newPop=matrix(NA,nrow=nrow(PopM),ncol=ncol(PopM))
                newPop[1,]=recruits  #recruits is vector of number of recruits by each area
                newPop[2:nrow(PopM),]=PopM[1:(nrow(PopM)-1),]
                newPop[nrow(PopM),]=newPop[nrow(PopM),]+PopM[nrow(PopM),]
                return(newPop)
                }
      
      
       Biomass<-function(PopM,ww){
                biomassage=PopM*ww
                biomass=colSums(biomassage)
                return(biomass)
                }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
#                        pdf(file="HilaryPlots.pdf")
        
      #########################################################################################################################
      ##START SIMS OVER SITES##
      ###########################

      for(loc in 1:nrow(paramsSite)){
           
           numNTZ=round((NTZarea[loc]/(TURFarea[loc]+NTZarea[loc]))*numMA)         #number of NTZ areas within the managed areas
           if((numNTZ%%2)==0){                                      #this sets up structure of area with 1 as managed and 0 as NTZ, with NTZ placed in cemter of managed
              MAstructure=c(rep(1,(numMA-numNTZ)/2),rep(0,numNTZ),rep(1,(numMA-numNTZ)/2))
            } else{
              MAstructure=c(rep(1,round((numMA-numNTZ)/2)),rep(0,numNTZ),rep(1,round((numMA-numNTZ)/2)-1))
            }  
          areaSize=(NTZarea[loc]+TURFarea[loc])/numMA
          
          #identify species at site
          species_pointer=which(species_site==site[loc])      #gives vector of locations for the species that belong to this site
          numSpecies=length(species_pointer)
      
           scen3CompareYield=matrix(NA,nrow=numSpecies,ncol=years)      #save scen 3 for species at a site to compare
          #########################################################################################################################
          ##START SIMS OVER SPECIES##
          ###########################
          for(ss in 1:numSpecies){
              sp=species_pointer[ss]  #find the location value for each species
                   
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
                #convert length units to match units for wt relationship
                if(Linf_units[sp]!=wtunit1[sp]){
                  if(Linf_units[sp]=="cm") len=len*10
                  if(Linf_units[sp]=="mm") len=len/10
                }
                wt<-wtpar1[sp]*len^wtpar2[sp]                             #vector of mass at age
                if(wtunit2[sp]=="g") wt=wt/1000                                #convert to kg
              
                mat<-c(rep(0,mat_age[sp]),rep(1,max_age[sp]+1-mat_age[sp]))     #vector of to flag mature fish, assuming knife edge maturity
                                                                          #NOTE: this rounds mat age from a conversion from mat length, best to change this to mat length somehow?
                fec<-rep(f[sp],max_age[sp]+1)
                surv<-c(1,rep(1-mort[sp],max_age[sp]))
                
                standardDist<-moveRange[sp]/areaSize                  #Standardized movement range in #patches from center of patch
                sigmaA<-min(standardDist/1.65,20)                  #Adult movement parameter, sd on standard normal gaussian, limit sigma to 20 for matrix setup 
                #placeholder for PLD to area size equation...
                sigmaL<-2                     #Larval dispersal distance parameter, sd on standard normal gaussian
                   
                v1=c(c(0,rep(1,max_age[sp])))  #vector of selectivity ASSUMING fishing all fish age 1 and up, regardless of size   
                v2=c(rep(0,mat_age[sp]+2),rep(1,max_age[sp]-1-mat_age[sp]))           #this allows each species to reach maturity and waits 2 years
                   
                 ##################################
                #Movement
                ##################################

                #dispersal:common larval pool
                	DL1=matrix(1/(P),nrow=P,ncol=P)

                #dispersal:gaussian movement
                	#create movement probability matrix, start with distance matrix
                	area.loc<-seq(-(P-1),2*P,1)
                	area.cur<-seq(1,P,1)

                	#create distance matrix
                	dist<-matrix(NA,nrow=P,ncol=P*3)
                	for(i in 1:P)
                		{
                			for(j in 1:(P*3))
                			{
                			dist[i,j]<-area.cur[i]-area.loc[j]
                			}
                		}
                	#now create the movement matrix of probabilities of movement
                	p.init<-round(exp(-((dist)^2)/(2*(sigmaL^2))),2)

                	#now add matrices on ends to wrap movement and normalize so movement from any one area sums to one
                	p.all<-matrix(NA,nrow=P,ncol=(3*P))
                	for(i in 1:P)
                	{
                		for(j in 1:(3*P))
                		{
                			p.all[i,j]<-(p.init[i,j])/sum(p.init[i,])
                		}
                	}
                  
                	p1<-p.all[,1:P]
                	p2<-p.all[,(2*P+1):(3*P)]
                	parea<-p.all[,(P+1):(2*P)]
                	DL2<-p1+p2+parea


                  ###FOR ADULTS#########
                	p.init.A<-round(exp(-((dist)^2)/(2*(sigmaA^2))),2)

                	#now add matrices on ends to wrap movement and normalize so movement from any one area sums to one
                	p.all.A<-matrix(NA,nrow=P,ncol=(3*P))
                	for(i in 1:P)
                	{
                		for(j in 1:(3*P))
                		{
                			p.all.A[i,j]<-(p.init.A[i,j])/sum(p.init.A[i,])
                		}
                	}
                
                	p1.A<-p.all.A[,1:P]
                	p2.A<-p.all.A[,(2*P+1):(3*P)]
                	parea.A<-p.all.A[,(P+1):(2*P)]
                	DA<-p1.A+p2.A+parea.A

                 ###############################


               
                 
                 
                 
                 
                 
                  ###########################################################################################################################
                  #find u OA and opt for each species and store
                  #find for one area, this does not find opt with movement and pressure
                 
                 
 
                  ####################
                  #starting conditions
                  ####################
                  x=NA
                  initPop=NA
                  initPopM=NA
                  SSB0_area=NA
                  B0_area=NA
                  
                 
                  x<-max_age[sp]
                  initPop=rep(NA,x+1)      
                  for(d in 1:(x+1)){                                          #create initial population structure at unfished equilibrium for isolated population
                        if (d==1) initPop[d]=R0
                        if  (d>1 & d<=x) initPop[d]= initPop[(d-1)]*surv[d-1]
                        if  (d>x) initPop[d]=initPop[(d-1)]*surv[d-1]/(1-surv[d])
                        } #end ages
                      
                  SSB0_area=sum(wt*fec*mat*initPop)                  #unfished spawning stock biomass for one area
                  B0_area=sum(wt*initPop)                          #unfished biomass for one area
                  initPopM=matrix(initPop,nrow=x+1,ncol=P)        #initial population matrix, rows represent ages, columns are areas
                  
                  
                   
                    ###########################
                  #function to find optimized harvest rate for single area (equal in each area for now)
                  ##NOTE, this has to be done here for now, as I don't know how to optimize with a function with multiple arguments 
                  maxHarvestRate=function(u){
                    u1=u[1]
                    pop=initPop
                    for(tempy in 1:yearsOA){
                      Eggs=sum(fec*mat*pop*wt)
                      Rec=(0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)
                      yieldB=sum(pop*u1*v1*wt)
                      yield=pop*u1*v1
                      pop=(pop-yield)*surv    
                      ptemp=pop
                      pop[1]<-Rec
                      pop[x+1]<-ptemp[x]+ptemp[x+1]
                      pop[2:x]<-ptemp[1:(x-1)]
                      
                      }
                   
                    return(-yieldB)
                  }
                  
                   
                  #find opt u for v1(selecting age 1 and up)
                  findopt<-optimize(maxHarvestRate,c(0,1))
                  u1_opt<-findopt$minimum
                  
                   #probably a much better way to do this, but optimizing for a different v2 (selecting one year after maturity)
                 maxHarvestRate2=function(u){
                    u1=u[1]
                    pop=initPop
                    for(tempy in 1:yearsOA){
                      Eggs=sum(fec*mat*pop*wt)
                      Rec=(0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)
                      yieldB=sum(pop*u1*v2*wt)
                      yield=pop*u1*v2
                      pop=(pop-yield)*surv    
                      ptemp=pop
                      pop[1]<-Rec
                      pop[x+1]<-ptemp[x]+ptemp[x+1]
                      pop[2:x]<-ptemp[1:(x-1)]
                      
                      }
                   
                    return(-yieldB)
                  }
                  
                   
                  #find opt u for v1(selecting age 1 and up)
                  findopt<-optimize(maxHarvestRate2,c(0,1))
                  u2_opt<-min(findopt$minimum,0.99)
                
                  
                  
                  #now also find an open access that makes sense, such that B/B0 is approximately 0.1, specifically OAfrac from control panel
                  
                  
                  
                  OAHarvestRate=function(u){
                    u1=u[1]
                    pop=initPop
                    for(tempy in 1:yearsOA){
                      Eggs=sum(fec*mat*pop*wt)
                      Rec=(0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)  
                      yieldB=sum(pop*u1*v1*wt)
                      yield=pop*u1*v1
                      pop=(pop-yield)*surv    
                      ptemp=pop
                      pop[1]<-Rec
                      pop[x+1]<-ptemp[x]+ptemp[x+1]
                      pop[2:x]<-ptemp[1:(x-1)]
                      }
                      Biomass=(sum(pop*wt))
                      BioFrac=((Biomass/B0_area)-OAfrac)^2
                    return(BioFrac)
                  }
                  
                  findOA<-optimize(OAHarvestRate,c(0,1))
                  OArate1<-findOA$minimum

                    ####and again for v2
                                  
                  OAHarvestRate2=function(u){
                    u1=u[1]
                    pop=initPop
                    for(tempy in 1:yearsOA){
                      Eggs=sum(fec*mat*pop*wt)
                      Rec=(0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)  
                      yieldB=sum(pop*u1*v2*wt)
                      yield=pop*u1*v2
                      pop=(pop-yield)*surv    
                      ptemp=pop
                      pop[1]<-Rec
                      pop[x+1]<-ptemp[x]+ptemp[x+1]
                      pop[2:x]<-ptemp[1:(x-1)]
                      }
                      Biomass=(sum(pop*wt))
                      BioFrac=((Biomass/B0_area)-OAfrac)^2
                    return(BioFrac)
                  }
                  
                  findOA<-optimize(OAHarvestRate2,c(0,1))
                  OArate2<-findOA$minimum
                  
                  
                  
                  
                  
                  ############################
                  #Scenaro SIMULATION
                  
                  setupA=rep(1,P*TRfraction)      #status quo, no NTZ
                  setupB=MAstructure              #existing NTZ structure
                  setupC=c(rep(1,(numMA-0.2*numMA)/2),rep(0,0.2*numMA),rep(1,(numMA-0.2*numMA)/2))                          #20%NTZ
                  scenarios=5
                  scens=matrix(NA,nrow=scenarios,ncol=P)
                  scens[1,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupA,rep(OArate1,(P-P*TRfraction)/2)) 
                  scens[2,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate1*setupB,rep(OArate1,(P-P*TRfraction)/2))
                  scens[3,]=c(rep(OArate1,(P-P*TRfraction)/2),u1_opt*setupB,rep(OArate1,(P-P*TRfraction)/2))
                  scens[4,]=c(rep(OArate1,(P-P*TRfraction)/2),u1_opt*setupC,rep(OArate1,(P-P*TRfraction)/2))
                  scens[5,]=c(rep(OArate1,(P-P*TRfraction)/2),u2_opt*setupB,rep(OArate1,(P-P*TRfraction)/2))
                  #scens[5,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate2*setupB,rep(OArate1,(P-P*TRfraction)/2))
                  #scens[7,]=c(rep(OArate1,(P-P*TRfraction)/2),u2_opt*setupC,rep(OArate1,(P-P*TRfraction)/2))
                  #scens[8,]=c(rep(OArate1,(P-P*TRfraction)/2),OArate2*setupC,rep(OArate1,(P-P*TRfraction)/2))
                  
                  sel1=matrix(v1,nrow=(max_age[sp]+1),ncol=P,byrow=F)          #build matrix of selectivity by age by area, all fully selected                                                   
                  sa=matrix(v1,nrow=(max_age[sp]+1),ncol=(P-P*TRfraction)/2,byrow=F)                                                             #build matrix of selectivity by age by area, managed area with size liimits
                  sb=matrix(v2,nrow=(max_age[sp]+1),ncol=numMA,byrow=F) 
                  sel2=cbind(sa,sb,sa)
                  
                  PopBiomass=matrix(NA,nrow=scenarios,ncol=years)
                  YieldBiomass=matrix(NA,nrow=scenarios,ncol=years)
                  locPopBiomass=matrix(NA,nrow=scenarios,ncol=years)        #local MA
                  locYieldBiomass=matrix(NA,nrow=scenarios,ncol=years)      #local MA
                 
                  
                  
                  for(scen in 1:scenarios){
                      
                    
                      
                      #run sims
                      for(tt in 1:years){
                          if(tt<=yearsOA) {
                            uvec=scens[1,]
                            age_v=sel1} else {
                            uvec=scens[scen,]
                            if(scen<5) age_v=sel1 else age_v=sel2            #adjust based on scenarios and selectivity per scenario
                            }
                          if(tt==1) PopSim=initPopM
                          Eggs=eggsProduce(PopSim,fec,wt,mat) 
                          Settlers=settle(Eggs,DL2)
                          Recruits=recruit(Settlers,R0,SSB0_area,steep[sp])
                          YieldN=yieldNumbers(PopSim,uvec,age_v)
                          PopSim=adultSurvival(PopSim,YieldN,surv)
                          PopSim=adultMove(PopSim,DA)
                          PopSim=ageFish(PopSim,Recruits)
                          PopBiomass[scen,tt]=sum(Biomass(PopSim,wt))
                          YieldBiomass[scen,tt]=sum(yieldBio(YieldN,wt))
                          locPopBiomass[scen,tt]=sum(Biomass(PopSim,wt)[((P-P*TRfraction)/2+1):(((P-P*TRfraction)/2)+numMA)])
                          locYieldBiomass[scen,tt]=sum(yieldBio(YieldN,wt)[((P-P*TRfraction)/2+1):(((P-P*TRfraction)/2)+numMA)])
                          
                          }
                        
                      
                       
                    }#end scenarios
                    
                   
                      
                   
#                      windows() 
                         
                      #jpeg(paste("c:/results/p",sp,".jpg") )
                      par( mfrow = c( 1, 2 ), oma = c( 0, 0, 2, 0 ))
                     
                    #colvec=c(1,"gold",3:6,"grey89","darkred")
                    colvec=c(1,"darkred","gold",3,"turquoise3")
                    tstart=90  #start graph at year 40 after most of OA access burn in
                   # matplot(t(PopBiomass[,tstart:years]/(B0_area*P)),ylab="Global biomass/Global B0",xaxt="n",ylim=c(0,1),type="l",main=paste(levels(site)[which(levels(site)==site[loc])],", ",levels(species)[which(levels(species)==species[sp])]),col=colvec,lty=1,lwd=2)
#                      axis(1, at=seq(1,(years-tstart+1),5),labels=seq((tstart-yearsOA),yearsTR,5),las=0)
#                    matplot(t(YieldBiomass[,tstart:years]),ylab="Global yield",yaxt="n",xaxt="n",ylim=c(0,max(YieldBiomass[,tstart:years])),type="l",col=colvec,lty=1,lwd=2,)
#                      axis(1, at=seq(1,(years-tstart+1),5),labels=seq((tstart-yearsOA),yearsTR,5),las=0)
#                      axis(2, at=c(0,max(YieldBiomass[,tstart:years])),labels=c("0","MaxGlobal"),las=0)
                    matplot(t(locPopBiomass[,tstart:years]/(B0_area*numMA)),main=levels(species)[which(levels(species)==species[sp])],ylab="Local biomass/local B0",xaxt="n",ylim=c(0,1),type="l",col=colvec,lty=1,lwd=2)
                      axis(1, at=seq(1,(years-tstart+1),5),labels=seq((tstart-yearsOA),yearsTR,5),las=0)
                    matplot(t(locYieldBiomass[,tstart:years]/max(locYieldBiomass[,tstart:years])),ylab="Local yield/ max local yield",xaxt="n",ylim=c(0,1),type="l",col=colvec,lty=1,lwd=2)
                      axis(1, at=seq(1,(years-tstart+1),5),labels=seq((tstart-yearsOA),yearsTR,5),las=0)
                      #axis(2, at=c(0,max(YieldBiomass[,tstart:years])),labels=c("0","Max"),las=0)
                    #plot(1,1,main=c(OArate1,OArate2,u1_opt))
                  title(paste(country[loc],", ",site[loc],"\n h1 = ",signif(OArate1,4),", h2 = ",signif(OArate1,4),", h3 = ",signif(u1_opt,4),", h4 = ",signif(u1_opt,4),", h5 = ",signif(u2_opt,4)),outer=T)
                   
          
                        scen3CompareYield[ss,]=locYieldBiomass[3,]/max( locYieldBiomass[3,])
    
          }#end loop over species  
                          #
                     #build legend
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
                      
                          
                          
                          
                          
      }  #end loop over sites 
  

  
      
par(mfrow=c(1,1))
#  windows()
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend("topleft",legend=c("Scenario 1: Status Quo","Scenario 2: Current NTZ","Scenario3: Cur NTZ + MA","Scenario 4: 20% NTZ + MA","Scenario 5: Size limit"),lty=1,col=colvec,cex=1.3,bty="n",lwd=3)  
  
#dev.off()

