#functions for sims of projected population biomass and yield
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