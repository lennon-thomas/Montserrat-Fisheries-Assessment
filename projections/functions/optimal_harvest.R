maxHarvestRate=function(u){
  u1 = u[1]
  pop = initPop
  
  for(tempy in 1:yearsOA){
    Eggs = sum(fec*mat*pop*wt)
    Rec = (0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)
    yieldB = sum(pop*u1*v1*wt)
    yield = pop*u1*v1
    pop = (pop-yield)*surv    
    ptemp = pop
    pop[1]<-Rec
    pop[x+1]<-ptemp[x]+ptemp[x+1]
    pop[2:x]<-ptemp[1:(x-1)]
    
  }
  
  return(-yieldB)
}

maxHarvestRate2 = function(u){
  u1 = u[1]
  pop = initPop
  for(tempy in 1:yearsOA){
    Eggs = sum(fec*mat*pop*wt)
    Rec = (0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)
    yieldB = sum(pop*u1*v2*wt)
    yield = pop*u1*v2
    pop = (pop-yield)*surv    
    ptemp = pop
    pop[1]<-Rec
    pop[x+1]<-ptemp[x]+ptemp[x+1]
    pop[2:x]<-ptemp[1:(x-1)]
    
  }
  
  return(-yieldB)
}


OAHarvestRate=function(u){
  u1 = u[1]
  pop = initPop
  for(tempy in 1:yearsOA){
    Eggs = sum(fec*mat*pop*wt)
    Rec = (0.8*R0*steep[sp]*Eggs)/(0.2*SSB0_area*(1.0 -steep[sp])+(steep[sp]-0.2)*Eggs)  
    yieldB = sum(pop*u1*v1*wt)
    yield = pop*u1*v1
    pop = (pop-yield)*surv    
    ptemp = pop
    pop[1]<-Rec
    pop[x+1]<-ptemp[x]+ptemp[x+1]
    pop[2:x]<-ptemp[1:(x-1)]
  }
  Biomass = (sum(pop*wt))
  BioFrac = ((Biomass/B0_area)-OAfrac)^2
  return(BioFrac)
}



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
