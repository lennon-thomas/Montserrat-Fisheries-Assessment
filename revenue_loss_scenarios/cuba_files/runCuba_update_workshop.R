#######################
### Cuba Run File ###
##### 02/25/2017 ######

## Set working directory
# setwd("~/")

## Set working directory
# setwd("~/Dropbox/Tracey's EcoA Work/Cuba/2017 Documents/Cuba_model_update")
setwd("~/Desktop/R Workshop")


## Read in data input file
dataInput = read.csv("revenue_loss_scenarios/projection_input.csv", header=TRUE,stringsAsFactors=FALSE)

## Read in functions file
source("revenue_loss_scenarios/cuba_files/functionsCuba_update.R")

## Read in libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(pander)

################################################
#### Set up model run
## Enter management scenarios to loop over - select 1 through 5
scenarios = c(1,2,3,4,5)
## Enter whether or not to loop over catch share cost and price scalars - put "yes" or "no"
catchShareLoop = "yes"
## Enter whether or not to loop over eliminating illegal fishing - put "yes" or "no"
illegalLoop = "yes"

################################################
################################################
### Implementation vector
# delayVec <- c(2:20)
delayVec <- c(2)

################################################################################################
### How to divide fishing mortality rate between the two legal fleets (ratio given to fleet 1)
# split <- 1

## Loop over all fisheries
# for (i in 1:1)
for (i in 1:nrow(dataInput))
{
    outputs = projectionModel(dataInput[i,],scenarios,catchShareLoop,illegalLoop)
    
    masterOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$BProjections))),
                          rep(dataInput[i,]$Species,nrow(melt(outputs$BProjections))),
                          melt(outputs$BProjections),
                          melt(outputs$HInt1Projections)$value,
                          melt(outputs$HInt2Projections)$value,
                          melt(outputs$HNonIntProjections)$value,
                          melt(outputs$profit1Projections)$value,
                          melt(outputs$profit2Projections)$value,
                          melt(outputs$bProjections)$value,
                          melt(outputs$fInt1Projections)$value,
                          melt(outputs$fInt2Projections)$value,
                          melt(outputs$fNonIntProjections)$value,
                          melt(outputs$fTotalProjections)$value)
    
    colnames(masterOutputi) = c("fishery","species","management","MC", "catchShare","illegalFishing","implementYear", "time","biomass",
                                "harvest1", "harvest2", "harvest_ill_for","profit1","profit2", "BvBMSY","FvFMSY1",
                                "FvFMSY2", "FvFMSYill", "FvFMSYtotal")
    
    if (i == 1) {
        masterOutput = masterOutputi
    } else {
        masterOutput = rbind(masterOutput,masterOutputi)
    }
    
    masterOutput$management[masterOutput$management == 1] = "SQ"
    masterOutput$management[masterOutput$management == 2] = "FMSY"
    masterOutput$management[masterOutput$management == 3] = "minRec"
    masterOutput$management[masterOutput$management == 4] = "econOpt"
    masterOutput$management[masterOutput$management == 5] = "close"
    masterOutput$management[masterOutput$management == 6] = "openA"
    masterOutput$catchShare[masterOutput$catchShare == 1] = "no_CS"
    masterOutput$catchShare[masterOutput$catchShare == 2] = "CS"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 1] = "illegal_fishing"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 2] = "no_illegal_fishing"

    recoveryOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$timeToRecovery))),
                            rep(dataInput[i,]$Species,nrow(melt(outputs$timeToRecovery))),
                            melt(outputs$timeToRecovery))
    
    colnames(recoveryOutputi) = c("fishery","species","management", "MC", "catchShare","illegalFishing","implementYear", "recTime")
    
    if (i == 1) {
      recoveryOutput = recoveryOutputi
    } else {
      recoveryOutput = rbind(recoveryOutput,recoveryOutputi)
    }
    
    recoveryOutput$management[recoveryOutput$management == 1] = "SQ"
    recoveryOutput$management[recoveryOutput$management == 2] = "FMSY"
    recoveryOutput$management[recoveryOutput$management == 3] = "minRec"
    recoveryOutput$management[recoveryOutput$management == 4] = "econOpt"
    recoveryOutput$management[recoveryOutput$management == 5] = "close"
    recoveryOutput$management[recoveryOutput$management == 6] = "openA"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 1] = "no_CS"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 2] = "CS"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 1] = "illegal_fishing"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 2] = "no_illegal_fishing"

    npvOutputi = cbind(rep(dataInput[i,]$Fishery,nrow(melt(outputs$npv1))),
                       rep(dataInput[i,]$Species,nrow(melt(outputs$npv1))),
                            melt(outputs$npv1),
                            melt(outputs$npv2)$value)
    
    colnames(npvOutputi) = c("fishery", "species","management", "MC", "catchShare","illegalFishing","implementYear", "npv1", "npv2")
    
    if (i == 1) {
      npvOutput = npvOutputi
    } else {
      npvOutput = rbind(npvOutput,npvOutputi)
    }
    
    npvOutput$management[npvOutput$management == 1] = "SQ"
    npvOutput$management[npvOutput$management == 2] = "fmsy"
    npvOutput$management[npvOutput$management == 3] = "minRec"
    npvOutput$management[npvOutput$management == 4] = "econOpt"
    npvOutput$management[npvOutput$management == 5] = "close"
    npvOutput$management[npvOutput$management == 6] = "openA"
    npvOutput$catchShare[npvOutput$catchShare == 1] = "no_CS"
    npvOutput$catchShare[npvOutput$catchShare == 2] = "CS"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 1] = "illegal_fishing"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 2] = "no_illegal_fishing"    
        
}

masterOutput$implementYear = (masterOutput$implementYear + 1)
write.csv(masterOutput,file="workshop_0613_mo_opt.csv") 


recoveryOutput$implementYear = (recoveryOutput$implementYear + 1)
write.csv(recoveryOutput,file="workshop_0613_recovery_opt.csv")

npvOutput$implementYear = (npvOutput$implementYear + 1)
write.csv(npvOutput,file="workshop_0613_npv_opt.csv") 

## opt at the end of csv names indicates results from model with policy that is optimized once at beginning
