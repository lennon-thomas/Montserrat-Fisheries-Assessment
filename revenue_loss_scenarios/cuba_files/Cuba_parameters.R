### Tracey Mangin
### May 3, 2017
### Run GUM package with catch history for 26 species


## install GUM
devtools::install_github('DanOvando/GUM', build_vignettes = T)

## Install Libraries
# library(dplyr)
library(tidyverse)
library(GUM)
library(stringr)
library(stats)

## Set working directory


# setwd("~/Dropbox/Tracey's EcoA Work/Cuba/2017 Documents/Cuba_model_update/Catch Data")
setwd("~/Desktop/R Workshop")

## Read data
cuba_catch <- read.csv('CapturasZonaA.csv',stringsAsFactors=FALSE)
lumped <- read.csv("~/Dropbox/Tracey's EcoA Work/Upsides/upside-share/ProjectionData.csv", header = T, stringsAsFactors = F)
asfis <- read.csv("~/Dropbox/Tracey's EcoA Work/Cuba/ASFIS_sp/ASFIS 6 languages_2016.csv", header = T, stringsAsFactors = F)

## To view data, run the following code
View(cuba_catch)

## Format data so it can be run in GUM package
cuba_catch <- cuba_catch %>%
  rename(SciName = Scientific.Name,
         CommName = Common.name..En.,
         CommNameSp = Common.name..Sp.,
         IdOrig = ID)

cuba_catch1 <- cuba_catch %>%
  gather(Year, Catch, X1981:X2015) %>%
  mutate(Year = as.integer(substring(Year, 2))) 

cuba_catch1 <- cuba_catch1[order(cuba_catch1$IdOrig, cuba_catch1$Year), ]

cuba_catch2 <- cuba_catch1 %>%
  mutate(IdOrig = paste("Cuba",IdOrig, sep = "-")) %>%
  mutate(SciName = ifelse(SciName == "Gerres cinereus + Diapterus rhombeus", "Gerres cinereus", 
                          ifelse(SciName == "Ophistonema oglynum", "Opisthonema oglinum",
                                 ifelse(SciName == "Selar crumehopthalmus", "Selar crumenophthalmus",  
                                        ifelse(SciName == "Magalops atlanticus", "Megalops atlanticus", SciName)))))

## Add SpeciesCat, SpeciesCatName, and make sure SciName and CommName are correct.
asfis2 <- asfis %>%
  select(ISSCAAP, Scientific_name, English_name) %>%
  rename(SciName = Scientific_name)

cuba_catch3 <- left_join(cuba_catch2, asfis2)

cuba_catch4 <- cuba_catch3 %>%
  rename(SpeciesCat = ISSCAAP,
         englishName = CommName,
         CommName = English_name)

### Add SpeciesCatName
unique(cuba_catch4$SpeciesCat) ## 35, 33, 37, 36

cuba_catch5 <- cuba_catch4 %>%
  mutate(SpeciesCatName = ifelse(SpeciesCat == 33, "Miscellaneous coastal fishes", 
                                 ifelse(SpeciesCat == 35, "Herrings, sardines, anchovies",
                                        ifelse(SpeciesCat == 36, "Tunas, bonitos, billfishes", "Miscellaneous pelagic fishes"))))

## make final df
cuba_catch6 <- cuba_catch5 %>%
  mutate(X = seq.int(nrow(cuba_catch5))) %>%
  select(X, IdOrig, SciName, CommName, SpeciesCat, SpeciesCatName, Year, Catch)

### Run GUM assessment
cuba_results <- run_gum_assessment(cuba_catch6)

## write csv
write.csv(cuba_results, "cuba_results_0503.csv", row.names = FALSE)


######################################################
##### Finish filling in the input sheet ##############
######################################################
cuba_inputs <- read.csv('cuba_inputs_0503.csv',stringsAsFactors=FALSE)

cuba_inputs <- cuba_inputs %>%
  mutate(Fishery = paste(Species, Site, sep = " "),
         Time = 25,
         N = 1,
         phi_lower = 0.188,
         phi_expected = 0.188,
         phi_upper = 0.188,
         theta_domestic = 1,
         theta_legal1_lower = 1,
         theta_legal1_expected = 1,
         theta_legal1_upper = 1,
         theta_legal2_lower = 0,
         theta_legal2_expected = 0,
         theta_legal2_upper = 0,
         disc_lower = 0,
         disc_expected = 0,
         disc_upper = 0,
         beta_lower = 1.3,
         beta_expected = 1.3,
         beta_upper = 1.3,
         gamma_p1 = 1,
         gamma_p2 = 1,
         gamma_c1 = 1,
         gamma_c2 = 1,
         lambda = 0.03,
         split = 1)

### add inputs from GUM results
cuba_results_2015 <- cuba_results %>%
  filter(year == 2015) %>%      # 2015 is the most recent year
  select(IdOrig, SciName, year, BvBmsy, MSY, phi, g, k, FvFmsy) %>%
  rename(Scientific = SciName)

cuba_results_2015 <- as.data.frame(cuba_results_2015)

cuba_inputs1 <- left_join(cuba_inputs, cuba_results_2015, by = c("Scientific"))

cuba_inputs2 <- cuba_inputs1 %>%
  mutate(g_lower = g,
         g_expected = g,
         g_upper = g,
         K_lower = k,
         K_expected = k,
         K_upper = k,
         b0_lower = BvBmsy,
         b0_expected = BvBmsy,
         b0_upper = BvBmsy,
         f0_total_lower = FvFmsy,
         f0_total_expected = FvFmsy,
         f0_total_upper = FvFmsy) %>%
  select(Fishery:split, MSY)

### add prices

scinames <- as.vector(cuba_inputs2$Scientific)

pdata <- lumped %>%
  filter(Year == 2012,
         SciName %in% scinames) %>%
  group_by(SciName) %>%
  summarise(mean_price = mean(Price)) %>%
  rename(Scientific = SciName)

pdata <- as.data.frame(pdata)

cuba_inputs3 <- cuba_inputs2 %>%
  mutate(p1_lower = pdata$mean_price[match(Scientific,pdata$Scientific)],
         p1_expected = pdata$mean_price[match(Scientific,pdata$Scientific)],
         p1_upper = pdata$mean_price[match(Scientific,pdata$Scientific)],
         p2_lower = pdata$mean_price[match(Scientific,pdata$Scientific)],
         p2_expected = pdata$mean_price[match(Scientific,pdata$Scientific)],
         p2_upper = pdata$mean_price[match(Scientific,pdata$Scientific)])

#########
# Harengula humeralis : speccat 35
# Harengula clupeola : 35
# Gerres cinereus : 33
# Archosargus rhomboidalis : 33
# Haemulon sciurus : 33
# Caranx latus : 37
# Trachinotus goodei : 37
# Lutjanus apodus : 33
# Calamus bajonado : 33

pdata_spc <- lumped %>%
  filter(Year == 2012,
         Country == "Cuba") %>%
  group_by(SpeciesCat) %>%
  summarise(mean_price = mean(Price))

sci33 <- c("Gerres cinereus", "Archosargus rhomboidalis", "Haemulon sciurus", "Lutjanus apodus", "Calamus bajonado")
sci35 <- c("Harengula humeralis", "Harengula clupeola")
sci37 <- c("Caranx latus", "Trachinotus goodei")

## 33: 2934.518
## 35: 349.062

cuba_inputs4 <- cuba_inputs3 %>%
  mutate(p1_lower = ifelse(Scientific %in% sci33, 2934.518,
                           ifelse(Scientific %in% sci35, 349.062, p1_lower)),
         p1_expected = ifelse(Scientific %in% sci33, 2934.518,
                               ifelse(Scientific %in% sci35, 349.062, p1_expected)),
         p1_upper = ifelse(Scientific %in% sci33, 2934.518,
                            ifelse(Scientific %in% sci35, 349.062, p1_upper)),
         p2_lower = ifelse(Scientific %in% sci33, 2934.518,
                            ifelse(Scientific %in% sci35, 349.062, p2_lower)),
         p2_expected = ifelse(Scientific %in% sci33, 2934.518,
                               ifelse(Scientific %in% sci35, 349.062, p2_expected)),
         p2_upper =  ifelse(Scientific %in% sci33, 2934.518,
                            ifelse(Scientific %in% sci35, 349.062, p2_upper)))
  
### species cat 37 is fao region 31

pdata37 <- lumped %>%
  filter(Year == 2012,
         RegionFAO == "31",
         SpeciesCat == 37) %>%
  group_by(SpeciesCat) %>%
  summarise(mean_price = mean(Price))

## mean price for species cat 37: 1563.375

cuba_inputs5 <- cuba_inputs4 %>%
  mutate(p1_lower = ifelse(Scientific %in% sci37, 1563.375, p1_lower),
         p1_expected = ifelse(Scientific %in% sci37, 1563.375, p1_expected),
         p1_upper = ifelse(Scientific %in% sci37, 1563.375, p1_upper),
         p2_lower = ifelse(Scientific %in% sci37, 1563.375, p2_lower),
         p2_expected = ifelse(Scientific %in% sci37, 1563.375, p2_expected),
         p2_upper = ifelse(Scientific %in% sci37, 1563.375, p2_upper))

###########
### calculate cost

## Functions
## Cost of fishing parameter calculation
fishMortSS <- function(phi, bbar) {
  fbar <- (phi + 1) / phi * (1 - (bbar ^ phi) / (phi + 1))
  return(fbar) ## storing output of function
}

cost <- function(p_exp, phi, bbar, MSY, g_exp, beta) {
  fbar <- fishMortSS(phi, bbar)
  c_exp <-p_exp * fbar * bbar * MSY / ((g_exp * fbar) ^ beta)
  return(c_exp)
}

bbar <- 0.3 # based on expert opinion

### Add to data frame
cuba_inputs6 <- cuba_inputs5 %>%
  mutate(c1_lower = cost(p_exp = p1_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
         c1_expected = cost(p_exp = p1_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
         c1_upper = cost(p_exp = p1_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper),
         c2_lower = cost(p_exp = p2_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
         c2_expected = cost(p_exp = p2_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
         c2_upper = cost(p_exp = p2_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper)) %>%
  select(Fishery:split)

write.csv(cuba_inputs6, "cuba_inputs_0504.csv", row.names = FALSE)

