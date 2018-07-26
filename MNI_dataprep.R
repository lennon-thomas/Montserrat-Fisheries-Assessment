# MNI Data prep for Dan

#7/23/18
http://r4ds.had.co.nz/lists.html
#1. Length composition data frame: columns: time, scientific name, length_bin, numbers
#2. effort data frame: columns: time, effort
#3. list with life history objects names scientific_name loo, k, t0, m, weight_a, weight_b, length/age at maturity
#one list per species if there are multiple

rm(list = ls())

library(tidyverse)
library(readr)




# Length Data -------------------------------------------------------------

all_length<-read_csv("Length_Assessment/all_length.csv") 

# Length frequencies by species and data type (fishery-dependent vs. survey)

bins<-seq(0,65,by=5)

# Identify species to filter out (bad LH data or sample size)

bad_sp<-c("LUTJMA","LUTJSY")

length_freq<-all_length %>%
  select(Species.ID,Data.Type,Length) %>%
  filter(!Species.ID %in% bad_sp) %>%
  mutate(Length = cut(Length, breaks=bins,include.lowest = TRUE)) %>%
  table() %>% data.frame() %>% 
  arrange(Species.ID, Data.Type, Length) %>%
  mutate(Year = 2016)
  
# Create list of dataframes by species
sp_length<- length_freq %>%
  split(.$Species.ID)

# Name each object in list as: 'Species.ID_length_data'

names(sp_length) <- paste0(names(sp_length),"_length_data" )


test<-mget(ls(pattern = "BALIVE"))
out <- lapply( test , function(x) x@data )
# Unlist objects

list2env(sp_length,envir=.GlobalEnv)


# Life history ------------------------------------------------------------

# Read in life history data and calculate age at maturity using M50 and vbk and split each species into separate object

life_history<-read_csv("Length_Assessment/MNI_LH_spread.csv")%>%
  select(sp_id,SciName,Linf,vbk,M,WeightA,WeightB,Mat50,t0) %>%
  group_by(sp_id) %>%
  mutate(Mat_Age = (log(1-(Mat50)/Linf)/-vbk)+t0) %>%
  gather(Parameter,Value,-c(sp_id,SciName)) %>%
  split(.$sp_id)

# Name each object in list as: 'sp_id_length_data'
names(life_history) <- paste0(names(sp_length),"_LH_data" )
