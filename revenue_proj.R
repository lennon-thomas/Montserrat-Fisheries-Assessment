##8/14/18

library(rfishbase)
library(devtools)
devtools::install_github('DanOvando/GUM',build_vignettes= F)
library(GUM)
library(Hmisc)

## Trying to get estimated revenue loss and gain for 30% no take scenario in MNI and BAU for 5, 10,15,20 and 30 years. Using Tracey's Cuba project work flow
# Step1 is to organize time series of catch by species from MNI catch data

#manually fixed a few id10 codes in 'MNI catch_cleangear.csv' file that had species codes but missing id10.
all_data<-read.csv("MNI catch_clean.csv")

mis_sp<-c("","--","N/A","na",NA, "NO CATCH","NO DATA", "NO DATA COLLECTED")

all_data$id10[all_data$id10 %in% mis_sp]<-NA

all_data$id10<-toupper(all_data$id10)

all_data$scientific<-tolower(all_data$scientific)

all_data$scientific<-capitalize(all_data$scientific)

all_data$common<-tolower(all_data$common)

missing_sp<-all_data %>%
  filter(id10 %in% mis_sp)


sp_id<-distinct(all_data,common,id10,scientific,.keep_all = FALSE)
#Save list of unique spid, common, scientific names and clean up lookup table by hand

write.csv(sp_id,"revenue_loss_scenarios/sp_lookup.csv")


sum_data<-all_data %>%
  group_by(year,id10) %>%
  


#Step 2 is estimate species current status using GUM packagelib


dat<-read.csv("revenue_loss_scenarios/BELO_catch.csv")
results<-run_gum_assessment(dat)

stocks = unique(dat$IdOrig)

test<-stocks(species_list = NULL, fields = NULL, query = NULL, limit = 200)
