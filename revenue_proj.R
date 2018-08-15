##8/14/18



library(rfishbase)
library(devtools)
devtools::install_github('DanOvando/GUM',build_vignettes= F)
library(GUM)
#library(Hmisc)

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


#Save list of unique spid, common, scientific names and clean up lookup table by hand and rename "sp_lookup_complete.csv"

#write.csv(sp_id,"revenue_loss_scenarios/sp_lookup.csv")


sp_lookup<-read.csv("revenue_loss_scenarios/sp_lookup_complete.csv")

# Get family and resilience data from fishbase
fb_data<-stocks(sp_lookup$scientific_name,fields = (c("Resilience")))

sp_lookup<-left_join(sp_lookup,fb_data,by = c("scientific_name" = "sciname"))

all_data_clean<-left_join(all_data,sp_lookup,by="id10")

## There are more trip numbers than tripid's, need to look into this later.


#write.csv(all_data_clean,'MNI_catch_clean_18.csv')

all_data_clean<-read.csv('MNI_catch_clean_18.csv')

all_data_clean<-all_data_clean %>%
  select(date,year,month,vesselid,fishername,fishery,tripid,gearid,weight_kgs,price,trip_no,Family,common_name,scientific_name,id10,new_id)

family_id<-distinct(all_data_clean,scientific_name,Family,id10,.keep_all=FALSE)
write.csv(family_id,"family_id.csv")

catch_summary<-all_data_clean %>%
  group_by(year,scientific_name,new_id)%>%
  summarise(catch=sum(weight_kgs,na.rm = TRUE))%>%
  arrange(scientific_name,year)%>% 
  select(year,catch,scientific_name,new_id) %>%
  mutate(SpeciesCatName="Miscellaneous coastal fishes")

catch_summary$scientific_name<-as.factor(catch_summary$scientific_name)

catch_data_analysis<- catch_summary %>% 
                        group_by(scientific_name) %>% 
                        filter(catch!=0)%>%
                        filter(n() >= 10) %>%
                        filter(!is.na(scientific_name))%>%
  ungroup()

colnames(catch_data_analysis)<-c("Year","Catch","SciName","IdOrig","SpeciesCatName")
catch_data_analysis$SciName<-as.character(catch_data_analysis$SciName)
catch_data_analysis$Catch<-round(catch_data_analysis$Catch,0)
catch_data_analysis<-as.data.frame(catch_data_analysis)
#catch_summary<-left_join(catch_summary,family_id)


#Step 2 is estimate species current status using GUM packagelib


results<-run_gum_assessment(catch_data_analysis)

final_results<-results %>%
  filter(year==2015)

stocks = unique(dat$IdOrig)

test<-stocks(species_list = NULL, fields = NULL, query = NULL, limit = 200)
