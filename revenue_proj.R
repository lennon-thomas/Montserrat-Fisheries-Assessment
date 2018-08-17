##8/14/18


library(tidyverse)
library(ggplus)
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


detach("package:Hmisc", unload=TRUE)


missing_sp<-all_data %>%
  filter(id10 %in% mis_sp)


sp_id<-distinct(all_data,common,id10,scientific,.keep_all = FALSE)


#Save list of unique spid, common, scientific names and clean up lookup table by hand and rename "sp_lookup_complete.csv"

#write.csv(sp_id,"revenue_loss_scenarios/sp_lookup.csv")


sp_lookup<-read.csv("revenue_loss_scenarios/sp_lookup_complete.csv")
sp_lookup$scientific_name<-as.character(sp_lookup$scientific_name)

# Get family and resilience data from fishbase
fb_data<-stocks(sp_lookup$scientific_name,fields = (c("Resilience")))
fb_data$SpecCode<-as.factor(fb_data$SpecCode)
common<-common_names(fb_data$sciname,limit =200, Language = "English")
common$SpecCode<-as.factor(common$SpecCode)
common<-distinct(common,SpecCode,.keep_all = TRUE)

fb_data<-left_join(fb_data,common, by = "SpecCode")

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

final_catch<-catch_summary%>%
  group_by(year) %>%
  summarise(total_catch = sum(catch))%>%
  ungroup() %>%
  filter(year==2015)



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

final_results<-left_join(final_results,fb_data, by= (c("SciName"="sciname")))%>%
  ungroup()


## Add length based results for the 5 species we did to overide GUM results
# F ratio from DPSA_reults.csv. Bratio from each species in CatchMSY.csv in results foler
#Blue tang fratio =1.71 bratio =0.48
# Silk snapper fratio = 1.33 bratio 0.34
# Red hind = 2.41 or 0.35?. bratio= 0.44
# old wife = 0.49 bratio = 0.45
#doctorfish 1.38, bratio = 0.38

final_results$BvBmsy[final_results$SciName=="Lutjanus vivanus"]<-0.0034

#final_results$FvFmsy[final_results$SciName=="Lutjanus vivanus"]<-1.33


final_results$BvBmsy[final_results$SciName=="Epinephelus guttatus"]<-0.0044

#final_results$FvFmsy[final_results$SciName=="Epinephelus guttatus"]<-0.35


final_results$BvBmsy[final_results$SciName=="Balistes vetula"]<-0.045

#final_results$FvFmsy[final_results$SciName=="Balistes vetula"]<-0.49


final_results$BvBmsy[final_results$SciName=="Acanthurus coeruleus"]<-0.0038

#final_results$FvFmsy[final_results$SciName=="Acanthurus coeruleus"]<-1.38


final_results$BvBmsy[final_results$SciName=="Acanthurus chirurgus"]<-0.0048

#final_results$FvFmsy[final_results$SciName=="Acanthurus chirurgus"]<-1.71




write.csv(final_results,"revenue_loss_scenarios/GUM_results.csv")

projection_input<-final_results%>%
  select(SciName,BvBmsy,phi,g,k,FvFmsy,ComName) %>%
  mutate(Fishery = "MNI Fishery",
         Country = "Montserrat",
         Site = "MNI",
         Species = SciName,
         Scientific = SciName,
         Time = 50,
         N =1,
         phi_lower = round(phi,3),
         phi_expected = round(phi,3),
         phi_upper = round(phi,3),
         g_lower = g,
         g_expected = g,
         g_upper = g,
         K_lower = k,
         K_expected = k,
         K_upper = k,
         b0_lower = BvBmsy,
         b0_expected = BvBmsy,
         b0_upper = BvBmsy,
         f0_total_low = FvFmsy,
         f0_total_expected = FvFmsy,
         f0_total_high = FvFmsy,
         theta_domestic = 1,
         theta_legal1_lower =1,
         theta_legal1_expected = 1,
         theta_legal1_upper = 1,
         theta_legal2_lower =0,
         theta_legal2_expected = 0,
         theta_legal2_upper = 0,
      #   p1_lower = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
      #   p1_expected = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
      #   p1_upper = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
      #   p2_lower = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
      #   p2_expected = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
      #   p2_upper = ifelse(SciName=="Belone belone",8*2.2,10*2.2),
         p1_lower = 10*2.2,
         p1_expected = 10*2.2,
         p1_upper = 10*2.2,
         p2_lower = 10*2.2,
         p2_expected = 10*2.2,
         p2_upper = 10*2.2,
         beta_lower = 1,
         beta_expected = 1,
         beta_upper = 1,
         c1_lower = 0,
         c1_expected = 0,
         c1_upper =0,
         c2_lower = 0,
         c2_expected = 0,
         c2_upper =0,
         disc_lower = 0,
         disc_expected = 0,
         disc_upper = 0,
         gamma_p1 = 1,
         gamma_p2 = 2,
         gamma_c1 =1,
         gamma_c2=1,
         lambda = 0.03,
         split = 1) %>%
  select(-c(SciName,BvBmsy,phi,g,k,FvFmsy,ComName))%>%
  filter(!is.na(phi_expected))

write.csv(projection_input,"revenue_loss_scenarios/projection_input.csv")

#  Now run 'run_projections' files with projection_input data


# Read in projection results

masterOutput<-read.csv("revenue_loss_scenarios/masterOutput.csv") %>%
  group_by(species) %>%
  mutate(relative_revenue = profit1/profit1[time == 1 & management == "BAU"]) %>%
  mutate(relative_biomass = biomass/biomass[time == 1 & management =="30% MPA"])  %>%
  ungroup() %>%
  filter(!is.na(relative_revenue))

# check to see how much of catch is incoporated into analysis

t_catch<-masterOutput %>%
  filter(management=="BAU" & time == 1)
 
 total_catch<-sum(t_catch$harvest1)
 
perc_catch_included<-total_catch/final_catch$total_catch*100
 # Plot results by species
 
library(ggplus)


t<- ggplot(masterOutput,aes(x = time, y = relative_revenue, color = management)) +
      geom_line (lwd = 1.1) +
   #   geom_line(aes(x=time,y=relative_revenue,color = management),linetype="dotted") +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme_bw() +
      xlab("Time after 30 % MPA implementation") +
      ylab("Relative Fishing Revenue") +
      scale_fill_brewer(palette = "Set1")
     # facet_wrap(~species, scales = "free")
#t <- t + scale_y_continuous(sec.axis = sec_axis(trans = ~.,name = "Relative Stock Biomass"))
facet_multiple(t,facets = "species", ncol = 1, nrow = 2)

ggsave("revenue_loss_scenarios/revenue_proj_sp.pdf")


#For the whole fishery

total_fishery<-masterOutput %>%
  group_by(time,management) %>%
  summarise(annual_revenue = sum(profit1),
            annual_biomass = sum(biomass)) %>%
  ungroup() %>%
  mutate(relative_revenue = annual_revenue/annual_revenue[time==1 & management =="BAU"],
         relative_biomass = annual_biomass/annual_biomass[time==1 & management == "BAU"]) 


ggplot(total_fishery,aes(x = time, y = relative_revenue, color = management)) +
  geom_line (lwd = 1.1) +
  #   geom_line(aes(x=time,y=relative_revenue,color = management),linetype="dotted") +
 # scale_x_continuous(expand = c(0,0), lim = c(0,35)) +
  scale_y_continuous(expand = c(0,0), lim = c(0,1.5)) +
  theme_bw() +
  xlab("Time after 30 % MPA implementation") +
  ylab("Relaive Fishing Revenue") +
  scale_color_discrete("Management Scenario")

ggplot(total_fishery,aes(x = time, y = relative_biomass, color = management)) +
  geom_line (lwd = 1.1) +
  #   geom_line(aes(x=time,y=relative_revenue,color = management),linetype="dotted") +
 # scale_x_continuous(expand = c(0,0)),lim = c(0,35)) +
  scale_y_continuous(expand = c(0,0), lim = c(0,1.5)) +
  theme_bw() +
  xlab("Time after 30 % MPA implementation") +
  ylab("Relaive Fishing Biomass") +
  scale_color_discrete("Management Scenario")