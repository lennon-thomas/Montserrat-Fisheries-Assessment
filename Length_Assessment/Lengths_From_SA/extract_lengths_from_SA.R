#extract lengths from SA data
library(readr)
library(dplyr)

rm(list = ls())


if (basename(getwd())!='Montserrat-Fisheries-Assessment'){
  print("Set working directory to home folder: Montserrat-Fisheries-Assessment")
}

#load length data to filter length matching target spps

length_data <- read.csv("./Length_Assessment/all_length.csv")

species <- table(length_data$Species.ID)

#load look up table to match spp codes
lookup <- read_csv('./Length_Assessment/Lengths_From_SA/lookup_spp_code.csv')

#load and trim down Scientific Assessment (SA) data
SA_data <- read.csv('./Length_Assessment/Lengths_From_SA/Montserrat_fish_data_FINAL_SA.csv', strip.white=TRUE, stringsAsFactors=FALSE) %>%
  mutate(Species.ID = plyr::revalue(
    SPECIES_CODE,
    setNames(lookup$Species.ID, lookup$SPECIES_CODE)
  )) %>%
  filter(Species.ID %in% names(species[species > 30])) %>%
  select(DATATYPE, COMMON_NAME, DIVER, NUMBER, MIN, MAX, AVG, Species.ID) %>%
  filter(AVG>1.5)

#select only complete observations (inclds number, min, max)  
SA_data = SA_data[complete.cases(SA_data[, 3:5]),]

#convert class of numeric observations
SA_data[,3:6] = apply(SA_data[,3:6], 2, as.numeric)

#create unique transcet ID
SA_data$trans_ID = seq(from = 1, to = length(SA_data$DATATYPE), by = 1)


#generate random individual length data (truncated normal distribution)

SA_lengths_temp = plyr::ddply(SA_data[SA_data$NUMBER>=1,], 'trans_ID', function(x){
  if (x$NUMBER == 1 ){ Length = x$MAX

  } else if (x$NUMBER == 2 ){ Length = c(x$MIN, x$MAX)

  }  else {Length = truncnorm::rtruncnorm(n=x$NUMBER, a=x$MIN, b=x$MAX, mean=x$AVG)
  }

   data.frame(Length)
})
                         
#join idividual lengths to orginal data                      
SA_lengths <- full_join(SA_lengths_temp, SA_data, by= 'trans_ID') 

# the truncnorm function cannot produce numbers where max==min fill in NAs with MAX
SA_lengths$Length[is.na(SA_lengths$Length)] <- SA_lengths$MAX[is.na(SA_lengths$Length)]             
              
# write file
#write.csv(SA_lengths, file = './Length_Assessment/Lengths_From_SA/SA_individ_lengths.csv')


