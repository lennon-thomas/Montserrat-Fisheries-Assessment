SelectForLengthSampleSize<- function(Data)
{
  Data<- LengthDataAll
  Data<- subset(Data,is.na(Length)==F)
  
  SampleSize<- ddply(Data[Data$FisheryDependent==1,],c('Species.ID'),summarize,SampleSize=length(Length))
  
  SufficientSamples<- SampleSize[SampleSize$SampleSize>=MinSampleSize,]
    
    Data<- Data[Data$Species.ID %in% SufficientSamples$Species.ID,]
    
    return(Data)

  
}