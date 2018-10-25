##Function for extracting rough spectra table that includes masses fragment labels and intensity from msms evidence file


Fact2Num <- function(mydataframe){
  
  mydataframe[,1] <- as.character(mydataframe[,1])
  mydataframe[,1] <- as.numeric(mydataframe[,1])
  
  return(mydataframe)
}



SpectraTable <- function(myMSMSTable, rawFileName, scanNumber){
  
  mySpectraLists <-  myMSMSTable %>% filter(Raw.file == rawFileName, Scan.number == scanNumber) %>% 
    select(Matches, Masses, Intensities)
  
  Masses_df <- as.data.frame(strsplit(mySpectraLists$Masses, ";"), col.names = "Masses", stringsAsFactors = F)
  Intensities_df <- as.data.frame(strsplit(mySpectraLists$Intensities, ";"), col.names = "Intensities", stringsAsFactors = F)
  Matches_df <- as.data.frame(strsplit(mySpectraLists$Matches, ";"), col.names = "Fragments", stringsAsFactors = F)
  
  Masses_df <- Fact2Num(Masses_df)
  Intensities_df <- Fact2Num(Intensities_df)
  
  mySpectraTable <- cbind(Matches_df, Masses_df, Intensities_df)
  
  MySpectraTable <- tbl_df(mySpectraTable) %>% arrange(desc(Intensities)) %>% mutate(Mass_2 = Masses/2, Mass_3 = Masses/3)
  
  
  
}
