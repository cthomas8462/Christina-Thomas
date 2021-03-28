setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")

library(rlang)
library(plyr)
library(dplyr)
library(tidyverse)

vegqc<-read.delim("vegQC_03262021.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE)

strictvegqc<-read.delim("vegQCparticipantIDs_03262021.txt", header=TRUE, sep="\t",stringsAsFactors =FALSE)


colnames(vegqc)<- c("FID", "Self_Reported_Vegetarians")

vegqc


vegqc$Self_Reported_Vegetarians[vegqc$Self_Reported_Vegetarians == "Non-vegetarian" ]<-0
vegqc$Self_Reported_Vegetarians[vegqc$Self_Reported_Vegetarians ==  "Vegetarian" ]<-1


vegqc<-vegqc%>%mutate_if(is.character, as.numeric)

vegqc



sum(vegqc$Self_Reported_Vegetarians) #3321



strictvegqc%>%filter(Veg == 1) #713 strictveg


colnames(strictvegqc)<- c("FID", "Strict.Vegetarians")

strictvegqc

strictvegqc<-strictvegqc%>%mutate_if(is.character, as.numeric) 



vegtable<-(combine = merge(vegqc, strictvegqc, by="FID", 
                           all.x = TRUE, all.y = TRUE))

#vegtable<-inner_join(strictvegqc, vegqc, by=c("FID"))

#vegtable<- merge(strictvegqc, vegqc, by = "FID")

vegtable

sum(is.na(vegtable$Strict.Vegetarians))


#na.omit(vegtable$Strict.Vegetarians)
#vegtable<-(!is.na(vegtable$Strict.Vegetarians))

vegtable

sum(vegtable$Strict.Vegetarians, na.rm = TRUE)
sum(vegtable$Self_Reported_Vegetarians)


write.table(vegtable, file = "strictandselfreported.txt", 
            sep = "\t", col.names = TRUE, quote = FALSE,
            row.names = FALSE)

#no longer need this since veg and strict veg combined in vegqc script
