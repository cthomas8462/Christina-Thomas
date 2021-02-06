setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")

library(rlang)
library(plyr)
library(dplyr)
library(tidyverse)

vegqc<-read.delim("vegQC.txt", header=TRUE, sep="\t")

strictvegqc<-read.delim("strictvegQC.txt", header=TRUE, sep="\t",stringsAsFactors =FALSE)


colnames(vegqc)<- c("FID", "Self_Reported_Vegetarians")

vegqc

vegqc<-vegqc%>%mutate_if(is.character, as.numeric) 



sum(vegqc$Self_Reported_Vegetarians)


strictvegqc$Veg[strictvegqc$Veg == "Non-vegetarian" ]<-0
strictvegqc$Veg[strictvegqc$Veg ==  "Vegetarian" ]<-1

strictvegqc%>%filter(Veg == 1) #713 strictveg


colnames(strictvegqc)<- c("FID", "Strict.Vegetarians")

strictvegqc

strictvegqc<-strictvegqc%>%mutate_if(is.character, as.numeric) 



vegtable<-(combine = merge(vegqc, strictvegqc, by="FID", 
                           all.x = TRUE, all.y = TRUE))

#vegtable<-inner_join(strictvegqc, vegqc, by=c("FID"))

#vegtable<- merge(strictvegqc, vegqc, by = "FID")

vegtable


#na.omit(vegtable$Strict.Vegetarians)
#vegtable<-(!is.na(vegtable$Strict.Vegetarians))



sum(vegtable$Strict.Vegetarians, na.rm = TRUE)
sum(vegtable$Self_Reported_Vegetarians)


write.table(vegtable, file = "strictandselfreported.txt", 
            sep = "\t", col.names = TRUE, quote = FALSE,
            row.names = FALSE)