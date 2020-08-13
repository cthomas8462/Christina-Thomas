setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")

#This script starts with the input of christina-06-24-2020.R 
#(vegQC.txt) and does another round of QC where specific answers
# from both initial food survey and 24HR are checked to make sure
# no meat was reported as eaten recently.
# 
# Removing all phenotype from this dataset as it is not related to 
# the main purpose of this script. These can be added later once the 
# lists are obtained

library(rlang)
library(plyr)
library(dplyr)
library(tidyverse)

vegqc<-read.delim("vegQC.txt", header=TRUE, sep="\t")

table(vegqc$Veg) #starting= 3321/ consistent self-reported vegetarians
nrow(vegqc) #135411

bd<-read.table("ukb37330.tab", header=TRUE, sep="\t")

ukbqc <- read.delim("UKB34137_QC_and_24HR.txt", header = TRUE, sep="\t")
ukbqc <- as_tibble(ukbqc)

#selecting data to tibble
new<-ukbqc%>%select(f.eid, 
                    #Category 100052: Diet. Initial assessment
                    f.1329.0.0, f.1339.0.0, 
                    f.1349.0.0, f.1359.0.0, 
                    f.1369.0.0, f.1379.0.0, 
                    f.1389.0.0, 
                    
                    #Category 100106: Meat/fish yesterday. 24HR.
                    f.103000.0.0, f.103140.0.0
                    )

#column names for tibble
colnames(new)<- c("FID", 
                #Category 100052: Diet. Initial assessment
                "Oily_fish_intake", "Non-oily_fish_intake",
                "Processed_meat_intake","Poultry_intake",
                "Beef_intake","Lamb.mutton_intake",
                "Pork_intake",
                  
                #Category 100106: Meat/fish yesterday. 24HR.
                "Meat_consumers","Fish_consumer"
                 )


#Category 100052: keep only those who answered "No"
#Category 100106: keep only those who answered "No"

#Combine diet data with result from previous QC step finding Carn/Veg
vegqc
Meal<-vegqc%>%select("FID", "Veg")
Meal

inner<- merge(new, Meal, by = "FID")
inner<-as_tibble(inner)
inner[,2:11]<-sapply(inner[,2:11], as.character)
inner%>%filter(Veg=="Vegetarian") #3,321 

#QC for diet data
strictvegetarians<-filter(inner, (Veg  == "Vegetarian" & Meat_consumers == "No"))
strictvegetarians #1046

strictvegetarians2<-filter(strictvegetarians, (
                        (Veg  == "Vegetarian") & 
                        (Oily_fish_intake =="Never") &
                        (`Non-oily_fish_intake` =="Never") &
                        (Processed_meat_intake =="Never") &
                        (Poultry_intake =="Never") &
                        (Beef_intake=="Never") &
                        (Lamb.mutton_intake =="Never") &
                        (Pork_intake =="Never") 
                        ))

strictvegetarians2 #713

strictvegqc<-vegqc[vegqc$Veg=="Non-vegetarian" | vegqc$FID %in% strictvegetarians2$FID,]
strictvegqc<-strictvegqc[,c(1, 8)] #132803 participants
strictvegqc<-as_tibble(strictvegqc)
table(strictvegqc$Veg)

