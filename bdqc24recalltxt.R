### MF: This is a script for generating lists of UK Biobank 
### participants which meet QC/filtering criteria.


setwd("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno")
source('ukb34137_loaddata.r') #generates bd (UKB dataset)

library(plyr)
library(dplyr)
library(tidyverse)

##===============================PART A=================================================
#Generate a list of participants who pass the following QC criteria:
#1.Genetic ethnicity = Caucasian
#2. Used in PCA analysis
#3. Not an outlier for heterogeneity and missing genotype rate (poor quality genotype)
#4. No Sex chromosome aneuploidy
#5. Do not have high degree of genetic kinship (Ten or more third-degree relatives identified)
#6. Self-reported sex matches genetic sex
#7. Does not appear in "maximum_set_of_unrelated_individuals.MF.pl"
#Adding self-reported white British/Irish/White was not necessary after
#these steps as all remaining participants are self-reported British 

bd_QC<- bd %>% select(f.eid, f.31.0.0, f.22001.0.0, f.21000.0.0, 
                      f.22020.0.0,
                      f.22027.0.0, f.22019.0.0, 
                      f.22021.0.0, f.22006.0.0)

colnames(bd_QC)<-c("FID", "Sex", "Genetic_Sex", "Race", 
                   "Used_in_PCA",
                   "Outliers_for_het_or_missing", "SexchrAneuploidy",
                   "Genetic_kinship", "Genetic_ethnic_grouping")
bd_QC<-as_tibble(bd_QC)

nrow(bd_QC) #Starting sample size = 502527

bd_QC<-bd_QC%>%filter(Genetic_ethnic_grouping == "Caucasian")
nrow(bd_QC) #409628. Number removed = 92899

bd_QC<-bd_QC%>%filter(Used_in_PCA == "Yes")
nrow(bd_QC) #337483. Number removed = 72145

bd_QC<-bd_QC%>%
    filter(is.na(Outliers_for_het_or_missing) | Outliers_for_het_or_missing !="Yes") 
nrow(bd_QC) #337483. Number removed = 0

bd_QC<-bd_QC%>%
    filter(is.na(SexchrAneuploidy) | SexchrAneuploidy != "Yes")
nrow(bd_QC) #337146. Number removed = 337

bd_QC<- bd_QC%>%
    filter(is.na(Genetic_kinship) | 
               Genetic_kinship != "Ten or more third-degree relatives identified")
nrow(bd_QC) #337146. Number removed = 0

#If Sex does not equal genetic sex, exclude participant
bd_QC<-bd_QC[bd_QC$Sex == bd_QC$Genetic_Sex,] 

nrow(bd_QC) #337146. Number removed = 0


#From maximum_set_of_unrelated_individuals.MF.pl output
max_unrelated<-read.table("ukb48818_rel_s488282_output.dat")
max_unrelated<-as.integer(unlist(max_unrelated))
bd_QC<-bd_QC%>%filter(!FID %in% max_unrelated)
nrow(bd_QC) #310999. Number removed = 26147


table(bd_QC$Race) #310,999 British ppts after these QC steps
QCkeepparticipants<-bd_QC%>%select(FID)

write.table(QCkeepparticipants, file= "bd_QC-keep_01292020.txt", 
            row.names = FALSE, quote = FALSE)

###==============PART B: Find participants who took 24HR  ===========================
daycols<-c("f.20080.0.0", "f.20080.1.0", "f.20080.2.0",
           "f.20080.3.0", "f.20080.4.0")
bd1<-bd
#Change all the values to characters for easier manipulation
bd1<-apply(bd1[,daycols], 2, as.character)
#Change NA's to zeros and days to 1's
bd1[(is.na(bd1[,daycols]))]<-0
bd1[bd1[,daycols]!="0"] <-1
#Change these back to numeric
bd1<-apply(bd1[,daycols], 2, as.integer)
#Now make a new column, everyone with rowSums zero write FALSE
#and those with >0 write TRUE
sum<-apply(bd1[,daycols], 1, sum)
bd1<-as.data.frame(bd1)
bd1<-bd1 %>% mutate (took_24HR = sum)
bd1<-bd1%>% mutate(took_24HR = replace(took_24HR, took_24HR>0, "Yes")) %>%
    mutate(took_24HR = replace(took_24HR, took_24HR==0, "No"))
sum(bd1$took_24HR=="Yes") #[1] 211018 **SUCCESS**
bd1$FID<-bd$f.eid
bd1<-bd1%>%select(FID, took_24HR)
took24participants<-bd1%>%filter(took_24HR=="Yes")%>%select(FID)
write.table(took24participants, file= "bd_took-24HR-211018-participants.txt", row.names = FALSE, 
            quote = FALSE)


firstlist<-read.table("bd_QC-keep_01292020.txt", stringsAsFactors = FALSE, header = TRUE)
secondlist<-read.table("bd_took-24HR-211018-participants.txt", stringsAsFactors = FALSE, header = TRUE)

#colnames(firstlist)<-c("FID")
firstlist<-as_tibble(firstlist)
firstlist

#colnames(secondlist)<-c("FID")
secondlist<-as_tibble(secondlist)
secondlist

bd[bd$f.eid %in% firstlist$FID & bd$f.eid %in% secondlist$FID,]
bd<-as_tibble(bd)
bd

write.table(bd, file= "bd_QC_24recallCT.txt", row.names = FALSE, 
            quote = FALSE)