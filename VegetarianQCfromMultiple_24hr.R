setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")

library(rlang)
library(plyr)
library(dplyr)
library(tidyverse)

ukbveg <- read.delim("UKB34137_QC_and_24HR.txt", header = TRUE, sep="\t")
ukbveg <- as_tibble(ukbveg)

#24hr special diet columns
VegAnswers<-ukbveg%>%select(f.eid, f.20086.0.0, f.20086.1.0,
                            f.20086.2.0, f.20086.3.0, f.20086.4.0)

#column names for tibble
colnames(VegAnswers)<- c("FID", "Veg0","Veg1", "Veg2", "Veg3", "Veg4")

VegAnswers


#***Use sapply here to perform repeated functions over columns***
#Code columns so vegetarian or vegan = 1, other = 0
VegAnswers[,2:6]<-sapply(VegAnswers[,2:6], as.character)
VegAnswers[,2:6]<-sapply(VegAnswers[,2:6], 
    mapvalues, c(NA, "Low calorie", "Gluten-free", "Lactose-free", "Other",
                        "Vegetarian", "Vegan"), c(0,0,0,0,0,1,1))

#******************************************************************


#Get 24h recall taken data-----------
daycols<-c("f.20080.0.0", "f.20080.1.0", "f.20080.2.0",
           "f.20080.3.0", "f.20080.4.0")
bd1<-ukbveg
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
bd1$FID<-ukbveg$f.eid
bd1<-as_tibble(bd1)
bd1
#--------------

###This section creates a table that has both answers to special diet
###columns (Veg0-4) and columns indicating whether people took the
###24hr survey (took0-4)

TooksurveyAndVeg <- merge(VegAnswers, bd1, by="FID")

TooksurveyAndVeg<-as_tibble(TooksurveyAndVeg)

colnames(TooksurveyAndVeg)[7:11]<-c("took0", "took1", "took2", "took3", "took4")
table(TooksurveyAndVeg$took_24HR)

nrow(TooksurveyAndVeg) #[1] 135411


#initialize answer columns
TooksurveyAndVeg$Answer0<-"d" #column 13
TooksurveyAndVeg$Answer1<-"d" #column 14
TooksurveyAndVeg$Answer2<-"d" #column 15
TooksurveyAndVeg$Answer3<-"d" #column 16
TooksurveyAndVeg$Answer4<-"d" #column 17

#There are 4 conditions here:
#a) took survey, was vegetarian
#b) took survey, wasn't vegetarian
#c) didn't take survey in this instance
#d) code failed to update answer

#This loop takes between 10 and 20 minutes

for(instance in 7:11){ #columns 7 through 11 are the "took" columns
    for(row in 1:nrow(TooksurveyAndVeg)){ #loop through every row in the table
        if(TooksurveyAndVeg[row, instance] == 1){ #if they took the survey in this instance
            if(TooksurveyAndVeg[row, instance-5]==1){ #if they had 1 in the same instance of veg
                TooksurveyAndVeg[row, instance+6]<-"a"
            }
            else TooksurveyAndVeg[row, instance+6]<-"b"
            }
        else(TooksurveyAndVeg[row, instance+6]<-"c")
        }
}


#adding column for diet 
TooksurveyAndVeg$Diet0<-"0"
TooksurveyAndVeg$Diet1<-"0"
TooksurveyAndVeg$Diet2<-"0"
TooksurveyAndVeg$Diet3<-"0"
TooksurveyAndVeg$Diet4<-"0"

TooksurveyAndVeg2<-TooksurveyAndVeg
TooksurveyAndVeg

#this part takes 20-30 minutes
# for(instance in 13:17){ #columns 13 through 17 are the "Answer0-4" columns
#     for(row in 1:nrow(TooksurveyAndVeg)){ #loop through every row in the table
#         if(TooksurveyAndVeg[row, instance] == "a"){ #if they are vegetarian 
#             TooksurveyAndVeg[row, instance+5]<- "1"
#         }
#         if(TooksurveyAndVeg[row, instance] == "b"){ #if they are meat eaters 
#             TooksurveyAndVeg[row, instance+5]<-"-100"
#         }
#         if(TooksurveyAndVeg[row, instance] == "c"){
#             TooksurveyAndVeg[row, instance+5]<- "0"
#         }
#     } #end inner for loop
# } #end outer for loop


#********************************************************************
#This above section also be done with subsetting like the following
#which drastically speeds up the process



TooksurveyAndVeg2


TooksurveyAndVeg2$Diet0[TooksurveyAndVeg2$Answer0=="a"]<-1
TooksurveyAndVeg2$Diet0[TooksurveyAndVeg2$Answer0=="b"]<-(-100)
TooksurveyAndVeg2$Diet0[TooksurveyAndVeg2$Answer0=="c"]<-0
TooksurveyAndVeg2$Diet1[TooksurveyAndVeg2$Answer1=="a"]<-1
TooksurveyAndVeg2$Diet1[TooksurveyAndVeg2$Answer1=="b"]<-(-100)
TooksurveyAndVeg2$Diet1[TooksurveyAndVeg2$Answer1=="c"]<-0
TooksurveyAndVeg2$Diet2[TooksurveyAndVeg2$Answer2=="a"]<-1
TooksurveyAndVeg2$Diet2[TooksurveyAndVeg2$Answer2=="b"]<-(-100)
TooksurveyAndVeg2$Diet2[TooksurveyAndVeg2$Answer2=="c"]<-0
TooksurveyAndVeg2$Diet3[TooksurveyAndVeg2$Answer3=="a"]<-1
TooksurveyAndVeg2$Diet3[TooksurveyAndVeg2$Answer3=="b"]<-(-100)
TooksurveyAndVeg2$Diet3[TooksurveyAndVeg2$Answer3=="c"]<-0
TooksurveyAndVeg2$Diet4[TooksurveyAndVeg2$Answer4=="a"]<-1
TooksurveyAndVeg2$Diet4[TooksurveyAndVeg2$Answer4=="b"]<-(-100)
TooksurveyAndVeg2$Diet4[TooksurveyAndVeg2$Answer4=="c"]<-0



#********************************************************************

Diet<-TooksurveyAndVeg%>%select(FID, Diet0, Diet1, Diet2, Diet3, Diet4) #subsetting information

Diet

Diet<-Diet%>%mutate_if(is.character, as.numeric) #changing Diet to numeric 
#this above line was necessary because these rows were initialized 
#with "o"'s (a character) instead of "0"'s (a number)
Diet

Diet$row_sum = rowSums(Diet[,c(2,3,4,5,6)]) #sum of the rows

Diet

Diet$Veg<-"a" #creating column for final answer

Diet

Diet$Veg[Diet$row_sum < 0]<-"Carnivore"
Diet$Veg[Diet$row_sum > 0]<-"Vegetarian"

identical(Diet, Diet2) #[1] TRUE


Diet<-Diet%>%mutate_if(is.integer, as.character)

Diet

write.table(Diet, file = "vegQC.txt", 
            sep = "\t", col.names = TRUE, quote = FALSE,
            row.names = FALSE)
