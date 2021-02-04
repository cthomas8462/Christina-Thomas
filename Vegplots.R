setwd ("/Users/christinathomas/Documents/R_files/UK_Biobank/pheno/")

library(rlang)
library(plyr)
library(dplyr)
library(tidyverse)

vegqc<-read.delim("vegQC.txt", header=TRUE, sep="\t")
allvegqc<-read.delim("strictandselfreported.txt", header=TRUE, sep="\t")

allvegqc<-as_tibble(allvegqc)
sum(allvegqc$Strict.Vegetarians)
sum(allvegqc$Self_Reported_Vegetarians)


biom <- read.delim("UKB34137_QC_and_24HR.txt", header = TRUE, sep="\t")
biom <- as_tibble(biom)


#f.30760.0.0 - HDL
#f.30860.0.0 - protein
#f.30870.0.0 - Triglyceride
#f.30750.0.0 - HbA1c
#f.30820.0.0- Rheumatoid factor
#f.30630.0.0 - Apolipoprotein A
#f.30640.0.0 - Apolipoprotein B
#f.30710.0.0 - C-reactive Protein
#f.30790.0.0 - Lipoprotein A
#f.30880.0.0 - Urate
#f.30680.0.0  - Calcium
#f.30690.0.0 - Cholesterol
#f.30670.0.0 - Urea
#f.30620.0.0 - Alanine aminotransferase
#f.30650.0.0 - 	Aspartate aminotransferase
#f.30660.0.0 - Direct Bilirubin
#f.30840.0.0 - Total Bilirubin
#f.30780.0.0 - LDL
#f.30730.0.0 - Gamma Glutamyltransferas
#f.30740.0.0 - Glucose
#f.30600.0.0 - Albumin
#f.30810.0.0 - Phosphate
#f.30610.0.0 - Alkaline Phosphatase
#f.30700.0.0 - Creatinine 
#f.30830.0.0 - SHBG
#f.30850.0.0 - Testosterone
#f.30800.0.0 - Oestradiol
#f.30890.0.0 - Vitamin D
#f.30770.0.0 - IGF-1



bio<-biom%>%select(f.eid, f.31.0.0, f.30760.0.0, f.30860.0.0, f.30870.0.0,
                   f.30750.0.0, f.30820.0.0, f.30630.0.0, f.30640.0.0, 
                   f.30710.0.0, f.30790.0.0, f.30880.0.0, f.30680.0.0, 
                   f.30690.0.0, f.30670.0.0, f.30620.0.0, f.30650.0.0, 
                   f.30660.0.0, f.30840.0.0, f.30780.0.0, f.30730.0.0, 
                   f.30740.0.0, f.30600.0.0, f.30810.0.0, f.30610.0.0, 
                   f.30700.0.0, f.30830.0.0, f.30850.0.0, f.30800.0.0,
                   f.30890.0.0, f.30770.0.0)

#total of 29 biomarkers
colnames(bio)<- c("FID", "Sex", "HDL", "Protein", "Triglyceride", 
                  "HbA1c", "Rheumatoid_factor", "Apolipoprotein_A",
                  "Apolipoprotein_B", "C_reactive_Protein",
                  "Lipoprotein_A", "Urate", "Calcium", "Cholesterol",
                  "Urea", "Alanine_aminotransferase", 
                  "Aspartate_aminotransferase", "Direct_Bilirubin",
                  "Total_Bilirubin", "LDL", 
                  "Gamma_Glutamyltransferase", "Glucose", "Albumin",
                  "Phosphate", "Alkaline_Phosphatase", "Creatinine", 
                  "SHBG", "Testosterone", "Oestradiol", "Vitamin_D",
                  "IGF_1")

#bioveg<- merge(bio, vegqc, by = "FID")

#bioveg<-as_tibble(bioveg)


bioveg<- merge(bio, allvegqc, by = "FID")

bioveg<-as_tibble(bioveg)
bioveg

table(bioveg$Self_Reported_Vegetarians) 
table(bioveg$Strict.Vegetarians)


bioveg$Self_Reported_Vegetarians<- as.factor(bioveg$Self_Reported_Vegetarians)
bioveg$Strict.Vegetarians<- as.factor(bioveg$Strict.Vegetarians)

#for continuous...
pheno<-colnames(bioveg[3:31])
pheno



#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#PLOTS FOR ALL PARTICIPANTS FOR EACH PHENOTYPE=-=-=-=-=-=-=-=-=-=-=-=-
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

for (i in 1:length(pheno)){
    plotthis<-(bioveg[,c("FID",pheno[i], "Self_Reported_Vegetarians",
                         "Strict.Vegetarians")])
    plotthis<-plotthis[complete.cases(plotthis),]
    
    print(plotthis)
    print(typeof(unlist(plotthis[,pheno[i]])))
    
    
    #Generate summary stats-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
    print(t.test(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,],
                 plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,]))
    
    tresult<-t.test(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,],
                    plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,])
    
    pval<-tresult$p.val
    meannonveg<-mean(unlist(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,]))
    meanveg<-mean(unlist(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,]))
    
    print(pval)
    print(meannonveg)
    #print(meanveg)
    
    #Generate box plots-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    print(ggplot(data = plotthis, 
                 mapping = aes(x = Self_Reported_Vegetarians, 
                               y = unlist(plotthis[,pheno[i]]))) +
              geom_boxplot() +
              labs(x = "Self-Reported Vegetarians",
                   y = pheno[i],
                   title = paste("Relationship of Vegetarians and",  pheno[i], 
                                 "\n n=", nrow(plotthis),
                                 "\n p-value: ", pval,
                                 "\n mean nonveg: ", meannonveg,
                                 "\n mean veg: ", meanveg,
                                 sep=" ")) +
              scale_x_discrete(limits=c("0", "1"))
    )#end print                                                                                                  
    
    ggsave(
        paste(pheno[i],"_boxplot_",Sys.Date(), sep=""),
        plot = last_plot()
    )
    
}

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#PLOTS FOR SEX STRATIFIED PARTICIPANTS FOR EACH PHENOTYPE=-=-=-=-=-=-=-=-=-=-=-=-
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

for (i in 1:length(pheno)){
    #for (i in 1:1){
    plotthisFULL<-(bioveg[,c("FID", "Sex", pheno[i], "Self_Reported_Vegetarians",
                             "Strict.Vegetarians")])
    plotSex<-c("Male", "Female")
    
    for (j in 1:length(plotSex)){
        
        print(plotSex[j])
        #complete cases only
        plotthis<-plotthisFULL[complete.cases(plotthisFULL),]
        
        #extract only one sex
        plotthis<-plotthis[plotthis$Sex==plotSex[j],]
        
        print(plotthis)
        print(typeof(unlist(plotthis[,pheno[i]])))
        
        
        #Generate summary stats-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        
        print(t.test(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,],
                     plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,]))
        
        tresult<-t.test(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,],
                        plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,])
        
        pval<-signif((tresult$p.val), digits = 5)
        meannonveg<-round(mean(unlist(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==0,])), digits = 2)
        meanveg<-round(mean(unlist(plotthis[,pheno[i]][plotthis$Self_Reported_Vegetarians==1,])), digits = 2)
        
        print(pval)
        print(meannonveg)
        print(meanveg)
        
        #Generate box plots-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        
        print(ggplot(data = plotthis, 
                     mapping = aes(x = Self_Reported_Vegetarians, 
                                   y = unlist(plotthis[,pheno[i]]))) +
                  geom_boxplot(fill = "lightblue1", color = "deepskyblue2") + stat_summary(fun = mean, geom = "point", 
                                                                                           shape = 18, size = 2.5, color = "royalblue4") +
                  annotate("text", x = .5, y = meannonveg, label = meannonveg, color = "royalblue4") +
                  annotate("text", x = 1.5, y = meanveg, label = meanveg, color = "royalblue4") +
                  labs(x = "Self-Reported Vegetarians",
                       y = pheno[i], 
                       title = paste("Relationship of Vegetarians and ",  
                                     pheno[i], " in \n", plotSex[j],"s", sep=""), 
                       subtitle = paste(
                           "\n n= ", nrow(plotthis),
                           "\n p-value: ", pval,
                           "\n mean nonveg: ", meannonveg,
                           "\n mean veg: ", meanveg,
                           sep="")) + 
                  theme(plot.title = element_text(size =12, color = "midnightblue"), 
                        axis.title.x = element_text(size = 10, color = "midnightblue"),
                        axis.title.y = element_text(size = 10, color = "midnightblue"),
                        plot.subtitle = element_text(color = "royalblue4", size = 8, face = "italic")) +
                  scale_x_discrete(limits=c("0", "1")) 
              
        )#end print                                                                                                  
        
        ggsave(paste(pheno[i],"_boxplot_",plotSex[j], "_", Sys.Date(), sep="", ".png"),
               plot = last_plot(), device = "png",
               path = NULL,
               height = 5.0,
               width = 5.0,
               scale = 1,
               dpi = 300,
               limitsize = FALSE
        )
        
    }#end inner Sex loop
}#end outer phenotype loop

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#SUMMARY STATS GITHUB=-=-=-=-=-=-=-=-=-=-=-=-
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

quantpheno<-c("HDL", "Protein", "Triglyceride", 
              "HbA1c", "Rheumatoid_factor", "Apolipoprotein_A",
              "Apolipoprotein_B", "C_reactive_Protein",
              "Lipoprotein_A", "Urate", "Calcium", "Cholesterol",
              "Urea", "Alanine_aminotransferase", 
              "Aspartate_aminotransferase", "Direct_Bilirubin",
              "Total_Bilirubin", "LDL", 
              "Gamma_Glutamyltransferase", "Glucose", "Albumin",
              "Phosphate", "Alkaline_Phosphatase", "Creatinine", 
              "SHBG", "Testosterone", "Oestradiol", "Vitamin_D",
              "IGF_1")

#Generate table for quantitative phenotypes
quantitative<-as.data.frame(
    bioveg%>%select("Self_Reported_Vegetarians", quantpheno)%>%
        group_by(Self_Reported_Vegetarians)%>%summarise_all( 
            funs(n = sum(!is.na(.)), 
                 min(., na.rm = TRUE),
                 max(., na.rm = TRUE),
                 mean(., na.rm = TRUE),
                 sd(., na.rm = TRUE),
                 median(., na.rm = TRUE),
                 IQR(., na.rm = TRUE),
            ))
)

quantitative

write.csv(quantitative, "quantitative_UKB_bySelf_Reported_Vegetarians.csv")



bioveg$Sex<-as.numeric(mapvalues(as.character(bioveg$Sex), 
                                 c("Male", "Female"), c(0,1)))


catpheno<-c("Sex")

#Generate table for categorical phenotypes
categorical<-as.data.frame(
    bioveg%>%select("Self_Reported_Vegetarians", catpheno)%>%
        group_by(Self_Reported_Vegetarians)%>%summarise_all( 
            funs(n = sum(!is.na(.)), 
                 percent = (100*mean(., na.rm = TRUE))
                 
            ))
)


categorical

write.csv(categorical, "categorical_UKB_bySelf_Reported_Vegetarians.csv")


hist(bioveg$LDL)

hist(bioveg$Testosterone)


