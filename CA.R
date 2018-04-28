library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)



library(VIM) 
library(survey)
library(nFactors)


#library(yarrr)
library(xtable)
library(questionr)
library(hrbrthemes)


library(FactoMineR)
library(FactoInvestigate)
library(factoextra)

options(mc.cores=parallel::detectCores()) # Run on multiple cores

set.seed(3875)
setwd("~/Documents/UH/dissertation/r_code")



### import data
#survey_data <- read.csv("pni_lang_survey.csv")
survey_data <- read.csv("new_data.csv",na.strings=c(""," ","NA"))
#survey_data <- read.csv("complete_data.csv",na.strings=c(""," ","NA"))
survey_data$questionnaire_id <- as.factor(survey_data$questionnaire_id)
survey_data$old_questionnaire_id <- as.factor(survey_data$old_questionnaire_id)
#survey_data[survey_data=="Both"] <- as.factor(as.character("Private school"))
#survey_data<- survey_data %>% dplyr::select(-badtempered) #remove badtempered

# recode the school type NAs if not random before recoding the variables
survey_data$elementary_type <- as.character(survey_data$elementary_type)
survey_data$hs_type <- as.character(survey_data$hs_type)

survey_data$hs_type <- ifelse(survey_data$education == "No schooling completed" | survey_data$education == "Kingergarten - 8th grade","None",survey_data$hs_type)

survey_data$elementary_type <- as.factor(survey_data$elementary_type)
survey_data$hs_type <- as.factor(survey_data$hs_type)

summary(survey_data$hs_type)
summary(survey_data$elementary_type)

levels(survey_data$education)[levels(survey_data$education)=="No schooling completed"] <- "Not high school graduate"
levels(survey_data$education)[levels(survey_data$education)=="Kingergarten - 8th grade"] <- "Not high school graduate"
levels(survey_data$education)[levels(survey_data$education)=="Some high school, no diploma"] <- "Not high school graduate"
levels(survey_data$education)[levels(survey_data$education)=="Master's degree"] <- "Advanced degree"
levels(survey_data$education)[levels(survey_data$education)=="Professional degree (JD, MD, etc.)"] <- "Advanced degree"
levels(survey_data$education)[levels(survey_data$education)=="Doctorate degree"] <- "Advanced degree"
levels(survey_data$education)[levels(survey_data$education)=="Trade/Technical/vocational training"] <- "Some college, no degree"


levels(survey_data$birth_location)[levels(survey_data$birth_location)=="Philippines"] <- "Other"
levels(survey_data$birth_location)[levels(survey_data$birth_location)=="CNMI"] <- "USA"
levels(survey_data$birth_location)[levels(survey_data$birth_location)=="Hawaii"] <- "USA"
levels(survey_data$birth_location)[levels(survey_data$birth_location)=="Guam"] <- "USA"
levels(survey_data$birth_location)[levels(survey_data$birth_location)=="US Mainland"] <- "USA"

summary(survey_data$birth_location)


## clean up location data
summary(survey_data$current_village)
locations <- read.csv("locations.csv")
written_locations <- read.csv("complete_sections_all.csv")
#written_locations <- read.csv("sections_written1.csv")
written_locations1 <- read.csv("current_section_pni_1.csv")
written_locations2 <- read.csv("current_section_pni_2.csv")
written_locations$questionnaire_id <- as.factor(written_locations$questionnaire_id)
written_locations1$questionnaire_id <- as.factor(written_locations1$questionnaire_id)
written_locations2$questionnaire_id <- as.factor(written_locations2$questionnaire_id)
summary(written_locations$questionnaire_id)

# find missing people
#missing<- anti_join(survey_data,written_locations,by="questionnaire_id")

survey_data <- left_join(survey_data,written_locations,by=c("old_questionnaire_id"="questionnaire_id"))
#survey_data<-merge(survey_data, written_locations, by.x = c("old_questionnaire_id","current_village"), by.y = c("questionnaire_id","current_village"), all.x = TRUE)
survey_data <- survey_data %>% gather(key=delete,value=current_village,-1:-127) # creates too many rows so delete the NAs
#villages.test <- survey_data$current_village
survey_data <- survey_data %>% filter(is.na(current_village) == 0) %>% filter(current_village != "") # removes the NAs before
#villages.test2 <- survey_data$current_village
#survey_selected <- survey_data %>% dplyr::select(questionnaire_id,current_village) # check to see what is going on with current_village


survey_data <- survey_data %>%  dplyr::select(-delete)

survey_data <- left_join(survey_data,locations,by=c("current_village"="place")) 

survey_data$current_village <- as.factor(survey_data$current_village)



######
# impution
regression_data <- survey_data %>% dplyr::select(1:42,current_village)
survey_data_imputed<-kNN(regression_data,k=17,weightDist=T) # sqrt(301)
survey_data_imputed
summary(survey_data_imputed$hs_type_imp)
survey_data_good <- survey_data_imputed 
survey_data_good$education <- factor(survey_data_good$education,
                                     levels = c("Not high school graduate","High school, diploma, or GED","Some college, no degree","Associate degree","Bachelor's degree","Advanced degree"))
survey_data_good$time_pni <- factor(survey_data_good$time_pni,
                                    levels = c("0 - 4 years","5 - 9 years","10 - 19 years","20 - 29 years","30 - 39 years","40 or more years"))
survey_data_good$elementary_type <- factor(survey_data_good$elementary_type,
                                           levels = c("Public school","Private school","Both"))
survey_data_good$hs_type <- factor(survey_data_good$hs_type,
                                   levels = c("Public school","Private school","None"))
#########
# language use CA
language_use_complete <- read.csv("language_use_complete.csv",na.strings=c(""," ","NA")) # remove french, italian,russian,chinese and a couple others in college, check for extra spaces

language_use_selections_gathered <- language_use_complete %>% gather(key=domain,value=language)
language_use_selections_gathered$domain <- as.factor(language_use_selections_gathered$domain)
language_use_selections_gathered$language <- as.factor(language_use_selections_gathered$language)
dt1 <- with(language_use_selections_gathered,table(domain,language))

language_use.ca <- CA(dt1, graph = FALSE)
#Investigate(language_use.ca,nclust = 0,file="language_use_ca.Rmd",document="pdf_document")
Hmisc::latexTranslate(language_use.ca)
#language_use.ca
summary(language_use.ca)
plot.CA(language_use.ca, axes = c(1,2), col.row = "blue", col.col = "red")
plot.CA(language_use.ca, axes = c(1,3), col.row = "blue", col.col = "red")
fviz_ca_biplot(language_use.ca,repel=T)
ggsave("calanguageuse.png",width=8,height=6,units="in")
# which rows and column contribute most to the CA?
fviz_ca_row(language_use.ca,repel=T)
ggsave("calanguageuserows.png",width=8,height=6,units="in")

fviz_contrib(language_use.ca, choice = "row", axes = 1) # row, dim 1
ggsave("calanguageuserowcontrib1.png",width=8,height=6,units="in")
fviz_contrib(language_use.ca, choice = "row", axes = 2) # row, dim 2
fviz_contrib(language_use.ca, choice = "row", axes = 3) # row, dim 3
fviz_ca_row(language_use.ca, col.row = "contrib",repel=T)

stargazer::stargazer(language_use.ca)

# columns
fviz_contrib(language_use.ca, choice = "col", axes = 1) # column, dim 1
ggsave("calanguageusercolcontrib1.png",width=8,height=6,units="in")
fviz_contrib(language_use.ca, choice = "col", axes = 2) # column, dim 2
ggsave("calanguageusecolcontrib2.png",width=8,height=6,units="in")
fviz_contrib(language_use.ca, choice = "col", axes = 3) # column, dim 3
fviz_ca_col(language_use.ca,axes=c(1,2),repel=T)
ggsave("calanguageusercols.png",width=8,height=6,units="in")
fviz_ca_col(language_use.ca,axes=c(1,3),repel=T)
fviz_ca_col(language_use.ca,axes=c(2,3), col.col = "contrib",repel=T)

# what if we cluster the CA?
language_use.ca.cluster <- HCPC(language_use.ca,nb.clust=-1)
#hcut(language_use.ca.cluster)

plot(language_use.ca.cluster, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = T, title = NULL,
     tree.barplot = T, centers.plot = T)
plot(language_use.ca.cluster,choice="tree",cex=0.6)
#ggsave("calanguageusecluster.png",width=8,height=6,units="in")
plot(language_use.ca.cluster,choice="map",draw.tree=F,centers.plot=T)
#ggsave("calanguageusecluster2.png",width=8,height=6,units="in")
summary(language_use.ca.cluster)
factoextra::fviz_dend(language_use.ca.cluster, rect = F,cex=0.6,repel=T,k=3,type="phylogenic")
ggsave("calanguageusecluster3.png",width=8,height=6,units="in")



####
## domains CA

CA_domains <- survey_data_imputed %>% dplyr::select(making_friends:us_relatives)
#write.csv(CA_domains,file="domains.csv",row.names=F)
survey_selected_gathered <- CA_domains %>% gather(key=domain,value=language)
survey_selected_gathered$domain <- as.factor(survey_selected_gathered$domain)
survey_selected_gathered$language <- as.factor(survey_selected_gathered$language)
dt <- with(survey_selected_gathered,table(domain,language))

#library(gplots)
#balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
#            label = FALSE, show.margins = FALSE)

survey.ca <- CA(dt, graph = FALSE)
survey.ca

Investigate(survey.ca,document="pdf_document")
summary(survey.ca)
plot.CA(survey.ca, axes = c(1,2), col.row = "blue", col.col = "red")
fviz_ca_biplot(survey.ca,repel=T)
ggsave("cadomains.png",width=8,height=6,units="in")
# which rows and column contribute most to the CA?
fviz_ca_row(survey.ca)

#he red dashed line on the graph above indicates the expected average contribution. 
#For a given dimension, any row with a contribution larger than this threshold 
#could be considered as important in contributing to that dimension.
fviz_contrib(survey.ca, choice = "row", axes = 1) # row, dim 1
ggsave("cadomainscontribrow1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "row", axes = 2) # row, dim 2
ggsave("cadomainscontribrow2.png",width=8,height=6,units="in")
fviz_ca_row(survey.ca, col.row = "contrib")


# columns
fviz_contrib(survey.ca, choice = "col", axes = 1) # column, dim 1
ggsave("cadomainscontribcol1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "col", axes = 2) # column, dim 2
ggsave("cadomainscontribcol2.png",width=8,height=6,units="in")
fviz_ca_col(survey.ca, col.row = "contrib")

survey.ca.cluster <- HCPC(survey.ca,nb.clust=-1)

#hcut(language_use.ca.cluster)

plot(survey.ca.cluster, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = T, title = NULL,
     tree.barplot = T, centers.plot = T)
plot(survey.ca.cluster,choice="tree",cex=0.6)
#ggsave("calanguageusecluster.png",width=8,height=6,units="in")
plot(survey.ca.cluster,choice="map",draw.tree=F,centers.plot=T)
#ggsave("calanguageusecluster2.png",width=8,height=6,units="in")

factoextra::fviz_dend(survey.ca.cluster, rect = F,cex=0.6,repel=T,k=3,type="phylogenic")
ggsave("cadomaincluster3.png",width=8,height=6,units="in")

### agreement

agreement_pre <- survey_data %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni) # not pni_respect, pni_unfashionable, (ones only in both versions)
agreement2_pre <- survey_data %>% dplyr::filter(survey_version != "1") %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)  # ones only in version 2
agreement<-kNN(agreement_pre,k=17,weightDist=T) # sqrt(301)
agreement2<-kNN(agreement2_pre,k=13,weightDist=T) # different n
agreement <- agreement %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni)
agreement2 <- agreement2 %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)

survey_selected_gathered <- agreement %>% gather(key=domain,value=language)
survey_selected_gathered$domain <- as.factor(survey_selected_gathered$domain)
survey_selected_gathered$language <- as.factor(survey_selected_gathered$language)
dt <- with(survey_selected_gathered,table(domain,language))

#library(gplots)
#balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
#            label = FALSE, show.margins = FALSE)

survey.ca <- CA(dt, graph = F,ncp=2)
survey.ca

Investigate(survey.ca,document="pdf_document")
summary(survey.ca)
plot.CA(survey.ca, axes = c(1,2), col.row = "blue", col.col = "red")
fviz_ca_biplot(survey.ca,repel=T)
ggsave("cadomains.png",width=8,height=6,units="in")
# which rows and column contribute most to the CA?
fviz_ca_row(survey.ca)

#he red dashed line on the graph above indicates the expected average contribution. 
#For a given dimension, any row with a contribution larger than this threshold 
#could be considered as important in contributing to that dimension.
fviz_contrib(survey.ca, choice = "row", axes = 1) # row, dim 1
ggsave("cadomainscontribrow1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "row", axes = 2) # row, dim 2
ggsave("cadomainscontribrow2.png",width=8,height=6,units="in")
fviz_ca_row(survey.ca, col.row = "contrib")


# columns
fviz_contrib(survey.ca, choice = "col", axes = 1) # column, dim 1
ggsave("cadomainscontribcol1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "col", axes = 2) # column, dim 2
ggsave("cadomainscontribcol2.png",width=8,height=6,units="in")
fviz_ca_col(survey.ca, col.row = "contrib")

survey.ca.cluster <- HCPC(survey.ca,nb.clust=-1)
#hcut(language_use.ca.cluster)

plot(survey.ca.cluster, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = T, title = NULL,
     tree.barplot = T, centers.plot = T)
plot(survey.ca.cluster,choice="tree",cex=0.6)
#ggsave("calanguageusecluster.png",width=8,height=6,units="in")
plot(survey.ca.cluster,choice="map",draw.tree=F,centers.plot=T)
#ggsave("calanguageusecluster2.png",width=8,height=6,units="in")

factoextra::fviz_dend(survey.ca.cluster, rect = F,cex=0.6,repel=T,k=3,type="phylogenic")
ggsave("cadomaincluster3.png",width=8,height=6,units="in")


#### level of agreement

agreement_level_pre <- survey_data %>% dplyr::select(quiet:modern,successful:uneducated) # no greedy, attractive
agreement_level<-kNN(agreement_level_pre,k=17,weightDist=T) # sqrt(301)
agreement_level <- agreement_level %>% dplyr::select(quiet:modern,successful:uneducated)

agreement_level2_pre <- survey_data %>% filter(survey_version !="1") %>%dplyr::select(greedy,smart:educated) # no greedy, attractive
agreement_level2<-kNN(agreement_level2_pre,k=13,weightDist=T) # sqrt(159)
agreement_level2 <- agreement_level2 %>% dplyr::select(greedy,smart:educated)


survey_selected_gathered <- agreement_level %>% gather(key=domain,value=language)
survey_selected_gathered$domain <- as.factor(survey_selected_gathered$domain)
survey_selected_gathered$language <- as.factor(survey_selected_gathered$language)
dt <- with(survey_selected_gathered,table(domain,language))

#library(gplots)
#balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
#            label = FALSE, show.margins = FALSE)

survey.ca <- CA(dt, graph = F,ncp=2)
survey.ca

Investigate(survey.ca,document="pdf_document")
summary(survey.ca)
plot.CA(survey.ca, axes = c(1,2), col.row = "blue", col.col = "red")
fviz_ca_biplot(survey.ca,repel=T)
ggsave("calevelagreement.png",width=8,height=6,units="in")
# which rows and column contribute most to the CA?
fviz_ca_row(survey.ca)

#he red dashed line on the graph above indicates the expected average contribution. 
#For a given dimension, any row with a contribution larger than this threshold 
#could be considered as important in contributing to that dimension.
fviz_contrib(survey.ca, choice = "row", axes = 1) # row, dim 1
ggsave("calevelagreementcontribrow1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "row", axes = 2) # row, dim 2
ggsave("calevelagreementcontribrow2.png",width=8,height=6,units="in")
fviz_ca_row(survey.ca, col.row = "contrib")


# columns
fviz_contrib(survey.ca, choice = "col", axes = 1) # column, dim 1
ggsave("calevelagreementcontribcol1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "col", axes = 2) # column, dim 2
ggsave("calevelagreementcontribcol2.png",width=8,height=6,units="in")
fviz_ca_col(survey.ca, col.row = "contrib")

survey.ca.cluster <- HCPC(survey.ca,nb.clust=-1)
#hcut(language_use.ca.cluster)

plot(survey.ca.cluster, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = T, title = NULL,
     tree.barplot = T, centers.plot = T)
plot(survey.ca.cluster,choice="tree",cex=0.6)
#ggsave("calanguageusecluster.png",width=8,height=6,units="in")
plot(survey.ca.cluster,choice="map",draw.tree=F,centers.plot=T)
#ggsave("calanguageusecluster2.png",width=8,height=6,units="in")

factoextra::fviz_dend(survey.ca.cluster, rect = F,cex=0.6,repel=T,k=3,type="phylogenic")
ggsave("calevelagreementcluster3.png",width=8,height=6,units="in")


## new questions
survey_selected_gathered <- agreement_level2 %>% gather(key=domain,value=language)
survey_selected_gathered$domain <- as.factor(survey_selected_gathered$domain)
survey_selected_gathered$language <- as.factor(survey_selected_gathered$language)
dt <- with(survey_selected_gathered,table(domain,language))

#library(gplots)
#balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
#            label = FALSE, show.margins = FALSE)

survey.ca <- CA(dt, graph = F,ncp=2)
survey.ca

Investigate(survey.ca,document="pdf_document")
summary(survey.ca)
plot.CA(survey.ca, axes = c(1,2), col.row = "blue", col.col = "red")
fviz_ca_biplot(survey.ca,repel=T)
ggsave("calevelagreement2.png",width=8,height=6,units="in")
# which rows and column contribute most to the CA?
fviz_ca_row(survey.ca)

#he red dashed line on the graph above indicates the expected average contribution. 
#For a given dimension, any row with a contribution larger than this threshold 
#could be considered as important in contributing to that dimension.
fviz_contrib(survey.ca, choice = "row", axes = 1) # row, dim 1
ggsave("calevelagreement2contribrow1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "row", axes = 2) # row, dim 2
ggsave("calevelagreement2contribrow2.png",width=8,height=6,units="in")
fviz_ca_row(survey.ca, col.row = "contrib")


# columns
fviz_contrib(survey.ca, choice = "col", axes = 1) # column, dim 1
ggsave("calevelagreement2contribcol1.png",width=8,height=6,units="in")
fviz_contrib(survey.ca, choice = "col", axes = 2) # column, dim 2
ggsave("calevelagreement2contribcol2.png",width=8,height=6,units="in")
fviz_ca_col(survey.ca, col.row = "contrib")

survey.ca.cluster <- HCPC(survey.ca,nb.clust=-1)
#hcut(language_use.ca.cluster)

plot(survey.ca.cluster, axes = c(1,2), choice = "3D.map", 
     draw.tree = TRUE, ind.names = T, title = NULL,
     tree.barplot = T, centers.plot = T)
plot(survey.ca.cluster,choice="tree",cex=0.6)
#ggsave("calanguageusecluster.png",width=8,height=6,units="in")
plot(survey.ca.cluster,choice="map",draw.tree=F,centers.plot=T)
#ggsave("calanguageusecluster2.png",width=8,height=6,units="in")

factoextra::fviz_dend(survey.ca.cluster, rect = F,cex=0.6,repel=T,k=3,type="phylogenic")
ggsave("calevelagreement2cluster3.png",width=8,height=6,units="in")

