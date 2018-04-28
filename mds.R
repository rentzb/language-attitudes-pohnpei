library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)


library(VIM) 
library(nFactors)
library(MASS)
library(vegan)
library(cluster)
library(fpc)
library(hrbrthemes)
library(ggrepel)
library(reporttools)
library(likert)

set.seed(3875)
setwd("~/Documents/UH/dissertation/r_code")
hrbrthemes::import_roboto_condensed()

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

survey_data$education <- factor(survey_data$education,
                                     levels = c("Not high school graduate","High school, diploma, or GED","Some college, no degree","Associate degree","Bachelor's degree","Advanced degree"))

##################
##################

### mds for domains
domains_pre <- survey_data %>% dplyr::select(making_friends:us_relatives)
# impute
domains<-kNN(domains_pre,k=17,weightDist=T) # sqrt(301)
domains <- domains %>% dplyr::select(making_friends:us_relatives)

domains.dist <- daisy(domains,metric="gower") # dissim matrix
domains.mds <- cmdscale(domains.dist) # mds

## find best k for PAM
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- cluster::pam(domains.dist, k)$silinfo$avg.width
k.best <- which.max(asw)



cat("silhouette-optimal number of clusters:", k.best, "\n")
plot(cluster::pam(domains.dist, k.best))
##

# compute PAM
domains.pam <- pam(domains.dist,k=2)

## save data for plotting
domains.mds.df <- as.data.frame(domains.mds)

domains.mds.df$domains_cluster <- as.factor(domains.pam$clustering)


ggplot(domains.mds.df,aes(x=V1,y=V2,color=domains_cluster)) + geom_point() + theme_ipsum() +
  labs(title="MDS plot for domain questions",subtitle="Colored by PAM cluster") + scale_color_brewer(palette="Set1")
ggsave("mdsdomainspam.png",width=8,height=6,units="in")

## double check these column numbers
survey_data$total_eng <- rowSums(domains[,c(1:25)] == "English")
survey_data$total_pni <- rowSums(domains[,c(1:25)] == "Pohnpeian")

survey_data$total_pif <- rowSums(domains[,c(1:25)] == "Pingelapese")
survey_data$total_mok <- rowSums(domains[,c(1:25)] == "Mokilese")
survey_data$total_kos <- rowSums(domains[,c(1:25)] == "Kosraean")
survey_data$total_mort <- rowSums(domains[,c(1:25)] == "Mortlockese")
survey_data$total_tkk <- rowSums(domains[,c(1:25)] == "Chuukese")
survey_data$total_other <- rowSums(domains[,c(1:25)] == "Other")

#survey_data$parent_english <- rowSums(survey_data[,c(108:109)]== 1)

survey_data$total_sum_other <- survey_data$total_mok + survey_data$total_pif+survey_data$total_other+survey_data$total_tkk+survey_data$total_mort+survey_data$total_kos

survey_data$domains_cluster <- domains.mds.df$domains_cluster

ggplot(survey_data,aes(x=domains_cluster,y=total_eng)) + geom_bar(stat="identity") + theme_ipsum(grid="Y") +xlab("Domain PAM cluster") + ylab("Total English selections") + labs(title="Counts of English Selections for Domains") 



tableNominal(vars = vars2, group = survey_data$domains_cluster, miss.cat = 3, print.pval = "none", cap = cap2, lab = "tab:nominal2",longtable = F)


# social solidarity
domains$domains_cluster <- domains.mds.df$domains_cluster

language_domains_social_solidarity <- domains %>% dplyr::select(making_friends,happy_relationships,accepted_pni,talking_villages,talking_kolonia,talking_neighbors,us_relatives,domains_cluster)
names(language_domains_social_solidarity) <-c("Making friends","Feeling happy in your relationships","Being accepted in Pohnpei","Talking with people in the sections of Pohnpei","Talking with people in Kolonia","Talking with your neighbors","Speaking with relatives who live in the US","Domain PAM cluster")
cols <- c(1:7)
language_domains_social_solidarity[cols] <- lapply(language_domains_social_solidarity[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))
#language_domains_social_solidarity_likert <- likert(language_domains_social_solidarity)
#plot(language_domains_social_solidarity_likert)

language_domains_social_solidarity_gathered <- language_domains_social_solidarity %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_social_solidarity_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge",width=0.7)  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="Social solidarity domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 35, hjust = 1,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMsocialsolidaritydomains.png",width=8,height=8,units="in")

# occupation
language_domains_occupation <- domains %>% dplyr::select(being_successful,getting_money,good_job,domains_cluster)
names(language_domains_occupation) <-c("Being successful","Getting money","Getting a good job","Domain PAM cluster")
cols <- c(1:3)
language_domains_occupation[cols] <- lapply(language_domains_occupation[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_occupation_gathered <- language_domains_occupation %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_occupation_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="Occupation domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMoccupationdomains.png",width=8.5,height=6,units="in")

# education
language_domains_education <- domains %>% dplyr::select(good_education,writing,talking_teachers,friends_school,domains_cluster)
names(language_domains_education) <-c("Getting a good education","Writing","Talking with teachers","Talking with friends from school","Domain PAM cluster")
cols <- c(1:4)
language_domains_education[cols] <- lapply(language_domains_education[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_education_gathered <- language_domains_education %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_education_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="Education domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 25, hjust = 1,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMeducationdomains.png",width=8,height=6,units="in")

# media
language_domains_media <- domains %>% dplyr::select(reading,radio,tv,facebook,domains_cluster)
names(language_domains_media) <-c("Reading","Listening to the radio","Watching TV","Using Facebook","Domain PAM cluster")
cols <- c(1:4)
language_domains_media[cols] <- lapply(language_domains_media[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_media_gathered <- language_domains_media %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_media_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="Media domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 25, hjust = 1,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMmediadomains.png",width=8,height=6,units="in")

# pohnpei-specific 
language_domains_pni <- domains %>% dplyr::select(funerals,kamadipw,sakau,talking_chief,talking_gov,church,domains_cluster)
names(language_domains_pni) <-c("Attending funerals","Attending a kamadipw","Drinking sakau en Pohnpei", "Talking with a kaunen kousapw","Talking with government officials","Going to church","Domain PAM cluster")
cols <- c(1:6)
language_domains_pni[cols] <- lapply(language_domains_pni[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_pni_gathered <- language_domains_pni %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_pni_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="Pohnpei-specific domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMpnispecificdomains.png",width=8,height=6,units="in")

# general

language_domains_general <- domains %>% dplyr::select(store,domains_cluster)
names(language_domains_general) <-c("Going to the store","Domain PAM cluster")
cols <- c(1)
language_domains_general[cols] <- lapply(language_domains_general[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_general_gathered <- language_domains_general %>% gather(key="domain",value="language",-"Domain PAM cluster")

ggplot(language_domains_general_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge",width=0.5)  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance by PAM cluster", subtitle="General domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents") + facet_grid(~`Domain PAM cluster`)
ggsave("PAMgeneraldomains.png",width=6,height=6,units="in")


#### agreements

# questions for both versions

agreement_pre <- survey_data %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni) # not pni_respect, pni_unfashionable, (ones only in both versions)
agreement2_pre <- survey_data %>% dplyr::filter(survey_version != "1") %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)  # ones only in version 2
agreement<-kNN(agreement_pre,k=17,weightDist=T) # sqrt(301)
agreement2<-kNN(agreement2_pre,k=13,weightDist=T) # different n
agreement <- agreement %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni)
agreement2 <- agreement2 %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)




## for agreement
agreement.dist <- daisy(agreement,metric="gower")
agreement.mds <- cmdscale(agreement.dist)

asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- cluster::pam(agreement.dist, k)$silinfo$avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

agreement.pam <- pam(agreement.dist, k=2)

# plot
agreement.mds.df <- as.data.frame(agreement.mds)
agreement.mds.df$agreement_cluster <- as.factor(agreement.pam$clustering)

ggplot(agreement.mds.df,aes(x=V1,y=V2,color=agreement_cluster)) + geom_point() + theme_ipsum() +
  labs(title="MDS plot for agreement questions",subtitle="Colored by PAM cluster") + scale_color_brewer(palette="Set1")
ggsave("mdsagreementpam.png",width=8,height=6,units="in")

# likert plots grouped agreement_cluster
# multilingualism
library(sjPlot)
library(sjmisc)
agreement$agreement_cluster <- as.factor(agreement.pam$clustering)
language_multilingualism <- agreement %>% filter(agreement_cluster == "1")%>% dplyr::select(local_lang,english_more_import_local,pni_more_import_eng,many_lang_easy,many_lang_important,one_lang_life_diff,eng_more_import_pni,all_lang_live_together,choose_pni,choose_eng)
names(language_multilingualism) <-c("It is important to know a local language","It is more important to know English than local languages", "It is more important to know Pohnpeian than English","Knowing many languages is easy","Knowing many languages is important","Knowing only one language makes life difficult","It is more important to know English than Pohnpeian", "English, Pohnpeian, and other Micronesian languages can live together in Pohnpei","If I had to choose only one language to speak, I would choose Pohnpeian", "If I had to choose only one language to speak, I would choose English")
#cols <- c(1:10)
#language_multilingualism[cols] <- lapply(language_multilingualism[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_multilingualism <- likert(language_multilingualism,grouping=agreement$agreement_cluster)
#plot(likert_language_multilingualism)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_multilingualism,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMmultilingualismlikert1.png",width=9,height=8,units="in")
# cluster 2
language_multilingualism <- agreement %>% filter(agreement_cluster == "2")%>% dplyr::select(local_lang,english_more_import_local,pni_more_import_eng,many_lang_easy,many_lang_important,one_lang_life_diff,eng_more_import_pni,all_lang_live_together,choose_pni,choose_eng)
names(language_multilingualism) <-c("It is important to know a local language","It is more important to know English than local languages", "It is more important to know Pohnpeian than English","Knowing many languages is easy","Knowing many languages is important","Knowing only one language makes life difficult","It is more important to know English than Pohnpeian", "English, Pohnpeian, and other Micronesian languages can live together in Pohnpei","If I had to choose only one language to speak, I would choose Pohnpeian", "If I had to choose only one language to speak, I would choose English")
cols <- c(1:10)
#language_multilingualism[cols] <- lapply(language_multilingualism[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_multilingualism <- likert(language_multilingualism,grouping=agreement$agreement_cluster)
#plot(likert_language_multilingualism)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_multilingualism,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)
ggsave("PAMmultilingualismlikert2.png",width=9,height=8,units="in")

# identity
language_identity <- agreement %>% filter(agreement_cluster == "1")%>% dplyr::select(english_smarter,sad.pni.no.pni,sad_pni_no_eng,sad_pni_abroad,sad_eng_abroad,youths_bad_pni, youths_bad_eng, micros_need_eng, pnis_need_eng, kolonia_need_eng)
names(language_identity) <-c("People who speak English are smarter","I feel sad for people in Pohnpei who don't know Pohnpeian","I feel sad for people in Pohnpei who don't know English", "I fee sad for Pohnpeians who live abroad who don't know Pohnpeian","I feel sad for Pohnpeians who live abroad who don’t know English","Youths don't know how to speak Pohnpeian properly",
                             "Youths don't know how to speak English properly","All Micronesians need to know English","All Pohnpeians need to know English","Everyone who lives in Kolonia needs to know English")
#cols <- c(1:10)
#language_identity[cols] <- lapply(language_identity[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity <- likert(language_identity,grouping=agreement$agreement_cluster)
#plot(likert_language_identity)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitylikert11.png",width=7.5,height=8,units="in")
# cluster2
language_identity <- agreement %>% filter(agreement_cluster == "2")%>% dplyr::select(english_smarter,sad.pni.no.pni,sad_pni_no_eng,sad_pni_abroad,sad_eng_abroad,youths_bad_pni, youths_bad_eng, micros_need_eng, pnis_need_eng, kolonia_need_eng)
names(language_identity) <-c("People who speak English are smarter","I feel sad for people in Pohnpei who don't know Pohnpeian","I feel sad for people in Pohnpei who don't know English", "I fee sad for Pohnpeians who live abroad who don't know Pohnpeian","I feel sad for Pohnpeians who live abroad who don’t know English","Youths don't know how to speak Pohnpeian properly",
                             "Youths don't know how to speak English properly","All Micronesians need to know English","All Pohnpeians need to know English","Everyone who lives in Kolonia needs to know English")
#cols <- c(1:10)
#language_identity[cols] <- lapply(language_identity[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity <- likert(language_identity,grouping=agreement$agreement_cluster)
#plot(likert_language_identity)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitylikert12.png",width=7.5,height=8,units="in")

language_identity2 <- agreement %>%filter(agreement_cluster == "1")%>% dplyr::select(micro_youth_like_eng,micro_older_like_eng,pni_older_like_pni,pni_youth_like_pni,pni_important_pni,feel_positive_pni,to_be_pni_speak_pni,cant_pni_no_pni)

names(language_identity2) <-c("Micronesian young people like to speak English","Older Micronesians like to speak English","Older Pohnpeians like to speak Pohnpeian","Pohnpeian young people like to speak Pohnpeian","Pohnpeian is important for Pohnpei","I have positive feelings about Pohnpeian","In order to be Pohnpeian, they have to speak Pohnpeian","Pohnpeians who can't speak Pohnpeian are not really Pohnpeian")
#cols <- c(1:8)
#language_identity2[cols] <- lapply(language_identity2[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity2 <- likert(language_identity2,grouping=agreement$agreement_cluster)
#plot(likert_language_identity2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity2,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitylikert21.png",width=7.5,height=6,units="in")
# cluster 2
language_identity2 <- agreement %>%filter(agreement_cluster == "2")%>% dplyr::select(micro_youth_like_eng,micro_older_like_eng,pni_older_like_pni,pni_youth_like_pni,pni_important_pni,feel_positive_pni,to_be_pni_speak_pni,cant_pni_no_pni)

names(language_identity2) <-c("Micronesian young people like to speak English","Older Micronesians like to speak English","Older Pohnpeians like to speak Pohnpeian","Pohnpeian young people like to speak Pohnpeian","Pohnpeian is important for Pohnpei","I have positive feelings about Pohnpeian","In order to be Pohnpeian, they have to speak Pohnpeian","Pohnpeians who can't speak Pohnpeian are not really Pohnpeian")
#cols <- c(1:8)
#language_identity2[cols] <- lapply(language_identity2[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity2 <- likert(language_identity2,grouping=agreement$agreement_cluster)
#plot(likert_language_identity2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity2,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitylikert22.png",width=8.25,height=6,units="in")
# education
#learn_pni_first,
#"People have to learn Pohnpeian before learning English"
language_education <- agreement %>%filter(agreement_cluster == "1")%>% dplyr::select(eng_pni_diff,learn_pni_first,mehn_wai_need_learn_pni,pni_simpler_eng)
names(language_education) <-c("English and Pohnpeian languages are very different","People have to learn Pohnpeian before learning English","Foreigners in Pohnpei should learn Pohnpeian","The Pohnpeian language is simpler than English")
#cols <- c(1:4)
#language_education[cols] <- lapply(language_education[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education <- likert(language_education,grouping=agreement$agreement_cluster)
#plot(likert_language_education)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_education,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMeducationlikert1.png",width=7.5,height=4,units="in")
# cluster 2
language_education <- agreement %>%filter(agreement_cluster == "2")%>% dplyr::select(eng_pni_diff,learn_pni_first,mehn_wai_need_learn_pni,pni_simpler_eng)
names(language_education) <-c("English and Pohnpeian languages are very different","People have to learn Pohnpeian before learning English","Foreigners in Pohnpei should learn Pohnpeian","The Pohnpeian language is simpler than English")
#cols <- c(1:4)
#language_education[cols] <- lapply(language_education[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education <- likert(language_education,grouping=agreement$agreement_cluster)
#plot(likert_language_education)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_education,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMeducationlikert2.png",width=7.5,height=4,units="in")



# utility
language_utility <- agreement %>%filter(agreement_cluster == "1")%>% dplyr::select(pni_jobs_pni,pni_jobs_abroad,eng_jobs_abroad,eng_jobs_pni,eng_more_val_pni)
names(language_utility) <-c("Knowing Pohnpeian can help get jobs in Pohnpei","Knowing Pohnpeian can help get jobs abroad","Knowing English can help get jobs abroad","Knowing English can help get jobs in Pohnpei","English is more valuable than Pohnpeian")
#cols <- c(1:5)
#language_utility[cols] <- lapply(language_utility[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_utility <- likert(language_utility,grouping=agreement$agreement_cluster)
#plot(likert_language_utility)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_utility,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMutilitylikert1.png",width=7,height=6,units="in")
# cluster 2
language_utility <- agreement %>%filter(agreement_cluster == "2")%>% dplyr::select(pni_jobs_pni,pni_jobs_abroad,eng_jobs_abroad,eng_jobs_pni,eng_more_val_pni)
names(language_utility) <-c("Knowing Pohnpeian can help get jobs in Pohnpei","Knowing Pohnpeian can help get jobs abroad","Knowing English can help get jobs abroad","Knowing English can help get jobs in Pohnpei","English is more valuable than Pohnpeian")
#cols <- c(1:5)
#language_utility[cols] <- lapply(language_utility[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_utility <- likert(language_utility,grouping=agreement$agreement_cluster)
#plot(likert_language_utility)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_utility,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMutilitylikert2.png",width=7,height=6,units="in")


# mish-mash
survey_data$agreement_cluster <- agreement$agreement_cluster
language_opps <- survey_data %>%filter(agreement_cluster == "1")%>% dplyr::select(pni_unfashionable,pni_respect)
names(language_opps) <-c("Pohnpeian is really unfashionable","Pohnpeian can show respect")
#cols <- c(1:2)

#language_opps[cols] <- lapply(language_opps[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_opps <- likert(language_opps,grouping=agreement$agreement_cluster)
#plot(likert_language_opps)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2,axis.textsize =0.95)
sjp.likert(language_opps,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMoppslikert1.png",width=7.5,height=2,units="in")
#cluster 2
language_opps <- survey_data %>%filter(agreement_cluster == "2")%>% dplyr::select(pni_unfashionable,pni_respect)
names(language_opps) <-c("Pohnpeian is really unfashionable","Pohnpeian can show respect")
#cols <- c(1:2)
#language_opps[cols] <- lapply(language_opps[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_opps <- likert(language_opps,grouping=agreement$agreement_cluster)
#plot(likert_language_opps)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2,axis.textsize =0.95)
sjp.likert(language_opps,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMoppslikert2.png",width=7.5,height=2,units="in")



## agreement2
agreement2.dist <- daisy(agreement2,metric="gower")
agreement2.mds <- cmdscale(agreement2.dist)

asw <- numeric(50)
for (k in 2:50)
  asw[[k]] <- cluster::pam(agreement2.dist, k)$silinfo$avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

agreement2.pam <- pam(agreement2.dist, k=2)
# plot
agreement2.mds.df <- as.data.frame(agreement2.mds)
agreement2.mds.df$agreement_cluster <- as.factor(agreement2.pam$clustering)

ggplot(agreement2.mds.df,aes(x=V1,y=V2,color=agreement_cluster)) + geom_point() + theme_ipsum() + 
  labs(title="MDS plot for version 2 only agreement questions",subtitle="Colored by PAM cluster") + scale_color_brewer(palette="Set1")
ggsave("mdsagreemen2tpam.png",width=8,height=6,units="in")

# likert plots here
agreement2$agreement_cluster <- as.factor(agreement2.pam$clustering)
# identity
language_identity_new <- agreement2 %>%filter(agreement_cluster == "1")%>% dplyr::select(micro_speak_micro,micro_speak_eng,pni_smarter,kitti_need_eng,kolonia_need_pni)
names(language_identity_new) <-c("In order to be Micronesian you have to speak a Micronesian language","In order to be Micronesian you have to speak English","People who know Pohnpeian are smarter","Everyone who lives in Kitti needs to know English","Everyone who lives in Kolonia needs to know Pohnpeian")
#cols <- c(1:5)
#language_identity_new[cols] <- lapply(language_identity_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity_new <- likert(language_identity_new,grouping=agreement2$agreement_cluster)
#plot(likert_language_identity_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity_new,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitynewlikert11.png",width=7.75,height=5,units="in")
#
language_identity_new <- agreement2 %>%filter(agreement_cluster == "2")%>% dplyr::select(micro_speak_micro,micro_speak_eng,pni_smarter,kitti_need_eng,kolonia_need_pni)
names(language_identity_new) <-c("In order to be Micronesian you have to speak a Micronesian language","In order to be Micronesian you have to speak English","People who know Pohnpeian are smarter","Everyone who lives in Kitti needs to know English","Everyone who lives in Kolonia needs to know Pohnpeian")
#cols <- c(1:5)
#language_identity_new[cols] <- lapply(language_identity_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity_new <- likert(language_identity_new,grouping=agreement2$agreement_cluster)
#plot(likert_language_identity_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity_new,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitynewlikert12.png",width=7.75,height=5,units="in")
#

##
language_identity_new2 <- agreement2 %>% filter(agreement_cluster == "1")%>%dplyr::select(kitti_need_pni,want_children_pni,want_children_eng,meing_important,want_children_meing,micro_pni_pni,eng_important_pni)
names(language_identity_new2) <-c("Everyone who lives in Kitti needs to know Pohnpeian","I want my children to speak Pohnpeian","I want my children to speak English","Meing is important for me to know","I want my children to learn Meing","All Micronesians living on Pohnpei should speak Pohnpeian","English is important for Pohnpei")
#cols <- c(1:6)
#language_identity_new2[cols] <- lapply(language_identity_new2[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity_new2 <- likert(language_identity_new2,grouping=agreement2$agreement_cluster)
#plot(likert_language_identity_new2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity_new2,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitynewlikert21.png",width=9.25,height=5,units="in")

#
language_identity_new2 <- agreement2 %>% filter(agreement_cluster == "2")%>%dplyr::select(kitti_need_pni,want_children_pni,want_children_eng,meing_important,want_children_meing,micro_pni_pni,eng_important_pni)
names(language_identity_new2) <-c("Everyone who lives in Kitti needs to know Pohnpeian","I want my children to speak Pohnpeian","I want my children to speak English","Meing is important for me to know","I want my children to learn Meing","All Micronesians living on Pohnpei should speak Pohnpeian","English is important for Pohnpei")
#cols <- c(1:6)
#language_identity_new2[cols] <- lapply(language_identity_new2[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity_new2 <- likert(language_identity_new2,grouping=agreement2$agreement_cluster)
#plot(likert_language_identity_new2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_identity_new2,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMidentitynewlikert22.png",width=9.25,height=5,units="in")


# education

language_education_new <- agreement2 %>%filter(agreement_cluster == "1")%>% dplyr::select(english_simpler_pni,pni_politer_eng,schools_teach_pni)
names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:3)
#language_education_new[cols] <- lapply(language_education_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education_new <- likert(language_education_new,grouping=agreement2$agreement_cluster)
#plot(likert_language_education_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_education_new,values = "sum.outside",geom.colors="Set2",show.prc.sign=T)

ggsave("PAMeducationnewlikert1.png",width=9.5,height=3,units="in")
#
language_education_new <- agreement2 %>%filter(agreement_cluster == "2")%>% dplyr::select(english_simpler_pni,pni_politer_eng,schools_teach_pni)
names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:3)
#language_education_new[cols] <- lapply(language_education_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education_new <- likert(language_education_new,grouping=agreement2$agreement_cluster)
#plot(likert_language_education_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_education_new,values = "sum.outside",geom.colors="Set2",show.prc.sign=F,digits=0)

ggsave("PAMeducationnewlikert2.png",width=9.5,height=3,units="in")



############# level of agreement
agreement_level_pre <- survey_data %>% dplyr::select(quiet:modern,successful:uneducated) # no greedy, attractive
agreement_level<-kNN(agreement_level_pre,k=17,weightDist=T) # sqrt(301)
agreement_level <- agreement_level %>% dplyr::select(quiet:modern,successful:uneducated)

agreement_level2_pre <- survey_data %>% filter(survey_version !="1") %>%dplyr::select(greedy,smart:educated) # no greedy, attractive
agreement_level2<-kNN(agreement_level2_pre,k=13,weightDist=T) # sqrt(159)
agreement_level2 <- agreement_level2 %>% dplyr::select(greedy,smart:educated)

cols <- c(1:26)
agreement_level[cols] <- lapply(agreement_level[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))


agreement_level.dist <- daisy(agreement_level,metric="gower")
agreement_level.mds <- cmdscale(agreement_level.dist)

asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- cluster::pam(agreement_level.dist, k)$silinfo$avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

agreement_level.pam <- pam(agreement_level.dist, k=2)
#
cols <- c(1:4)
agreement_level2[cols] <- lapply(agreement_level2[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))

agreement_level2.dist <- daisy(agreement_level2,metric="gower")
agreement_level2.mds <- cmdscale(agreement_level2.dist)

asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- cluster::pam(agreement_level2.dist, k)$silinfo$avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")

agreement_level2.pam <- pam(agreement_level2.dist, k=2)

# plot
agreement_level.mds.df <- as.data.frame(agreement_level.mds)
agreement_level.mds.df$agreement_cluster <- as.factor(agreement_level.pam$clustering)

ggplot(agreement_level.mds.df,aes(x=V1,y=V2,color=agreement_cluster)) + geom_point() + theme_ipsum() + 
  labs(title="MDS plot for level of agreement questions",subtitle="Colored by PAM cluster") + scale_color_brewer(palette="Set1")
ggsave("mdslevelagreementpam.png",width=8,height=6,units="in")

agreement_level2.mds.df <- as.data.frame(agreement_level2.mds)
agreement_level2.mds.df$agreement_cluster <- as.factor(agreement_level2.pam$clustering)

ggplot(agreement_level2.mds.df,aes(x=V1,y=V2,color=agreement_cluster)) + geom_point() + theme_ipsum() + 
  labs(title="MDS plot for level of agreement questions (version 2 only)",subtitle="Colored by PAM cluster") + scale_color_brewer(palette="Set1")
ggsave("mdslevelagreementpam2.png",width=8,height=6,units="in")


# likert plots here
agreement_level$agreement_cluster <- as.factor(agreement_level.pam$clustering)
cols<-c(1:26)
agreement_level[cols] <- lapply(agreement_level[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))

# positive # remember attractive
language_positive <- agreement_level %>%filter(agreement_cluster == "1")%>% dplyr::select(quiet,kindhearted,honest,modern,successful,peaceful)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:6)
#language_positive[cols] <- lapply(language_positive[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_positive <- likert(language_positive,grouping=agreement_level$agreement_cluster)
#plot(likert_language_positive)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_positive,values = "sum.outside",geom.colors="PiYG",show.prc.sign=T,digits=1,reverse.color=T)

ggsave("PAMpositivelikert11.png",width=8,height=3.5,units="in")

#
language_positive <- agreement_level %>%filter(agreement_cluster == "2")%>% dplyr::select(quiet,kindhearted,honest,modern,successful,peaceful)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:6)
#language_positive[cols] <- lapply(language_positive[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_positive <- likert(language_positive,grouping=agreement_level$agreement_cluster)
#plot(likert_language_positive)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_positive,values = "sum.outside",geom.colors="PiYG",show.prc.sign=T,digits=1,reverse.color=T)

ggsave("PAMpositivelikert12.png",width=8,height=3.5,units="in")


#
language_positive2 <- agreement_level %>%filter(agreement_cluster == "1")%>% dplyr::select(rich,proud,respectful,wise,cultured,humble,generous)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:7)
#language_positive2[cols] <- lapply(language_positive2[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_positive2 <- likert(language_positive2,grouping=agreement_level$agreement_cluster)
#plot(likert_language_positive2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_positive2,values = "sum.outside",geom.colors="PiYG",show.prc.sign=T,digits=1,reverse.color=T)

ggsave("PAMpositivelikert21.png",width=8,height=3.5,units="in")

#
language_positive2 <- agreement_level %>%filter(agreement_cluster == "2")%>% dplyr::select(rich,proud,respectful,wise,cultured,humble,generous)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:7)
#language_positive2[cols] <- lapply(language_positive2[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_positive2 <- likert(language_positive2,grouping=agreement_level$agreement_cluster)
#plot(likert_language_positive2)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_positive2,values = "sum.outside",geom.colors="PiYG",show.prc.sign=F,digits=0,reverse.color=T)

ggsave("PAMpositivelikert22.png",width=8,height=3.5,units="in")




# neutral
language_neutral <- agreement_level %>%filter(agreement_cluster == "1")%>% dplyr::select(feminine,masculine,young,old,patriotic)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:5)
#language_neutral[cols] <- lapply(language_neutral[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_neutral <- likert(language_neutral,grouping=agreement_level$agreement_cluster)
#plot(likert_language_neutral)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_neutral,values = "sum.outside",geom.colors="PiYG",show.prc.sign=T,digits=1,reverse.color=T)

ggsave("PAMneutrallikert1.png",width=8,height=3.5,units="in")

#
language_neutral <- agreement_level %>%filter(agreement_cluster == "2")%>% dplyr::select(feminine,masculine,young,old,patriotic)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:5)
#language_neutral[cols] <- lapply(language_neutral[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_neutral <- likert(language_neutral,grouping=agreement_level$agreement_cluster)
#plot(likert_language_neutral)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_neutral,values = "sum.outside",geom.colors="PiYG",show.prc.sign=F,digits=0,reverse.color=T)

ggsave("PAMneutrallikert2.png",width=8,height=3.5,units="in")



# negative
language_negative <- agreement_level %>%filter(agreement_cluster == "1")%>% dplyr::select(stupid,loud,badtempered,violent,poor,pretentious,showoffs,uneducated)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:8)
#language_negative[cols] <- lapply(language_negative[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_negative <- likert(language_negative,grouping=agreement_level$agreement_cluster)
#plot(likert_language_negative)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_negative,values = "sum.outside",geom.colors="PiYG",show.prc.sign=T,digits=1,reverse.color=T)

ggsave("PAMnegativelikert1.png",width=8,height=3.5,units="in")

#
language_negative <- agreement_level %>%filter(agreement_cluster == "2")%>% dplyr::select(stupid,loud,badtempered,violent,poor,pretentious,showoffs,uneducated)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:8)
#language_negative[cols] <- lapply(language_negative[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_negative <- likert(language_negative,grouping=agreement_level$agreement_cluster)
#plot(likert_language_negative)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_negative,values = "sum.outside",geom.colors="PiYG",show.prc.sign=F,digits=1,reverse.color=T)

ggsave("PAMnegativelikert2.png",width=8,height=3.5,units="in")






# only in survey 2
agreement_level2$agreement_cluster <- as.factor(agreement_level2.pam$clustering)
cols <- c(1:4)
agreement_level2[cols] <- lapply(agreement_level2[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))

language_new <- agreement_level2 %>%filter(agreement_cluster == "1")%>% dplyr::select(-agreement_cluster) #names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")

#agreement_level[cols] <- lapply(agreement_level[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))

#language_new[cols] <- lapply(language_new[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))
#likert_language_new <- likert(language_new,grouping=agreement_level2$agreement_cluster)
#plot(likert_language_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_new,values = "sum.outside",geom.colors="PiYG",show.prc.sign=F,digits=1,reverse.color=T)

ggsave("PAMlevelnewlikert1.png",width=8,height=3,units="in")

#
language_new <- agreement_level2 %>%filter(agreement_cluster == "2")%>% dplyr::select(-agreement_cluster) #names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")

#agreement_level[cols] <- lapply(agreement_level[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))

#language_new[cols] <- lapply(language_new[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))
#likert_language_new <- likert(language_new,grouping=agreement_level2$agreement_cluster)
#plot(likert_language_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.95)
sjp.likert(language_new,values = "sum.outside",geom.colors="PiYG",show.prc.sign=F,digits=1,reverse.color=T)

ggsave("PAMlevelnewlikert2.png",width=8,height=3,units="in")


