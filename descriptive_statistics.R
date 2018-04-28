library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

#library(rstanarm)

library(VIM) 
library(survey)
library(nFactors)

#library(brms)
#library(yarrr)
library(xtable)
library(questionr)
library(reporttools)
library(hrbrthemes)
library(likert)

#library(bayesplot)

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
survey_data_imputed<-kNN(regression_data,k=17,weightDist=T) # sqrt(17)
survey_data_imputed
summary(survey_data_imputed$hs_type_imp)
survey_data_good <- survey_data_imputed 
survey_data_good$education <- factor(survey_data_good$education,
                                     levels = c("Not high school graduate","High school, diploma, or GED","Some college, no degree","Associate degree","Bachelor's degree","Advanced degree"))
survey_data_good$time_pni <- factor(survey_data_good$time_pni,
                                    levels = c("0 - 4 years","5 - 9 years","10 - 19 years","20 - 29 years","30 - 39 years","40 or more years"))
cols <- c(17) # double check for Meing
survey_data_good[cols] <- lapply(survey_data_good[cols], ordered,levels=c("Not at all","Somewhat well","Well","Very well"))


#########
# poststratify

survey.unweighted <-svydesign(ids=~1, data=survey_data_good)

sex.dist <- data.frame(sex = c("Male", "Female"),
                       Freq = nrow(survey_data_good) * c(0.5064819, 0.4935181))
age.dist <- data.frame(age = c("65 -- 74", "18 -- 24","25 -- 34", "35 -- 44", "45 -- 54", "55 -- 64","75+"),
                       Freq = nrow(survey_data_good) * c(0.02,0.139, 0.138,0.119,0.096,0.054,0.01)) # equals 55%, double check this
muni.dist <- data.frame(current_muni = c("Nett","Kitti","Madolenihmw","Sokehs","Uh"),
                        Freq = nrow(survey_data_good) * c(0.3654161,0.1859997,0.1657324,0.1910881,0.09176369))

citizenship.dist <- data.frame(citizenship = c("FSM","Other","Palau","Philippines","RMI","USA"),
                               Freq = nrow(survey_data_good) * c(0.9524275,0.02638764,0.001322257,0.001546466,0.0007761074,0.003621834))

#education.dist <- data.frame(education = c("1No schooling completed","2Some high school, no diploma","3Some college, no degree","4Associate degree","5Master's degree","6Doctorate degree","7Kingergarten - 8th grade","8High school, diploma, or GED","9Trade/Technical/vocational training","10Bachelor's degree","11Professional degree (JD, MD, etc.)"),
# Freq = nrow(survey_data_good) * c(10.262,20.2024766,30.1211962,40.0840469,50.01146094,60.002832301,70.1769859,80.08734027,90.003622711,100.04413121,110.003029904))

education.dist <- data.frame(education = c("Not high school graduate","High school, diploma, or GED","Some college, no degree","Associate degree","Bachelor's degree","Advanced degree"),
                             Freq = nrow(survey_data_good) * c(0.6414625,0.08734027,0.1248189,0.0840469,0.04413121,0.01732314))

# why won't this now work? something was lost (missing some people)
survey.svy.rake <- rake(design = survey.unweighted,
                        sample.margins = list(~sex, ~age,~citizenship,~current_muni,~education),
                        population.margins = list(sex.dist, age.dist,citizenship.dist,muni.dist,education.dist))
summary(weights(survey.svy.rake))
survey.svy.rake.trim <- trimWeights(survey.svy.rake,lower=0.2,upper=5,strict=T)
summary(weights(survey.svy.rake.trim))

survey.weights.trim <- as.numeric(weights(survey.svy.rake.trim)) ## use this for weights
survey.weights <- as.numeric(weights(survey.svy.rake))
survey_data_good$weights.trim <- survey.weights.trim
survey_data_good$weights <- survey.weights

############
# summary statitistics of respondents

vars1 <- survey_data_good[,c("sex","age","birth_location","current_muni","education","elementary_type","hs_type","travelled_abroad")]
cap1 <- "Non-weighted respondent demographics"
tableNominal(vars = vars1, cap = cap1, vertical = FALSE, lab ="tab:nominal1", longtable = T)

vars2 <- survey_data_good[,c("sex","age","birth_location","education")]
cap2 <- "Non-weighted respondent demographics by current municipality"


tableNominal(vars = vars2, group = survey_data_good$current_muni, miss.cat = 3,cumsum=F, print.pval = "none", cap = cap2, lab = "tab:nominal2",longtable = T)

vars3 <- survey_data_good[,c("education","sex")]
cap3 <- "Non-weighted respondent demographics by age"

tableNominal(vars = vars3, group = survey_data_good$age, print.pval = "none", cap = cap3, lab = "tab:nominal3",longtable = F)

## actual language use summary
# number of languages spoken
language_use <- language_use %>% mutate(language_total = number_L1 + number_L2_well + number_L2_little)
summary(language_use$language_total)
vars4 <- language_use[,c("number_L1","number_L2_well","number_L2_little","language_total")]
cap4 <- "Number of languages spoken by respondents"

tableContinuous(vars = vars4,print.pval = "none", cap = cap4, lab = "tab:cont1",longtable = F)

# weighted tables
wtd.table(survey_data_good$sex,weights=survey_data_good$weights.trim)
wtd.table(survey_data_good$age,weights=survey_data_good$weights.trim)
wtd.table(survey_data_good$birth_location,weights=survey_data_good$weights.trim)
wtd.table(survey_data_good$current_muni,weights=survey_data_good$weights.trim)
wtd.table(survey_data_good$education,weights=survey_data_good$weights.trim)


# language use 
language_use <- read.csv("language_use_combined.csv",na.string=c(""," ","NA","N/A"))

## actual language use summary
# number of languages spoken
language_use <- language_use %>% mutate(language_total = number_L1 + number_L2_well + number_L2_little)
summary(language_use$language_total)
vars4 <- language_use[,c("number_L1","number_L2_well","number_L2_little","language_total")]
cap4 <- "Number of languages spoken by respondents"

tableContinuous(vars = vars4,print.pval = "none", cap = cap4, lab = "tab:cont1",longtable = F)

## slit language use variables to get summary statitists
L1 <- language_use %>% separate(L1,c("f1","f2"),sep=",") %>% dplyr::select(f1,f2)
L1 <- L1 %>% gather(key=domain,value=L1,na.rm=T) %>% dplyr::select(L1)
L1$L1 <- stringr::str_trim(L1$L1,side="both")
L1$L1 <- as.factor(L1$L1)
summary(L1$L1) # make this into latex table

L2_well <- language_use %>% separate(L2_well,c("f1","f2","f3","f4","f5","f6"),sep=",") %>% dplyr::select(f1,f2,f3,f4,f5,f6)
L2_well <- L2_well %>% gather(key=domain,value=L2_well,na.rm=T) %>% dplyr::select(L2_well)
L2_well$L2_well <- stringr::str_trim(L2_well$L2_well,side="both")
L2_well$L2_well <- as.factor(L2_well$L2_well)
summary(L2_well$L2_well)

L2_little <- language_use %>% separate(L2_little,c("f1","f2","f3","f4","f5"),sep=",") %>% dplyr::select(f1,f2,f3,f4,f5)
L2_little <- L2_little %>% gather(key=domain,value=L2_little,na.rm=T) %>% dplyr::select(L2_little)
L2_little$L2_little <- stringr::str_trim(L2_little$L2_little,side="both")
L2_little$L2_little <- as.factor(L2_little$L2_little)
xtable(L2_little)

ggplot(L1,aes(x=L1)) + geom_bar() + theme_ipsum(grid="Y") + labs(title="Count of first language responses") + xlab("First language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("L1.png",width=8,height=6,units="in")

ggplot(L2_well,aes(x=L2_well)) + geom_bar() + theme_ipsum(grid="Y") + labs(title="Count of second languages spoken well") + xlab("Language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("L2well.png",width=8,height=6,units="in")

ggplot(L2_little,aes(x=L2_little)) + geom_bar() + theme_ipsum(grid="Y") + labs(title="Count of second languages spoken a little") + xlab("Language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("L2little.png",width=8,height=6,units="in")

varsLang <- language_use[,c("number_L1","number_L2_well","number_L2_little","language_total")]
cap4 <- "Number of languages spoken by respondents"

tableContinuous(vars = vars4,print.pval = "none", cap = cap4, lab = "tab:cont1",longtable = F)


# mother lang
mother <- language_use %>% separate(language_mother,c("f1","f2","f3","f4","f5","f6"),sep=",") %>% dplyr::select(f1,f2,f3,f4,f5,f6)
mother <- mother %>% gather(key=domain,value=mother,na.rm=T) %>% dplyr::select(mother)
mother$mother <- stringr::str_trim(mother$mother,side="both")
mother$mother <- as.factor(mother$mother)
summary(mother)
levels(mother$mother)[levels(mother$mother)=="Mwortlockese"] <- "Mortlockese"


ggplot(mother,aes(x=mother)) + geom_bar() + theme_ipsum(grid="Y") +
  labs(title="Count of respondent's mother's languages") +
  xlab("Language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("mother.png",width=8,height=6,units="in")

# languages_want_know_better
languages_want_know_better <- language_use %>% separate(languages_want_know_better,c("f1","f2","f3","f4","f5","f6"),sep=",") %>% dplyr::select(f1,f2,f3,f4,f5,f6)
languages_want_know_better <- languages_want_know_better %>% gather(key=domain,value=languages_want_know_better,na.rm=T) %>% dplyr::select(languages_want_know_better)
languages_want_know_better$languages_want_know_better <- stringr::str_trim(languages_want_know_better$languages_want_know_better,side="both")
languages_want_know_better$languages_want_know_better <- as.factor(languages_want_know_better$languages_want_know_better)
summary(languages_want_know_better)
levels(languages_want_know_better$languages_want_know_better)[levels(languages_want_know_better$languages_want_know_better)=="Mwortlockese"] <- "Mortlockese"


ggplot(languages_want_know_better,aes(x=languages_want_know_better)) + geom_bar() + theme_ipsum(grid="Y") +
  labs(title="Count of languages respondents want to know better") +
  xlab("Language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("languagesbetter.png",width=8,height=6,units="in")

# father
father <- language_use %>% separate(language_father,c("f1","f2","f3","f4","f5","f6"),sep=",") %>% dplyr::select(f1,f2,f3,f4,f5,f6)
father <- father %>% gather(key=domain,value=father,na.rm=T) %>% dplyr::select(father)
father$father <- stringr::str_trim(father$father,side="both")
father$father <- as.factor(father$father)
summary(father)
levels(father$father)[levels(father$father)=="Mwortlockese"] <- "Mortlockese"


ggplot(father,aes(x=father)) + geom_bar() + theme_ipsum(grid="Y") +
  labs(title="Count of respondent's father's languages") +
  xlab("Language")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of respondents")
ggsave("father.png",width=8,height=6,units="in")

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

data.parents <- cbind.fill(mother$mother,father$father)

data.parents<-as.data.frame(data.parents)
names(data.parents) <- c("Mother's languages","Father's languages")
parents.gathered <- data.parents %>% gather(key=parent,value=language,na.rm=T)
parents.gathered$parent <- as.factor(parents.gathered$parent)
parents.gathered$language <- as.factor(parents.gathered$language)

levels(parents.gathered$language)[levels(parents.gathered$language)=="Outer island Yapese"] <- "Outer Island Yapese"


colors <- colorRampPalette(brewer.pal(8, "Set1"))
myPal <- colors(length(unique(parents.gathered$language)))

ggplot(parents.gathered,aes(x=parent,fill=language)) + geom_bar(stat="count",position=position_dodge(width=.60),size=0.3,width=0.7)  +
  theme_ipsum(grid="Y") + scale_fill_manual(values = myPal) + labs(title="Parents' languages") + xlab("Parent")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=12),legend.position="bottom",legend.text=element_text(size=11))+ ylab("Number of respondents")
ggsave("parentslanguages.png",width=9,height=7.5,units="in")

## compare L1, L2, L2 well
data.languages <- cbind.fill(L1$L1,L2_well$L2_well,L2_little$L2_little)

data.languages<-as.data.frame(data.languages)
names(data.languages) <- c("L1","L2 spoken well", "L2 spoken a little")
data.languages <- data.languages %>% gather(key=type,value=language,na.rm=T)
data.languages$type <- as.factor(data.languages$type)
data.languages$language <- as.factor(data.languages$language)

levels(parents.gathered$language)[levels(parents.gathered$language)=="Outer island Yapese"] <- "Outer Island Yapese"


colors <- colorRampPalette(brewer.pal(8, "Set1"))
myPal <- colors(length(unique(data.languages$language)))

ggplot(data.languages,aes(x=type,fill=language)) + geom_bar(stat="count",position=position_dodge(width=.60),size=0.3,width=0.7)  +
  theme_ipsum(grid="Y") + scale_fill_manual(values = myPal) + labs(title="Reported languages spoken") + xlab("Parent")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=12),legend.position="bottom",legend.text=element_text(size=11))+ ylab("Number of respondents")
ggsave("languagestypes.png",width=8,height=7.5,units="in")

# meing
varsLang2 <- survey_data_good[,c("meing","age")]

tableNominal(vars = varsLang2, print.pval = "none", cap = cap1, lab = "tab:meing",longtable = F)

ggplot(survey_data_good,aes(x=meing)) + geom_bar(width=0.25) + theme_ipsum(grid="Y") + labs(title="Reported level of Meing knowledge") + xlab("Reported Meing knowledge")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+ ylab("Number of respondents")
ggsave("meing.png",width=6,height=6,units="in")

#### lang-background domains
language_use_complete <- read.csv("language_use_complete.csv",na.strings=c(""," ","NA")) # remove french, italian,russian,chinese and a couple others in college, check for extra spaces

#education
library(RColorBrewer)
language_use_education <- language_use_complete %>% dplyr::select(college_lang,elementary_lang,hs_lang,school_lang)
names(language_use_education) <-c("In college, what languages did your teachers use in class?","In K-8, what languages did your teachers use in class?","In high school, what languages did your teachers use in class?","At school, what languages do you use?")


language_use_education_gathered <- language_use_education %>% gather(key="domain",value="language")
language_use_education_gathered$domain <- as.factor(language_use_education_gathered$domain)
language_use_education_gathered$language <- as.factor(language_use_education_gathered$language)
colors <- colorRampPalette(brewer.pal(8, "Set1"))
myPal <- colors(length(unique(language_use_education_gathered$language)))
language_use_education_gathered.all <- rbind(language_use_education_gathered, cbind(expand.grid(domain=levels(language_use_education_gathered$domain), language=NA)))


ggplot(language_use_education_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position=position_dodge(width=.60),size=0.3,width=0.7)  +
  theme_ipsum(grid="Y") + scale_fill_manual(values = myPal) + labs(title="Language use", subtitle="Education domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 33, hjust = 1,size=11),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents")
ggsave("languageuseeducationdomains.png",width=9,height=8,units="in")

# family
language_use_relationships <- language_use_complete %>% dplyr::select(family_lang,friends_lang,home_lang)
names(language_use_relationships) <-c("When you talk to your family what languages do you use?","When you talk to your friends what languages do you use?","At home, what languages do you use?")


language_use_relationships_gathered <- language_use_relationships %>% gather(key="domain",value="language")
language_use_relationships_gathered$domain <- as.factor(language_use_relationships_gathered$domain)
language_use_relationships_gathered$language <- as.factor(language_use_relationships_gathered$language)

colors <- colorRampPalette(brewer.pal(8, "Set1"))
myPal <- colors(length(unique(language_use_relationships_gathered$language)))

ggplot(language_use_relationships_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_manual(values = myPal) + labs(title="Language use", subtitle="Relationships domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 20, hjust = 1,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents")
ggsave("languageuserelationshipsdomains.png",width=9,height=8,units="in")



# work and foreigners
language_use_work <- language_use_complete %>% dplyr::select(work_lang,foreigners_lang)
names(language_use_work) <-c("At work, what languages do you use?","When you talk to foreigners what languages do you use?")


language_use_work_gathered <- language_use_work %>% gather(key="domain",value="language")
language_use_work_gathered$domain <- as.factor(language_use_work_gathered$domain)
language_use_work_gathered$language <- as.factor(language_use_work_gathered$language)

colors <- colorRampPalette(brewer.pal(8, "Set1"))
myPal <- colors(length(unique(language_use_work_gathered$language)))

ggplot(language_use_work_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge",width=0.5)  +
  theme_ipsum(grid="Y") + scale_fill_manual(values = myPal) + labs(title="Language use", subtitle="Work and foreigners") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=10),legend.position="bottom",legend.text=element_text(size=10))+ ylab("Number of respondents")
ggsave("languageuseworkdomains.png",width=8,height=6,units="in")


######### lang use plots
language_domains_social_solidarity <- survey_data_good %>% dplyr::select(making_friends,happy_relationships,accepted_pni,talking_villages,talking_kolonia,talking_neighbors,us_relatives)
names(language_domains_social_solidarity) <-c("Making friends","Feeling happy in your relationships","Being accepted in Pohnpei","Talking with people in the sections of Pohnpei","Talking with people in Kolonia","Talking with your neighbors","Speaking with relatives who live in the US")
cols <- c(1:7)
language_domains_social_solidarity[cols] <- lapply(language_domains_social_solidarity[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))
language_domains_social_solidarity_likert <- likert(language_domains_social_solidarity)
plot(language_domains_social_solidarity_likert)

language_domains_social_solidarity_gathered <- language_domains_social_solidarity %>% gather(key="domain",value="language")

ggplot(language_domains_social_solidarity_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="Social solidarity domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Number of respondents")
ggsave("socialsolidaritydomains.png",width=8,height=6,units="in")

# occupation
language_domains_occupation <- survey_data_good %>% dplyr::select(being_successful,getting_money,good_job)
names(language_domains_occupation) <-c("Being successful","Getting money","Getting a good job")
cols <- c(1:3)
language_domains_occupation[cols] <- lapply(language_domains_occupation[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_occupation_gathered <- language_domains_occupation %>% gather(key="domain",value="language")

ggplot(language_domains_occupation_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="Occupation domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+ ylab("Number of respondents")
ggsave("occupationdomains.png",width=8,height=6,units="in")


# education
language_domains_education <- survey_data_good %>% dplyr::select(good_education,writing,talking_teachers,friends_school)
names(language_domains_education) <-c("Getting a good education","Writing","Talking with teachers","Talking with friends from school")
cols <- c(1:4)
language_domains_education[cols] <- lapply(language_domains_education[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_education_gathered <- language_domains_education %>% gather(key="domain",value="language")

ggplot(language_domains_education_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="Education domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Number of respondents")
ggsave("educationdomains.png",width=8,height=6,units="in")



# media
language_domains_media <- survey_data_good %>% dplyr::select(reading,radio,tv,facebook)
names(language_domains_media) <-c("Reading","Listening to the radio","Watching TV","Using Facebook")
cols <- c(1:4)
language_domains_media[cols] <- lapply(language_domains_media[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_media_gathered <- language_domains_media %>% gather(key="domain",value="language")

ggplot(language_domains_media_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="Media domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle =0, hjust = .5))+ ylab("Number of respondents")
ggsave("mediadomains.png",width=8,height=6,units="in")


# pohnpei-specific 
language_domains_pni <- survey_data_good %>% dplyr::select(funerals,kamadipw,sakau,talking_chief,talking_gov,church)
names(language_domains_pni) <-c("Attending funerals","Attending a kamadipw","Drinking sakau en Pohnpei", "Talking with a kaunen kousapw","Talking with government officials","Going to church")
cols <- c(1:6)
language_domains_pni[cols] <- lapply(language_domains_pni[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_pni_gathered <- language_domains_pni %>% gather(key="domain",value="language")

ggplot(language_domains_pni_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="Pohnpei-specific domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ylab("Number of respondents")
ggsave("pnispecificdomains.png",width=8,height=6,units="in")

# general

language_domains_general <- survey_data_good %>% dplyr::select(store)
names(language_domains_general) <-c("Going to the store")
cols <- c(1)
language_domains_general[cols] <- lapply(language_domains_general[cols], ordered,levels=c("Pohnpeian","Pingelapese","Mokilese","Chuukese","Kosraean","Mortlockese","Other","English"))

language_domains_general_gathered <- language_domains_general %>% gather(key="domain",value="language")

ggplot(language_domains_general_gathered,aes(x=domain,fill=language)) + geom_bar(stat="count",position="dodge")  +
  theme_ipsum(grid="Y") + scale_fill_brewer(palette="Set2") + labs(title="Language importance", subtitle="General domains") + xlab("Domain")+ 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+ ylab("Number of respondents")
ggsave("generaldomains.png",width=8,height=6,units="in")

############## Likert plots

## agreement
library(sjPlot)
library(sjmisc)
agreement_pre <- survey_data %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni) # not pni_respect, pni_unfashionable, (ones only in both versions)
agreement2_pre <- survey_data %>% dplyr::filter(survey_version != "1") %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)  # ones only in version 2
agreement<-kNN(agreement_pre,k=17,weightDist=T) # sqrt(301)
agreement2<-kNN(agreement2_pre,k=13,weightDist=T) # different n
agreement <- agreement %>% dplyr::select(local_lang:all_lang_live_together,eng_more_val_pni:cant_pni_no_pni)
agreement2 <- agreement2 %>% dplyr::select(pni_respect,micro_speak_micro:eng_important_pni)


# multilingualism
language_multilingualism <- agreement %>% dplyr::select(local_lang,english_more_import_local,pni_more_import_eng,many_lang_easy,many_lang_important,one_lang_life_diff,eng_more_import_pni,all_lang_live_together,choose_pni,choose_eng)
names(language_multilingualism) <-c("It is important to know a local language","It is more important to know English than local languages", "It is more important to know Pohnpeian than English","Knowing many languages is easy","Knowing many languages is important","Knowing only one language makes life difficult","It is more important to know English than Pohnpeian", "English, Pohnpeian, and other Micronesian languages can live together in Pohnpei","If I had to choose only one language to speak, I would choose Pohnpeian", "If I had to choose only one language to speak, I would choose English")
#cols <- c(1:10)
#language_multilingualism[cols] <- lapply(language_multilingualism[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_multilingualism <- likert(language_multilingualism)
#plot(likert_language_multilingualism)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_multilingualism,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("multilingualismlikert.png",width=9.5,height=7.5,units="in")


# identity
language_identity <- agreement %>% dplyr::select(english_smarter,sad.pni.no.pni,sad_pni_no_eng,sad_pni_abroad,sad_eng_abroad,youths_bad_pni, youths_bad_eng, micros_need_eng, pnis_need_eng, kolonia_need_eng,micro_youth_like_eng,micro_older_like_eng,pni_older_like_pni,pni_youth_like_pni,pni_important_pni,feel_positive_pni,to_be_pni_speak_pni,cant_pni_no_pni)
names(language_identity) <-c("People who speak English are smarter","I feel sad for people in Pohnpei who don't know Pohnpeian","I feel sad for people in Pohnpei who don't know English", "I fee sad for Pohnpeians who live abroad who don't know Pohnpeian","I feel sad for Pohnpeians who live abroad who don’t know English","Youths don't know how to speak Pohnpeian properly",
"Youths don't know how to speak English properly","All Micronesians need to know English","All Pohnpeians need to know English","Everyone who lives in Kolonia needs to know English","Micronesian young people like to speak English","Older Micronesians like to speak English","Older Pohnpeians like to speak Pohnpeian","Pohnpeian young people like to speak Pohnpeian","Pohnpeian is important for Pohnpei","I have positive feelings about Pohnpeian","In order to be Pohnpeian, they have to speak
Pohnpeian","Pohnpeians who can't speak Pohnpeian are not really Pohnpeian")
#cols <- c(1:18)
#language_identity[cols] <- lapply(language_identity[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity <- likert(language_identity)
#plot(likert_language_identity)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3,axis.textsize =0.9)
sjp.likert(language_identity,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("identitylikert.png",width=8.74,height=9.5,units="in")

# education
#learn_pni_first,
#"People have to learn Pohnpeian before learning English"
language_education <- agreement %>% dplyr::select(eng_pni_diff,learn_pni_first,mehn_wai_need_learn_pni,pni_simpler_eng)
names(language_education) <-c("English and Pohnpeian languages are very different","People have to learn Pohnpeian before learning English","Foreigners in Pohnpei should learn Pohnpeian","The Pohnpeian language is simpler than English")
#cols <- c(1:4)
#language_education[cols] <- lapply(language_education[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education <- likert(language_education)
#plot(likert_language_education,width=0.5)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2.9)
sjp.likert(language_education,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("educationlikert.png",width=8,height=3,units="in")



# utility
language_utility <- agreement %>% dplyr::select(pni_jobs_pni,pni_jobs_abroad,eng_jobs_abroad,eng_jobs_pni,eng_more_val_pni)
names(language_utility) <-c("Knowing Pohnpeian can help get jobs in Pohnpei","Knowing Pohnpeian can help get jobs abroad","Knowing English can help get jobs abroad","Knowing English can help get jobs in Pohnpei","English is more valuable than Pohnpeian")
#cols <- c(1:5)
#language_utility[cols] <- lapply(language_utility[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_utility <- likert(language_utility)
#plot(likert_language_utility)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2.9)
sjp.likert(language_utility,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("utilitylikert.png",width=8,height=4,units="in")

# mish-mash
language_opps <- survey_data %>% dplyr::select(pni_unfashionable,pni_respect)
names(language_opps) <-c("Pohnpeian is really unfashionable","Pohnpeian can show respect")
#cols <- c(1:2)
#language_opps[cols] <- lapply(language_opps[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_opps <- likert(language_opps)
#plot(likert_language_opps)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2.9)
sjp.likert(language_opps,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("oppslikert.png",width=8,height=2,units="in")



## new version

# identity
library(sjPlot)
library(sjmisc)
language_identity_new <- agreement2 %>% dplyr::select(micro_speak_micro,micro_speak_eng,pni_smarter,kitti_need_eng,kolonia_need_pni,kitti_need_pni,want_children_pni,want_children_eng,meing_important,want_children_meing,micro_pni_pni,eng_important_pni)
names(language_identity_new) <-c("In order to be Micronesian you have to speak a Micronesian language","In order to be Micronesian you have to speak English","People who know Pohnpeian are smarter","Everyone who lives in Kitti needs to know English","Everyone who lives in Kolonia needs to know Pohnpeian","Everyone who lives in Kitti needs to know Pohnpeian","I want my children to speak Pohnpeian","I want my children to speak English","Meing is important for me to know","I want my children to learn Meing","All Micronesians living on Pohnpei should speak Pohnpeian","English is important for Pohnpei")
#cols <- c(1:11)
#language_identity_new[cols] <- lapply(language_identity_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_identity_new <- likert(language_identity_new)
#plot(likert_language_identity_new)

set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 2.9)
sjp.likert(language_identity_new,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)
ggsave("identitynewlikert.png",width=9.5,height=7,units="in")
# education
language_education_new <- agreement2 %>% dplyr::select(english_simpler_pni,pni_politer_eng,schools_teach_pni)
names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:3)
#language_education_new[cols] <- lapply(language_education_new[cols], ordered,levels=c("Disagree","Agree"))
#likert_language_education_new <- likert(language_education_new)
#plot(likert_language_education_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_education_new,values = "sum.outside",sort.frq="pos.desc",geom.colors="Set2",show.prc.sign=T)

ggsave("educationnewlikert.png",width=9,height=3,units="in")

## level of agreement

agreement_level_pre <- survey_data %>% dplyr::select(quiet:modern,successful:uneducated) # no greedy, attractive
agreement_level<-kNN(agreement_level_pre,k=17,weightDist=T) # sqrt(301)
agreement_level <- agreement_level %>% dplyr::select(quiet:modern,successful:uneducated)

cols <- c(1:26)
agreement_level[cols] <- lapply(agreement_level[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))


agreement_level2_pre <- survey_data %>% filter(survey_version !="1") %>%dplyr::select(greedy,smart:educated) # no greedy, attractive
agreement_level2<-kNN(agreement_level2_pre,k=13,weightDist=T) # sqrt(159)
agreement_level2 <- agreement_level2 %>% dplyr::select(greedy,smart:educated)

cols <- c(1:4)
agreement_level2[cols] <- lapply(agreement_level2[cols], ordered,levels=c("Really agree","Agree somewhat","Disagree somewhat","Really disagree"))


# positive # remember attractive
language_positive <- agreement_level %>% dplyr::select(quiet,kindhearted,honest,modern,successful,peaceful,rich,proud,respectful,wise,cultured,humble,generous)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
 #                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:13)
#labels <- c("Really disagree","Disagree somewhat","Agree somewhat","Really agree")
#likert_language_positive <- likert(language_positive)
#plot(likert_language_positive)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3) #PuOr
sjp.likert(language_positive,values = "sum.outside",sort.frq="pos.desc",geom.colors="PiYG",show.prc.sign=T,reverse.colors = T)

ggsave("positivelikert.png",width=8,height=6,units="in")



# neutral
language_neutral <- agreement_level %>% dplyr::select(feminine,masculine,young,old,patriotic)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:5)
#language_neutral[cols] <- lapply(language_neutral[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_neutral <- likert(language_neutral)
#plot(likert_language_neutral)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_neutral,values = "sum.outside",sort.frq="pos.desc",geom.colors="PiYG",show.prc.sign=T,reverse.color=T)

ggsave("neutrallikert.png",width=8,height=4,units="in")



# negative
language_negative <- agreement_level %>% dplyr::select(stupid,loud,badtempered,violent,poor,pretentious,showoffs,uneducated)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:8)
#language_negative[cols] <- lapply(language_negative[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_negative <- likert(language_negative)
#plot(likert_language_negative)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_negative,values = "sum.outside",sort.frq="pos.desc",geom.colors="PiYG",show.prc.sign=T,reverse.colors = T)

ggsave("negativelikert.png",width=8,height=5,units="in")



# only in survey 1
language_only1 <- survey_data %>% dplyr::select(attractive)
#names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:1)
#language_only1[cols] <- lapply(language_only1[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_only1 <- likert(language_only1)
#plot(likert_language_only1)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_only1,values = "sum.outside",sort.frq="pos.desc",geom.colors="PiYG",show.prc.sign=T,reverse.colors = T)

ggsave("attractivelikert.png",width=8,height=1.5,units="in")




# only in survey 2
language_new <- agreement_level2 #names(language_education_new) <-c("The English language is simpler than Pohnpeian","The Pohnpeian language is more polite than the English
#                                 language","Schools on Pohnpei should teach classes in Pohnpeian")
#cols <- c(1:4)
##language_new[cols] <- lapply(language_new[cols], ordered,levels=c("Really disagree","Disagree somewhat","Agree somewhat","Really agree"))
#likert_language_new <- likert(language_new)
#plot(likert_language_new)
set_theme(base=theme_ipsum(grid="Y"),geom.label.size = 3)
sjp.likert(language_new,values = "sum.outside",sort.frq="pos.desc",geom.colors="PiYG",show.prc.sign=T,reverse.colors = T)

ggsave("levelnewlikert.png",width=8,height=4,units="in")




