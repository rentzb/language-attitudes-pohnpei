library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

library(rstanarm)

library(VIM) 
library(survey)
library(nFactors)

library(brms)
#library(yarrr)
library(xtable)
library(questionr)
library(hrbrthemes)

library(bayesplot)
library(Cairo)

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
survey_data_good$elementary_type <- factor(survey_data_good$elementary_type,
                                    levels = c("Public school","Private school","Both"))
survey_data_good$hs_type <- factor(survey_data_good$hs_type,
                                           levels = c("Public school","Private school","None"))
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

####
# regressions
#####

#set ref levels
survey_data_good <- within(survey_data_good, age <- relevel(age, ref = "18 -- 24"))
survey_data_good <- within(survey_data_good, citizenship <- relevel(citizenship, ref = "FSM"))
#survey_data <- within(survey_data, all_fsm_eng <- relevel(all_fsm_eng, ref = "Agree"))
survey_data_good <- within(survey_data_good, birth_location <- relevel(birth_location, ref = "Pohnpei State"))
survey_data_good <- within(survey_data_good, current_muni <- relevel(current_muni, ref = "Nett"))
survey_data_good <- within(survey_data_good, time_pni <- relevel(time_pni, ref = "0 - 4 years"))
survey_data_good <- within(survey_data_good, education <- relevel(education, ref = "Not high school graduate"))
survey_data_good <- within(survey_data_good, elementary_type <- relevel(elementary_type, ref = "Public school"))
survey_data_good <- within(survey_data_good, hs_type <- relevel(hs_type, ref = "Public school"))
survey_data_good <- within(survey_data_good, elementary_lang_coded <- relevel(elementary_lang_coded, ref = "English"))
survey_data_good <- within(survey_data_good, hs_lang_coded <- relevel(hs_lang_coded, ref = "English"))

cols <- c(17) # double check for Meing
survey_data_good[cols] <- lapply(survey_data_good[cols], ordered,levels=c("Not at all","Somewhat well","Well","Very well"))

# change to deviation coding
contrasts(survey_data_good$age) = contr.sum(7)
contrasts(survey_data_good$sex) = contr.sum(2)
contrasts(survey_data_good$birth_location) = contr.sum(7)
contrasts(survey_data_good$education)  = contr.sum(6)
contrasts(survey_data_good$hs_type)  = contr.sum(3)
contrasts(survey_data_good$elementary_type)  = contr.sum(3)
contrasts(survey_data_good$meing)  = contr.sum(4)
contrasts(survey_data_good$travelled_abroad)  = contr.sum(2)
contrasts(survey_data_good$time_pni)  = contr.sum(6)

# create two scales: 1) add up all the English uses, 2) add up all the Pohnpeian uses
# two regressions: englishes ~ age + sex + birth_location + citizenship + (1|current_muni) + time_pni + travelled_abroad + education + elementary_type + hs_type + meing (add weights.trim)

survey_data_good$total_eng <- rowSums(survey_data_good[,c(16:40)] == "English")
survey_data_good$total_pni <- rowSums(survey_data_good[,c(16:40)] == "Pohnpeian")
survey_data_good$total_pif <- rowSums(survey_data_good[,c(16:40)] == "Pingelapese")
survey_data_good$total_mok <- rowSums(survey_data_good[,c(16:40)] == "Mokilese")
survey_data_good$total_kos <- rowSums(survey_data_good[,c(16:40)] == "Kosraean")
survey_data_good$total_mort <- rowSums(survey_data_good[,c(16:40)] == "Mortlockese")
survey_data_good$total_tkk <- rowSums(survey_data_good[,c(16:40)] == "Chuukese")
survey_data_good$total_other <- rowSums(survey_data_good[,c(16:40)] == "Other")

survey_data_good$parent_english <- rowSums(survey_data_good[,c(108:109)]== 1)

survey_data_good$total_sum_other <- survey_data_good$total_mok + survey_data_good$total_pif+survey_data_good$total_other+survey_data_good$total_tkk+survey_data_good$total_mort+survey_data_good$total_kos

summary(survey_data_good$total_eng)
summary(survey_data_good$total_pni)
summary(survey_data_good$total_pif)
summary(survey_data_good$total_mok)
summary(survey_data_good$total_sum_other)


# pirate plots
library(yarrr)
pirateplot(data=survey_data_good,total_eng~education,theme=2)
ggsave("engRDIgender.png",width=8,height=6,units="in")

## eng total
eng_total.blmer = stan_lmer(total_eng ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village), data=survey_data_good,weights=survey.weights.trim,
                            prior_intercept = normal(0, .5), #cauchy has fatest tail of normal, student_t
                            #prior_intercept = student_t(df=14,location=13, scale=4),
                            prior = normal(0,.5), # should this be cauchy too?
                            prior_covariance = decov(regularization = 2),
                            chains = 4,
                            iter = 2000,adapt_delta=0.99999)

eng_total.glmer.neg_binom = stan_glmer(total_eng ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village), data=survey_data_good,weights=survey.weights.trim,
                                       prior_intercept = normal(0, .5), #cauchy has fatest tail of normal, student_t
                                       #prior_intercept = student_t(df=14,location=13, scale=4),
                                       prior = normal(0,.5), # should this be cauchy too?
                                       prior_covariance = decov(regularization = 2),
                                       chains = 4,
                                       iter = 2000,adapt_delta=0.99999,family=poisson,warmup=1000)

eng.gamma <- lme4::glmer(total_eng ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village),
                   data=survey_data_good,weights=survey.weights.trim,family=poisson)
summary(eng.gamma)

summary(eng_total.blmer)
launch_shinystan(eng_total.blmer)
summary(eng_total.glmer.neg_binom)
launch_shinystan(eng_total.glmer.neg_binom)


pni_total.blmer = stan_lmer(total_pni ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village), data=survey_data_good,weights=survey.weights.trim,
                            prior_intercept = normal(0,0.5), #cauchy has fatest tail of normal, student_t
                            #prior_intercept = student_t(df=14,location=13, scale=4),
                            prior = normal(0,0.5), # should this be cauchy too?
                            prior_covariance = decov(regularization = 2),
                            chains = 4,
                            iter = 2000,adapt_delta=0.99999)

pni_total.blmer.neg.binom = stan_glmer(total_pni ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village), data=survey_data_good,weights=survey.weights.trim,
                            prior_intercept = normal(0,0.5), #cauchy has fatest tail of normal, student_t
                            #prior_intercept = student_t(df=14,location=13, scale=4),
                            prior = normal(0,0.5), # should this be cauchy too?
                            prior_covariance = decov(regularization = 2),
                            chains = 4,
                            iter = 2000,adapt_delta=0.99999,family=poisson)

summary(pni_total.blmer)
summary(pni_total.blmer.neg.binom)
launch_shinystan(pni_total.blmer)
launch_shinystan(pni_total.blmer.neg.binom)

other_total.blmer.neg.binom = stan_glmer(total_sum_other ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +meing+(1|current_muni/current_village), data=survey_data_good,weights=survey.weights.trim,
                            prior_intercept = normal(0, 0.5), #cauchy has fatest tail of normal, student_t
                            #prior_intercept = student_t(df=14,location=13, scale=4),
                            prior = normal(0,0.5), # should this be cauchy too?
                            prior_covariance = decov(regularization = 2),
                            chains = 4,
                            iter = 2000,adapt_delta=0.99999,family=neg_binomial_2())


summary(other_total.blmer.neg.binom)
launch_shinystan(other_total.blmer.neg.binom)

summary(survey_data_good$total_eng)
sd(survey_data_good$total_eng)

summary(survey_data_good$total_pni)
sd(survey_data_good$total_pni)

summary(survey_data_good$total_sum_other)
sd(survey_data_good$total_sum_other)

ggplot(survey_data_good, aes(total_eng)) + geom_density() 
ggplot(survey_data_good, aes(total_pni)) + geom_density()
ggplot(survey_data_good, aes(total_sum_other)) + geom_density()


### plots

#eng
theme_set(theme_ipsum(grid="X"))
bayesplot::color_scheme_set("darkgray")

posterior <- as.matrix(eng_total.glmer.neg_binom)#eng_total.blmer)

plot_title <- ggtitle("Age & Gender",
                      "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","age1","age2","age3","age4","age5","age6","sex1"), 
           prob=0.95) + plot_title + ggplot2::xlab("Log count of English choices")
ggsave("engAgeGender.png",width=8,height=6,units="in")


plot_title2 <- ggtitle("Birth Location & Travel Abroad",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","birth_location1","birth_location2", "birth_location3", "birth_location4","birth_location5","birth_location6","travelled_abroad1"), 
           prob = 0.95) + plot_title2+ ggplot2::xlab("Log count of English choices")
ggsave("engBirthTravel.png",width=8,height=6,units="in")


plot_title3 <- ggtitle("Current Municipality",
                       "with medians and 95% intervals")

mcmc_areas(posterior, 
           pars = c("b[(Intercept) current_muni:Kitti]","b[(Intercept) current_muni:Madolenihmw]", "b[(Intercept) current_muni:Sokehs]", "b[(Intercept) current_muni:Uh]", "b[(Intercept) current_muni:Nett]"), 
           prob = 0.95) + plot_title3+ ggplot2::xlab("Log count of English choices")
ggsave("engMuni.png",width=8,height=6,units="in")


plot_title4 <- ggtitle("Years on PNI & Meing",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","time_pni1","time_pni2", "time_pni3", "time_pni4","time_pni5","meing1","meing2","meing3"), 
           prob = 0.95) + plot_title4+ ggplot2::xlab("Log count of English choices")
ggsave("engYearsMeing.png",width=8,height=6,units="in")


plot_title5 <- ggtitle("Education Level & Type of Schools",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","education1","education2","education3","education4","education5","elementary_type1","elementary_type2","hs_type1","hs_type2"), # add interaction 
           prob = 0.95) + plot_title5+ ggplot2::xlab("Log count of English choices")
ggsave("engEducation.png",width=8,height=6,units="in")

####
# PNI
posterior <- as.matrix(pni_total.blmer)
posterior <- as.matrix(pni_total.blmer.neg.binom)


plot_title <- ggtitle("Age & Gender",
                      "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","age1","age2","age3","age4","age5","age6","sex1"), 
           prob=0.95) + plot_title + ggplot2::xlab("Log count of Pohnpeian choices")
ggsave("pniAgeGender.png",width=8,height=6,units="in")


plot_title2 <- ggtitle("Birth Location & Travel Abroad",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","birth_location1","birth_location2", "birth_location3", "birth_location4","birth_location5","birth_location6","travelled_abroad1"), 
           prob = 0.95) + plot_title2+ ggplot2::xlab("Log count of Pohnpeian choices")
ggsave("pniBirthTravel.png",width=8,height=6,units="in")


plot_title3 <- ggtitle("Current Municipality",
                       "with medians and 95% intervals")

mcmc_areas(posterior, 
           pars = c("b[(Intercept) current_muni:Kitti]","b[(Intercept) current_muni:Madolenihmw]", "b[(Intercept) current_muni:Sokehs]", "b[(Intercept) current_muni:Uh]", "b[(Intercept) current_muni:Nett]"), 
           prob = 0.95) + plot_title3+ ggplot2::xlab("Log count of Pohnpeian choices")
ggsave("pniMuni.png",width=8,height=6,units="in")


plot_title4 <- ggtitle("Years on PNI & Meing",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","time_pni1","time_pni2", "time_pni3", "time_pni4","time_pni5","meing1","meing2","meing3"), 
           prob = 0.95) + plot_title4+ ggplot2::xlab("Log count of Pohnpeian choices")
ggsave("pniYearsMeing.png",width=8,height=6,units="in")


plot_title5 <- ggtitle("Education Level & Type of Schools",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","education1","education2","education3","education4","education5","elementary_type1","elementary_type2","hs_type1","hs_type2"), # add interaction 
           prob = 0.95) + plot_title5+ ggplot2::xlab("Log count of Pohnpeian choices")
ggsave("pniEducation.png",width=8,height=6,units="in")

#####
# All other
posterior <- as.matrix(other_total.blmer.neg.binom)

plot_title <- ggtitle("Age & Gender",
                      "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","age1","age2","age3","age4","age5","age6","sex1"), 
           prob=0.95) + plot_title + ggplot2::xlab("Log count of other language choices")
ggsave("otherAgeGender.png",width=8,height=6,units="in")


plot_title2 <- ggtitle("Birth Location & Travel Abroad",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","birth_location1","birth_location2", "birth_location3", "birth_location4","birth_location5","birth_location6","travelled_abroad1"), 
           prob = 0.95) + plot_title2+ ggplot2::xlab("Log count of other language choices")
ggsave("otherBirthTravel.png",width=8,height=6,units="in")


plot_title3 <- ggtitle("Current Municipality",
                       "with medians and 95% intervals")

mcmc_areas(posterior, 
           pars = c("b[(Intercept) current_muni:Kitti]","b[(Intercept) current_muni:Madolenihmw]", "b[(Intercept) current_muni:Sokehs]", "b[(Intercept) current_muni:Uh]", "b[(Intercept) current_muni:Nett]"), 
           prob = 0.95) + plot_title3+ ggplot2::xlab("Log count of other language choices")
ggsave("otherMuni.png",width=8,height=6,units="in")


plot_title4 <- ggtitle("Years on PNI & Meing",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","time_pni1","time_pni2", "time_pni3", "time_pni4","time_pni5","meing1","meing2","meing3"), 
           prob = 0.95) + plot_title4+ ggplot2::xlab("Log count of other language choices")
ggsave("otherYearsMeing.png",width=8,height=6,units="in")


plot_title5 <- ggtitle("Education Level & Type of Schools",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("(Intercept)","education1","education2","education3","education4","education5","elementary_type1","elementary_type2","hs_type1","hs_type2"), # add interaction 
           prob = 0.95) + plot_title5+ ggplot2::xlab("Log count of other language choices")
ggsave("otherEducation.png",width=8,height=6,units="in")


## CLMMs

# meing HCLM
#find way to add weights?
prior <- get_prior(meing ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +(1|current_muni/current_village), survey_data_good, family=cumulative("logit"), threshold="flexible",
                   internal = FALSE)
meing.hclm <- brm(meing ~ age + sex + birth_location+travelled_abroad+time_pni+education + elementary_type + hs_type +(1|current_muni/current_village), data=survey_data_good,
            family=cumulative("logit"), threshold="flexible",chains=4,iter=2000,control = list(adapt_delta = 0.999),prior=prior)
summary(meing.hclm)
#launch_shinystan(meing.hclm)



# plots
posterior <- as.matrix(meing.hclm)

plot_title <- ggtitle("Age & Gender",
                      "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("b_age1","b_age2","b_age3","b_age4","b_age5","b_age6","b_sex1"), 
           prob=0.95) + plot_title + ggplot2::xlab("Log odds of higher response")
ggsave("meingGender.png",width=8,height=6,units="in")


plot_title2 <- ggtitle("Birth Location & Travel Abroad",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("b_birth_location1","b_birth_location2", "b_birth_location3", "b_birth_location4","b_birth_location5","b_birth_location6","b_travelled_abroad1"), 
           prob = 0.95) + plot_title2+ ggplot2::xlab("Log odds of higher response")
ggsave("meingBirthTravel.png",width=8,height=6,units="in")


plot_title3 <- ggtitle("Current Municipality",
                       "with medians and 95% intervals")

mcmc_areas(posterior, 
           pars = c("r_current_muni[Kitti,Intercept]","r_current_muni[Madolenihmw,Intercept]", "r_current_muni[Sokehs,Intercept]", "r_current_muni[Uh,Intercept]","r_current_muni[Nett,Intercept]"), 
           prob = 0.95) + plot_title3+ ggplot2::xlab("Log odds of higher response")
ggsave("meingMuni.png",width=8,height=6,units="in")


plot_title4 <- ggtitle("Years on PNI",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("b_time_pni1","b_time_pni2", "b_time_pni3", "b_time_pni4","b_time_pni5"), 
           prob = 0.95) + plot_title4+ ggplot2::xlab("Log odds of higher response")
ggsave("meingYears.png",width=8,height=6,units="in")


plot_title5 <- ggtitle("Education Level & Type of Schools",
                       "with medians and 95% intervals")
mcmc_areas(posterior, 
           pars = c("b_education1","b_education2","b_education3","b_education4","b_education5","b_elementary_type1","b_elementary_type2","b_hs_type1","b_hs_type2"), # add interaction 
           prob = 0.95) + plot_title5+ ggplot2::xlab("Log odds of higher response")
ggsave("meingEducation.png",width=8,height=6,units="in")


