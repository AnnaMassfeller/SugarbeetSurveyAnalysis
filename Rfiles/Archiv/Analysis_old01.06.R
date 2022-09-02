library(pscl)
require(nnet)
library(ivtools)
library(AER)
library("cowplot")
##file to run models for the analysis
#source("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis/Rfiles/DataPreProcessing.R")
#Run first regression
FullSample$q1_adopt <- as.factor(FullSample$q1_adopt)
FullSample$q3_info <- as.factor(FullSample$q3_info)
FullSample$FieldDist <- as.factor(FullSample$FieldDist)
FullSample$q6_col1 <- as.factor(FullSample$q6_col1)
FullSample$q6_col2 <- as.factor(FullSample$q6_col2)
FullSample$q6_col3 <- as.factor(FullSample$q6_col3)
FullSample$q7_age <- as.factor(FullSample$q7_age)
FullSample$q7_size <- as.factor(FullSample$q7_size)
FullSample$q7_farm <- as.factor(FullSample$q7_farm)
FullSample$q7_speci_select <- as.factor(FullSample$q7_speci_select)
FullSample$q7_AES <- as.factor(FullSample$q7_AES)
FullSample$q5_len_other <- as.numeric(FullSample$q5_len_other)
FullSample$info_b<-ifelse(FullSample$q3_info == "0",0,1)
FullSample$fields_b<-ifelse(FullSample$NrFields == "0",0,1)
# create also squared term sin data set



summary(m.Full <- glm(q1_adopt ~  q3_info + NrFields + age_b + farmsize_b + AES_b, 
                      data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full_mfx <-probitmfx(m.Full, data = FullSample)
m.Full_mfx

m.Full.table <- stargazer(m.Full, m.Full_mfx$fit,
                          coef = list(NULL, m.Full_mfx$mfxest[,1]),
                          se = list(NULL, m.Full_mfx$mfxest[,2]),
                          type = "html", out = "m.Full.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
                          dep.var.caption  = "Mechanical weeding yes/no",
                          column.labels = c("Odds ratio","marginal effect"),
                          covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                                               "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed",
                                               "older than 45 years", "farm size > 50 ha", "AES participation"),
                          dep.var.labels   = "", single.row=FALSE)

p.mFull_mfx <- plot_summs(m.Full_mfx, 
                          coefs = c("1-5 adopters known"="q3_info1",
                                    "6-10 adopters known"="q3_info2",
                                    "more than 10 adopters known"="q3_info3",
                                    "1-5 fields observed"="NrFields1",
                                    "6-10 fields observed"="NrFields2",
                                    "11-15 fields observed"="NrFields3",
                                    "more than 15 fields observed"="NrFields4",
                                    "older than 45 years"="age_b1"
                                    , "farm size > 50 ha"="farmsize_b1",
                                    "AES participation"="AES_b1"),
                          model.names = "Probit model on adoption decision",
                          scale = TRUE, robust = TRUE)
#+ q7_age + q7_speci_select+ q7_size + q7_AES
#q6_col1 + q6_col2 + q6_col3 +q7_speci_select+q7_age 
#+ q7_size + q7_farm  + + q7_AES
p.mFull_mfx

#par(mar = rep(2, 4))
#plot(m.Full)
#plot(m.no48)
#par(mfrow=c(1,1))
#vif(m.Full)

#correlation between peer und field?
fisher.test(table(FullSample$NrFields,FullSample$q3_info))#no dependence
chisq.test(table(FullSample$info_b, FullSample$fields_b))
#univariate checks
#adoption&field
fisher.test(table(FullSample$NrFields,FullSample$q1_adopt))#dependence

#adopt&peers
fisher.test(table(FullSample$q3_info,FullSample$q1_adopt))#dependece

#adoption & distFields
fisher.test(table(FullSample$FieldDist,FullSample$q1_adopt))#no

#adopt&AES
fisher.test(table(FullSample$q7_AES,FullSample$q1_adopt))#no

#adopt&Intention trad.mech.weeding
fisher.test(table(FullSample$q6_col1,FullSample$q1_adopt))#yes

#adopt&Intention modern.mech.weeding
fisher.test(table(FullSample$q6_col2,FullSample$q1_adopt))#yes

#adopt$auton.mech.weeding
fisher.test(table(FullSample$q6_col3,FullSample$q1_adopt))#no


#intention&fields
FullSample$q6_col1 <- as.factor(FullSample$q6_col1)
FullSample$q6_col2 <- as.factor(FullSample$q6_col2)
FullSample$q6_col3 <- as.factor(FullSample$q6_col3)

summary(mInt1<- multinom(q6_col1 ~ info_b +  fields_b + age_b + farmsize_b + AES_b, 
                         data = FullSample))

summary(mInt2<- multinom(q6_col2 ~ info_b +  fields_b + age_b + farmsize_b + AES_b, 
                         data = FullSample))

summary(mInt3<- multinom(q6_col3 ~ info_b +  fields_b + age_b + farmsize_b + AES_b, 
                         data = FullSample))


m.Intentions.table <- stargazer(mInt1, mInt2,mInt3,
                                #coef = list(NULL, m.Full_mfx$mfxest[,1]),
                                #se = list(NULL, m.Full_mfx$mfxest[,2]),
                                type = "html", out = "m.Intentions.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
                                dep.var.caption  = "Intention to use",
                              #  covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                               #                      "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed",
                                #                     "older than 45 years", "farm size > 50 ha", "AES participation"),
                                dep.var.labels   = "", single.row=FALSE)
p.intentions<-plot_summs(mInt1,mInt2,mInt3)

#info&Intention trad.mech.weeding
fisher.test(table(FullSample$q6_col1,FullSample$info_b))#yes

#info&Intention modern.mech.weeding
fisher.test(table(FullSample$q6_col2,FullSample$info_b))#yes

#info$auton.mech.weeding
fisher.test(table(FullSample$q6_col3,FullSample$info_b))#yes


#fields&Intention trad.mech.weeding
fisher.test(table(FullSample$q6_col1,FullSample$fields_b))#yes

#fields&Intention modern.mech.weeding
fisher.test(table(FullSample$q6_col2,FullSample$fields_b))#yes

#fields$auton.mech.weeding
fisher.test(table(FullSample$q6_col3,FullSample$fields_b))#no




#################
##IV approach####
#################


#FullSample$info_b<-ifelse(FullSample$q3_info == "0",0,1)
#FullSample$fields_b<-ifelse(FullSample$NrFields == "0",0,1)


#we can only use those, who have no NA in BTR030
SampleIV<-FullSample[!is.na(FullSample$BTR030)&!is.na(FullSample$FLC048)&(!FullSample$advisory == "Cosun"),]
#exxclude those who have NA for age
SampleIV<-SampleIV[!is.na(SampleIV$age_b),]

SampleIV$q1_adopt <- as.factor(SampleIV$q1_adopt)
#prep
#check correlation between IV and endo and IV and exog. var
AdoptersIV <- SampleIV[SampleIV$q1_adopt == "1",]
NonAdoptersIV <- SampleIV[SampleIV$q1_adopt == "0",]

InfoIV <- SampleIV[SampleIV$info_b == "1",]
NonInfoIV <- SampleIV[SampleIV$info_b == "0",]

FieldsIV <- SampleIV[SampleIV$fields_b == "1",]
NonFieldsIV <- SampleIV[SampleIV$fields_b == "0",]

#IV and adoption
#organic farms
t.test(AdoptersIV$BTR030, NonAdoptersIV$BTR030) #not significant

#organic fields
t.test(AdoptersIV$FLC048, NonAdoptersIV$FLC048) #not significant

#IV and endo
#organic farms and info
t.test(InfoIV$BTR030, NonInfoIV$BTR030) #significant
t.test(InfoIV$FLC048, NonInfoIV$FLC048)#not significant
#check if that differs when having info not as binary
chisq.test(FullSample$q3_info, FullSample$BTR030)

#organic fields and field
t.test(FieldsIV$FLC048, NonFieldsIV$FLC048)#significant (0.05)
t.test(FieldsIV$BTR030, NonFieldsIV$BTR030)#significant
SampleIV$advisory <- relevel(SampleIV$advisory, ref = "Nordzucker")
#to compare we need to run the full mdoel with binary variables for info and field on the SAMPLEIV
summary(m.Full.comp <- glm(q1_adopt ~  info_b + fields_b +
                             minDist_demo + advisory + 
                             age_b + farmsize_b + AES_b, 
                           data = SampleIV, family = binomial("probit")))

m.Full.comp_mfx <-probitmfx(m.Full.comp, data = SampleIV)
plot_summs(m.Full.comp_mfx,
           coefs = c("knowing other farmers"="info_b",
                     "observing fields"="fields_b",
                     "minimal distance to demo farm" = "minDist_demo",
                     "advisory Cosun" = "advisoryCosun",
                     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                     "advisory Südzucker"= "advisorySüdzucker",
                     "older than 45 years"="age_b1",
                     "farm size > 50 ha"="farmsize_b1",
                     "AES participation"="AES_b1"),
           model.names = "Probit model on adoption decision",
           scale = TRUE, robust = TRUE)
#correlation between IV and error term
#first need model with endo and instrument
summary(m.Full.forInfo <- glm(q1_adopt ~  info_b + BTR030 + FLC048 + age_b + farmsize_b + AES_b, 
                           data = SampleIV, family = binomial("probit")))
summary(m.Full.forInfo2 <- glm(q1_adopt ~  info_b + age_b + farmsize_b + AES_b, 
                              data = SampleIV, family = binomial("probit")))


pR2(m.Full.forInfo)
pR2(m.Full.forInfo2)
pR2(m.Full.forField)
pR2(m.Full.forField2)


residualsI <- m.Full.forInfo$residuals
SampleIV <- cbind(SampleIV, residualsI)
cor(SampleIV$residualsI, SampleIV$BTR030)

summary(m.Full.forField <- glm(q1_adopt ~  fields_b + BTR030 + FLC048 + age_b + farmsize_b + AES_b, 
                              data = SampleIV, family = binomial("probit")))
summary(m.Full.forField2 <- glm(q1_adopt ~  fields_b + age_b + farmsize_b + AES_b, 
                               data = SampleIV, family = binomial("probit")))

residualsF <- m.Full.forField$residuals
SampleIV <- cbind(SampleIV, residualsF)
cor(SampleIV$residualsF, SampleIV$FLC048)


#regress x on z, z should be significant now
summary(m.Full.relInfo <- glm(info_b ~ BTR030 + FLC048 + age_b + farmsize_b + AES_b + minDist_demo + advisory, 
                              data = SampleIV, family = binomial("probit")))

summary(m.Full.relField <- glm(fields_b ~  BTR030 +  FLC048 + age_b + farmsize_b + AES_b + minDist_demo + advisory, 
                               data = SampleIV, family = binomial("probit")))



#following MHE: 
#use fitted values from 1. stage as instruments in OLS first stage

#####
#now add all the other variables
#exxclude those who have NA for age
#SampleIV<-SampleIV[!is.na(SampleIV$age_b),]
#info
#1. calculate non-linear fitted values ("1st stage")
nonli2 <- glm(info_b ~ BTR030 + FLC048+ minDist_demo + advisory + age_b + farmsize_b + AES_b , data = SampleIV, family = binomial("probit"))
#store values
y_hatnonli2 <- nonli2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS

MHE1i2 <- lm(info_b ~ y_hatnonli2+FLC048+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV)
info_iv <- MHE1i2$fitted.values

#2.second stage

MHE2i2 <- glm(q1_adopt ~ info_iv+FLC048+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV, family = binomial("probit"))

summary(MHE2i2)

probitmfx(q1_adopt ~ info_iv+FLC048+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV)


#now add all the other variables
#exxclude those who have NA for age
#field
#1. calculate non-linear fitted values ("1st stage")
nonlf2 <- glm(fields_b ~ FLC048 + BTR030 +minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV, family = binomial("probit"))
#store values
y_hatnonlf2 <- nonlf2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS

MHE1f2 <- lm(fields_b ~ y_hatnonlf2+BTR030 +minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV)
fields_iv <- MHE1f2$fitted.values

#2.second stage

MHE2f2 <- glm(q1_adopt ~ fields_iv+BTR030 +minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV, family = binomial("probit"))

summary(MHE2f2)

probitmfx(q1_adopt ~ fields_iv+BTR030+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = SampleIV)

#now put both IVs teogether in the original full model
summary(m.Full.IV <- glm(q1_adopt ~  info_iv + fields_iv+
                           minDist_demo + advisory +
                           age_b + farmsize_b + AES_b, 
                      data = SampleIV, family = binomial("probit")))

m.Full.IV_mfx <-probitmfx(m.Full.IV, data = SampleIV)

p.mFull_mfx_compareIV <- plot_summs(m.Full.comp_mfx,m.Full.IV_mfx, 
                       coefs = c("Info = knowing other farmers"="info_b",
                                 "Info_IV =knowing other farmers"="info_iv",
                                 "Field =observing fields"="fields_b",
                                 "Field_IV = observing fields"="fields_iv",
                                 "minimal distance to demo farm" = "minDist_demo",
                                 #"advisory Cosun" = "advisoryCosun",
                                 "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                 "advisory Südzucker"= "advisorySüdzucker",
                                 "older than 45 years"="age_b1",
                                 "farm size > 50 ha"="farmsize_b1",
                                 "AES participation"="AES_b1"),
                        model.names = c("Probit model", "Probit model with 3SLS-IV"),
                         scale = TRUE, robust = TRUE,colors = c("grey60", "grey36"))

p.mFull_mfx_compareIV +theme(legend.position="bottom")

####
##include distance variables
#e.g. distance to fields observed
#those who didn't observe fields are set to inf
#FullSample[is.na(FullSample$meanDist),]$meanDist <- 200
#those who didn't observe fields go to category "more than 30"

#FullSample[(FullSample$FieldDist == "6"),]$FieldDist <- "5"

#add to "full model"
#as numeric
summary(m.Full_dist <- glm(q1_adopt ~  q3_info + NrFields + meanDist + age_b + farmsize_b + AES_b, 
                     data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_dist_mfx <-probitmfx(m.Full_dist, data = FullSample)
m.Full_dist_mfx

#as category

summary(m.Full_dist.cat <- glm(q1_adopt ~  q3_info #+ NrFields 
                               + FieldDist + age_b + farmsize_b + AES_b, 
                           data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_dist.cat_mfx <-probitmfx(m.Full_dist.cat, data = FullSample)
m.Full_dist.cat_mfx

p.dist <-plot_summs(m.Full_dist.cat_mfx)


#open questions:
#include field and info as binary or in categories??
#same for field dist? 

#include all asdummy 
summary(m.Full_bi <- glm(q1_adopt ~  info_b + fields_b + meanDist + age_b + farmsize_b + AES_b, 
                             data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_bi_mfx <-probitmfx(m.Full_bi, data = FullSample)
m.Full_bi_mfx



#+ add region
summary(m.Full_region <- glm(q1_adopt ~  info_b + fields_b + meanDist + age_b + farmsize_b + AES_b + bundesland, 
                               data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_region_mfx <-probitmfx(m.Full_region, data = FullSample)
m.Full_region_mfx

p.bi_reg<- plot_summs(m.Full_bi_mfx, m.Full_region_mfx) #compared to BAWÜ
#to do
#belonging to advisory region

#add distance to demonstration farm
summary(m.Full_demo <- glm(q1_adopt ~  info_b + fields_b + meanDist + age_b + farmsize_b + AES_b #+ bundesland 
                           + minDist_demo, 
                             data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_demo_mfx <-probitmfx(m.Full_demo, data = FullSample)
m.Full_demo_mfx
p.bi_demo <- plot_summs(m.Full_demo_mfx)

#include distance variable as interaction term + as squared + bundesland as proxy for adv. region
#first create interaction variable in data frame
FullSample$field_meanDist <- FullSample$fields_b*FullSample$meanDist
FullSample$sq.minDist_demo <- FullSample$minDist_demo*FullSample$minDist_demo
FullSample$sq.meanDist<- FullSample$meanDist*FullSample$meanDist

#FullSample$field_catDist <- FullSample$fields_b*FullSample$FieldDist

#allin1: include distance to demo as nuemric, thereby reduce to those who indicated fields on the map
#those who didn't observe fields were set to 200

summary(m.Allin1 <- glm(q1_adopt ~  info_b + fields_b 
                       + field_meanDist + I(field_meanDist^2) 
                             + minDist_demo + I(minDist_demo^2) + advisory
                             + age_b + farmsize_b + AES_b, 
                             data = FullSample,family = binomial("probit")))#ref-category:0 = 0 peers

m.Allin1_mfx <-probitmfx(m.Allin1, data = FullSample)
p.Allin1_mfx <- plot_summs(m.Allin1, 
                          coefs =c("info (knowing other farmers yes/no)"="info_b", 
                            "fields (observing yes/no)"="fields_b", 
                            "fields*meanDist to fields"="field_meanDist",
                            "sq(fields*meanDist to fields)"="I(field_meanDist^2)",
                            "minimalDist to demofarm"="minDist_demo",
                            "sq(minimalDist to demofarm)"="I(minDist_demo^2)",
                            "Südzucker" = "advisorySüdzucker","P&L"="advisoryPfeifferLangen",
                            "older than 45 years"="age_b1"
                            , "farm size > 50 ha"="farmsize_b1",
                            "AES participation"="AES_b1"))
                          #scale = TRUE, robust = TRUE)

#now enter distance to fields as factor and remove fields_b
FullSample_forModels<- FullSample[!is.na(FullSample$minDist_demo)&!is.na(FullSample$advisory)&
                                    !is.na(FullSample$age_b)&!(FullSample$advisory == "Cosun"),]

FullSample_forModels$advisory <- relevel(FullSample_forModels$advisory, ref = "Nordzucker")
FullSample_forModels$FieldDist <- relevel(FullSample_forModels$FieldDist, ref = "6")
summary(m.Allin2 <- glm(q1_adopt ~  info_b #+ fields_b 
                        + FieldDist #+ I(field_meanDist^2) 
                        + minDist_demo #+ I(minDist_demo^2) 
                        + advisory
                        + age_b + farmsize_b + AES_b, 
                        data = FullSample_forModels,family = binomial("probit")))#ref-category:0 = 0 peers

m.Allin2_mfx <-probitmfx(m.Allin2, data = FullSample_forModels)
p.Allin2_mfx <- plot_summs(m.Allin2, 
                          coefs =c("knowing other farmers"="info_b",
                                   "observed fields in 0-5km"="FieldDist0",
                                   "observed fields in 6-10km"="FieldDist1",
                                   "observed fields in 11-15km"="FieldDist2",
                                   "observed fields in 16-20km"="FieldDist3",
                                   "observed fields in 21-30km"="FieldDist4",
                                   "observed fields in more than 30km"="FieldDist5",
                                  # "observed fields in more than 30km"="FullSample_forModels$FieldDist5",
                                   #"No fields observed"="FullSample_forModels$FieldDist6",
                                   "minimal distance to demo farm" = "minDist_demo",
                                   #"advisory Cosun" = "advisoryCosun",
                                   "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                   "advisory Südzucker"= "advisorySüdzucker",
                                   "older than 45 years"="age_b1",
                                   "farm size > 50 ha"="farmsize_b1",
                                   "AES participation"="AES_b1",
                           scale = TRUE, robust = TRUE))


p.Allin2_mfx


#number of afields observed as cat.
summary(m.Full <- glm(q1_adopt ~  NrFields +info_b
                      + minDist_demo 
                      + advisory 
                      + age_b + farmsize_b + AES_b, 
                      data = FullSample_forModels, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full_mfx <-probitmfx(m.Full, data = FullSample_forModels)
p.mFull_mfx <- plot_summs(m.Full_mfx, 
                          coefs = c("knowing other farmers"="info_b",
                            "1-5 fields observed"="NrFields1",
                                    "6-10 fields observed"="NrFields2",
                                    "11-15 fields observed"="NrFields3",
                                    "more than 15 fields observed"="NrFields4",
                                    "minimal distance to demo farm" = "minDist_demo",
                                    #"advisory Cosun" = "advisoryCosun",
                                    "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                    "advisory Südzucker"= "advisorySüdzucker",
                                    "older than 45 years"="age_b1",
                                    "farm size > 50 ha"="farmsize_b1",
                                    "AES participation"="AES_b1"),
                          model.names = "Probit model on adoption decision",
                          scale = TRUE, robust = TRUE)

#now enter nr, of adopters known as category
summary(m.Full2 <- glm(q1_adopt ~   q3_info + fields_b
                      + minDist_demo 
                      + advisory 
                      + age_b + farmsize_b + AES_b, 
                      data = FullSample_forModels, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full2_mfx <-probitmfx(m.Full2, data = FullSample_forModels)
p.mFull2_mfx <- plot_summs(m.Full2_mfx, 
                          coefs = c("1-5 adopters known"="q3_info1",
                                    "6-10 adopters known"="q3_info2",
                                    "more than 10 adopters known"="q3_info3",
                                    "minimal distance to demo farm" = "minDist_demo",
                                    #"advisory Cosun" = "advisoryCosun",
                                    "observing fields" = "fields_b",
                                    "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                    "advisory Südzucker"= "advisorySüdzucker",
                                    "older than 45 years"="age_b1",
                                    "farm size > 50 ha"="farmsize_b1",
                                    "AES participation"="AES_b1"),
                          model.names = "Probit model on adoption decision",
                          scale = TRUE, robust = TRUE)



models_cat <-plot_summs(m.Full2_mfx,m.Allin2_mfx, m.Full_mfx, 
           coefs = c("1-5 adopters known"="q3_info1",
                     "6-10 adopters known"="q3_info2",
                     "more than 10 adopters known"="q3_info3",
                     "1-5 fields observed"="NrFields1",
                     "6-10 fields observed"="NrFields2",
                     "11-15 fields observed"="NrFields3",
                     "more than 15 fields observed"="NrFields4",
                     "observed fields in 0-5km"="FieldDist0",
                     "observed fields in 6-10km"="FieldDist1",
                     "observed fields in 11-15km"="FieldDist2",
                     "observed fields in 16-20km"="FieldDist3",
                     "observed fields in 21-30km"="FieldDist4",
                     "observed fields in more than 30km"="FieldDist5",
                     "knowing other farmers"="info_b",
                     "observing fields" = "fields_b",
                     "minimal distance to demo farm" = "minDist_demo",
                     #"advisory Cosun" = "advisoryCosun",
                     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                     "advisory Südzucker"= "advisorySüdzucker",
                     "older than 45 years"="age_b1",
                     "farm size > 50 ha"="farmsize_b1",
                     "AES participation"="AES_b1"),
         model.names = c("Model (3)","Model (4)", "Model (5)"),
           scale = TRUE, robust = TRUE, colors = c("grey60", "grey36", "grey76"))

stargazer(m.Full2_mfx$fit,m.Allin2_mfx$fit, m.Full_mfx$fit,
          type = "html", out = "Categorical.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
          dep.var.caption  = "Mechanical weeding yes/no",
          coef = list(m.Full2_mfx$mfxest[,1],m.Allin2_mfx$mfxest[,1],m.Full_mfx$mfxest[,1]) ,
          se = list(m.Full2_mfx$mfxest[,2],m.Allin2_mfx$mfxest[,2],m.Full_mfx$mfxest[,2]),
          dep.var.labels   = "")


#start to compare different odels for substituitability
#To do so, we compare different models, that include
#1. intercept (naive model)
#2. intercept + controls
#3. intercept + controls + Fieldi
#4. intercept + controls + Infoi
#5. intercept + controls + Fieldi + Infoi
#exclude those with some NA in one of the variable sincluded in the model



m.1.mfx <- probitmfx(m.1<- glm(q1_adopt ~ 1,  
                        data = FullSample_forModels,family = binomial("probit")), data = FullSample_forModels) 

m.2.mfx <- probitmfx(m.2 <-glm(q1_adopt ~ minDist_demo 
                        + advisory
                        + age_b + farmsize_b + AES_b, 
                        data = FullSample_forModels,family = binomial("probit")), data = FullSample_forModels) 

m.3.mfx <- probitmfx(m.3 <-glm(q1_adopt ~ info_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample_forModels,family = binomial("probit")), data = FullSample_forModels)

m.4.mfx <- probitmfx(m.4 <-glm(q1_adopt ~ fields_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample_forModels,family = binomial("probit")), data = FullSample_forModels)

m.5.mfx <- probitmfx(m.5 <-glm(q1_adopt ~ info_b + fields_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample_forModels,family = binomial("probit")), data = FullSample_forModels)

#COmpare AIC?
subtitute <- plot_summs(m.2.mfx, m.3.mfx, m.4.mfx, m.5.mfx,
                        coefs = c("knowing other farmers"="info_b",
                                  "IV_knowing other farmers"="info_iv",
                                  "observing fields"="fields_b",
                                  "IV_observing fields"="fields_iv",
                                  "minimal distance to demo farm" = "minDist_demo",
                                  #"advisory Cosun" = "advisoryCosun",
                                  "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                  "advisory Südzucker"= "advisorySüdzucker",
                                  "older than 45 years"="age_b1",
                                  "farm size > 50 ha"="farmsize_b1",
                                  "AES participation"="AES_b1"),
                        scale = TRUE, robust = TRUE,
                        model.names = c("Model2", "Model3", "Model4", "Model5"))#+
 # theme_bw()+
  #theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#list.coefs <- 
#seems alone they give more, together they substitute each other?
m.1.intercept <- m.1.mfx[["fit"]][["coefficients"]][["(Intercept)"]]
m.subs.compare <- stargazer(m.1.mfx$fit, m.2.mfx$fit, m.3.mfx$fit, m.4.mfx$fit, m.5.mfx$fit,
                                            #  coef = list(m.1.intercept,m.2.mfx$mfxest[,1],m.3.mfx$mfxest[,1],m.4.mfx$mfxest[,1],m.5.mfx$mfxest[,1]) ,
                                          #      se = list(m.1.mfx$mfxest[,2],m.2.mfx$mfxest[,2],m.3.mfx$mfxest[,2],m.4.mfx$mfxest[,2],m.5.mfx$mfxest[,2]),
                                                 type = "html", out = "m.subs.compare.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
                                                dep.var.caption  = "Mechanical weeding yes/no",
                          #                     column.labels = c("Odds ratio","marginal effect"),
                          #                    covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                          #                                        "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed",
                          #                                       "older than 45 years", "farm size > 50 ha", "AES participation"),
                                           dep.var.labels   = "", single.row=FALSE)

#compare modles base don AIkaike inf crit
#install.packages("AICcmodavg")
#https://www.scribbr.com/statistics/akaike-information-criterion/#:~:text=To%20compare%20models%20using%20AIC%2C%20you%20need%20to%20calculate%20the,calculating%20log%2Dlikelihood%20is%20complicated!
library(AICcmodavg)
models <- list(m.1, m.2, m.3, m.4,m.5)

model.names <- c('Model1', 'Model2', 'Model3', 'Model4', 'Model5')
AIC.compTable<- aictab(cand.set = models,modnames = NULL,
       second.ord = TRUE, nobs = NULL, sort = TRUE, c.hat = 1)
stargazer(AIC.compTable,
          type = "html", out = "AIC.html")


#check also via R2
pR2(m.1)
pR2(m.2)
pR2(m.3)
pR2(m.4)
pR2(m.5)


#get percent of correct predicitons
probabilities_m.1 <- round(predict(m.1, type = "response"))
probabilities_m.2 <- round(predict(m.2, type = "response"))
probabilities_m.3 <- round(predict(m.3, type = "response"))
probabilities_m.4 <- round(predict(m.4, type = "response"))
probabilities_m.5 <- round(predict(m.5, type = "response"))

#model1
m1<- matrix(table(FullSample_forModels$q1_adopt, probabilities_m.1))
#correct:
m1[1]/(m1[1]+m1[2])

#model2
m2<- matrix(table(FullSample_forModels$q1_adopt, probabilities_m.2))
#correct:
(m2[1]+m2[4])/(m2[1]+m2[2]+m2[3]+m2[4])

#model3
m3<- matrix(table(FullSample_forModels$q1_adopt, probabilities_m.3))
#correct:
(m3[1]+m3[4])/(m3[1]+m3[2]+m3[3]+m3[4])

#model4
m4<- matrix(table(FullSample_forModels$q1_adopt, probabilities_m.4))
#correct:
(m4[1]+m4[4])/(m4[1]+m4[2]+m4[3]+m4[4])

#model5
m5<- matrix(table(FullSample_forModels$q1_adopt, probabilities_m.5))
#correct:
(m5[1]+m5[4])/(m5[1]+m5[2]+m5[3]+m5[4])





#get plots fo distance variables

FullSample_forModels$pred_Y <- predict(m.Allin2, FullSample_forModels, type = "response")
m.Allin2$fitted.values


#first need one regression that includes the distance variables

#remove 200 from those who didn't observe any fields
FullSample_forDist<- FullSample_forModels
FullSample_forDist$meanDist<-ifelse(FullSample_forDist$meanDist == "200", "NA",FullSample_forDist$meanDist)
m.dist <-glm(q1_adopt ~ info_b 
            + meanDist
            + minDist_demo 
             + I(meanDist^2) 
            + I(minDist_demo^2)
              + advisory
              + age_b + farmsize_b + AES_b, 
              data = FullSample_forDist, family = binomial("probit"))

p.1<-effect_plot(m.dist, pred = minDist_demo, y.label = "probability",
                 x.label = "minimal distance to demo farm (km)", interval = TRUE)+#, outcome.scale = "link") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#p.2<-effect_plot(m.dist, pred = sq.minDist_demo,interval = TRUE)#,outcome.scale = "link")
p.3<-effect_plot(m.dist, pred = meanDist, y.label = "probability",
                 x.label = "mean distance to other farmers' fields (km)", interval = TRUE)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#,outcome.scale = "link")
#p.4<-effect_plot(m.dist, pred = sq.meanDist,interval = TRUE)#,outcome.scale = "link")




plot_grid(p.1, p.3, nrow = 1, ncol = 2)
plot_grid(p.1, p.2, p.3, p.4, nrow = 2, ncol = 2)

#make descrptives table
#list of variables to include
vars <- data.frame(FullSample$info_b, FullSample$fields_b, FullSample$minDist_demo, 
          FullSample$meanDist, as.numeric(FullSample$q3_info), as.numeric(FullSample$advisory), as.numeric(FullSample$q7_age),
          as.numeric(FullSample$q7_size), as.numeric(FullSample$q7_AES))
stargazer(vars,
          type = "html", out = "descriptives.html")
mean(FullSample_forModels$minDist_demo)


#check difference between distances dependant on region
#exclude Cosun

chisq.test(FullSample_forModels$advisory, FullSample_forModels$meanDist_cat)
table(FullSample_forModels$advisory, FullSample_forModels$meanDist_cat)
table(FullSample$advisory, FullSample$meanDist)
m.adv_dist <- summary(lm(meanDist ~ advisory, data = FullSample_forModels))



