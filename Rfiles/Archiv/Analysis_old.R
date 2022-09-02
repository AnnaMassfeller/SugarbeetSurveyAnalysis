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

#remove columns not needed for regression
summary(m.FieldPeer <- glm(q1_adopt ~ q3_info + NrFields , 
                           data = FullSample, family = binomial("probit")))
stargazer(m.FieldPeer, type = "text", out = "FieldPeer.doc", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
          dep.var.caption  = "Mechanical weeding yes/no",
          covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                               "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed"),
          dep.var.labels   = "", single.row=TRUE)
m.FieldPeer_mfx <- probitmfx(m.FieldPeer, data = FullSample)
m.FieldPeer_mfx

summary(m.NrFields <- glm(q1_adopt ~  NrFields, 
                          data = FullSample, family = binomial("probit"))) #ref.category: 0 = 0 fields
m.NrFields_mfx <-probitmfx(m.NrFields, data = FullSample) 
m.NrFields_mfx

summary(m.NrPeers <- glm(q1_adopt ~  q3_info, 
                         data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers

m.NrPeers_mfx <-probitmfx(m.NrPeers, data = FullSample)
m.NrPeers_mfx

summary(m.Full <- glm(q1_adopt ~  q3_info + NrFields + age_b + farmsize_b + AES_b, 
                      data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers

no48<-FullSample[-48,]
summary(m.no48 <- glm(q1_adopt ~  q3_info + NrFields + age_b + farmsize_b + AES_b, 
                      data = no48, family = binomial("probit")))#ref-category:0 = 0 peers

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

par(mar = rep(2, 4))
plot(m.Full)
plot(m.no48)
par(mfrow=c(1,1))
vif(m.Full)

#correlation between peer und field?
fisher.test(table(FullSample$NrFields,FullSample$q3_info))#no dependence

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

require(nnet)
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
library(ivtools)
library(AER)

FullSample$info_b<-ifelse(FullSample$q3_info == "0",0,1)
FullSample$fields_b<-ifelse(FullSample$NrFields == "0",0,1)


#we can only use those, who have no NA in BTR030
SampleIV<-FullSample[!is.na(FullSample$BTR030)&!is.na(FullSample$FLC048),]
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
#to compare we need to run the full mdoel with binary variables for info and field on the SAMPLEIV
summary(m.Full.comp <- glm(q1_adopt ~  info_b + fields_b +
                             minDist_demo + advisory + 
                             age_b + farmsize_b + AES_b, 
                           data = SampleIV, family = binomial("probit")))

m.Full.comp_mfx <-probitmfx(m.Full.comp, data = SampleIV)
plot_summs(m.Full.comp_mfx)
#correlation between IV and error term
#first need model with endo and instrument
summary(m.Full.forInfo <- glm(q1_adopt ~  info_b + BTR030 + fields_b + age_b + farmsize_b + AES_b, 
                           data = SampleIV, family = binomial("probit")))

residualsI <- m.Full.forInfo$residuals
SampleIV <- cbind(SampleIV, residualsI)
cor(SampleIV$residualsI, SampleIV$BTR030)

summary(m.Full.forField <- glm(q1_adopt ~  info_b + fields_b + FLC048 + age_b + farmsize_b + AES_b, 
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
SampleIV<-SampleIV[!is.na(SampleIV$age_b),]
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
                         coefs = c("info (knowing other farmers yes/no)"="info_b", "info_IV (knowing other farmers yes/no)"="info_iv",
                                   "fields (observing yes/no)"="fields_b", "fields_IV (observing yes/no)"="fields_iv",
                                   "minimal distance to demofarm" = "minDist_demo",
                                   "advisoryPfeifferLangen" = "Pfeiffer&Langen",
                                   "advisorySüdzucker"= "Südzucker",
                                   "older than 45 years"="age_b1"
                                   , "farm size > 50 ha"="farmsize_b1",
                                  "AES participation"="AES_b1"),
                          model.names = c("Probit model", "Probit model with 3SLS-IV"),
                         scale = TRUE, robust = TRUE)

p.mFull_mfx_compareIV

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

summary(m.Full_dist.cat <- glm(q1_adopt ~  q3_info + NrFields + FieldDist + age_b + farmsize_b + AES_b, 
                           data = FullSample, family = binomial("probit")))#ref-category:0 = 0 peers


m.Full_dist.cat_mfx <-probitmfx(m.Full_dist.cat, data = FullSample)
m.Full_dist.cat_mfx

p.dist <-plot_summs(m.Full_dist_mfx, m.Full_dist.cat_mfx)


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
summary(m.Allin2 <- glm(q1_adopt ~  info_b #+ fields_b 
                        + relevel(FullSample$FieldDist, ref = "6") #+ I(field_meanDist^2) 
                        + minDist_demo #+ I(minDist_demo^2) 
                        + advisory
                        + age_b + farmsize_b + AES_b, 
                        data = FullSample,family = binomial("probit")))#ref-category:0 = 0 peers

m.Allin2_mfx <-probitmfx(m.Allin2, data = FullSample)
p.Allin2_mfx <- plot_summs(m.Allin2)#, 
                         #  coefs =c("info (knowing other farmers yes/no)"="info_b", 
                                  #  "Observed fields in 0-5km"="FieldDist0",
                                  #  "Observed fields in 6-10km"="FieldDist1",
                                  #  "Observed fields in 11-15km"="FieldDist2",
                                  #  "Observed fields in 16-20km"="FieldDist3",
                                  #  "Observed fields in 21-30km"="FieldDist4",
                                  #  "Observed fields in more than 30km"="FieldDist5",
                                  #  "No fields observed"="FieldDist6",
                                #  "Observed fields in 0-5km"= "relevel(FullSample$FieldDist, ref = "6")0",
                                 # "Observed fields in 6-10km"="relevel(FullSample$FieldDist, ref = "6")1",
                                 # "Observed fields in 11-15km"="relevel(FullSample$FieldDist, ref = "6")2",
                                #  "Observed fields in 16-20km"="relevel(FullSample$FieldDist, ref = "6")3",
                                #  "Observed fields in 21-30km"="relevel(FullSample$FieldDist, ref = "6")4",
                                 # "Observed fields in more than 30km"="relevel(FullSample$FieldDist, ref = "6")5",
                                   #"fields*meanDist to fields"="field_meanDist",
                                   # "sq(fields*meanDist to fields)"="I(field_meanDist^2)",
                          #          "minimalDist to demofarm"="minDist_demo",
                                    #"sq(minimalDist to demofarm)"="I(minDist_demo^2)",
                           #         "Südzucker" = "advisorySüdzucker","P&L"="advisoryPfeifferLangen",
                            #        "older than 45 years"="age_b1",
                             #       "farm size > 50 ha"="farmsize_b1",
                              #      "AES participation"="AES_b1"))#scale = TRUE, robust = TRUE)


#start to compare different odels for substituitability
#To do so, we compare different models, that include
#1. intercept (naive model)
#2. intercept + controls
#3. intercept + controls + Fieldi
#4. intercept + controls + Infoi
#5. intercept + controls + Fieldi + Infoi

summary(m.1 <- glm(q1_adopt ~ 1,  
                        data = FullSample,family = binomial("probit"))) 

summary(m.2 <- glm(q1_adopt ~ minDist_demo 
                        + advisory
                        + age_b + farmsize_b + AES_b, 
                        data = FullSample,family = binomial("probit")))

summary(m.3 <- glm(q1_adopt ~ info_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample,family = binomial("probit")))

summary(m.4 <- glm(q1_adopt ~ fields_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample,family = binomial("probit")))

summary(m.5 <- glm(q1_adopt ~ info_b + fields_b +minDist_demo 
                   + advisory
                   + age_b + farmsize_b + AES_b, 
                   data = FullSample,family = binomial("probit")))

#COmpare AIC?
subtitute <- plot_summs(m.2, m.3, m.4, m.5)
#seems alone they give more, together they substitute each other?
