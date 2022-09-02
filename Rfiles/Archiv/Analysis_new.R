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
FullSample$fields_b <- as.factor(FullSample$fields_b)
FullSample$info_b <- as.factor(FullSample$info_b)

#################
##IV approach####
#################
#first we need to check if our var. are really endogeneou, e.g. are they correlated with the eroor term?
#"pure, old" model
df.Models <- FullSample[!is.na(FullSample$info_b)&!is.na(FullSample$fields_b)&!is.na(FullSample$minDist_demo)&!is.na(FullSample$advisory)&
                          !is.na(FullSample$age_b)&!is.na(FullSample$farmsize_b)&!is.na(FullSample$AES_b)&(!FullSample$advisory == "Cosun")
                        & !is.na(FullSample$areaDens)& !is.na(FullSample$farmDens),]

m.pure <- glm(q1_adopt ~  info_b 
              + fields_b
              +minDist_demo 
              + advisory
              + age_b 
              + farmsize_b 
              + AES_b, 
              data = df.Models, family = binomial("probit"))

m.pure_mfx <- probitmfx(m.pure, data = df.Models)
plot_summs(m.pure_mfx)
residualsPURE <- m.pure$residuals
df.Models <- cbind(df.Models, residualsPURE)
t.test(df.Models[df.Models$info_b == 1,]$residualsPURE, df.Models[df.Models$info_b == 0,]$residualsPURE)
t.test(df.Models[df.Models$fields_b == 1,]$residualsPURE, df.Models[df.Models$fields_b == 0,]$residualsPURE)


chisq.test(df.Models$residualsPURE, df.Models$q3_info)
chisq.test(df.Models$residualsPURE, df.Models$NrFields)

#maybe we even do not need an IV approach?

#following MHE: 
#use fitted values from 1. stage as instruments in OLS first stage

#####
#now add all the other variables
#exxclude those who have NA for age
#SampleIV<-SampleIV[!is.na(SampleIV$age_b),]
#info
#1. calculate non-linear fitted values ("1st stage")
nonli2 <- glm(info_b ~ farmDens + areaDens+ minDist_demo + advisory + age_b + farmsize_b + AES_b , data = df.Models, family = binomial("probit"))
#store values
y_hatnonli2 <- nonli2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
df.Models$info_b <- as.numeric(df.Models$info_b)
MHE1i2 <- lm(info_b ~ y_hatnonli2+minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models)
info_iv <- MHE1i2$fitted.values

#2.second stage
MHE2i2 <- glm(q1_adopt ~ info_iv+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models, family = binomial("probit"))

summary(MHE2i2)

probitmfx(q1_adopt ~ info_iv+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models)
df.Models$info_b <- as.factor(df.Models$info_b)

#now add all the other variables
#exxclude those who have NA for age
#field
#1. calculate non-linear fitted values ("1st stage")
nonlf2 <- glm(fields_b ~ areaDens + farmDens +minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models, family = binomial("probit"))
#store values
y_hatnonlf2 <- nonlf2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
df.Models$fields_b <- as.numeric(df.Models$fields_b)
MHE1f2 <- lm(fields_b ~ y_hatnonlf2+minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models)
fields_iv <- MHE1f2$fitted.values

#2.second stage

MHE2f2 <- glm(q1_adopt ~ fields_iv+minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models, family = binomial("probit"))

summary(MHE2f2)

probitmfx(q1_adopt ~ fields_iv+ minDist_demo + advisory + age_b + farmsize_b + AES_b, data = df.Models)
df.Models$fields_b <- as.factor(df.Models$fields_b)
#now put both IVs teogether in the original full model
summary(m.Full.IV <- glm(q1_adopt ~  info_iv + fields_iv+
                           minDist_demo + advisory +
                           age_b + farmsize_b + AES_b, 
                         data = df.Models, family = binomial("probit")))

m.Full.IV_mfx <-probitmfx(m.Full.IV, data = df.Models)

p.compareIV <- plot_summs(m.pure_mfx,m.Full.IV_mfx, 
                                    coefs = c("Info = knowing other farmers"="info_b1",
                                              "Info_IV =knowing other farmers"="info_iv",
                                              "Field =observing fields"="fields_b1",
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

p.compareIV +theme(legend.position="bottom")
