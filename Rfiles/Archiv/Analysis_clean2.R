library(pscl)
require(nnet)
library(ivtools)
library(AER)
library(cowplot)
library(regclass)
library(margins)
library(marginaleffects)
library(insight)
library(ggExtra)
library(ggpubr)
library(MASS)
library(mfx)
##file to run models for the analysis
#source("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis/Rfiles/DataPreProcessing.R")
#Run first regression

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
#first we need to check if our var. are really endogeneou, e.g. are they correlated with the eroor term?
#"pure, old" model
df.Models <- FullSample[!is.na(FullSample$info_b)&!is.na(FullSample$fields_b)&!is.na(FullSample$minDist_demo)&!is.na(FullSample$advisory)&
                          !is.na(FullSample$age_b)&!is.na(FullSample$farmsize_b)&!is.na(FullSample$AES_b)&(!FullSample$advisory == "Cosun")
                       # & !is.na(FullSample$areaDens)& !is.na(FullSample$farmDens)
                       ,]
df.Models$advisory <- droplevels(df.Models$advisory)
#intention&fields
df.Models$q6_col1 <- as.factor(df.Models$q6_col1)
df.Models$q6_col2 <- as.factor(df.Models$q6_col2)
df.Models$q6_col3 <- as.factor(df.Models$q6_col3)

summary(mInt1<- multinom(q6_col1 ~ info_b + 
                           fields_b + age_b + farmsize_b + AES_b +minDist_demo 
                         + advisory, 
                         data = df.Models))

summary(mInt2<- multinom(q6_col2 ~ info_b +  fields_b + age_b + farmsize_b + AES_b+minDist_demo 
                         + advisory, 
                         data = df.Models))

summary(mInt3<- multinom(q6_col3 ~ info_b +  fields_b + age_b + farmsize_b + AES_b+minDist_demo 
                         + advisory, 
                         data = df.Models))
int1_mfx <- marginaleffects(mInt1, type = "probs" )
int2_mfx <- marginaleffects(mInt2, type = "probs" )
int3_mfx <- marginaleffects(mInt3, type = "probs" )

stargazer(summary(int1_mfx),type = "html", out = "m.Intentions1.html")
plot(int1_mfx)
plot(int2_mfx)
plot(int3_mfx)
plot_summs(int1_mfx, int2_mfx, int3_mfx)



m.Intentions.table <- stargazer(mInt1, mInt2,mInt3,
                                #coef = list(NULL, m.Full_mfx$mfxest[,1]),
                                #se = list(NULL, m.Full_mfx$mfxest[,2]),
                                type = "html", out = "m.Intentions.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
                                dep.var.caption  = "Intention to use",
                                #  covariate.labels = c("1-5 adopters known","6-10 adopters known","more than 10 adopters known",
                                #                      "1-5 fields observed","6-10 fields observed","11-15 fields observed","more than 15 fields observed",
                                #                     "older than 45 years", "farm size > 50 ha", "AES participation"),
                                dep.var.labels   = "", single.row=FALSE)
p.intentions<-plot_summs(mInt1,mInt2,mInt3, model.names = c("traditional m.w.", "modern m.w.", "autonomous m.w."))


#try ordered probit model
orderedprobit1<-polr(q6_col1 ~ info_b + 
       fields_b + age_b + farmsize_b + AES_b +minDist_demo 
     + advisory, 
     data = df.Models, method = "probit")
summary(orderedprobit1)
#how to get marginal effects?
#probitmfx(orderedprobit1, data = df.Models)


m.pure <- glm(q1_adopt ~  info_b 
             + fields_b
             +minDist_demo 
              + advisory
              + age_b 
              + farmsize_b 
              + AES_b, 
              data = df.Models, family = binomial("probit"))

residualsPURE <- m.pure$residuals
df.Models <- cbind(df.Models, residualsPURE)
t.test(df.Models[df.Models$info_b == 1,]$residualsPURE, df.Models[df.Models$info_b == 0,]$residualsPURE)
t.test(df.Models[df.Models$fields_b == 1,]$residualsPURE, df.Models[df.Models$fields_b == 0,]$residualsPURE)


chisq.test(df.Models$residualsPURE, df.Models$q3_info)
chisq.test(df.Models$residualsPURE, df.Models$NrFields)

#maybe we even do not need an IV approach?


#FullSample$info_b<-ifelse(FullSample$q3_info == "0",0,1)
#FullSample$fields_b<-ifelse(FullSample$NrFields == "0",0,1)


#we can only use those, who have no NA in ShareOrgFarms

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
t.test(AdoptersIV$ShareOrgFarms, NonAdoptersIV$ShareOrgFarms) #not significant

#organic fields
t.test(AdoptersIV$ShareOrgArea, NonAdoptersIV$ShareOrgArea) #not significant

#IV and endo
#organic farms and info
t.test(InfoIV$ShareOrgFarms, NonInfoIV$ShareOrgFarms) #significant
t.test(InfoIV$ShareOrgArea, NonInfoIV$ShareOrgArea)#not significant
#check if that differs when having info not as binary
chisq.test(FullSample$q3_info, FullSample$ShareOrgFarms)
chisq.test(FullSample$q3_info, FullSample$ShareOrgArea)

#organic fields and field
t.test(FieldsIV$ShareOrgArea, NonFieldsIV$ShareOrgArea)#significant (0.05)
t.test(FieldsIV$ShareOrgFarms, NonFieldsIV$ShareOrgFarms)#significant
chisq.test(FullSample$NrFields, FullSample$ShareOrgFarms)
chisq.test(FullSample$NrFields, FullSample$ShareOrgArea)

#fields_b2?! negativ?! 2+advisory Pfeiffer&Langen?
SampleIV$advisory <- as.factor(SampleIV$advisory)
SampleIV$advisory <- relevel(SampleIV$advisory, ref = "Nordzucker")
#to compare we need to run the full mdoel with binary variables for info and field on the SAMPLEIV
summary(m.Full.comp <- glm(q1_adopt ~  info_b +
                             fields_b +
                          fields_dist+
                            sq.neidist+
                           minDist_demo + 
                             sq.demodist+
                             age_b + 
                             farmsize_b + 
                             AES_b +
                            # advisory,
                             Fabrikstandort, 
                           data = SampleIV, family = binomial("probit")))

m.Full.comp_mfx <-probitmfx(m.Full.comp, data = SampleIV)
plot_summs(m.Full.comp_mfx,
          coefs = c("knowing other farmers"="info_b1",
                     "observing fields"="fields_b1",
                    "distance to fields observed"="fields_dist",
                    "squared distance to fields observed"="sq.neidist",
                     "minimal distance to demo farm" = "minDist_demo",
                     "squared minimal distance to demo farm" = "sq.demodist",
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
#summary(m.Full.forInfo <- glm(q1_adopt ~  info_b + ShareOrgFarms + ShareOrgArea + age_b + farmsize_b + AES_b, 
#                           data = SampleIV, family = binomial("probit")))
#summary(m.Full.forInfo2 <- glm(q1_adopt ~  info_b + age_b + farmsize_b + AES_b, 
#                              data = SampleIV, family = binomial("probit")))



#residualsI <- m.Full.forInfo$residuals
#SampleIV <- cbind(SampleIV, residualsI)
#cor(SampleIV$residualsI, SampleIV$ShareOrgFarms)

#summary(m.Full.forField <- glm(q1_adopt ~  fields_b + ShareOrgFarms + ShareOrgArea + age_b + farmsize_b + AES_b, 
 #                             data = SampleIV, family = binomial("probit")))
#summary(m.Full.forField2 <- glm(q1_adopt ~  fields_b + age_b + farmsize_b + AES_b, 
  #                             data = SampleIV, family = binomial("probit")))
#
#residualsF <- m.Full.forField$residuals
#SampleIV <- cbind(SampleIV, residualsF)
#cor(SampleIV$residualsF, SampleIV$ShareOrgArea)

#pR2(m.Full.forInfo)
#pR2(m.Full.forInfo2)
#pR2(m.Full.forField)
#pR2(m.Full.forField2)



#regress x on z, z should be significant now
#summary(m.Full.relInfo <- glm(info_b ~ ShareOrgFarms + ShareOrgArea + age_b + farmsize_b + AES_b + minDist_demo + advisory, 
 #                             data = SampleIV, family = binomial("probit")))

#summary(m.Full.relField <- glm(fields_b ~  ShareOrgFarms +  ShareOrgArea + age_b + farmsize_b + AES_b + minDist_demo + advisory, 
 #                              data = SampleIV, family = binomial("probit")))



#following MHE: 
#use fitted values from 1. stage as instruments in OLS first stage

#####
#now add all the other variables
#exxclude those who have NA for age
#SampleIV<-SampleIV[!is.na(SampleIV$age_b),]
#info
#1. calculate non-linear fitted values ("1st stage")
nonli2 <- glm(info_b ~ ShareOrgFarms + ShareOrgArea+ fields_dist+
                sq.neidist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort, data = SampleIV, family = binomial("probit"))
#store values
y_hatnonli2 <- nonli2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
SampleIV$info_b <- as.numeric(as.character(SampleIV$info_b))
MHE1i2 <- lm(info_b ~ y_hatnonli2+ fields_dist+
               sq.neidist+
               minDist_demo + 
               sq.demodist+
               age_b + 
               farmsize_b + 
               AES_b +
               # advisory,
               Fabrikstandort, data = SampleIV)
info_iv <- MHE1i2$fitted.values

#2.second stage

MHE2i2 <- glm(q1_adopt ~ info_iv+fields_dist+
                sq.neidist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort, data = SampleIV, family = binomial("probit"))

summary(MHE2i2)

probitmfx(q1_adopt ~ info_iv+ fields_dist+
            sq.neidist+
            minDist_demo + 
            sq.demodist+
            age_b + 
            farmsize_b + 
            AES_b +
            # advisory,
            Fabrikstandort, data = SampleIV)

SampleIV$info_b <- as.factor(SampleIV$info_b)
#now add all the other variables
#exxclude those who have NA for age
#field
#1. calculate non-linear fitted values ("1st stage")
nonlf2 <- glm(fields_b ~ ShareOrgArea + ShareOrgFarms +fields_dist+
                sq.neidist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort, data = SampleIV, family = binomial("probit"))
#store values
y_hatnonlf2 <- nonlf2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
SampleIV$fields_b <- as.numeric(as.character(SampleIV$fields_b))
MHE1f2 <- lm(fields_b ~ y_hatnonlf2+fields_dist+
               sq.neidist+
               minDist_demo + 
               sq.demodist+
               age_b + 
               farmsize_b + 
               AES_b +
               # advisory,
               Fabrikstandort, data = SampleIV)
fields_iv <- MHE1f2$fitted.values

#2.second stage

MHE2f2 <- glm(q1_adopt ~ fields_iv +fields_dist+
                sq.neidist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort, data = SampleIV, family = binomial("probit"))

summary(MHE2f2)

probitmfx(q1_adopt ~ fields_iv+fields_dist+
            sq.neidist+
            minDist_demo + 
            sq.demodist+
            age_b + 
            farmsize_b + 
            AES_b +
            # advisory,
            Fabrikstandort, data = SampleIV)
SampleIV$fields_b <- as.factor(SampleIV$fields_b)
#now put both IVs teogether in the original full model
summary(m.Full.IV <- glm(q1_adopt ~  info_iv + fields_iv+
                           fields_dist+
                           sq.neidist+
                           minDist_demo + 
                           sq.demodist+
                           age_b + 
                           farmsize_b + 
                           AES_b +
                           # advisory,
                           Fabrikstandort, 
                      data = SampleIV, family = binomial("probit")))

m.Full.IV_mfx <-probitmfx(m.Full.IV, data = SampleIV)

p.mFull_mfx_compareIV <- plot_summs(m.Full.comp_mfx,m.Full.IV_mfx, 
                       coefs = c("knowing other farmers (info)"="info_b1",
                                 "Info_IV =knowing other farmers"="info_iv",
                                 "observing fields (field)"="fields_b1",
                                 "Field_IV = observing fields"="fields_iv",
                                 "distance to fields observed"="fields_dist",
                                 "squared distance to fields observed"="sq.neidist",
                                 "minimal distance to demo farm" = "minDist_demo",
                                 "squared minimal distance to demo farm" = "sq.demodist",
                                 "advisory Cosun" = "advisoryCosun",
                                 "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                 "advisory Südzucker"= "advisorySüdzucker",
                                 "older than 45 years"="age_b1",
                                 "farm size > 50 ha"="farmsize_b1",
                                 "AES participation"="AES_b1"),
                        model.names = c("Probit model", "Probit model with 3SLS-IV"),
                         scale = TRUE, robust = TRUE)#,colors = c("grey60", "grey36"))

p.mFull_mfx_compareIV +theme(legend.position="bottom")


#now enter distance to fields and number of peers known as factor and remove fields_b
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
                          coefs =c("knowing other farmers"="info_b1",
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
                          coefs = c("knowing other farmers"="info_b1",
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
                                    "observing fields" = "fields_b1",
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
                     "knowing other farmers"="info_b1",
                     "observing fields" = "fields_b1",
                     "minimal distance to demo farm" = "minDist_demo",
                     #"advisory Cosun" = "advisoryCosun",
                     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                     "advisory Südzucker"= "advisorySüdzucker",
                     "older than 45 years"="age_b1",
                     "farm size > 50 ha"="farmsize_b1",
                     "AES participation"="AES_b1"),
         model.names = c("Model (3)","Model (4)", "Model (5)"),
           scale = TRUE, robust = TRUE)#, colors = c("grey60", "grey36", "grey76"))

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
                        coefs = c("knowing other farmers"="info_b1",
                                  "IV_knowing other farmers"="info_iv",
                                  "observing fields"="fields_b1",
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
FullSample_forDist$meanDist <- as.numeric(FullSample_forDist$meanDist)
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
  scale_x_continuous(limits = c(0, 50))+
  #scale_y_continuous(limits = c(0, 0.6))+
  theme(#panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#+
 
p.hist1 <- ggplot(FullSample_forDist, aes(minDist_demo))+
  geom_histogram(binwidth = 5)+
  theme_bw()+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 60))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())+
  labs(x = "", y = "")
  
  
p.hist_effect_demo<-ggarrange( p.hist1,p.1, ncol =1, nrow = 2, heights = c(1,2))


  

#p.2<-effect_plot(m.dist, pred = sq.minDist_demo,interval = TRUE)#,outcome.scale = "link")
p.3<-effect_plot(m.dist, pred = meanDist, y.label = "probability",
                 x.label = "mean distance to other farmers' fields (km)", interval = TRUE)+
  theme_bw()+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 0.4))+
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#,outcome.scale = "link")
#p.4<-effect_plot(m.dist, pred = sq.meanDist,interval = TRUE)#,outcome.scale = "link")

p.hist3 <- ggplot(FullSample_forDist, aes(meanDist))+
  geom_histogram(binwidth = 5)+
  theme_bw()+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 60))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x = "", y = "")


p.hist_effect_neiFields<-ggarrange( p.hist3,p.3, ncol =1, nrow = 2, heights = c(1,2))




plot_grid(p.1, p.3, nrow = 1, ncol = 2)

#check ho that looks between the different type sof indication (map vs. multiple choice)
FullSample_forDist<- FullSample_forModels
FullSample_forDist$meanDist<-ifelse(FullSample_forDist$meanDist == "200", "NA",FullSample_forDist$meanDist)
FullSample_forDist$meanDist <- as.numeric(FullSample_forDist$meanDist)
FullSample_forDist_map <- FullSample_forDist[!is.na(FullSample_forDist$q5_other),]
FullSample_forDist_mc <- FullSample_forDist[!is.na(FullSample_forDist$q5_alt_dist),]
FullSample_forDist$map <- ifelse(!is.na(FullSample_forDist$q5_other),1,0)

#map
m.dist_map <-glm(q1_adopt ~ info_b 
             + meanDist
             + minDist_demo 
             + I(meanDist^2) 
             + I(minDist_demo^2)
             + advisory
             + age_b + farmsize_b + AES_b, 
             data = FullSample_forDist_map, family = binomial("probit"))

p.3_map<-effect_plot(m.dist_map, pred = meanDist, y.label = "probability",
                   #  main.title ="map",
                 x.label = "mean distance to other farmers' fields (km)", interval = TRUE)+#, outcome.scale = "link") +
  theme_bw()+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 1))+
  theme(#panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#+

p.hist3_map <- ggplot(FullSample_forDist_map, aes(meanDist))+
  geom_histogram(binwidth = 2)+
  theme_bw()+
  labs(title = "map")+
scale_x_continuous(limits = c(0, 50))+
 #scale_y_continuous(limits = c(0, 40))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
      #  axis.title.x=element_blank(),
       # axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
      )+
  labs(x = "", y = "")


p.hist_effect_demo_map<-ggarrange( p.hist3_map,p.3_map, ncol =1, nrow = 2, heights = c(1,2))
#multiple choice
m.dist_mc <-glm(q1_adopt ~ info_b 
                 + meanDist
                 + minDist_demo 
                 + I(meanDist^2) 
                 + I(minDist_demo^2)
                 + advisory
                 + age_b + farmsize_b + AES_b, 
                 data = FullSample_forDist_mc, family = binomial("probit"))

p.3_mc<-effect_plot(m.dist_mc, pred = meanDist, y.label = "probability",
                   # main.title = "multiple choice",
                     x.label = "mean distance to other farmers' fields (km)", interval = TRUE)+#, outcome.scale = "link") +
  theme_bw()+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 1))+
  theme(#panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#+

p.hist3_mc <- ggplot(FullSample_forDist_mc, aes(meanDist))+
  geom_histogram(binwidth = 2)+
  theme_bw()+
  labs(title = "multiple choice")+
 scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 40))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
      #axis.title.x=element_blank(),
       #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
      )+
  labs(x = "", y = "")


p.hist_effect_demo_mc<-ggarrange( p.hist3_mc,p.3_mc, ncol =1, nrow = 2, heights = c(1,2))

p.compare_hist_effect_mc_map <- ggarrange(p.hist_effect_demo_map,p.hist_effect_demo_mc, ncol = 2)

p.hist3_mc_map <- ggplot(FullSample_forDist, aes(meanDist, fill = map))+
  geom_histogram(binwidth = 2)+
  theme_bw()+
 # labs(title = "multiple choice")+
  scale_x_continuous(limits = c(0, 50))+
  scale_y_continuous(limits = c(0, 40))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()
  )+
  labs(x = "", y = "")



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
m.adv_demo <- summary(lm(minDist_demo ~ advisory, data = FullSample_forModels))




#check "new" regional variables

#check for subsample of those who indicated fields (to avoid bias through those fo which we assume the centroid of the landkreis)
Sample_CoordIndicated <- SampleIV[!is.na(SampleIV$q4_own),] #no difference found for this subgroup



summary(m.Full_regional <- glm(q1_adopt ~ info_b +
                                fields_b +
                                 fields_dist+
                                 sq.neidist+
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 Fabrikstandort+
                                 ShareSB+
                                meanFarmSize+
                                sand_content_percent+
                                elevation_in_m
                               , 
                           data = SampleIV, family = binomial("probit")))

m.Full_regional_mfx <-probitmfx(m.Full_regional, data = SampleIV)
plot_summs(m.Full_regional_mfx,
           coefs = c("knowing other farmers (info)"="info_b1",
                     "Info_IV =knowing other farmers"="info_iv",
                     "observing fields (field)"="fields_b1",
                     "Field_IV = observing fields"="fields_iv",
                     "distance to fields observed"="fields_dist",
                     "squared distance to fields observed"="sq.neidist",
                     "minimal distance to demo farm" = "minDist_demo",
                     "squared minimal distance to demo farm" = "sq.demodist",
                     "advisory Cosun" = "advisoryCosun",
                     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                     "advisory Südzucker"= "advisorySüdzucker",
                     "older than 45 years"="age_b1",
                     "farm size > 50 ha"="farmsize_b1",
                     "AES participation"="AES_b1",
                     "Share of sugarbeet area/ county"="ShareSB",
                     "mean farm size (ha)/ county" = "meanFarmSize",
                     "%Sand content in county"="sand_content_percent",
                     "elevation (m)"="elevation_in_m"),
            model.names = "Probit model on adoption decision",
           scale = TRUE, robust = TRUE)

#not inlcuded but possible:
#  advisory +#  ShareSmallFarms +#  areaDens+
#  farmDens+ #bundesland+ slope_in_degreesd+
#Kreis #+
# clay_content_percent +#  mainly_crop+
#  SB_region

VIF(m.Full_regional)
library(corrplot)
#create df with all variables probably in the model
df.models_corr <- SampleIV %>% dplyr::select(#q1_adopt,info_b ,
                                              # fields_b ,
                                               minDist_demo, 
                                               #advisory ,
                                                #age_b , 
                                                #farmsize_b , 
                                                #AES_b ,
                                                 ShareSmallFarms ,
                                                  ShareSB,
                                                 areaDens,
                                                 farmDens,
                                               meanFarmSize,
                                                #bundesland,
                                                #Kreis ,
                                                 clay_content_percent ,
                                               sand_content_percent,
                                                  slope_in_degreesd,
                                                elevation_in_m#,
                                               # mainly_crop,
                                               #SB_region
                                               )

#corrplot(df.models_corr)

