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
                           #  meanDist+
                           # sq.meanDist+
                       fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                          sq.fields_dist+
                           minDist_demo + 
                             sq.demodist+
                            age_b + 
                             farmsize_b + 
                             AES_b +
                            # advisory +
                            Fabrikstandort_agg 
                           ,data = SampleIV, family = binomial("probit")))

summary(m.Full.comp2 <- glm(q1_adopt ~  info_b +
                             fields_b +
                               meanDist+
                              sq.meanDist+
                             #fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                             #sq.fields_dist+
                             minDist_demo + 
                             sq.demodist+
                             age_b + 
                             farmsize_b + 
                             AES_b +
                             # advisory +
                             Fabrikstandort_agg 
                           ,data = SampleIV, family = binomial("probit")))

summary(m.Full.comp3 <- glm(q1_adopt ~  info_b +
                              fields_b +
                            #  meanDist+
                             # sq.meanDist+
                              #fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                              #sq.fields_dist+
                              minDist_demo + 
                              sq.demodist+
                              age_b + 
                              farmsize_b + 
                              AES_b +
                              # advisory +
                              Fabrikstandort_agg 
                            ,data = SampleIV, family = binomial("probit")))



m.Full.comp_mfx <-probitmfx(m.Full.comp, data = SampleIV, robust = TRUE)
m.Full.comp_mfx2 <-probitmfx(m.Full.comp2, data = SampleIV, robust = TRUE)
m.Full.comp_mfx3 <-probitmfx(m.Full.comp3, data = SampleIV, robust = TRUE)

plot_summs(m.Full.comp_mfx,m.Full.comp_mfx2,m.Full.comp_mfx3, scale = TRUE, robust = TRUE,
          coefs = c("knowing other farmers"="info_b1",
                    "observing fields"="fields_b1"))
         #           "distance to fields observed"="fields_dist",
          #          "squared distance to fields observed"="sq.fields_dist",
           #          "minimal distance to demo farm" = "minDist_demo",
            #         "squared minimal distance to demo farm" = "sq.demodist",
             #        "advisory Cosun" = "advisoryCosun",
              #       "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
               #      "advisory Südzucker"= "advisorySüdzucker",
                #    "older than 45 years"="age_b1",
                 #    "farm size > 50 ha"="farmsize_b1",
                  #   "AES participation"="AES_b1"),
           #model.names = "Probit model on adoption decision"))
          
#correlation between IV and error term
#first need model with endo and instrument
summary(m.Full.forInfo <- glm(q1_adopt ~  info_b + ShareOrgFarms + ShareOrgArea + age_b + farmsize_b + AES_b + Fabrikstandort_agg, 
                           data = SampleIV, family = binomial("probit")))
summary(m.Full.forInfo2 <- glm(q1_adopt ~  info_b + age_b + farmsize_b + AES_b + Fabrikstandort_agg, 
                              data = SampleIV, family = binomial("probit")))



residualsI <- m.Full.forInfo$residuals
SampleIV <- cbind(SampleIV, residualsI)
cor(SampleIV$residualsI, SampleIV$ShareOrgFarms)

summary(m.Full.forField <- glm(q1_adopt ~  fields_b + ShareOrgFarms + ShareOrgArea + age_b + farmsize_b + AES_b  + Fabrikstandort_agg, 
                              data = SampleIV, family = binomial("probit")))
summary(m.Full.forField2 <- glm(q1_adopt ~  fields_b + age_b + farmsize_b + AES_b  + Fabrikstandort_agg, 
                               data = SampleIV, family = binomial("probit")))
#
residualsF <- m.Full.forField$residuals
SampleIV <- cbind(SampleIV, residualsF)
cor(SampleIV$residualsF, SampleIV$ShareOrgArea)

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
nonli2 <- glm(info_b ~ ShareOrgFarms + ShareOrgArea+ 
               # fields_dist+
              #  sq.fields_dist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort_agg, data = SampleIV, family = binomial("probit"))
#store values
y_hatnonli2 <- nonli2$fitted.values 

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
SampleIV$info_b <- as.numeric(as.character(SampleIV$info_b))
MHE1i2 <- lm(info_b ~ y_hatnonli2+ 
             #  fields_dist+
             # sq.fields_dist+
               minDist_demo + 
               sq.demodist+
               age_b + 
               farmsize_b + 
               AES_b +
               # advisory,
               Fabrikstandort_agg, data = SampleIV)
info_iv <- MHE1i2$fitted.values

#2.second stage

MHE2i2 <- glm(q1_adopt ~ info_iv+
             #   fields_dist+
             #  sq.fields_dist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort_agg, data = SampleIV, family = binomial("probit"))

summary(MHE2i2)

m.info<-probitmfx(q1_adopt ~ info_iv+ 
        #   fields_dist+
        #  sq.fields_dist+
            minDist_demo + 
            sq.demodist+
            age_b + 
            farmsize_b + 
            AES_b +
            # advisory,
            Fabrikstandort_agg, data = SampleIV, robust = TRUE)

SampleIV$info_b <- as.factor(SampleIV$info_b)
#now add all the other variables
#exxclude those who have NA for age
#field
#1. calculate non-linear fitted values ("1st stage")
nonlf2 <- glm(fields_b ~ ShareOrgArea + ShareOrgFarms +
            #    fields_dist+
            #   sq.fields_dist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort_agg, data = SampleIV, family = binomial("probit"))
#store values
y_hatnonlf2 <- nonlf2$fitted.values #why only 227 observations here?

#2. use non-linear fitted values as Instrument for original endogenous variable
#1. "new" first stage as OLS
SampleIV$fields_b <- as.numeric(as.character(SampleIV$fields_b))
MHE1f2 <- lm(fields_b ~ y_hatnonlf2+
            #  fields_dist+
            #  sq.fields_dist+
               minDist_demo + 
               sq.demodist+
               age_b + 
               farmsize_b + 
               AES_b +
               # advisory,
               Fabrikstandort_agg, data = SampleIV)
fields_iv <- MHE1f2$fitted.values

#2.second stage

MHE2f2 <- glm(q1_adopt ~ fields_iv +
            #   fields_dist+
            #  sq.fields_dist+
                minDist_demo + 
                sq.demodist+
                age_b + 
                farmsize_b + 
                AES_b +
                # advisory,
                Fabrikstandort_agg, data = SampleIV, family = binomial("probit"))

summary(MHE2f2)

m.fields<-probitmfx(q1_adopt ~ fields_iv+
         #  fields_dist+
         #  sq.fields_dist+
            minDist_demo + 
            sq.demodist+
            age_b + 
            farmsize_b + 
            AES_b +
            # advisory,
            Fabrikstandort_agg, data = SampleIV, robust = TRUE)
SampleIV$fields_b <- as.factor(SampleIV$fields_b)
#now put both IVs teogether in the original full model
summary(m.Full.IV <- glm(q1_adopt ~  info_iv + 
                           fields_iv+
                         #  fields_dist+
                         #  sq.fields_dist+
                           minDist_demo + 
                           sq.demodist+
                           age_b + 
                           farmsize_b + 
                           AES_b +
                           # advisory,
                           Fabrikstandort_agg, 
                      data = SampleIV, family = binomial("probit")))

m.Full.IV_mfx <-probitmfx(m.Full.IV, data = SampleIV, robust = TRUE)

p.mFull_mfx_compareIV <- plot_summs(m.Full.comp_mfx3,m.Full.IV_mfx,scale = TRUE, robust = TRUE,
                      coefs = c("knowing other farmers (info)"="info_b1",
                                 "Info_IV =knowing other farmers"="info_iv",
                                 "observing fields (field)"="fields_b1",
                                 "Field_IV = observing fields"="fields_iv"),
                           #      "distance to fields observed"="fields_dist",
                          #       "squared distance to fields observed"="sq.fields_dist",
                         #        "minimal distance to demo farm" = "minDist_demo",
                          #       "squared minimal distance to demo farm" = "sq.demodist",
                           #      "advisory Cosun" = "advisoryCosun",
                            #     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                             #    "advisory Südzucker"= "advisorySüdzucker",
                              #   "older than 45 years"="age_b1",
                               #  "farm size > 50 ha"="farmsize_b1",
                                # "AES participation"="AES_b1",
                      colors = c("#FB9A99", "#E31A1C"),
                        model.names = c("Pre-registration model", "3SLS-IV Model"))
                         #,colors = c("grey60", "grey36"))

p.mFull_mfx_compareIV +theme(legend.position="bottom")


p.mFull_mfx_compareIV2 <- plot_summs(m.Full.comp_mfx,m.fields,m.info, 
                                    coefs = c("knowing other farmers (info)"="info_b1",
                                              "Info_IV =knowing other farmers"="info_iv",
                                              "observing fields (field)"="fields_b1",
                                              "Field_IV = observing fields"="fields_iv",
                                              "distance to fields observed"="fields_dist",
                                              "squared distance to fields observed"="sq.fields_dist",
                                              "minimal distance to demo farm" = "minDist_demo",
                                              "squared minimal distance to demo farm" = "sq.demodist",
                                              "advisory Cosun" = "advisoryCosun",
                                              "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                              "advisory Südzucker"= "advisorySüdzucker",
                                              "older than 45 years"="age_b1",
                                              "farm size > 50 ha"="farmsize_b1",
                                              "AES participation"="AES_b1"),
                                   # model.names = c("Probit model", "Probit model with 3SLS-IV"),
                                    scale = TRUE, robust = TRUE)#,colors = c("grey60", "grey36"))

#now enter distance to fields and number of peers known as factor and remove fields_b
SampleIV<- FullSample[!is.na(FullSample$minDist_demo)&!is.na(FullSample$advisory)&
                                    !is.na(FullSample$age_b)&!(FullSample$advisory == "Cosun"),]

SampleIV$advisory<- as.factor(SampleIV$advisory)
SampleIV$advisory <- relevel(SampleIV$advisory, ref = "Nordzucker")
SampleIV$FieldDist <- relevel(SampleIV$FieldDist, ref = "6")
summary(m.Allin2 <- glm(q1_adopt ~  info_b #+ fields_b 
                        + FieldDist+#+ I(field_meanDist^2) 
                       # fields_dist+
                        #  sq.fields_dist+
                          minDist_demo + 
                          sq.demodist+
                          age_b + 
                          farmsize_b + 
                          AES_b +
                          # advisory,
                          Fabrikstandort_agg, 
                        data = SampleIV,family = binomial("probit")))#ref-category:0 = 0 peers

m.Allin2_mfx <-probitmfx(m.Allin2, data = SampleIV)
p.Allin2_mfx <- plot_summs(m.Allin2, 
                          coefs =c("knowing other farmers"="info_b1",
                                   "observed fields in 0-5km"="FieldDist0",
                                   "observed fields in 6-10km"="FieldDist1",
                                   "observed fields in 11-15km"="FieldDist2",
                                   "observed fields in 16-20km"="FieldDist3",
                                   "observed fields in 21-30km"="FieldDist4",
                                   "observed fields in more than 30km"="FieldDist5",
                                   "distance to fields observed"="fields_dist",
                                   "squared distance to fields observed"="sq.fields_dist",
                                   "minimal distance to demo farm" = "minDist_demo",
                                   "squared minimal distance to demo farm" = "sq.demodist",
                                  # "observed fields in more than 30km"="SampleIV$FieldDist5",
                                   #"No fields observed"="SampleIV$FieldDist6",
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
summary(m.Full <- glm(q1_adopt ~  NrFields +info_b+
                      fields_dist+
                        sq.fields_dist+
                        minDist_demo + 
                        sq.demodist+
                        age_b + 
                        farmsize_b + 
                        AES_b +
                        # advisory,
                        Fabrikstandort_agg, 
                      data = SampleIV, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full_mfx <-probitmfx(m.Full, data = SampleIV)
p.mFull_mfx <- plot_summs(m.Full_mfx, 
                          coefs = c("knowing other farmers"="info_b1",
                            "1-5 fields observed"="NrFields1",
                                    "6-10 fields observed"="NrFields2",
                                    "11-15 fields observed"="NrFields3",
                                    "more than 15 fields observed"="NrFields4",
                                    "minimal distance to demo farm" = "minDist_demo",
                            "distance to fields observed"="fields_dist",
                            "squared distance to fields observed"="sq.fields_dist",
                            "minimal distance to demo farm" = "minDist_demo",
                            "squared minimal distance to demo farm" = "sq.demodist",
                                    #"advisory Cosun" = "advisoryCosun",
                                    "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                    "advisory Südzucker"= "advisorySüdzucker",
                                    "older than 45 years"="age_b1",
                                    "farm size > 50 ha"="farmsize_b1",
                                    "AES participation"="AES_b1"),
                          model.names = "Probit model on adoption decision",
                          scale = TRUE, robust = TRUE)

#now enter nr, of adopters known as category
summary(m.Full2 <- glm(q1_adopt ~   q3_info + fields_b+
                       fields_dist+
                         sq.fields_dist+
                         minDist_demo + 
                         sq.demodist+
                         age_b + 
                         farmsize_b + 
                         AES_b +
                         # advisory,
                         Fabrikstandort_agg, 
                      data = SampleIV, family = binomial("probit")))#ref-category:0 = 0 peers

m.Full2_mfx <-probitmfx(m.Full2, data = SampleIV)
p.mFull2_mfx <- plot_summs(m.Full2_mfx, 
                          coefs = c("1-5 adopters known"="q3_info1",
                                    "6-10 adopters known"="q3_info2",
                                    "more than 10 adopters known"="q3_info3",
                                    "minimal distance to demo farm" = "minDist_demo",
                                    "distance to fields observed"="fields_dist",
                                    "squared distance to fields observed"="sq.fields_dist",
                                    "minimal distance to demo farm" = "minDist_demo",
                                    "squared minimal distance to demo farm" = "sq.demodist",
                                    #"advisory Cosun" = "advisoryCosun",
                                    "observing fields" = "fields_b1",
                                    "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                                    "advisory Südzucker"= "advisorySüdzucker",
                                    "older than 45 years"="age_b1",
                                    "farm size > 50 ha"="farmsize_b1",
                                    "AES participation"="AES_b1"),
                          model.names = "Probit model on adoption decision",
                          scale = TRUE, robust = TRUE)



models_cat <-plot_summs(m.Allin2_mfx,m.Full2_mfx, m.Full_mfx, 
           coefs = c("1-5 adopters known"="q3_info1",
                     "6-10 adopters known"="q3_info2",
                     "more than 10 adopters known"="q3_info3",
                     "observed fields in 0-5km"="FieldDist0",
                     "observed fields in 6-10km"="FieldDist1",
                     "observed fields in 11-15km"="FieldDist2",
                     "observed fields in 16-20km"="FieldDist3",
                     "observed fields in 21-30km"="FieldDist4",
                     "observed fields in more than 30km"="FieldDist5",
                     "1-5 fields observed"="NrFields1",
                     "6-10 fields observed"="NrFields2",
                     "11-15 fields observed"="NrFields3",
                     "more than 15 fields observed"="NrFields4",
                     "knowing other farmers"="info_b1",
                     "observing fields" = "fields_b1"),
           colors = c("Paired"),
                   #  "minimal distance to demo farm" = "minDist_demo",
                    # "distance to fields observed"="fields_dist",
                    # "squared distance to fields observed"="sq.fields_dist",
                    # "minimal distance to demo farm" = "minDist_demo",
                    # "squared minimal distance to demo farm" = "sq.demodist",
                     #"advisory Cosun" = "advisoryCosun",
                    # "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                    # "advisory Südzucker"= "advisorySüdzucker",
                    # "older than 45 years"="age_b1",
                    # "farm size > 50 ha"="farmsize_b1",
                    # "AES participation"="AES_b1"),
         model.names = c("NrInfo","NrFields","FieldDistance"),
           scale = TRUE, robust = TRUE)#, colors = c("grey60", "grey36", "grey76"))

models_cat +theme(legend.position="bottom")

#stargazer(m.Full2_mfx$fit,m.Allin2_mfx$fit, m.Full_mfx$fit,
 #         type = "html", out = "Categorical.html", star.char = c("*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01),
  #        dep.var.caption  = "Mechanical weeding yes/no",
   #       coef = list(m.Full2_mfx$mfxest[,1],m.Allin2_mfx$mfxest[,1],m.Full_mfx$mfxest[,1]) ,
    #      se = list(m.Full2_mfx$mfxest[,2],m.Allin2_mfx$mfxest[,2],m.Full_mfx$mfxest[,2]),
     #     dep.var.labels   = "")


#start to compare different odels for substituitability
#To do so, we compare different models, that include
#1. intercept (naive model)
#2. intercept + controls
#3. intercept + controls + Fieldi
#4. intercept + controls + Infoi
#5. intercept + controls + Fieldi + Infoi
#exclude those with some NA in one of the variable sincluded in the model



m.1.mfx <- probitmfx(m.1<- glm(q1_adopt ~ 1,  
                        data = SampleIV,family = binomial("probit")), data = SampleIV) 

m.2.mfx <- probitmfx(m.2 <-glm(q1_adopt ~ 
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory,
                                 Fabrikstandort_agg, 
                        data = SampleIV,family = binomial("probit")), data = SampleIV) 

m.3.mfx <- probitmfx(m.3 <-glm(q1_adopt ~ info_b +
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory,
                                 Fabrikstandort_agg, 
                   data = SampleIV,family = binomial("probit")), data = SampleIV)

m.4.mfx <- probitmfx(m.4 <-glm(q1_adopt ~ fields_b+ 
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory,
                                 Fabrikstandort_agg, 
                   data = SampleIV,family = binomial("probit")), data = SampleIV)

m.5.mfx <- probitmfx(m.5 <-glm(q1_adopt ~ info_b + fields_b+ 
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory,
                                 Fabrikstandort_agg, 
                   data = SampleIV,family = binomial("probit")), data = SampleIV)

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
m1<- matrix(table(SampleIV$q1_adopt, probabilities_m.1))
#correct:
m1[1]/(m1[1]+m1[2])

#model2
m2<- matrix(table(SampleIV$q1_adopt, probabilities_m.2))
#correct:
(m2[1]+m2[4])/(m2[1]+m2[2]+m2[3]+m2[4])

#model3
m3<- matrix(table(SampleIV$q1_adopt, probabilities_m.3))
#correct:
(m3[1]+m3[4])/(m3[1]+m3[2]+m3[3]+m3[4])

#model4
m4<- matrix(table(SampleIV$q1_adopt, probabilities_m.4))
#correct:
(m4[1]+m4[4])/(m4[1]+m4[2]+m4[3]+m4[4])

#model5
m5<- matrix(table(SampleIV$q1_adopt, probabilities_m.5))
#correct:
(m5[1]+m5[4])/(m5[1]+m5[2]+m5[3]+m5[4])





#get plots fo distance variables

SampleIV$pred_Y <- predict(m.Allin2, SampleIV, type = "response")
m.Allin2$fitted.values


#first need one regression that includes the distance variables

#regression from above we take for demo-dist
summary(m.Full.comp3 <- glm(q1_adopt ~  info_b +
                              fields_b +
                              #  meanDist+
                              # sq.meanDist+
                             # fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                            #  sq.fields_dist+
                              minDist_demo + 
                              sq.demodist+
                              age_b + 
                              farmsize_b + 
                              AES_b +
                              # advisory +
                              Fabrikstandort_agg 
                            ,data = SampleIV, family = binomial("probit")))

p.1<-effect_plot(m.Full.comp3, pred = minDist_demo, y.label = "probability",
                 x.label = "minimal distance to demo farm (km)")+#, outcome.scale = "link") +
  theme_bw()+
 scale_x_continuous(limits = c(0, 50))+
# scale_y_continuous(limits = c(0, 0.1))+
  theme(#panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#+
 
p.hist1 <- ggplot(SampleIV, aes(minDist_demo))+
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

#do the same for distance to other farmers fields
#herefore we need to subset the sample to those who observe fields

SampleObserver <- SampleIV %>% dplyr::filter(fields_b != "0")
#all who have larger values than 50 reduc eto 50
SampleObserver$fields_dist <- ifelse(SampleObserver$fields_dist > 50, 50,SampleObserver$fields_dist)
SampleObserver$sq.fields_dist <- SampleObserver$fields_dist*SampleObserver$fields_dist
SampleObserver$selection_distance <- droplevels(SampleObserver$selection_distance)
#and then run a regression where distance to other farmrs fields enters a s numeric variable

summary(m.Observers_fielddist<- glm(q1_adopt ~  info_b +
                             # fields_b +
                              #  meanDist+
                              # sq.meanDist+
                               fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                                sq.fields_dist+
                              minDist_demo + 
                              sq.demodist+
                              age_b + 
                              farmsize_b + 
                              AES_b +
                              # advisory +
                              Fabrikstandort_agg 
                            ,data = SampleObserver, family = binomial("probit")))
  

#p.2<-effect_plot(m.dist, pred = sq.minDist_demo,interval = TRUE)#,outcome.scale = "link")
p.3<-effect_plot(m.Observers_fielddist, pred = fields_dist, y.label = "probability",
                 x.label = "mean distance to other farmers' fields (km)")+#, interval = TRUE, outcome.scale = "link")+
  theme_bw()+
  #scale_x_continuous(limits = c(0, 50))+
  #scale_y_continuous(limits = c(0, 0.4))+
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#p.4<-effect_plot(m.dist, pred = sq.meanDist,interval = TRUE)#,outcome.scale = "link")

p.hist3 <- ggplot(SampleObserver, aes(fields_dist))+
  geom_histogram(binwidth = 5)+
  theme_bw()+
 # scale_x_continuous(limits = c(0, 50))+
 # scale_y_continuous(limits = c(0, 60))+
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
ggplot(SampleObserver, aes(fields_dist, fill = selection_distance))+
  geom_histogram(binwidth = 5,alpha = 0.5, position = 'identity')+
  theme_bw()+
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x = "Distance to other farmers' fields [km]", y = "")+
    scale_fill_discrete(name = "Selection of fields", labels = c("map", "multiple choice"))

#check via t.test
t.test(SampleObserver[SampleObserver$selection_distance == 0,]$fields_dist,SampleObserver[SampleObserver$selection_distance == 1,]$fields_dist, )
SampleObserver$FieldDist <- droplevels(SampleObserver$FieldDist)
chisq.test(table(SampleObserver$selection_distance, SampleObserver$FieldDist))
#check for effect on adoption

summary(m.Observers_selectiondist<- glm(q1_adopt ~  info_b +
                                          selection_distance +
                                      # fields_b +
                                      #  meanDist+
                                      # sq.meanDist+
                                     # fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                                    #  sq.fields_dist+
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory +
                                      Fabrikstandort_agg 
                                    ,data = SampleObserver, family = binomial("probit")))


#univariate
chisq.test(table(SampleObserver$q1_adopt, SampleObserver$selection_distance))





#check number of fields observed in same way
ggplot(SampleObserver, aes(NrFields, fill = selection_distance))+
  geom_bar(alpha = 0.5, position = 'identity')+
  theme_bw()+
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Number of fields observed", y = "")+
  scale_fill_discrete(name = "Selection of fields", labels = c("map", "multiple choice"))+
  scale_x_discrete(labels = c("1-5","6-10","11-15","more than 15"))

chisq.test(table(SampleObserver$selection_distance, SampleObserver$NrFields))
chisq.test(table(SampleObserver$q1_adopt, SampleObserver$NrFields))


OwnFieldsSample$Mean_ownfield_dist <- as.numeric(OwnFieldsSample$Mean_ownfield_dist)
OwnFieldsSample$q1_adopt <- as.factor(OwnFieldsSample$q1_adopt)
#check for role of distance between own fields
summary(m.Observers_Ownfielddist<- glm(q1_adopt ~  info_b +
                                      # fields_b +
                                      #  meanDist+
                                      # sq.meanDist+
                                     # fields_dist+ #0 if no fields observed, otherwise km to fields observed 
                                      #sq.fields_dist+
                                       Mean_ownfield_dist+
                                   #   I(Mean_ownfield_dist^2)+
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory +
                                      Fabrikstandort_agg 
                                    ,data = OwnFieldsSample, family = binomial("probit")))


#p.2<-effect_plot(m.dist, pred = sq.minDist_demo,interval = TRUE)#,outcome.scale = "link")
p.own_fields1<-effect_plot(m.Observers_Ownfielddist, pred = Mean_ownfield_dist, y.label = "probability",
                 x.label = "mean distance to own fields (km)")+#, interval = TRUE)+#outcome.scale = "link")+
  theme_bw()+
 # xlim(0,5)+
# scale_x_continuous(limits = c(0, 5))+
  #scale_y_continuous(limits = c(0, 0.4))+
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#p.4<-effect_plot(m.dist, pred = sq.meanDist,interval = TRUE)#,outcome.scale = "link")


#add density plot above
p.own_fields2<-ggplot(OwnFieldsSample,aes(Mean_ownfield_dist)) +            
  geom_density()+
  theme_bw()+
 # scale_x_continuous(limits = c(0, 5))+
  theme_bw()+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x = "", y = "")
 

p.effect_ownFields<-ggarrange(p.own_fields2, p.own_fields1, ncol =1, nrow = 2, heights = c(1,2))

#check "new" regional variables

#check for subsample of those who indicated fields (to avoid bias through those fo which we assume the centroid of the landkreis)
Sample_CoordIndicated <- SampleIV[!is.na(SampleIV$q4_own),] #no difference found for this subgroup



summary(m.Full_regional <- glm(q1_adopt ~ info_b +
                                fields_b +
                            #    + fields_dist+
                             #   sq.fields_dist+
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory+
                                Fabrikstandort_agg+
                             #   SB_region+
                               meanFarmSize2+
                               sand_content_percent_mean+
                                 elevation_in_m_mean
                               , 
                           data = SampleIV, family = binomial("probit")))

m.Full_regional_mfx <-probitmfx(m.Full_regional, data = SampleIV)

summary(m.Full_regional2 <- glm(q1_adopt ~ info_b +
                                 fields_b +
                            #     + meanDist+
                             #   sq.meanDist +
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 # advisory+
                                 Fabrikstandort_agg+
                                 SB_region+
                                 meanFarmSize2+
                                 sand_content_percent_mean+
                                 elevation_in_m_mean
                               , 
                               data = SampleIV, family = binomial("probit")))

m.Full_regional_mfx2 <-probitmfx(m.Full_regional2, data = SampleIV)
plot_summs(m.Full_regional_mfx,m.Full_regional_mfx2, scale = TRUE, robust = TRUE,
           coefs = c("knowing other farmers (info)"="info_b1",
                     "Info_IV =knowing other farmers"="info_iv",
                     "observing fields (field)"="fields_b1",
                     "Field_IV = observing fields"="fields_iv",
                     "distance to fields observed"="fields_dist",
                     "squared distance to fields observed"="sq.fields_dist",
                     "minimal distance to demo farm" = "minDist_demo",
                     "squared minimal distance to demo farm" = "sq.demodist",
                     "advisory Cosun" = "advisoryCosun",
                     "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                     "advisory Südzucker"= "advisorySüdzucker",
                     "older than 45 years"="age_b1",
                     "farm size > 50 ha"="farmsize_b1",
                     "AES participation"="AES_b1",
                     "sugar beet area"="SB_region",
                     "mean farm size (ha)/ county" = "meanFarmSize2",
                     "sand content/ county"="sand_content",
                     "elevation (m)"="elev_mean"),
            model.names = "Probit model on adoption decision")
          

#not inlcuded but possible:
#  advisory +#  ShareSmallFarms +#  areaDens+
#  farmDens+ #bundesland+ slope_in_degreesd+
#Kreis #+
# clay_content_percent +#  mainly_crop+
#  SB_region

VIF(m.Full_regional)


#####INTENTION#####

#problem with anklam in fabrikstandort_agg
#SampleIV$Fabrikstandort_agg <- relevel(SampleIV$Fabrikstandort_agg, ref = "West")

#try ordered probit model
orderedprobit1<-polr(q6_col1 ~ info_b + fields_b+
                      minDist_demo + 
                      sq.demodist+
                      age_b + 
                      farmsize_b + 
                       AES_b +
                       # advisory,
                       Fabrikstandort_agg
                       , 
                     data = SampleIV,Hess= TRUE)
summary(orderedprobit1)
m.Intention1_mfx <-probitmfx(orderedprobit1, data = SampleIV, robust = TRUE)
plot_summs(m.Intention1_mfx)
#how to get marginal effects?
#probitmfx(orderedprobit1, data = df.Models)
orderedprobit2<-polr(q6_col2 ~ info_b + fields_b+
                       minDist_demo + 
                       sq.demodist+
                       age_b + 
                       farmsize_b + 
                       AES_b +
                       # advisory,
                       Fabrikstandort_agg
                     , 
                     data = SampleIV,Hess = TRUE)

m.Intention2_mfx <-probitmfx(orderedprobit2, data = SampleIV, robust = TRUE)

orderedprobit3<-polr(q6_col3 ~ info_b + fields_b+
                       minDist_demo + 
                       sq.demodist+
                       age_b + 
                       farmsize_b + 
                       AES_b +
                       # advisory,
                      Fabrikstandort_agg
                     , 
                     data = SampleIV,Hess = TRUE)

m.Intention3_mfx <-probitmfx(orderedprobit3, data = SampleIV, robust = TRUE)

models_Intention <- plot_summs(m.Full.comp_mfx3, m.Intention1_mfx,m.Intention2_mfx,m.Intention3_mfx,
           coefs = c("knowing other farmers (info)"="info_b1",
                     "Info_IV =knowing other farmers"="info_iv",
                     "observing fields (field)"="fields_b1",
                     "Field_IV = observing fields"="fields_iv",
                     "distance to fields observed"="fields_dist"),
           colors = c("#FB9A99", "dodgerblue", "royalblue2", "royalblue4"),
                   #  "squared distance to fields observed"="sq.fields_dist",
                  #   "minimal distance to demo farm" = "minDist_demo",
                  #   "squared minimal distance to demo farm" = "sq.demodist",
                  #   "advisory Cosun" = "advisoryCosun",
                  #   "advisory Pfeiffer&Langen" = "advisoryPfeifferLangen",
                  #   "advisory Südzucker"= "advisorySüdzucker",
                  #   "older than 45 years"="age_b1",
                  #   "farm size > 50 ha"="farmsize_b1",
                  #   "AES participation"="AES_b1",
                  #   "share of sugarbeet area/ county"="ShareSB",
                  #   "mean farm size (ha)/ county" = "meanFarmSize2",
                  #   "sand content/ county"="sand_content",
                  #   "elevation (m)"="elev_mean"),
           model.names = c("pre-registration model","Intention traditional weeding ","Intention modern weeding ", "Intention autonomous weeding"),
           scale = TRUE, robust = TRUE
           )


models_Intention +theme(legend.position="bottom")+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


#test effect of distance between own fields on info and field

t.test(InfoIV$Mean_ownfield_dist, NonInfoIV$Mean_ownfield_dist)
t.test(FieldsIV$Mean_ownfield_dist, NonFieldsIV$Mean_ownfield_dist)
t.test(AdoptersIV$Mean_ownfield_dist, NonAdoptersIV$Mean_ownfield_dist)






