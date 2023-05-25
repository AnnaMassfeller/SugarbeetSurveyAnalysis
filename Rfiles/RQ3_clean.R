#RQ3
library(dplyr)
library(caret)
library(ggpubr)
#create aggregated variables
SampleIV$info_b <- plyr::revalue(SampleIV$info_b, c("0"="noAdoptersKnown", "1"="AdoptersKnown"))
SampleIV$fields_b <- plyr::revalue(SampleIV$fields_b, c("0"="noFieldsObserved", "1"="FieldsObserved"))
SampleIV$q3_info <- plyr::revalue(SampleIV$q3_info, c("0"="noAdopters", "1"="1-5Adopters", "2"="6-10Adopters", "3"="more than 10Adopters"))
SampleIV$NrFields <- plyr::revalue(SampleIV$NrFields, c("0"="noFields", "1"="1-5Fields", "2"="6-10Fields", "3"="11-15Fields", "4"="more than 15Fields"))
SampleIV$FieldDist <- plyr::revalue(SampleIV$FieldDist, c("0"="0-5km", "1"="6-10km", "2"="11-15km", "3"="16-20km", "4"="21-30km", "5"="more than 30km", "6"= "noFields"))

#nrfields
SampleIV$NrFields_agg <- SampleIV$NrFields #create aggregated var where 11-15 and more than 15 are in one group
SampleIV<-SampleIV %>%
  mutate(NrFields_agg = dplyr::recode(NrFields_agg, "more than 15Fields" ="11-15Fields"))
SampleIV$NrFields_agg<- plyr::revalue(SampleIV$NrFields_agg, c("11-15Fields"="11 and more Fields"))
SampleIV$NrFields_agg<- as.factor(SampleIV$NrFields_agg)
SampleIV$NrFields_agg<- factor(SampleIV$NrFields_agg, levels = c("noFields", "1-5Fields", "6-10Fields", "11 and more Fields"))
table(SampleIV$NrFields_agg)

#distance to fields
SampleIV$FieldDist_agg <- SampleIV$FieldDist #create aggregated var where 11-15, 16-20 & 21-30 are in one group
SampleIV<-SampleIV %>%
  dplyr::mutate(FieldDist_agg= dplyr::recode(FieldDist_agg, "16-20km" = "11-15km"))
SampleIV<-SampleIV %>%
  dplyr::mutate(FieldDist_agg= dplyr::recode(FieldDist_agg, "21-30km" = "11-15km"))
SampleIV$FieldDist_agg<- plyr::revalue(SampleIV$FieldDist_agg, c("11-15km"="11-30km"))
SampleIV$FieldDist_agg<- as.factor(SampleIV$FieldDist_agg)
SampleIV$FieldDist_agg<- factor(SampleIV$FieldDist_agg, levels = c("noFields", "0-5km", "6-10km", "11-30km","more than 30km"))
table(SampleIV$FieldDist_agg)

#aggregate even further: distance to fields
SampleIV$FieldDist_agg2 <- SampleIV$FieldDist #create aggregated var where 11-15, 16-20 & 21-30 are in one group
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "16-20km" = "11-15km"))
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "21-30km" = "11-15km"))
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "more than 30km" = "11-15km"))
SampleIV$FieldDist_agg2<- plyr::revalue(SampleIV$FieldDist_agg2, c("11-15km"="11km and more"))
SampleIV$FieldDist_agg2<- as.factor(SampleIV$FieldDist_agg2)
SampleIV$FieldDist_agg2<- factor(SampleIV$FieldDist_agg2, levels = c("noFields", "0-5km", "6-10km", "11km and more"))
table(SampleIV$FieldDist_agg2)


#nrAdopters
SampleIV$NrAdopters_agg <- SampleIV$q3_info #create aggregated var where 6-10 and more than 10
SampleIV$NrAdopters_agg<-as.factor(SampleIV$NrAdopters_agg)
SampleIV<-SampleIV %>%
  mutate(NrAdopters_agg= dplyr::recode(NrAdopters_agg, "more than 10Adopters" = "6-10Adopters"))
SampleIV$NrAdopters_agg<- plyr::revalue(SampleIV$NrAdopters_agg, c("6-10Adopters"="6 and more Adopters"))
SampleIV$NrAdopters_agg<- as.factor(SampleIV$NrAdopters_agg)
table(SampleIV$NrAdopters_agg)

#set reference categories
SampleIV <- within(SampleIV, NrFields_agg <- relevel(NrFields_agg, ref = "noFields"))
SampleIV <- within(SampleIV, FieldDist_agg <- relevel(FieldDist_agg, ref = "noFields"))
SampleIV <- within(SampleIV, NrAdopters_agg <- relevel(NrAdopters_agg, ref = "noAdopters"))
SampleIV$q3_info <- as.factor(SampleIV$q3_info)
SampleIV <- within(SampleIV, q3_info <- relevel(q3_info, ref = "noAdopters"))
SampleIV <- within(SampleIV, FieldDist_agg2 <- relevel(FieldDist_agg2, ref = "noFields"))

#create subsample of observers

df.Observer <- SampleIV %>% filter(fields_b == "FieldsObserved")
df.Observer$NrFields_agg <- droplevels(df.Observer$NrFields_agg)
df.Observer$FieldDist_agg2 <- droplevels(df.Observer$FieldDist_agg2)

summary(PreReg_NrAdopters <- glm(q1_adopt ~ 
                                   q3_info+
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_NrAdopters_mfx<-mfx::probitmfx(PreReg_NrAdopters, data = SampleIV)

summary(PreReg_fields_bNrAdopters <- glm(q1_adopt ~ 
                                           fields_b+q3_info +
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_fields_bNrAdopters_mfx<-mfx::probitmfx(PreReg_fields_bNrAdopters, data = SampleIV)
plot_summs(PreReg_fields_bNrAdopters_mfx, robust = TRUE, scale = TRUE)

summary(PreReg_NrFields <- glm(q1_adopt ~ 
                                 NrFields_agg +
                                 minDist_demo + 
                                 sq.demodist+
                                 age_b + 
                                 farmsize_b + 
                                 AES_b +
                                 Fabrikstandort_agg 
                               ,data = SampleIV, family = binomial("probit")))

PreReg_NrFields_mfx<-mfx::probitmfx(PreReg_NrFields, data = SampleIV)

summary(PreReg_info_bNrFields <- glm(q1_adopt ~ 
                                       info_b+NrFields_agg +
                                       minDist_demo + 
                                       sq.demodist+
                                       age_b + 
                                       farmsize_b + 
                                       AES_b +
                                       Fabrikstandort_agg 
                                     ,data = SampleIV, family = binomial("probit")))

PreReg_info_bNrFields_mfx<-mfx::probitmfx(PreReg_info_bNrFields, data = SampleIV)

summary(PreReg_NrFieldsNrAdopters <- glm(q1_adopt ~ 
                                           NrFields_agg+q3_info+
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_NrFieldsNrAdopters_mfx<-mfx::probitmfx(PreReg_NrFieldsNrAdopters, data = SampleIV)


summary(PreReg_FieldsDist <- glm(q1_adopt ~ 
                                           FieldDist_agg +
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDist_mfx<-mfx::probitmfx(PreReg_FieldsDist, data = SampleIV)

summary(PreReg_FieldsDistinfo_b <- glm(q1_adopt ~ 
                                   FieldDist_agg + q3_info+
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistinfo_b_mfx<-mfx::probitmfx(PreReg_FieldsDistinfo_b, data = SampleIV)

summary(PreReg_FieldsDistNrFields <- glm(q1_adopt ~ 
                                   FieldDist_agg +
                                     NrFields_agg +
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrFields_mfx<-mfx::probitmfx(PreReg_FieldsDistNrFields, data = df.Observer)

summary(PreReg_FieldsDistNrAdopters <- glm(q1_adopt ~ 
                                   FieldDist_agg+
                                     q3_info +
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrAdopters_mfx<-mfx::probitmfx(PreReg_FieldsDistNrAdopters, data = SampleIV)


summary(PreReg_FieldsDistNrFieldsNrAdopters <- glm(q1_adopt ~ 
                                   FieldDist_agg +
                                     q3_info+
                                     NrFields_agg+
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrFieldsNrAdopters_mfx<-mfx::probitmfx(PreReg_FieldsDistNrFieldsNrAdopters, data = SampleIV)

plot_summs(PreReg_FieldsDistNrFieldsNrAdopters_mfx,
           scale = TRUE, robust = TRUE, colors = "Greys",
           omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
               "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
               "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
               "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
               "Fabrikstandort_aggOffstein"))



m.1.mfx <- mfx::probitmfx(m.1<- glm(q1_adopt ~ 1,  
                                    data = SampleIV,family = binomial("probit")), data = SampleIV) 

m.2.mfx <- mfx::probitmfx(m.2 <-glm(q1_adopt ~ 
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory,
                                      Fabrikstandort_agg, 
                                    data = SampleIV,family = binomial("probit")), data = SampleIV) 

m.3.mfx <- mfx::probitmfx(m.3 <-glm(q1_adopt ~ info_b +
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory,
                                      Fabrikstandort_agg, 
                                    data = SampleIV,family = binomial("probit")), data = SampleIV)

m.4.mfx <- mfx::probitmfx(m.4 <-glm(q1_adopt ~ fields_b+ 
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory,
                                      Fabrikstandort_agg, 
                                    data = SampleIV,family = binomial("probit")), data = SampleIV)
summary(m.4)
m.5.mfx <- mfx::probitmfx(m.5 <-glm(q1_adopt ~ info_b + fields_b+ 
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory,
                                      Fabrikstandort_agg, 
                                    data = SampleIV,family = binomial("probit")), data = SampleIV)


plot_summs(#m.2.mfx,
           PreReg_NrFields_mfx,
           m.4.mfx,
           PreReg_FieldsDistNrFields_mfx,
           PreReg_FieldsDist_mfx,
           m.3.mfx,
           PreReg_NrFieldsNrAdopters_mfx,
           m.5.mfx,
           PreReg_info_bNrFields_mfx,
           PreReg_NrAdopters_mfx,
           PreReg_fields_bNrAdopters_mfx,
           PreReg_FieldsDistinfo_b_mfx,
           PreReg_FieldsDistNrAdopters_mfx,
           scale = TRUE, robust = TRUE,
          model.names = c("c) NrFields", "d) ObserveFields", "e) FieldDist&NrFields", 
                          "f) FieldDist", "g) KnowAdopters", "h) NrAdopters&NrFields", 
                          "i) KnowAdopters&ObserveFields", "j) KnowAdopters&NrFields",
                          "k) NrAdopters", "l) NrAdopters&ObserveFields", "m) KnowAdopters&FieldDist"
                          , "n) NrAdopters&FieldDist"), 
          colors = c("grey50","grey50","grey70","grey50","grey50","grey20","grey20","grey20","grey50","grey20","grey20","grey20"),
          # omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
           #             "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffstein","Fabrikstandort_aggOffenau",
            #            "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain",
                      #  "Fabrikstandort_aggSachsenAnhalt",
                     #   "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest"))#,
          coefs = c("Observing fields (binary)" = "fields_bFieldsObserved",
                    "Knowing Adopters (binary)" = "info_bAdoptersKnown",
                    "1-5 adopters known"="q3_info1-5Adopters",
                    "6-10 adopters known"="q3_info6-10Adopters",
                    ">10 adopters known"="q3_infomore than 10Adopters",
                    "1-5 fields observed"="NrFields_agg1-5Fields",
                    "6-10 fields observed"="NrFields_agg6-10Fields",
                    ">10 fields observed"="NrFields_agg11 and more Fields",
                    "observed fields in 0-5km"="FieldDist_agg0-5km",
                    "observed fields in 6-10km"="FieldDist_agg6-10km",
                    "observed fields in 11-30km"="FieldDist_agg11-30km",
                    "observed fields in >30km"="FieldDist_aggmore than 30km"))


plot_summs(
  PreReg_InteractFieldDistNrAdopters_mfx,
  PreReg_InteractNrFieldsFieldDist_mfx,
  scale = TRUE, robust = TRUE, colors = "Greys",
  omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
                 "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
                 "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
                 "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
                 "Fabrikstandort_aggOffstein"))

library(coefplot)
coefplot(PreReg_FieldsDist,
         PreReg_NrAdopters,
        # PreReg_fields_bNrAdopters,
        # PreReg_InteractFieldDistNrAdopters,
        # PreReg_InteractFieldDistinfo_b,
        # PreReg_Interactfields_bNrAdopters,
        # PreReg_FieldsDistinfo_b,
         #PreReg_FieldsDistNrAdopters,
        decreasing = TRUE, sort = "magnitude")

#####


#get percent of correct predicitons
probabilities_m.1 <- round(predict(m.1, type = "response"))
probabilities_m.2 <- round(predict(m.2, type = "response"))
probabilities_m.3 <- round(predict(m.3, type = "response"))
probabilities_m.4 <- round(predict(m.4, type = "response"))
probabilities_m.5 <- round(predict(m.5, type = "response"))
probabilities_PreReg_NrFields <- round(predict(PreReg_NrFields, type = "response"))
probabilities_PreReg_NrAdopters <- round(predict(PreReg_NrAdopters, type = "response"))
probabilities_PreReg_fields_bNrAdopters <- round(predict(PreReg_fields_bNrAdopters, type = "response"))
probabilities_PreReg_info_bNrFields <- round(predict(PreReg_info_bNrFields, type = "response"))
probabilities_PreReg_NrFieldsNrAdopters <- round(predict(PreReg_NrFieldsNrAdopters, type = "response"))
probabilities_PreReg_InteractNrFieldsNrAdopters <- round(predict(PreReg_InteractNrFieldsNrAdopters, type = "response"))
probabilities_PreReg_InteractNrFieldsFieldDist <- round(predict(PreReg_InteractNrFieldsFieldDist, type = "response"))
probabilities_PreReg_InteractFieldDistNrAdopters <- round(predict(PreReg_InteractFieldDistNrAdopters, type = "response"))
probabilities_PreReg_FieldsDist <- round(predict(PreReg_FieldsDist, type = "response"))
probabilities_PreReg_FieldsDistNrFields <- round(predict(PreReg_FieldsDistNrFields, type = "response"))
probabilities_PreReg_FieldsDistNrAdopters <- round(predict(PreReg_FieldsDistNrAdopters, type = "response"))
probabilities_PreReg_FieldsDistNrFieldsNrAdopters <- round(predict(PreReg_FieldsDistNrFieldsNrAdopters, type = "response"))
probabilities_PreReg_FieldsDistinfo_b <- round(predict(PreReg_FieldsDistinfo_b, type = "response"))
probabilities_PreReg_InteractFieldDistinfo_b <- round(predict(PreReg_InteractFieldDistinfo_b, type = "response"))
probabilities_PreReg_InteractNrFieldsinfo_b <- round(predict(PreReg_InteractNrFieldsinfo_b, type = "response"))
probabilities_PreReg_Interactfields_bNrAdopters <- round(predict(PreReg_Interactfields_bNrAdopters, type = "response"))



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

#model6
m6<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_NrFields))
#correct:
(m6[1]+m6[4])/(m6[1]+m6[2]+m6[3]+m6[4])

#model7
m7<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_NrAdopters))
#correct:
(m7[1]+m7[4])/(m7[1]+m7[2]+m7[3]+m7[4])

#model8
m8<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_fields_bNrAdopters))
#correct:
(m8[1]+m8[4])/(m8[1]+m8[2]+m8[3]+m8[4])

#model9
m9<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_info_bNrFields))
#correct:
(m9[1]+m9[4])/(m9[1]+m9[2]+m9[3]+m9[4])

#model10
m10<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_NrFieldsNrAdopters))
#correct:
(m10[1]+m10[4])/(m10[1]+m10[2]+m10[3]+m10[4])

#model11
m11<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_InteractNrFieldsNrAdopters))
#correct:
(m11[1]+m11[4])/(m11[1]+m11[2]+m11[3]+m11[4])

#model12
m12<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_FieldsDist))
#correct:
(m12[1]+m12[4])/(m12[1]+m12[2]+m12[3]+m12[4])

#model13
m13<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_FieldsDistNrFields))
#correct:
(m13[1]+m13[4])/(m13[1]+m13[2]+m13[3]+m13[4])

#model14
m14<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_FieldsDistNrAdopters))
#correct:
(m14[1]+m14[4])/(m14[1]+m14[2]+m14[3]+m14[4])

#model15
m15<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_FieldsDistNrFieldsNrAdopters))
#correct:
(m15[1]+m15[4])/(m15[1]+m15[2]+m15[3]+m15[4])

#model16
m16<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_FieldsDistinfo_b))
#correct:
(m16[1]+m16[4])/(m16[1]+m16[2]+m16[3]+m16[4])

#model17
m17<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_InteractNrFieldsFieldDist))
#correct:
(m17[1]+m17[4])/(m17[1]+m17[2]+m17[3]+m17[4])


#model18
m18<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_InteractFieldDistNrAdopters))
#correct:
(m18[1]+m18[4])/(m18[1]+m18[2]+m18[3]+m18[4])


#model19
m19<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_InteractFieldDistinfo_b))
#correct:
(m19[1]+m19[4])/(m19[1]+m19[2]+m19[3]+m19[4])

#model18
m20<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_Interactfields_bNrAdopters))
#correct:
(m20[1]+m20[4])/(m20[1]+m20[2]+m20[3]+m20[4])

#model18
m21<- matrix(table(SampleIV$q1_adopt, probabilities_PreReg_InteractNrFieldsinfo_b))
#correct:
(m21[1]+m21[4])/(m21[1]+m21[2]+m21[3]+m21[4])
#calculate prediciton ccuracy manually
######
#explore combination of NRAdopters and FieldDist further as seems to be most helpful to correctly predict adoption

#install.packages("devtools")
#require(devtools)
#install_version("ggplot2", version = "2.2.1", repos = "http://cran.us.r-project.org")

table(SampleIV$FieldDist_agg, SampleIV$q3_info)

#NrAdoptersFieldDist
p.1 <- ggplot(SampleIV, aes(q3_info))+
  geom_bar(aes(fill = q1_adopt), position = "fill")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  ylab("Share of adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg)

ggplot(SampleIV, aes(q3_info))+
  geom_bar(aes(fill = q1_adopt), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg)


p.a<- ggplot(SampleIV, aes(FieldDist_agg2))+
  geom_bar(aes(fill = NrAdopters_agg), position = "fill")+
  scale_fill_manual(values = c("grey90","grey70","grey40", "grey20"), name = "NrAdopters",labels = c("noAdopters", "1-5Adopters", ">6Adopters"))+#,name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  scale_x_discrete(labels = c("noFields", "0-5km", "6-10km", ">10km"))+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")

p.b<- ggplot(df.Observer, aes(NrFields_agg))+
  geom_bar(aes(fill = FieldDist_agg2), position = "fill")+
  scale_fill_manual(values = c("grey90","grey70","grey40", "grey20"), name = "FieldDist",labels = c("0-5km", "6-10km", ">10km"))+#,name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  scale_x_discrete(labels = c("1-5Fields", "6-10Fields", ">10Fields"))+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")
  

  
p.c<- ggplot(SampleIV, aes(NrAdopters_agg))+
  geom_bar(aes(fill = NrFields_agg), position = "fill")+
  scale_fill_manual(values = c("grey90","grey70","grey40", "grey20"), name = "NrFields",labels = c("no Fields","1-5Fields", "6-10Fields", ">10Fields"))+#,name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  scale_x_discrete(labels = c("noAdopters", "1-5Adopters", ">6Adopters"))+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right") 



ggarrange(p.a, p.c,p.b, nrow = 3)


l <- ggplot(SampleIV, aes(NrFields_agg, q3_info))

l+ geom_tile(aes(fill = FieldDist_agg), hjust=0.5, vjust=0.5, 
              interpolate=FALSE)



#NrAdoptersNrFields
p.2 <- ggplot(SampleIV, aes(q3_info))+
  geom_bar(aes(fill = q1_adopt), position = "fill")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  ylab("Share of adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~NrFields_agg)

g.NrAdopterNrfields2 <-ggplot(SampleIV, aes(q3_info))+
  geom_bar(aes(), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~NrFields_agg)


#g.NrAdoptersNrFields<- ggarrange(g.NrAdopterNrfields2, g.NrAdopterNrfields1, nrow = 2)

#FieldDistNrFields
p.3 <- ggplot(df.Observer, aes(NrFields_agg))+
  geom_bar(aes(fill = q1_adopt), position = "fill")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  ylab("Share of adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg)

ggplot(df.Observer, aes(NrFields_agg))+
  geom_bar(aes(), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg)

ggarrange(p.1,p.2, p.3, nrow = 3)

ggplot(SampleIV, aes(q3_info, FieldDist_agg))+
  geom_count(aes(size = after_stat(n), colour = NrFields_agg, shape= q1_adopt))+
  scale_size_area(max_size = 10)



#contingency table info/Field
ggplot(SampleIV, aes(info_b))+
  geom_bar(aes(fill = q1_adopt), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  facet_grid(~fields_b)+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")
  

chisq.test(SampleIV$info_b, SampleIV$fields_b)



plot_summs(PreReg_FieldsDist_aggNrAdopters_b_mfx,PreReg_NrFieldsNrAdopters_b_mfx, 
           coefs = c("1-5 adopters known"="q3_info1-5Adopters",
                     "6-10 adopters known"="q3_info6-10Adopters",
                     "more than 10 adopters known"="q3_infomore than 10Adopters",
                     "1-5 fields observed"="NrFields_agg1-5Fields",
                     "6-10 fields observed"="NrFields_agg6-10Fields",
                     "more than 11 fields observed"="NrFields_aggmore than 11Fields",
                     "observed fields in 0-5km"="FieldDist_agg0-5km",
                     "observed fields in 6-10km"="FieldDist_agg6-10km",
                     "observed fields in 11-30km"="FieldDist_agg11-30km",
                     "observed fields in more than 30km"="FieldDist_aggmore than 30km"),
           model.names = c("Model Pr3","Model Pr4"),
           colors = c("Grey28", "Grey55"),
           scale = TRUE, robust = TRUE)
#plots
#####

#get correct predicition accuracy OLD
#library(performance)
#get prediction power for model with category dummies for nrfields and Nradopt
#set.seed(500)
#naive model
#pa_naive <- performance_accuracy(m.1)
#set.seed(500)
#prereg model with inly controls
#pa_controls <-performance_accuracy(m.2)
#set.seed(500)
#1prereg model with inly controls+info_b
#pa_KnowAdopters <-performance_accuracy(m.3)
#set.seed(500)
#2prereg model with inly controls+fields_b
#pa_ObserveFields <-performance_accuracy(m.4)
#set.seed(500)
#3prereg model with inly controls+info_b+fields_b
#pa_KnowAdoptersObserveFields <-performance_accuracy(m.5)
#set.seed(500)
#4prereg model with inly controls+NrFields
#pa_NrFields <-performance_accuracy(PreReg_NrFields)
#set.seed(500)
#5prereg model with inly controls+NrAdopters
#pa_NrAdopters <-performance_accuracy(PreReg_NrAdopters)
#set.seed(500)
#6prereg model with inly controls+NrAdopters
#pa_FieldDist <-performance_accuracy(PreReg_FieldsDist)
#set.seed(500)
#7prereg model with inly controls+NrAdopters+fields_b
#pa_ObserveFieldsNrAdopters <-performance_accuracy(PreReg_fields_bNrAdopters)
#set.seed(500)
#8prereg model with inly controls+NrFields + info_b
#pa_KnowAdoptersNrFields <-performance_accuracy(PreReg_info_bNrFields)
#set.seed(500)
#9prereg model with inly controls+NrFields + info_b
#pa_KnowAdoptersFieldDist <-performance_accuracy(PreReg_FieldsDistinfo_b)
#set.seed(500)
#10prereg model with inly controls+NrAdopters+NrFields
#pa_NrFieldsNrAdopters <-performance_accuracy(PreReg_NrFieldsNrAdopters)
#set.seed(500)
#11prereg model with inly controls+NrAdopters+NrFields
#pa_FieldDistNrFields <-performance_accuracy(PreReg_FieldsDistNrFields)
#set.seed(500)
#12prereg model with inly controls+NrAdopters+NrFields
#pa_FieldDistNrAdopters <-performance_accuracy(PreReg_FieldsDistNrAdopters)
#set.seed(500)
#13 all three in
#pa_FieldDistNrAdoptersNrFields <-performance_accuracy(PreReg_FieldsDistNrFieldsNrAdopters)




#get predicted probability for interaction term models
######
########interaction models
#exclude one observation that doesn't observe fields but knows more than 10 adopters
#SampleIV <- SampleIV %>% filter(date != "2022-03-17 11:34:46")

summary(PreReg_InteractNrFieldsNrAdopters <- glm(q1_adopt ~ 
                                                   NrFields_agg*NrAdopters_agg+
                                                   minDist_demo + 
                                                   sq.demodist+
                                                   age_b + 
                                                   farmsize_b + 
                                                   AES_b +
                                                   Fabrikstandort_agg 
                                                 ,data = SampleIV, family = binomial("probit")))

PreReg_InteractNrFieldsNrAdopters_mfx<-mfx::probitmfx(PreReg_InteractNrFieldsNrAdopters, data = SampleIV)
plot_summs(PreReg_InteractNrFieldsNrAdopters_mfx, robust = TRUE, scale = TRUE)

summary(PreReg_InteractNrFieldsFieldDist <- glm(q1_adopt ~ 
                                                  NrFields_agg*FieldDist_agg2+
                                                  minDist_demo + 
                                                  sq.demodist+
                                                  age_b + 
                                                  farmsize_b + 
                                                  AES_b +
                                                  Fabrikstandort_agg 
                                                ,data = df.Observer, family = binomial("probit")))

PreReg_InteractNrFieldsFieldDist_mfx<-mfx::probitmfx(PreReg_InteractNrFieldsFieldDist, df.Observer)
plot_summs(PreReg_InteractNrFieldsFieldDist_mfx, robust = TRUE, scale = TRUE)



summary(PreReg_InteractFieldDistNrAdopters <- glm(q1_adopt ~ 
                                                    NrAdopters_agg*FieldDist_agg2+
                                                    minDist_demo + 
                                                    sq.demodist+
                                                    age_b + 
                                                    farmsize_b + 
                                                    AES_b +
                                                    Fabrikstandort_agg 
                                                  ,data = SampleIV, family = binomial("probit")))

PreReg_InteractFieldDistNrAdopters_mfx <-mfx::probitmfx(PreReg_InteractFieldDistNrAdopters, data = SampleIV, robust = TRUE)
plot_summs(PreReg_InteractFieldDistNrAdopters_mfx, robust = TRUE, scale = TRUE)

#create plot of interaction term sused for the heatmaps

plot_summs(PreReg_InteractFieldDistNrAdopters_mfx,PreReg_InteractNrFieldsNrAdopters_mfx,PreReg_InteractNrFieldsFieldDist_mfx,
           robust = TRUE, scale = TRUE,
           model.names = c("InteractionNrAdoptersFieldDist","InteractionNrFieldsNrAdopters","InteractionNrFieldsFieldDist"),
           colors = "Greys",
           coefs = c(#"Observing fields (binary)" = "fields_bFieldsObserved",
                     #"Knowing Adopters (binary)" = "info_bAdoptersKnown",
                     #"1-5 adopters known"="NrAdopters_agg1-5Adopters",
                     #">5 adopters known"="NrAdopters_agg6 and more Adopters",
                    # ">10 adopters known"="q3_infomore than 10Adopters",
                     #"1-5 fields observed"="NrFields_agg1-5Fields",
                     #"6-10 fields observed"="NrFields_agg6-10Fields",
                     #">10 fields observed"="NrFields_agg11 and more Fields",
                     #"observed fields in 0-5km"="FieldDist_agg20-5km",
                     #"observed fields in 6-10km"="FieldDist_agg26-10km",
                     #"observed fields in >10km"="FieldDist_agg211km and more",
                     "1-5 adopters known*observed fields in 0-5km"="NrAdopters_agg1-5Adopters:FieldDist_agg20-5km",                
                     ">5 adopters known*observed fields in 0-5km"="NrAdopters_agg6 and more Adopters:FieldDist_agg20-5km",        
                     "1-5 adopters known*observed fields in 6-10km"="NrAdopters_agg1-5Adopters:FieldDist_agg26-10km",              
                     ">5 adopters known*observed fields in 6-10km"="NrAdopters_agg6 and more Adopters:FieldDist_agg26-10km",       
                     "1-5 adopters known*observed fields in >10km"="NrAdopters_agg1-5Adopters:FieldDist_agg211km and more",        
                     ">5 adopters known*observed fields in >10km"="NrAdopters_agg6 and more Adopters:FieldDist_agg211km and more",
                     "1-5 fields observed*1-5 adopters known"="NrFields_agg1-5Fields:NrAdopters_agg1-5Adopters",                 
                     "6-10 fields observed*1-5 adopters known"="NrFields_agg6-10Fields:NrAdopters_agg1-5Adopters",                
                     ">10 fields observed*1-5 adopters known"="NrFields_agg11 and more Fields:NrAdopters_agg1-5Adopters",        
                     "1-5 fields observed*>5 adopters known"="NrFields_agg1-5Fields:NrAdopters_agg6 and more Adopters",         
                     "6-10 fields observed*>5 adopters known"="NrFields_agg6-10Fields:NrAdopters_agg6 and more Adopters",        
                     ">10 fields observed*>5 adopters known"="NrFields_agg11 and more Fields:NrAdopters_agg6 and more Adopters",
                    "6-10 fields observed*observed fields in 6-10km"="NrFields_agg6-10Fields:FieldDist_agg26-10km",               
                    ">10 fields observed*observed fields in 6-10km"="NrFields_agg11 and more Fields:FieldDist_agg26-10km",       
                    "6-10 fields observed* fields in >10km"="NrFields_agg6-10Fields:FieldDist_agg211km and more",        
                    ">10 fields observed*observed fields in >10km"="NrFields_agg11 and more Fields:FieldDist_agg211km and more"
                     ))+theme(legend.position="bottom")
           
         #  omit.coefs = c("(Intercept)","minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
          #                "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
           #               "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
            #              "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
             #             "Fabrikstandort_aggOffstein"))







summary(PreReg_InteractFieldDistinfo_b <- glm(q1_adopt ~ 
                                                info_b*FieldDist_agg2+
                                                minDist_demo + 
                                                sq.demodist+
                                                age_b + 
                                                farmsize_b + 
                                                AES_b +
                                                Fabrikstandort_agg 
                                              ,data = SampleIV, family = binomial("probit")))

PreReg_InteractFieldDistinfo_b_mfx<-mfx::probitmfx(PreReg_InteractFieldDistinfo_b, data = SampleIV)
plot_summs(PreReg_InteractFieldDistinfo_b_mfx, robust = TRUE, scale = TRUE)

summary(PreReg_Interactfields_bNrAdopters <- glm(q1_adopt ~ 
                                                   fields_b*NrAdopters_agg+
                                                   minDist_demo + 
                                                   sq.demodist+
                                                   age_b + 
                                                   farmsize_b + 
                                                   AES_b +
                                                   Fabrikstandort_agg 
                                                 ,data = SampleIV, family = binomial("probit")))

PreReg_Interactfields_bNrAdopters_mfx<-mfx::probitmfx(PreReg_Interactfields_bNrAdopters, data = SampleIV)


summary(PreReg_InteractNrFieldsinfo_b <- glm(q1_adopt ~ 
                                               info_b*NrFields_agg+
                                               minDist_demo + 
                                               sq.demodist+
                                               age_b + 
                                               farmsize_b + 
                                               AES_b +
                                               Fabrikstandort_agg 
                                             ,data = SampleIV, family = binomial("probit")))

PreReg_InteractNrFieldsinfo_b_mfx<-mfx::probitmfx(PreReg_InteractNrFieldsinfo_b, data = SampleIV)
plot_summs(PreReg_InteractNrFieldsinfo_b_mfx, robust = TRUE, scale = TRUE) #interaction models
#####
#predict probabilities

#NrFieldsNrAdopters

newdata1 <- with(SampleIV, data.frame(NrFields_agg = levels(SampleIV$NrFields_agg),
                                     NrAdopters_agg = "noAdopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                     minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                     sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                     AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.NrFieldsNoAdopters<- predict(PreReg_InteractNrFieldsNrAdopters, newdata1, type="response")

newdata2 <- with(SampleIV, data.frame(NrFields_agg = levels(SampleIV$NrFields_agg),
                                      NrAdopters_agg = "1-5Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                      minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                      sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                      AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.NrFields1to5Adopters<-predict(PreReg_InteractNrFieldsNrAdopters, newdata2, type="response")

newdata3 <- with(SampleIV, data.frame(NrFields_agg = levels(SampleIV$NrFields_agg),
                                      NrAdopters_agg = "6 and more Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                      minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                      sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                      AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.NrFieldsmore6Adopters<-predict(PreReg_InteractNrFieldsNrAdopters, newdata3, type="response")

ppNrFieldsNrAdopters <- as.data.frame(rbind(pp.NrFieldsNoAdopters*100, pp.NrFields1to5Adopters*100, pp.NrFieldsmore6Adopters*100))
ppNrFieldsNrAdopters <-round(ppNrFieldsNrAdopters, digits = 0)
#View(ppNrFieldsNrAdopters)


#FieldDistNrAdopters

newdata1b <- with(SampleIV, data.frame(FieldDist_agg2 = levels(SampleIV$FieldDist_agg2),
                                       NrAdopters_agg = "noAdopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                      minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                      sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                      AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDistNoAdopters<- predict(PreReg_InteractFieldDistNrAdopters, newdata1b, type="response")

newdata2b <- with(SampleIV, data.frame(FieldDist_agg2 = levels(SampleIV$FieldDist_agg2),
                                       NrAdopters_agg = "1-5Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDist1to5Adopters<- predict(PreReg_InteractFieldDistNrAdopters, newdata2b, type="response")

newdata3b <- with(SampleIV, data.frame(FieldDist_agg2 = levels(SampleIV$FieldDist_agg2),
                                       NrAdopters_agg = "6 and more Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDistmore6Adopters<- predict(PreReg_InteractFieldDistNrAdopters, newdata3b, type="response")



ppFieldDistNrAdopters <- as.data.frame(rbind(pp.FieldDistNoAdopters*100, pp.FieldDist1to5Adopters*100, pp.FieldDistmore6Adopters*100))
ppFieldDistNrAdopters <-round(ppFieldDistNrAdopters, digits = 0)
#View(ppFieldDistNrAdopters)


#FieldDistNrFields for observers only

newdata1c <- with(df.Observer, data.frame(FieldDist_agg2 = levels(df.Observer$FieldDist_agg2),
                                       NrFields_agg = "1-5Fields",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDist1to5Fields<- predict(PreReg_InteractNrFieldsFieldDist, newdata1c, type="response")

newdata2c <- with(df.Observer, data.frame(FieldDist_agg2 = levels(df.Observer$FieldDist_agg2),
                                          NrFields_agg = "6-10Fields",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDist6to10Fields<- predict(PreReg_InteractNrFieldsFieldDist, newdata2c, type="response")

newdata3c <- with(df.Observer, data.frame(FieldDist_agg2 = levels(df.Observer$FieldDist_agg2),
                                          NrFields_agg = "11 and more Fields",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDistmore11Fields<- predict(PreReg_InteractNrFieldsFieldDist, newdata3c, type="response")



ppFieldDistNrFields <- as.data.frame(rbind(pp.FieldDist1to5Fields*100, pp.FieldDist6to10Fields*100, pp.FieldDistmore11Fields*100))
ppFieldDistNrFields <-round(ppFieldDistNrFields, digits = 0)
#View(ppFieldDistNrFields)





#create heatmaps of tables
#library(gplots)
#col <- rev(colorRampPalette(brewer.pal(10, "RdYlBu"))(256))
matrix.NrFieldsNrAdopters <- as.matrix(ppNrFieldsNrAdopters)
colnames(matrix.NrFieldsNrAdopters)<-c("noFields","1-5Fields","6-10Fields",">10Fields")
rownames(matrix.NrFieldsNrAdopters) <-c("noAdopters", "1-5Adopters",">5Adopters") 

#png(file="NrFieldsNrAdopters.png")
#heatmap.2(x = matrix.NrFieldsNrAdopters, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #         cellnote = matrix.NrFieldsNrAdopters, notecol = "black", notecex = 2,
  #        trace = "none", key = FALSE, margins = c(10, 12), col = col)
#dev.off()


matrix.FieldDistNrAdopters <- as.matrix(ppFieldDistNrAdopters)
colnames(matrix.FieldDistNrAdopters)<-c("noFields","0-5km","6-10km",">10km")
rownames(matrix.FieldDistNrAdopters) <-c("noAdopters", "1-5Adopters",">5Adopters") 

#png(file="FieldDistNrAdopters.png")
#heatmap.2(x = matrix.FieldDistNrAdopters, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #                                      cellnote =matrix.FieldDistNrAdopters, notecol = "black", notecex = 2,
  #                                     trace = "none", key = FALSE, margins = c(10, 12),col = col)
#dev.off()

matrix.FieldDistNrFields <- as.matrix(ppFieldDistNrFields)
colnames(matrix.FieldDistNrFields)<-c("0-5km","6-10km",">11km")
rownames(matrix.FieldDistNrFields) <- c("1-5Fields","6-10Fields",">10Fields")


#png(file="FieldDistNrFields.png")
#heatmap.2(x = matrix.FieldDistNrFields, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #         cellnote =matrix.FieldDistNrFields, notecol = "black", notecex = 2,
  #        trace = "none", key = FALSE, margins = c(10, 12),col = col)
#dev.off()



#create clean models for RQ2 with aggregated groups
summary(PreReg_NrFieldsNrAdopters <- glm(q1_adopt ~ 
                                           NrFields_agg+q3_info+
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_NrFieldsNrAdopters_mfx<-mfx::probitmfx(PreReg_NrFieldsNrAdopters, data = SampleIV)

summary(PreReg_FieldsDistNrAdopters_agg <- glm(q1_adopt ~ 
                                             FieldDist_agg2+
                                             q3_info +
                                             minDist_demo + 
                                             sq.demodist+
                                             age_b + 
                                             farmsize_b + 
                                             AES_b +
                                             Fabrikstandort_agg 
                                           ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrAdopters_agg_mfx<-mfx::probitmfx(PreReg_FieldsDistNrAdopters_agg, data = SampleIV)



plot_summs(#PreReg_info_bNrFields_mfx, PreReg_fields_bNrAdopters_mfx, PreReg_FieldsDistinfo_b_mfx,
           PreReg_FieldsDistNrAdopters_agg_mfx,PreReg_NrFieldsNrAdopters_mfx,
           robust = TRUE, scale = TRUE,
           colors = c("Greys"),
           model.names = c("Model Pr3","Model Pr4"),
           omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
                          "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
                          "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
                          "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
                          "Fabrikstandort_aggOffstein"),
           coefs = c("Adopters known"="info_bAdoptersKnown" ,
                      "Fields observed"="fields_bFieldsObserved",
                     "1-5 fields observed"="NrFields_agg1-5Fields",
                     "6-10 fields observed"="NrFields_agg6-10Fields",
                     ">10 fields observed"="NrFields_agg11 and more Fields",
                     "1-5 adopters known"="q3_info1-5Adopters",
                     "6-10 adopters known"="q3_info6-10Adopters",
                     ">10 adopters known"="q3_infomore than 10Adopters",
                     "observed fields in 0-5km"="FieldDist_agg20-5km",
                     "observed fields in 6-10km"="FieldDist_agg26-10km",
                     "observed fields in 11-30km"="FieldDist_agg211-30km",
                     "observed fields in  >10km"="FieldDist_agg211km and more"))


#####

#create confusion matrix
#Insatll required packages


#1naive
SampleIV$pred_Y_naive <- as.factor(round(stats::predict(m.1, SampleIV, type = "response")))
CM_naive <- confusionMatrix(data=SampleIV$pred_Y_naive, reference = SampleIV$q1_adopt)
pa_naive <- CM_naive[["overall"]][["Accuracy"]]

#2controls only
SampleIV$pred_Y_Controls <- as.factor(round(stats::predict(m.2, SampleIV, type = "response")))
CM_Controls <- confusionMatrix(data=SampleIV$pred_Y_Controls, reference = SampleIV$q1_adopt)
pa_controls <- CM_Controls[["overall"]][["Accuracy"]]

#3KnowAdopters
SampleIV$pred_Y_KnowAdopters <- as.factor(round(stats::predict(m.3, SampleIV, type = "response")))
CM_KnowAdopters <- confusionMatrix(data=SampleIV$pred_Y_KnowAdopters, reference = SampleIV$q1_adopt)
pa_KnowAdopters <- CM_KnowAdopters[["overall"]][["Accuracy"]]

#4ObserveFields
SampleIV$pred_Y_ObserveFields <- as.factor(round(stats::predict(m.4, SampleIV, type = "response")))
CM_ObserveFields <- confusionMatrix(data=SampleIV$pred_Y_ObserveFields, reference = SampleIV$q1_adopt)
pa_ObserveFields <- CM_ObserveFields[["overall"]][["Accuracy"]]

#5NrFields
SampleIV$pred_Y_NrFields <- as.factor(round(stats::predict(PreReg_NrFields, SampleIV, type = "response")))
CM_NrFields <- confusionMatrix(data=SampleIV$pred_Y_NrFields, reference = SampleIV$q1_adopt)
pa_NrFields <- CM_NrFields[["overall"]][["Accuracy"]]

#6FieldDist
SampleIV$pred_Y_FieldDist <- as.factor(round(stats::predict(PreReg_FieldsDist, SampleIV, type = "response")))
CM_FieldDist <- confusionMatrix(data=SampleIV$pred_Y_FieldDist, reference = SampleIV$q1_adopt)
pa_FieldDist <- CM_FieldDist[["overall"]][["Accuracy"]]

#7NrAdopters
SampleIV$pred_Y_NrAdopters <- as.factor(round(stats::predict(PreReg_NrAdopters, SampleIV, type = "response")))
CM_NrAdopters <- confusionMatrix(data=SampleIV$pred_Y_NrAdopters, reference = SampleIV$q1_adopt)
pa_NrAdopters <- CM_NrAdopters[["overall"]][["Accuracy"]]

#8FieldsObserved + KnowAdopters
SampleIV$pred_Y_KnowAdoptersFieldsObserved <- as.factor(round(stats::predict(m.5, SampleIV, type = "response")))
CM_KnowAdoptersFieldsObserved <- confusionMatrix(data=SampleIV$pred_Y_KnowAdoptersFieldsObserved, reference = SampleIV$q1_adopt)
pa_KnowAdoptersFieldsObserved <- CM_KnowAdoptersFieldsObserved[["overall"]][["Accuracy"]]

#9KnowAdopters +NrFields
SampleIV$pred_Y_KnowAdoptersNrFields <- as.factor(round(stats::predict(PreReg_info_bNrFields, SampleIV, type = "response")))
CM_KnowAdoptersNrFields <- confusionMatrix(data=SampleIV$pred_Y_KnowAdoptersNrFields, reference = SampleIV$q1_adopt)
pa_KnowAdoptersNrFields <- CM_KnowAdoptersNrFields[["overall"]][["Accuracy"]]

#10KnowAdopters + FieldDist
SampleIV$pred_Y_KnowAdoptersFieldDist <- as.factor(round(stats::predict(PreReg_FieldsDistinfo_b, SampleIV, type = "response")))
CM_KnowAdoptersFieldDist <- confusionMatrix(data=SampleIV$pred_Y_KnowAdoptersFieldDist, reference = SampleIV$q1_adopt)
pa_KnowAdoptersFieldDist <- CM_KnowAdoptersFieldDist[["overall"]][["Accuracy"]]

#11FieldsObserved + NrAdopters
SampleIV$pred_Y_ObserveFieldsNrAdopters <- as.factor(round(stats::predict(PreReg_fields_bNrAdopters, SampleIV, type = "response")))
CM_ObserveFieldsNrAdopters <- confusionMatrix(data=SampleIV$pred_Y_ObserveFieldsNrAdopters, reference = SampleIV$q1_adopt)
pa_ObserveFieldsNrAdopters <- CM_ObserveFieldsNrAdopters[["overall"]][["Accuracy"]]

#12NrFields + NrAdopters
SampleIV$pred_Y_NrFieldsNrAdopters <- as.factor(round(stats::predict(PreReg_NrFieldsNrAdopters, SampleIV, type = "response")))
CM_NrFieldsNrAdopters <- confusionMatrix(data=SampleIV$pred_Y_NrFieldsNrAdopters, reference = SampleIV$q1_adopt)
pa_NrFieldsNrAdopters <- CM_NrFieldsNrAdopters[["overall"]][["Accuracy"]]

#13FieldDist + NrAdopters
SampleIV$pred_Y_FieldsDistNrAdopters <- as.factor(round(stats::predict(PreReg_FieldsDistNrAdopters, SampleIV, type = "response")))
CM_FieldsDistNrAdopters <- confusionMatrix(data=SampleIV$pred_Y_FieldsDistNrAdopters, reference = SampleIV$q1_adopt)
pa_FieldsDistNrAdopters <- CM_FieldsDistNrAdopters[["overall"]][["Accuracy"]]

#14FieldDisr + NrFields
SampleIV$pred_Y_FieldsDistNrFields <- as.factor(round(stats::predict(PreReg_FieldsDistNrFields, SampleIV, type = "response")))
CM_FieldsDistNrFields <- confusionMatrix(data=SampleIV$pred_Y_FieldsDistNrFields, reference = SampleIV$q1_adopt)
pa_FieldsDistNrFields <- CM_FieldsDistNrFields[["overall"]][["Accuracy"]]


df.PredictionAccuracy<- as.data.frame(rbind(pa_naive, pa_controls, 
                                            pa_KnowAdopters, pa_ObserveFields, pa_KnowAdoptersFieldsObserved,
                                            pa_NrFields,pa_NrAdopters, pa_FieldDist,
                                            pa_KnowAdoptersFieldDist, pa_KnowAdoptersNrFields,pa_ObserveFieldsNrAdopters,
                                            pa_NrFieldsNrAdopters, pa_FieldsDistNrFields, pa_FieldsDistNrAdopters))



df.PredictionAccuracy$V1 <- as.numeric(df.PredictionAccuracy$V1)
df.PredictionAccuracy$V1 <- df.PredictionAccuracy$V1*100
df.PredictionAccuracy$V1<-(round(df.PredictionAccuracy$V1, digits = 2))
df.PredictionAccuracy$DiffToMax <- 0
df.PredictionAccuracy <- df.PredictionAccuracy %>% dplyr::mutate(DiffToMax = max(V1) - V1)
View(df.PredictionAccuracy)

#check if dummy groups are correct
table(SampleIV$fields_b)
table(SampleIV$NrFields)
table(SampleIV$FieldDist)
table(SampleIV$info_b)
table(SampleIV$q3_info)


#matrix.test1 <- as.matrix(table(SampleIV$NrAdopters_agg, SampleIV$NrFields_agg))
#heatmap.2(x = matrix.test1, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #         cellnote = matrix.test1, notecol = "black", notecex = 2,
  #        trace = "none", key = FALSE, margins = c(10, 12), col = col)

#matrix.test2 <- as.matrix(table(SampleIV$NrAdopters_agg, SampleIV$FieldDist_agg))
#heatmap.2(x = matrix.test2, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #         cellnote = matrix.test2, notecol = "black", notecex = 2,
  #        trace = "none", key = FALSE, margins = c(10, 12), col = col)

#matrix.test3 <- as.matrix(table(df.Observer$FieldDist_agg, df.Observer$NrFields_agg))
#heatmap.2(x = matrix.test3, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
 #         cellnote = matrix.test3, notecol = "black", notecex = 2,
  #        trace = "none", key = FALSE, margins = c(10, 12), col = col)


#create heatmap with number of observations and nr of adopters included
#create heatmaps of tables
#with ggplot

#1

df.NrFieldsNrAdopters <- as.data.frame(matrix.NrFieldsNrAdopters)
df.NrFieldsNrAdopters$NrAdopters <- rownames(df.NrFieldsNrAdopters)
df.NrFieldsNrAdopters_long <- reshape2::melt(df.NrFieldsNrAdopters, id.vars = "NrAdopters")

#get count
df.countNrFieldsNrAdopters <- as.data.frame(table(SampleIV$NrAdopters_agg,SampleIV$NrFields_agg))
df.NrFieldsNrAdopters_long$Count <- df.countNrFieldsNrAdopters$Freq

#get share of adopters
df.ShareAdoptersNrFieldsNrAdopters <- as.data.frame(table(SampleIV[SampleIV$q1_adopt==1,]$NrAdopters_agg,SampleIV[SampleIV$q1_adopt==1,]$NrFields_agg))
df.NrFieldsNrAdopters_long$Adopters <- df.ShareAdoptersNrFieldsNrAdopters$Freq

df.NrFieldsNrAdopters_long$ShareAdopters <- (round((df.NrFieldsNrAdopters_long$Adopters/df.NrFieldsNrAdopters_long$Count),digits = 2))*100

heatmap.NrFieldsNrAdopters <- ggplot(df.NrFieldsNrAdopters_long, aes(NrAdopters, variable)) +    # Create default ggplot2 heatmap
  geom_tile(aes(fill = value))+
  ylab(("NrFields"))+
  xlab(("NrAdopters_agg"))+
  geom_text(aes(label=gsub(" ", "",paste(value,"\n","(",Count,",",ShareAdopters,"%",")"))))+theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_gradient(low = "white", high = "grey30")+
  theme(legend.position = "none")
heatmap.NrFieldsNrAdopters 

#2

df.FieldDistNrAdopters <- as.data.frame(matrix.FieldDistNrAdopters)
df.FieldDistNrAdopters$NrAdopters <- rownames(df.FieldDistNrAdopters)
df.FieldDistNrAdopters_long <- reshape2::melt(df.FieldDistNrAdopters, id.vars = "NrAdopters")

#get count
df.countFieldDistNrAdopters <- as.data.frame(table(SampleIV$NrAdopters_agg,SampleIV$FieldDist_agg2))
df.FieldDistNrAdopters_long$Count <- df.countFieldDistNrAdopters$Freq

#get share of adopters
df.ShareAdoptersFieldDistNrAdopters <- as.data.frame(table(SampleIV[SampleIV$q1_adopt==1,]$NrAdopters_agg,SampleIV[SampleIV$q1_adopt==1,]$FieldDist_agg2))
df.FieldDistNrAdopters_long$Adopters <- df.ShareAdoptersFieldDistNrAdopters$Freq

df.FieldDistNrAdopters_long$ShareAdopters <- (round((df.FieldDistNrAdopters_long$Adopters/df.FieldDistNrAdopters_long$Count),digits = 2))*100

heatmap.FieldDistNrAdopters <- ggplot(df.FieldDistNrAdopters_long, aes(NrAdopters, variable)) +    # Create default ggplot2 heatmap
  geom_tile(aes(fill = value))+
  ylab(("FieldDist_agg"))+
  xlab(("NrAdopters_agg"))+
  geom_text(aes(label=gsub(" ", "",paste(value,"\n","(",Count,",",ShareAdopters,"%",")"))))+theme_bw()+
  scale_x_discrete(limits=rev)+
  scale_fill_gradient(low = "white", high = "grey30")+
  theme(legend.position = "none")
heatmap.FieldDistNrAdopters 

#3

df.FieldDistNrFields<- as.data.frame(matrix.FieldDistNrFields)
df.FieldDistNrFields$NrFields <- rownames(df.FieldDistNrFields)
df.FieldDistNrFields_long <- reshape2::melt(df.FieldDistNrFields, id.vars = "NrFields")

#get count
df.countFieldDistNrFields<- as.data.frame(table(df.Observer$NrFields_agg,df.Observer$FieldDist_agg2))
df.FieldDistNrFields_long$Count <- df.countFieldDistNrFields$Freq

#get share of adopters
df.ShareAdoptersFieldDistNrFields<- as.data.frame(table(df.Observer[df.Observer$q1_adopt==1,]$NrFields_agg,df.Observer[df.Observer$q1_adopt==1,]$FieldDist_agg2))
df.FieldDistNrFields_long$Adopters <- df.ShareAdoptersFieldDistNrFields$Freq

df.FieldDistNrFields_long$ShareAdopters <- (round((df.FieldDistNrFields_long$Adopters/df.FieldDistNrFields_long$Count),digits = 2))*100
df.FieldDistNrFields_long$NrFields <- factor(df.FieldDistNrFields_long$NrFields, levels=c("1-5Fields", "6-10Fields", ">10Fields"))


heatmap.FieldDistNrFields<- ggplot(df.FieldDistNrFields_long, aes(NrFields, variable)) +    # Create default ggplot2 heatmap
  geom_tile(aes(fill = value))+
  xlab(("NrFields"))+
  ylab("FieldDist_agg")+
  geom_text(aes(label=gsub(" ", "",paste(value,"\n","(",Count,",",ShareAdopters,"%",")"))))+theme_bw()+
  #scale_x_discrete(limits=rev)+
  scale_fill_gradient(low = "white", high = "grey30")+
  theme(legend.position = "none")
heatmap.FieldDistNrFields







