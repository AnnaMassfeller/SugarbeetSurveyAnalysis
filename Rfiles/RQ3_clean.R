#RQ3

#create aggregated variables
SampleIV$info_b <- revalue(SampleIV$info_b, c("0"="noAdoptersKnown", "1"="AdoptersKnown"))
SampleIV$fields_b <- revalue(SampleIV$fields_b, c("0"="noFieldsObserved", "1"="FieldsObserved"))
SampleIV$q3_info <- revalue(SampleIV$q3_info, c("0"="noAdopters", "1"="1-5Adopters", "2"="6-10Adopters", "3"="more than 10Adopters"))
SampleIV$NrFields <- revalue(SampleIV$NrFields, c("0"="noFields", "1"="1-5Fields", "2"="6-10Fields", "3"="11-15Fields", "4"="more than 15Fields"))
SampleIV$FieldDist <- revalue(SampleIV$FieldDist, c("0"="0-5km", "1"="6-10km", "2"="11-15km", "3"="16-20km", "4"="21-30km", "5"="more than 30km", "6"= "noFields"))

#nrfields
SampleIV$NrFields_agg <- SampleIV$NrFields #create aggregated var where 11-15 and more than 15 are in one group
SampleIV<-SampleIV %>%
  mutate(NrFields_agg = dplyr::recode(NrFields_agg, "more than 15Fields" ="11-15Fields"))
SampleIV$NrFields_agg<- revalue(SampleIV$NrFields_agg, c("11-15Fields"="more than 11Fields"))
SampleIV$NrFields_agg<- as.factor(SampleIV$NrFields_agg)
table(SampleIV$NrFields_agg)

#distance to fields
SampleIV$FieldDist_agg <- SampleIV$FieldDist #create aggregated var where 11-15, 16-20 & 21-30 are in one group
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg= recode(FieldDist_agg, "16-20km" = "11-15km"))
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg= dplyr::recode(FieldDist_agg, "21-30km" = "11-15km"))
SampleIV$FieldDist_agg<- revalue(SampleIV$FieldDist_agg, c("11-15km"="11-30km"))
SampleIV$FieldDist_agg<- as.factor(SampleIV$FieldDist_agg)
table(SampleIV$FieldDist_agg)

#aggregate even further: distance to fields
SampleIV$FieldDist_agg2 <- SampleIV$FieldDist #create aggregated var where 11-15, 16-20 & 21-30 are in one group
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "16-20km" = "11-15km"))
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "21-30km" = "11-15km"))
SampleIV<-SampleIV %>%
  mutate(FieldDist_agg2= dplyr::recode(FieldDist_agg2, "more than 30km" = "11-15km"))
SampleIV$FieldDist_agg2<- revalue(SampleIV$FieldDist_agg2, c("11-15km"="more than 11km"))
SampleIV$FieldDist_agg2<- as.factor(SampleIV$FieldDist_agg2)
table(SampleIV$FieldDist_agg2)


#nrAdopters
SampleIV$NrAdopters_agg <- SampleIV$q3_info #create aggregated var where 6-10 and more than 10
SampleIV$NrAdopters_agg<-as.factor(SampleIV$NrAdopters_agg)
SampleIV<-SampleIV %>%
  mutate(NrAdopters_agg= dplyr::recode(NrAdopters_agg, "more than 10Adopters" = "6-10Adopters"))
SampleIV$NrAdopters_agg<- revalue(SampleIV$NrAdopters_agg, c("6-10Adopters"="more than 6Adopters"))
SampleIV$NrAdopters_agg<- as.factor(SampleIV$NrAdopters_agg)
table(SampleIV$NrAdopters_agg)

#set reference categories
SampleIV <- within(SampleIV, NrFields_agg <- relevel(NrFields_agg, ref = "noFields"))
SampleIV <- within(SampleIV, FieldDist_agg2 <- relevel(FieldDist_agg2, ref = "noFields"))
SampleIV <- within(SampleIV, NrAdopters_agg <- relevel(NrAdopters_agg, ref = "noAdopters"))
SampleIV <- within(SampleIV, FieldDist_agg2 <- relevel(FieldDist_agg2, ref = "noFields"))

#create subsample of observers

df.Observer <- SampleIV %>% filter(fields_b == "FieldsObserved")
df.Observer$NrFields_agg <- droplevels(df.Observer$NrFields_agg)
df.Observer$FieldDist_agg2 <- droplevels(df.Observer$FieldDist_agg2)

summary(PreReg_NrAdopters <- glm(q1_adopt ~ 
                                   NrAdopters_agg+
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_NrAdopters_mfx<-mfx::probitmfx(PreReg_NrAdopters, data = SampleIV)

summary(PreReg_fields_bNrAdopters <- glm(q1_adopt ~ 
                                           fields_b+NrAdopters_agg +
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
                                           NrFields_agg+NrAdopters_agg+
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_NrFieldsNrAdopters_mfx<-mfx::probitmfx(PreReg_NrFieldsNrAdopters, data = SampleIV)


summary(PreReg_FieldsDist <- glm(q1_adopt ~ 
                                           FieldDist_agg2 +
                                           minDist_demo + 
                                           sq.demodist+
                                           age_b + 
                                           farmsize_b + 
                                           AES_b +
                                           Fabrikstandort_agg 
                                         ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDist_mfx<-mfx::probitmfx(PreReg_FieldsDist, data = SampleIV)

summary(PreReg_FieldsDistinfo_b <- glm(q1_adopt ~ 
                                   FieldDist_agg2 + NrAdopters_agg+
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistinfo_b_mfx<-mfx::probitmfx(PreReg_FieldsDistinfo_b, data = SampleIV)

summary(PreReg_FieldsDistNrFields <- glm(q1_adopt ~ 
                                   FieldDist_agg2 +
                                     NrFields_agg +
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrFields_mfx<-mfx::probitmfx(PreReg_FieldsDistNrFields, data = SampleIV)

summary(PreReg_FieldsDistNrAdopters <- glm(q1_adopt ~ 
                                   FieldDist_agg2+
                                     NrAdopters_agg +
                                   minDist_demo + 
                                   sq.demodist+
                                   age_b + 
                                   farmsize_b + 
                                   AES_b +
                                   Fabrikstandort_agg 
                                 ,data = SampleIV, family = binomial("probit")))

PreReg_FieldsDistNrAdopters_mfx<-mfx::probitmfx(PreReg_FieldsDistNrAdopters, data = SampleIV)


summary(PreReg_FieldsDistNrFieldsNrAdopters <- glm(q1_adopt ~ 
                                   FieldDist_agg2 +
                                     NrAdopters_agg+
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

m.5.mfx <- mfx::probitmfx(m.5 <-glm(q1_adopt ~ info_b + fields_b+ 
                                      minDist_demo + 
                                      sq.demodist+
                                      age_b + 
                                      farmsize_b + 
                                      AES_b +
                                      # advisory,
                                      Fabrikstandort_agg, 
                                    data = SampleIV,family = binomial("probit")), data = SampleIV)


plot_summs(#m.1.mfx,
           #m.2.mfx,
           m.3.mfx,
           m.4.mfx,
           m.5.mfx,
         #  PreReg_NrFields_mfx,
           PreReg_FieldsDist_mfx,
           PreReg_NrAdopters_mfx,
           PreReg_fields_bNrAdopters_mfx,
          # PreReg_info_bNrFields_mfx,
          # PreReg_NrFieldsNrAdopters_mfx,
          # PreReg_InteractNrFieldsNrAdopters_mfx,
          # PreReg_InteractNrFieldsFieldDist_mfx,
           PreReg_InteractFieldDistNrAdopters_mfx,
           PreReg_InteractFieldDistinfo_b_mfx,
           PreReg_Interactfields_bNrAdopters_mfx,
          # PreReg_InteractNrFieldsinfo_b_mfx,
           PreReg_FieldsDistinfo_b_mfx,
         #  PreReg_FieldsDistNrFields_mfx,
           PreReg_FieldsDistNrAdopters_mfx,
          # PreReg_FieldsDistNrFieldsNrAdopters_mfx,
           scale = TRUE, robust = TRUE, colors = "Greys",
         omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
                        "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
                        "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
                        "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
                        "Fabrikstandort_aggOffstein"))

plot_summs(
#  PreReg_FieldsDist_mfx,
#  PreReg_NrAdopters_mfx,
 # PreReg_fields_bNrAdopters_mfx,
  PreReg_InteractFieldDistNrAdopters_mfx,
 # PreReg_InteractFieldDistinfo_b_mfx,
#  PreReg_Interactfields_bNrAdopters_mfx,
#  PreReg_FieldsDistinfo_b_mfx,
 # PreReg_FieldsDistNrAdopters_mfx,
  PreReg_InteractNrAdoptersFieldDist_mfx,
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

table(SampleIV$FieldDist_agg2, SampleIV$q3_info)

#NrAdoptersFieldDist
p.1 <- ggplot(SampleIV, aes(NrAdopters_agg))+
  geom_bar(aes(fill = q1_adopt), position = "fill")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  ylab("Share of adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg2)

ggplot(SampleIV, aes(NrAdopters_agg))+
  geom_bar(aes(fill = q1_adopt), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg2)


#NrAdoptersNrFields
p.2 <- ggplot(SampleIV, aes(NrAdopters_agg))+
  geom_bar(aes(fill = q1_adopt), position = "fill")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  ylab("Share of adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~NrFields_agg)

g.NrAdopterNrfields2 <-ggplot(SampleIV, aes(NrAdopters_agg))+
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
  facet_grid(~FieldDist_agg2)

ggplot(df.Observer, aes(NrFields_agg))+
  geom_bar(aes(), position = "stack")+
  scale_fill_manual(values = c("grey80", "grey40"),name = "Adoption", labels = c("No", "Yes"))+
  xlab("")+
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")+
  facet_grid(~FieldDist_agg2)

ggarrange(p.1,p.2, p.3, nrow = 3)

ggplot(SampleIV, aes(NrAdopters_agg, FieldDist_agg2))+
  geom_count(aes(size = after_stat(n)))+
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
                     "observed fields in 0-5km"="FieldDist_agg20-5km",
                     "observed fields in 6-10km"="FieldDist_agg26-10km",
                     "observed fields in 11-30km"="FieldDist_agg211-30km",
                     "observed fields in more than 30km"="FieldDist_agg2more than 30km"),
           model.names = c("Model Pr3","Model Pr4"),
           colors = c("Grey28", "Grey55"),
           scale = TRUE, robust = TRUE)
#plots
#####

#get correct predicition accuracy
library(performance)
#get prediction power for model with category dummies for nrfields and Nradopt
set.seed(500)
#naive model
pa_naive <- performance_accuracy(m.1)
set.seed(500)
#prereg model with inly controls
pa_controls <-performance_accuracy(m.2)
set.seed(500)
#1prereg model with inly controls+info_b
pa_KnowAdopters <-performance_accuracy(m.3)
set.seed(500)
#2prereg model with inly controls+fields_b
pa_ObserveFields <-performance_accuracy(m.4)
set.seed(500)
#3prereg model with inly controls+info_b+fields_b
pa_KnowAdoptersObserveFields <-performance_accuracy(m.5)
set.seed(500)
#4prereg model with inly controls+NrFields
pa_NrFields <-performance_accuracy(PreReg_NrFields)
set.seed(500)
#5prereg model with inly controls+NrAdopters
pa_NrAdopters <-performance_accuracy(PreReg_NrAdopters)
set.seed(500)
#6prereg model with inly controls+NrAdopters
pa_FieldDist <-performance_accuracy(PreReg_FieldsDist)
set.seed(500)
#7prereg model with inly controls+NrAdopters+fields_b
pa_ObserveFieldsNrAdopters <-performance_accuracy(PreReg_fields_bNrAdopters)
set.seed(500)
#8prereg model with inly controls+NrFields + info_b
pa_KnowAdoptersNrFields <-performance_accuracy(PreReg_info_bNrFields)
set.seed(500)
#9prereg model with inly controls+NrFields + info_b
pa_KnowAdoptersFieldDist <-performance_accuracy(PreReg_FieldsDistinfo_b)
set.seed(500)
#10prereg model with inly controls+NrAdopters+NrFields
pa_NrFieldsNrAdopters <-performance_accuracy(PreReg_NrFieldsNrAdopters)
set.seed(500)
#11prereg model with inly controls+NrAdopters+NrFields
pa_FieldDistNrFields <-performance_accuracy(PreReg_FieldsDistNrFields)
set.seed(500)
#12prereg model with inly controls+NrAdopters+NrFields
pa_FieldDistNrAdopters <-performance_accuracy(PreReg_FieldsDistNrAdopters)
set.seed(500)
#13 all three in
pa_FieldDistNrAdoptersNrFields <-performance_accuracy(PreReg_FieldsDistNrFieldsNrAdopters)


df.PredictionAccuracy<- as.data.frame(rbind(pa_naive, pa_controls, 
                                            pa_KnowAdopters, pa_ObserveFields, pa_KnowAdoptersObserveFields,
                                            pa_NrFields,pa_NrAdopters, pa_FieldDist,
                                            pa_KnowAdoptersFieldDist, pa_KnowAdoptersNrFields,pa_ObserveFieldsNrAdopters,
                                            pa_NrFieldsNrAdopters, pa_FieldDistNrFields, pa_FieldDistNrAdopters,
                                            pa_FieldDistNrAdoptersNrFields))



df.PredictionAccuracy$Accuracy <- as.numeric(df.PredictionAccuracy$Accuracy)
df.PredictionAccuracy$Accuracy <- df.PredictionAccuracy$Accuracy*100
df.PredictionAccuracy<- df.PredictionAccuracy %>% dplyr::select(-c(Method, SE))
df.PredictionAccuracy$Accuracy<-(round(df.PredictionAccuracy$Accuracy, digits = 2))
df.PredictionAccuracy$DiffToMax <- 0
df.PredictionAccuracy <- df.PredictionAccuracy %>% dplyr::mutate(DiffToMax = max(Accuracy) - Accuracy)
View(df.PredictionAccuracy)


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
                                      NrAdopters_agg = "more than 6Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                      minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                      sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                      AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.NrFieldsmore6Adopters<-predict(PreReg_InteractNrFieldsNrAdopters, newdata3, type="response")

ppNrFieldsNrAdopters <- as.data.frame(rbind(pp.NrFieldsNoAdopters*100, pp.NrFields1to5Adopters*100, pp.NrFieldsmore6Adopters*100))
ppNrFieldsNrAdopters <-round(ppNrFieldsNrAdopters, digits = 2)
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
                                       NrAdopters_agg = "more than 6Adopters",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDistmore6Adopters<- predict(PreReg_InteractFieldDistNrAdopters, newdata3b, type="response")



ppFieldDistNrAdopters <- as.data.frame(rbind(pp.FieldDistNoAdopters*100, pp.FieldDist1to5Adopters*100, pp.FieldDistmore6Adopters*100))
ppFieldDistNrAdopters <-round(ppFieldDistNrAdopters, digits = 2)
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
                                          NrFields_agg = "more than 11Fields",                       #c("noFields","1-5Fields","6-10Fields","more than 11Fields"), NrAdopters_agg = c("noAdopters"),#,"1-5Adopters","more than 6Adopters"),
                                       minDist_demo=mean(minDist_demo, na.rm=TRUE), 
                                       sq.demodist=mean(sq.demodist, na.rm=TRUE),age_b="1",farmsize_b="1",
                                       AES_b="0", Fabrikstandort_agg= "Ochsenfurt"))
pp.FieldDistmore11Fields<- predict(PreReg_InteractNrFieldsFieldDist, newdata3c, type="response")



ppFieldDistNrFields <- as.data.frame(rbind(pp.FieldDist1to5Fields*100, pp.FieldDist6to10Fields*100, pp.FieldDistmore11Fields*100))
ppFieldDistNrFields <-round(ppFieldDistNrFields, digits = 2)
#View(ppFieldDistNrFields)





#create heatmaps of tables
library(gplots)
col <- rev(colorRampPalette(brewer.pal(10, "RdYlBu"))(256))
matrix.NrFieldsNrAdopters <- as.matrix(ppNrFieldsNrAdopters)
colnames(matrix.NrFieldsNrAdopters)<-c("noFields","1-5Fields","6-10Fields",">11Fields")
rownames(matrix.NrFieldsNrAdopters) <-c("noAdopters", "1-5Adopters",">6Adopters") 

png(file="NrFieldsNrAdopters.png")
heatmap.2(x = matrix.NrFieldsNrAdopters, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = matrix.NrFieldsNrAdopters, notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(10, 12), col = col)
dev.off()


matrix.FieldDistNrAdopters <- as.matrix(ppFieldDistNrAdopters)
colnames(matrix.FieldDistNrAdopters)<-c("noFields","1-5km","6-10km",">11km")
rownames(matrix.FieldDistNrAdopters) <-c("noAdopters", "1-5Adopters",">6Adopters") 

png(file="FieldDistNrAdopters.png")
heatmap.2(x = matrix.FieldDistNrAdopters, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
                                       cellnote =matrix.FieldDistNrAdopters, notecol = "black", notecex = 2,
                                       trace = "none", key = FALSE, margins = c(10, 12),col = col)
dev.off()

matrix.FieldDistNrFields <- as.matrix(ppFieldDistNrFields)
colnames(matrix.FieldDistNrFields)<-c("1-5km","6-10km",">11km")
rownames(matrix.FieldDistNrFields) <- c("1-5Fields","6-10Fields",">11Fields")


png(file="FieldDistNrFields.png")
heatmap.2(x = matrix.FieldDistNrFields, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote =matrix.FieldDistNrFields, notecol = "black", notecex = 2,
          trace = "none", key = FALSE, margins = c(10, 12),col = col)
dev.off()



#create clean models for RQ2 with aggregated groups

plot_summs(PreReg_FieldsDistNrAdopters_mfx,PreReg_NrFieldsNrAdopters_mfx, 
           robust = TRUE, scale = TRUE,
           colors = c("Grey28", "Grey55"),
           model.names = c("Model Pr3","Model Pr4"),
           coefs = c("more than 10 adopters known"="q3_infomore than 10Adopters",
                     "1-5 fields observed"="NrFields_agg1-5Fields",
                     "6-10 fields observed"="NrFields_agg6-10Fields",
                     "more than 11 fields observed"="NrFields_aggmore than 11Fields",
                     "1-5 adopters known"="NrAdopters_agg1-5Adopters",
                     "6-10 adopters known"="NrAdopters_aggmore than 6Adopters",
                     "observed fields in 0-5km"="FieldDist_agg20-5km",
                     "observed fields in 6-10km"="FieldDist_agg26-10km",
                     "observed fields in 11-30km"="FieldDist_agg211-30km",
                     "observed fields in more than 11km"="FieldDist_agg2more than 11km"),
           omit.coefs = c("minDist_demo","sq.demodist","age_b1","farmsize_b1","AES_b1","Fabrikstandort_aggElsdorf",
                          "Fabrikstandort_aggLageNordst","Fabrikstandort_aggOchsenfurt","Fabrikstandort_aggOffenau",
                          "Fabrikstandort_aggPlatting","Fabrikstandort_aggRain","Fabrikstandort_aggSachsenAnhalt",
                          "Fabrikstandort_aggSchladen","Fabrikstandort_aggUelzen","Fabrikstandort_aggWabern","Fabrikstandort_aggWest",
                          "Fabrikstandort_aggOffstein"))





