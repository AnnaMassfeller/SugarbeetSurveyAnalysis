#Contractors#

df.Contractors <- df.technique %>% dplyr::select(date, q2_machine, type_tech)
df.Contractors <- left_join(df.Contractors, FullSample, by = "date")
df.Contractors$Contractor <- ifelse(df.Contractors$q2_machine.x == 3, 1, 0)
df.Contractors$Contractor <- as.factor(df.Contractors$Contractor)
table(df.Contractors$Contractor)

#run simple regression with all available vars similar to var sin LASSO model

dat_contractors<-df.Contractors %>% dplyr::select("Contractor","type_tech"#,"fields_b"
                                                 # ,"minDist_demo"#,"sq.demodist"
                                                 # ,"farmsize_b",
                                                  #,"AES_b"#,"age_b","farm_organic"
                                                  #,"mainly_crop" 
                              # ,"meanFarmSize2"#,"ShareOrgFarms"
                                #,"ShareOrgArea"
                               #,"populationdensity","farmDens"#,"areaDens"
                               #,"ShareSmallFarms"
                              # ,"ShareSmallArea","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean"
                              # ,"slope_in_degrees_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean"
                               #,"sq.slope_in_degrees_mean", "ShareArableUAA","ShareArableInTotalArea",#"Fabrikstandort_agg"
                                #,"meanDist",# "sq.meanDist"
                              ,"Mean_ownfield_dist"
                              , "bundesland"
                               )

table(dat_contractors$mainly_crop, dat_contractors$Contractor)
#exclude info_b and "q3_info"as there is no one, who knows no other farmers but uses contractors

dat_contractors <- dat_contractors[!is.na(dat_contractors$Mean_ownfield_dist),]
summary(dat_contractors)

m.Contractors <- glm(Contractor ~ .,data=dat_contractors,family = "binomial")
m.Contractors_mfx <- probitmfx(m.Contractors , data = dat_contractors)
m.Contractors_mfx
plot_summs(m.Contractors_mfx,
           robust = TRUE, standard = TRUE)
dat_contractors$y_pred = predict(m.Contractors, dat_contractors, type="response")


#19.04.2023: no significant result of variables chosen on decision to use contractor service
#maybe try LASSO
#problem: only 98 observations as only adopters, maybe use heckman selection?



