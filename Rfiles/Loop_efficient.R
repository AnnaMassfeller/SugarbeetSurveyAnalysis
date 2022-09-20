######Loop over lasso
#packags needed
library(glmnet)
library(plotmo) # for plot_glmnet
library(dplyr)
library(expss)
library(qpcR)
library(MuMIn)

####loop over 
##1. seeds
##2. nfolds
##3. Adoption, Info and fields

#first get data we need
dat<-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop"
                                , "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","ShareSmallArea","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")#,"Fabrikstandort_agg")
XVars <- c("fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms"
           ,"ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","ShareSmallArea","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")#Fabrikstandort_agg")
df <-dat[XVars]
df<-df %>% drop_na()
dat<-dat %>% drop_na()
dfExog <- df[ , !names(df) %in% c("info_b", "fields_b")]

#create table for paper
dat_for_paper<-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop"
                                          , "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","ShareSmallArea","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Verband_agg","Fabrikstandort_agg")
vtable(dat_for_paper, missing = FALSE,summ=c('mean(x)'), class = FALSE)
#st(dat_for_paper), summ='mean(x)')

#1. create dataframe of objective of the loop
#we want a df with all vars as the rows and a column for each model
#indicating via 0 or 1 if the variable was choosen
#we need one df for Adoption, one for info and one for field
#assuming 5 different seeds and 10 different nfolds we have 50 models = 50 columns
XExog <- c("minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity",
           "farmDens","areaDens","ShareSmallFarms","ShareSmallArea","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")
df.vars_selected <- as.data.frame(matrix(data = NA, nrow= length(XExog), ncol = 10))
rownames(df.vars_selected) <- XExog 

df.vars_selected_Adopt <- df.vars_selected
df.vars_selected_Info <- df.vars_selected
df.vars_selected_Fields <- df.vars_selected

#create another data frame to save marginal effects
#XVars <- c("fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b",
 #          "farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")
df.margEffects <- as.data.frame(matrix(data = NA, nrow = 2, ncol = 10))
rownames(df.margEffects) <- c("info_b", "fields_b")


#2. create all we need for a function that allows to select the dependent variable and the df of exogenous vars

#list of depndenent variables
lst.dep.vars <-lst(dat$q1_adopt, dat$info_b, dat$fields_b)

#independent vars for cv.glmnet
m.Exog <- data.matrix(dfExog)


########################
#Loop with three layers#
########################

#outer layer: loop over seeds
seeds <- list(10,50,100,500,1000)
for (s in 1:length(seeds)){
  set.seed(seeds[[s]])
  
  #set.seed(10)
  #middle layer: loop over nfolds
  nfolds <- list(10,20,30,40,50,60,70,80,90,100) #in10er schritten bis 100
  for (k in 1:length(nfolds)) {
  
  
    #inner layer: loop over dependent variable
    cv.function <- function(x,y) cv.glmnet(x = x, y = y[[i]], nfolds=nfolds[[k]], family='binomial', type.measure = "class")
    for (i in 1:length(lst.dep.vars)){
      cv <- cv.function(x = m.Exog  , y = lst.dep.vars) #run cross validation for each depndent variable
      coefLambdamin <- coef(cv,s ="lambda.min") #save vars selected for lambda min
      lstCoefLambdamin <- coefLambdamin@Dimnames[[1]][which(coefLambdamin != 0 ) ] 
      lstCoefLambdamin <- tail(lstCoefLambdamin,-1) # exclude constant
      assign( paste("lstCoefLambdamin", i, sep = "_") , lstCoefLambdamin)
      finallst.lambdamin<- do.call("list",mget(ls(pattern = "^lstCoefLambdamin_.*")))
      }
      #we now have three "lists" of vars selected for model 1, seed X, nfolds X
      #we need to combine these three lists to the data frame that will be used for the final model
      dfDouble1 <- dat[,union(union(finallst.lambdamin[[1]],finallst.lambdamin[[3]]),c("q1_adopt","fields_b"))]
      dfDouble2 <- dat[,union(union(finallst.lambdamin[[1]],finallst.lambdamin[[2]]),c("q1_adopt", "info_b"))]
      dfDouble3 <- dat[,union(union(finallst.lambdamin[[2]],finallst.lambdamin[[3]]),c("fields_b", "info_b"))]
      dfDouble <- cbind(dfDouble1,dfDouble2,dfDouble3)
      dfDouble <-dfDouble %>% 
        dplyr::select(unique(colnames(.))) 
            
      #then run the final model 50 times
      reg_double <- glm(q1_adopt ~ .,data=dfDouble, family = "binomial")
      assign( paste("reg_double", k, sep = "_") , reg_double) #save each model to later get average
      final.lstlstreg_double<- do.call("list",mget(ls(pattern = "^reg_double_.*")))
      assign( paste("final.lstreg_double", s, sep = "_") , final.lstlstreg_double)
      #get marginal effects and save them
      m.Lasso_select_mfx <-probitmfx(reg_double, data = dfDouble)
      assign( paste("m.Lasso_select_mfx", k, sep = "_") , m.Lasso_select_mfx)
      final.lstlasso_mfx<- do.call("list",mget(ls(pattern = "^m.Lasso_select_mfx_.*")))
      assign( paste("final.lstlasso_mfx", s, sep = "_") , final.lstlasso_mfx)
      
      #save marg effects in data frame prepared above
      #for (j in 1:length(nfolds)){
      Lasso_select_mfx <-  as.data.frame(m.Lasso_select_mfx[["mfxest"]])
      Lasso_select_mfx<- dplyr::select(Lasso_select_mfx, 1)#we only need the first column
      Lasso_select_mfx <- Lasso_select_mfx[c("info_b1", "fields_b1"),] #we only want info and fields
      df.margEffects[1,k] <- Lasso_select_mfx[1] # 3D arrays
      df.margEffects[2,k] <- Lasso_select_mfx[2]
      assign( paste("df.margEffects", s, sep = "_") , df.margEffects)
      
      #now save which vars have been selcted for each combination of seeds and nfolds
      df.vars_selected_Adopt[,k]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_1 == TRUE,1,0)
      assign( paste("df.vars_selected_Adopt", s, sep = "_") , df.vars_selected_Adopt)
      df.vars_selected_Info[,k]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_2 == TRUE,1,0)
      assign( paste("df.vars_selected_Info", s, sep = "_") , df.vars_selected_Info)
      df.vars_selected_Fields[,k]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_3 == TRUE,1,0)
      assign( paste("df.vars_selected_Fields", s, sep = "_") , df.vars_selected_Fields)
      
   }
}

#save all mfx_models in a huge list
#finalfinal.lstlasso_mfx<- do.call("list",mget(ls(pattern = "^final.lstlasso_mfx_.*")))

#now bind the dfs of the different runs of seeds
df.margEffects <- cbind(df.margEffects_1, df.margEffects_2, df.margEffects_3, df.margEffects_4, df.margEffects_5)
df.vars_selected_Adopt <- cbind(df.vars_selected_Adopt_1, df.vars_selected_Adopt_2, df.vars_selected_Adopt_3, df.vars_selected_Adopt_4, df.vars_selected_Adopt_5)
df.vars_selected_Info <- cbind(df.vars_selected_Info_1, df.vars_selected_Info_2, df.vars_selected_Info_3, df.vars_selected_Info_4, df.vars_selected_Info_5)
df.vars_selected_Fields <- cbind(df.vars_selected_Fields_1, df.vars_selected_Fields_2, df.vars_selected_Fields_3, df.vars_selected_Fields_4, df.vars_selected_Fields_5)

#create sum as last column
df.vars_selected_Adopt$sum_selected <- rowSums(df.vars_selected_Adopt)
df.vars_selected_Adopt$Variables_Adoption <- rownames(df.vars_selected_Adopt)
df.vars_selected_Adopt<-df.vars_selected_Adopt %>%  dplyr::select(Variables_Adoption,sum_selected)

df.vars_selected_Info$sum_selected <- rowSums(df.vars_selected_Info)
df.vars_selected_Info$Variables_Info <- rownames(df.vars_selected_Info)
df.vars_selected_Info<-df.vars_selected_Info %>%  dplyr::select(Variables_Info,sum_selected)

df.vars_selected_Fields$sum_selected <- rowSums(df.vars_selected_Fields)
df.vars_selected_Fields$Variables_Fields <- rownames(df.vars_selected_Fields)
df.vars_selected_Fields<-df.vars_selected_Fields %>%  dplyr::select(Variables_Fields,sum_selected)

df.vars_selected <- list(df.vars_selected_Adopt,df.vars_selected_Info,df.vars_selected_Fields)
#ou as excel
write_xlsx(df.vars_selected,"Output/df.vars_selected.xlsx")

#calculate mean marginal effect and save as excel
df.margEffects$mean_marg_effect <- rowMeans(df.margEffects)
df.margEffects$Variables <- rownames(df.margEffects)
df.margEffects<-df.margEffects %>%  dplyr::select(Variables,mean_marg_effect)
write_xlsx(df.margEffects,"Output/df.margEffects.xlsx")

#last step
#Compare that to the result that we would obtained for OLS on all variables
#no loop on depdenent vars, seeds and nfolds needed as we only have one full model for all
reg_Full <- glm(q1_adopt ~ .,data=dat,family = "binomial")
reg_Full_mfx <- probitmfx(reg_Full, data = dat)
plot_summs(reg_Full_mfx,robust = TRUE, standard = TRUE, coefs = c("fields_b1", "info_b1"))




#all same seeds, different nfolds, take 3 as average

plot_summs(final.lstlasso_mfx_1,
           robust = TRUE, standard = TRUE,  coefs = c("knowing other farmers (info)"="info_b1",
                                                      "observing fields (fields)"="fields_b1"),
           model.names = c("10","20","30","40","50","60","70","80","90", "100"),
           legend.title = "Nr. of folds",
           colors = c("Grey38","Grey40","Grey42","Grey44","Grey46","Grey48","Grey50","Grey52","Grey54","Grey56"))+ theme(legend.position="bottom")






#library("dotwhisker")
#dwplot(final.lstlasso_mfx_1,final.lstlasso_mfx_2, vars_order = c("info_b1", "fields_b1"))+ theme(legend.position="none")

