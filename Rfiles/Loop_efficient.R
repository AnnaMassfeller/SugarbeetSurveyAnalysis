####loop over 
##1. seeds
##2. nfolds
##3. Adoption, Info and fields

#first get data we need
dat<-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Verband_agg")#,"Fabrikstandort_agg")
XVars <- c("fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Verband_agg")#Fabrikstandort_agg")
df <-dat[XVars]
df<-df %>% drop_na()
dat<-dat %>% drop_na()
dfExog <- df[ , !names(df) %in% c("info_b1", "fields_b1")]





#1. create dataframe of objective of the loop
#we want a df with all vars as the rows and a column for each model
#indicating via 0 or 1 if the variable was choosen
#we need one df for Adoption, one for info and one for field
#assuming 5 different seeds and 10 different nfolds we have 50 models = 50 columns
XExog <- c("minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean",
                    "sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Verband_agg")
df.vars_selected <- as.data.frame(matrix(data = NA, nrow= length(XExog), ncol = 50))
rownames(df.vars_selected) <- XExog 

df.vars_selected_Adopt <- df.vars_selected
df.vars_selected_Info <- df.vars_selected
df.vars_selected_Fields <- df.vars_selected

#create another data frame to save marginal effects
#XVars <- c("fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b",
 #          "farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")
df.margEffects <- as.data.frame(matrix(data = NA, nrow = 2, ncol = 50))
rownames(df.margEffects) <- c("info_b", "fields_b")


#2. create function that allows to select the dependent variable and the df of exogenous vars

#list of depndenent variables
lst.dep.vars <-lst(dat$q1_adopt, dat$info_b, dat$fields_b)

#independent vars for cv.glmnet
m.Exog <- data.matrix(dfExog)

#outer layer
seeds <- c(10,50,100,500,1000)
for (s in seeds){
set.seed(s)


#middle layer: loop over nfolds
nfolds <- c(10,20,30,40,50,60,70,80,90,100) #in10er schritten bis 100
for (k in nfolds) {


#inner layer: loop over dependent variable
cv.function <- function(x,y) cv.glmnet(x = x, y = y[[i]], nfolds=k, family='binomial', type.measure = "class")
for (i in 1:length(lst.dep.vars)){
cv <- cv.function(x = m.Exog  , y = lst.dep.vars) #run cross validation for each depndent variable
coefLambdamin <- coef(cv,s ="lambda.min") #save vars selected for lambda min
lstCoefLambdamin <- coefLambdamin@Dimnames[[1]][which(coefLambdamin != 0 ) ] 
lstCoefLambdamin <- tail(lstCoefLambdamin,-1) # exclude constant
assign( paste("lstCoefLambdamin", i, sep = "_") , lstCoefLambdamin)
finallst.lambdamin<- do.call("list",mget(ls(pattern = "^lstCoefLambdamin_.*")))
#we now have three "lists" of vars selected for model 1, seed X, nfolds X
#we need to combine these three lists to the data frame that will be used for the final model
dfDouble1 <- dat[,union(union(finallst.lambdamin[[1]],finallst.lambdamin[[3]]),c("q1_adopt","fields_b"))]
dfDouble2 <- dat[,union(union(finallst.lambdamin[[1]],finallst.lambdamin[[2]]),c("q1_adopt", "info_b"))]
dfDouble3 <- dat[,union(union(finallst.lambdamin[[2]],finallst.lambdamin[[3]]),c("fields_b", "info_b"))]
dfDouble <- cbind(dfDouble1,dfDouble2,dfDouble3)
dfDouble <-dfDouble %>% 
  dplyr::select(unique(colnames(.))) 
      }

#then run the final model 50 times
reg_double <- glm(q1_adopt ~ .,data=dfDouble, family = "binomial")
final.lstreg_double<- do.call("list",mget(ls(pattern = "^reg_double.*")))
#get marginal effects and save them
m.Lasso_select_mfx <-probitmfx(reg_double, data = dfDouble)
final.lstlasso_mfx<- do.call("list",mget(ls(pattern = "^m.Lasso_select_mfx.*")))

 }
}

#save marg effects in data frame prepared above
for (i in 1:50){
Lasso_select_mfx <-  as.data.frame(m.Lasso_select_mfx[[i]][["mfxest"]])
Lasso_select_mfx<- select(Lasso_select_mfx, 1)#we only need the first column
Lasso_select_mfx <- Lasso_select_mfx[c("info_b1", "fields_b1"),] #we only want info and fields
df.margEffects[i,1] <- Lasso_select_mfx[1]
df.margEffects[i,1] <- Lasso_select_mfx[2]
  }








#now save which bars have been selcted for each combination of seeds and nfolds
for ( j in length()){
df.vars_selected_Adopt[,1]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_1 == TRUE,1,0)
df.vars_selected_Info[,1]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_2 == TRUE,1,0)
df.vars_selected_Fields[,1]<-ifelse(rownames(df.vars_selected_Adopt) %in% lstCoefLambdamin_3 == TRUE,1,0)
}












#last step
#Compare that to the result that we would obtained for OLS on all variables
#no loop on depdenent vars, seeds and nfolds needed as we only have one full model for all
reg_Full <- glm(q1_adopt ~ .,data=dat,family = "binomial")







