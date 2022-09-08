
library(glmnet)
library(plotmo) # for plot_glmnet
library(dplyr)
library(expss)
library(qpcR)


# Set random number seed
set.seed(40)
## Clear workspace 
#rm(list = ls())


#Add labels to get better overview of vars included

#apply labels to get overview
SampleIV <- apply_labels(SampleIV,q1_adopt="Adoption mechanical weeding binary",
                         fields_b="observing fields binary",
                         info_b="knowing adopters binary",
                         minDist_demo="minimal distance to demonstration farm",
                         sq.demodist="squared minimal distance to demonstration farm",
                         Fabrikstandort_agg="sugar factory location aggregated",
                         farmsize_b="farm size in ha over 50 binary",
                         AES_b="participation in AES binary",
                         age_b="farmer age over 45 binary",
                         farm_organic="production type binary",
                         meanFarmSize2="mean farm size at county level",
                         UAA_unter5="UAA in farms with less than 5ha at county level",
                         UAA_Sugarbeet="UAA where sugarbeet is grown at county level",
                         SB_region="share of SB in UAA larger 10% binary",
                         ShareSB="Share of UAA for Sugarbeets in arable UAA at county level",
                         lwBetr_Anzahl="number of farms in county",
                         lwBetrOrganic_Anzahl="number of farms with organic agriculture at county level",
                         lwBetrUnter5_Anzahl="number of farms with less than 5ha at county level",
                         lwBetr5b10_Anzahl="number of farms with 5-10ha at county level",
                         UAA_5b10="UAA in farms with 5- 10ha at county level",
                         UAA="UAA at county level",
                         UAA_Organic="organic UAA at county level",
                         UAA_arable="arable UAA at county level",
                         ShareOrgFarms="Share of organic farms in all farms at county level",
                         ShareOrgArea="Share of organic area in UAA at county level",
                         Area="total area county",
                         population="Number of habitants at county level",
                         populationdensity="habitants per sq.km",
                         farmDens="farms per sq.km",
                         areaDens="UAA per total county area",
                         ShareSmallFarms="Share of small farms (< 10ha) in all farms at county level",
                         elevation_in_m_mean="mean elevation at county or field level",
                         sand_content_percent_mean="mean sand content in soil at county or field level",
                         clay_content_percent_mean="mean clay content in soil at county or field level",
                         sq.elevation_in_m_mean="sq.mean elevation at county or field level",
                         sq.sand_content_percent_mean="sq.mean sand content in soil at county or field level",
                         sq.clay_content_percent_mean="sq.mean clay content in soil at county or field level",
                         mainly_crop="farm specialized in crops/ arable farming binary",
                         Verband_agg = "producer associations aggregated",
                         meanDist = "mean distance to other farmers' fields, set to 50 for NA",
                         sq.meanDist ="squared mean distance to other farmers' fields",
                         fields_dist ="interaction of fields_b*meanDist =0 for non-observators",
                         sq.fields_dist = "squared distance to other farmers' fields with 0 for non-observators",
                         ShareSmallArea= "share of area of farms wih 5 to 10 ha in total UAA",
                         ShareArableUAA = "share of arable area in total UAA", 
                         ShareArableInTotalArea = "share of arable area in total county area")




#Get explanatory variables and create subsample including dependant and independant variable

dat<-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")

#take kreislevel data away:
#


XVars <- c("fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")


df <-dat[XVars]

df<-df %>% drop_na()
dat<-dat %>% drop_na()


#get overview of vars/ group of vars

vars.prereg <- SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","fields_dist","sq.fields_dist","minDist_demo","sq.demodist","Fabrikstandort_agg","farmsize_b","AES_b","age_b")

vars.prereg_extended <-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","fields_dist","sq.fields_dist","minDist_demo","sq.demodist","Fabrikstandort_agg","farmsize_b","AES_b","age_b", "SB_region","meanFarmSize2","sand_content_percent_mean","elevation_in_m_mean")

vars.all_lasso <-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","Fabrikstandort_agg","farmsize_b","AES_b","age_b","farm_organic","SB_region","meanFarmSize2","UAA_unter5","UAA_Sugarbeet","ShareSB","lwBetr_Anzahl","lwBetrOrganic_Anzahl","lwBetrUnter5_Anzahl","lwBetr5b10_Anzahl","UAA_5b10","UAA","UAA_Organic","UAA_arable","ShareOrgFarms","ShareOrgArea","Area","population","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean","mainly_crop", "Verband_agg")

vars.identified11 <-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","Fabrikstandort_agg","farmsize_b","AES_b","age_b","sand_content_percent_mean")

vars.identified8 <-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","Fabrikstandort_agg","farmsize_b","AES_b","age_b","sand_content_percent_mean","farm_organic","ShareSB","lwBetr_Anzahl","UAA_Organic","population","populationdensity","areaDens","ShareSmallFarms","sq.sand_content_percent_mean","SB_region")



vtable(dat, missing = TRUE)




### Run lasso on the model directly 

lasso_mod <- glmnet(df,dat$q1_adopt,alpha=1, family='binomial')
plot_glmnet(lasso_mod, label=15)


## Double selection approach 
 

dfExog <- df[ , !names(df) %in% c("info_b", "fields_b")]
#create df for corss-check for fields w/o info
dfExog2 <- df[ , !names(df) %in% c("info_b")]
dfExog3 <- df[ , !names(df) %in% c("fields_b")]


### 1) Perform model selection explaining adoption by all the exogenous variables 
#(excluding info and field)

#set nfolds
nfolds <- c(20,30,40,50,60,70)


# Run cross validation
for (i in nfolds) {
cvfit_AdoptExog <- cv.glmnet(data.matrix(dfExog), dat$q1_adopt,nfolds=i, family = "binomial",type.measure = "class")
assign( paste("cvfit_AdoptExog",i, sep = "_") ,cvfit_AdoptExog)
}
plot(cvfit_AdoptExog_70)
list.cvfit_AdoptExog<- do.call("list",mget(ls(pattern = "^cvfit_AdoptExog.*")))

#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
coefAdoptmin <- coef(list.cvfit_AdoptExog[[i]],s ="lambda.min")
assign( paste("coefAdoptmin",i, sep = "_") ,coefAdoptmin)
list.coefAdoptmin<- do.call("list",mget(ls(pattern = "^coefAdoptmin.*")))
lstCoefAdoptmin <- list.coefAdoptmin[[i]]@Dimnames[[1]][which(list.coefAdoptmin[[i]] != 0 ) ] #feature names: intercept included
lstCoefAdoptmin <- lstCoefAdoptmin[-1] # exclude constant
assign( paste("lstCoefAdoptmin",i, sep = "_") ,lstCoefAdoptmin)
}
final.lstCoefAdoptmin<- do.call("list",mget(ls(pattern = "^lstCoefAdoptmin.*")))


### 2) Perform model selection explaining info by all the exogenous variables 

# Run cross validation
for (i in nfolds) {
cvfit_info <- cv.glmnet(data.matrix(dfExog), dat$info_b,nfolds=i,family = "binomial",type.measure = "class")
assign( paste("cvfit_info",i, sep = "_") ,cvfit_info)
}
plot(cvfit_info_20)
list.cvfit_info<- do.call("list",mget(ls(pattern = "^cvfit_info.*")))

#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
  coefinfomin <- coef(list.cvfit_info[[i]],s ="lambda.min")
  assign( paste("coefinfomin",i, sep = "_") ,coefinfomin)
  list.coefinfomin<- do.call("list",mget(ls(pattern = "^coefinfomin.*")))
  lstCoefinfomin <- list.coefinfomin[[i]]@Dimnames[[1]][which(list.coefinfomin[[i]] != 0 ) ] #feature names: intercept included
  lstCoefinfomin <- lstCoefinfomin[-1] # exclude constant
  assign( paste("lstCoefinfomin",i, sep = "_") ,lstCoefinfomin)
}
final.lstCoefinfomin<- do.call("list",mget(ls(pattern = "^lstCoefinfomin.*")))



### 3) Perform model selection explaining fields by all the exogenous variables 

# Run cross validation
for (i in nfolds) {
  cvfit_fields <- cv.glmnet(data.matrix(dfExog), dat$fields_b,nfolds=i,family = "binomial",type.measure = "class")
  assign( paste("cvfit_fields",i, sep = "_") ,cvfit_fields)
}
plot(cvfit_fields_20)
list.cvfit_fields<- do.call("list",mget(ls(pattern = "^cvfit_fields.*")))

#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
  coeffieldsmin <- coef(list.cvfit_fields[[i]],s ="lambda.min")
  assign( paste("coeffieldsmin",i, sep = "_") ,coeffieldsmin)
  list.coeffieldsmin<- do.call("list",mget(ls(pattern = "^coeffieldsmin.*")))
  lstCoeffieldsmin <- list.coeffieldsmin[[i]]@Dimnames[[1]][which(list.coeffieldsmin[[i]] != 0) ]#feature names: intercept included
  lstCoeffieldsmin <- lstCoeffieldsmin[-1] # exclude constant
  assign( paste("lstCoeffieldsmin",i, sep = "_") ,lstCoeffieldsmin)
}
final.lstCoeffieldsmin<- do.call("list",mget(ls(pattern = "^lstCoeffieldsmin.*")))




### Run the final model explaining Adoption by the union of variables selected in 1), 2) and 3). 

#save lists of vars selected in different steps
for (i in 1:length(nfolds)){
lstvarsselected <- lst(final.lstCoefAdoptmin[[i]],final.lstCoefinfomin[[i]],final.lstCoeffieldsmin[[i]])
assign( paste("lstvarsselected",i, sep = "_") ,lstvarsselected)
}

#save lists of vars selected in different steps
final.lstvarsselected<- do.call("list",mget(ls(pattern = "^lstvarsselected.*")))


for (i in 1:length(nfolds)){
dfDouble1<- dat[,union(union(final.lstCoeffieldsmin[[i]],final.lstCoefAdoptmin[[i]]),c("q1_adopt","fields_b"))]
assign( paste("dfDouble1",i, sep = "_") ,dfDouble1)

dfDouble2 <- dat[,union(union(final.lstCoefinfomin[[i]],final.lstCoefAdoptmin[[i]]),c("q1_adopt", "info_b"))]
assign( paste("dfDouble2",i, sep = "_") ,dfDouble1)

dfDouble3 <- dat[,union(union(final.lstCoefinfomin[[i]],final.lstCoeffieldsmin[[i]]),c("fields_b", "info_b"))]
assign( paste("dfDouble3",i, sep = "_") ,dfDouble1)

}

#continue here, save double df for each nfolds selection


dfDouble <- cbind(dfDouble1,dfDouble2,dfDouble3)
dfDouble <-dfDouble %>% 
  dplyr::select(unique(colnames(.)))


reg_double <- glm(q1_adopt ~ .,data=dfDouble, family = "binomial")
summary(reg_double)
df.Lasso_selected <- reg_double[["model"]] 
vtable(df.Lasso_selected, values = TRUE, missing = TRUE)
#marginal effects
m.Lasso_select_mfx <-probitmfx(reg_double, data = dfDouble)
m.Lasso_select_mfx
coefs <-  m.Lasso_select_mfx[["mfxest"]]

#Compare that to the result that we would obtained for OLS on all variables

reg_Full <- glm(q1_adopt ~ .,data=dat,family = "binomial")
summary(reg_Full)
m.reg_Full_mfx <-probitmfx(reg_Full, data = dat)
m.reg_Full_mfx






#loop ofer different nfolds

nfolds <- c(20,30,40,50)
# LIST OF GLM FITTED MODELS
for (i in nfolds) {
  model <- cv.glmnet(data.matrix(dfExog), dat$q1_adopt,nfolds= i, family = "binomial",type.measure = "class")
  assign( paste("models_orig",i, sep = "_") ,model)
  }

list.lassomodels_orig<- do.call("list",mget(ls(pattern = "^models_orig.*")))



plot(models_orig_50)
coef(models_orig_50 ,s ="lambda.min")

























