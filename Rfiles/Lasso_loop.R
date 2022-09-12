
library(glmnet)
library(plotmo) # for plot_glmnet
library(dplyr)
library(expss)
library(qpcR)

#read processed data
SampleIV<-read_xlsx("Processed/SampleIV.xlsx")

# Set random number seed
set.seed(200)
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

SampleIV$q1_adopt <- as.factor(SampleIV$q1_adopt)


#Get explanatory variables and create subsample including dependant and independant variable

dat<-SampleIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo",
                                "sq.demodist","farmsize_b","AES_b","age_b","farm_organic",
                                "mainly_crop", "meanFarmSize2","ShareOrgFarms","ShareOrgArea",
                                "populationdensity","farmDens","areaDens","ShareSmallFarms",
                                "elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean",
                                "sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", 
                                "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")

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



#vtable(dat, missing = TRUE)




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
nfolds <- c(10,30,40,50,60,90)


# Run cross validation
for (i in nfolds) {
cvfit_AdoptExog <- cv.glmnet(data.matrix(dfExog), dat$q1_adopt,nfolds=i, family = "binomial",type.measure = "class")
assign( paste("cvfit_AdoptExog",i, sep = "_") ,cvfit_AdoptExog)
}

list.cvfit_AdoptExog<- do.call("list",mget(ls(pattern = "^cvfit_AdoptExog_.*")))
plot(list.cvfit_AdoptExog[[1]])




#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
coefAdoptmin <- coef(list.cvfit_AdoptExog[[i]],s ="lambda.min")
assign( paste("coefAdoptmin",i, sep = "_") ,coefAdoptmin)
list.coefAdoptmin<- do.call("list",mget(ls(pattern = "^coefAdoptmin.*")))
lstCoefAdoptmin <- list.coefAdoptmin[[i]]@Dimnames[[1]][which(list.coefAdoptmin[[i]] != 0 ) ] #feature names: intercept included
lstCoefAdoptmin <- lstCoefAdoptmin[-1] # exclude constant
assign( paste("lstCoefAdoptmin",i, sep = "_") ,lstCoefAdoptmin)
}
final.lstCoefAdoptmin<- do.call("list",mget(ls(pattern = "^lstCoefAdoptmin_.*")))


### 2) Perform model selection explaining info by all the exogenous variables 

# Run cross validation
for (i in nfolds) {
cvfit_info <- cv.glmnet(data.matrix(dfExog), dat$info_b,nfolds=i,family = "binomial",type.measure = "class")
assign( paste("cvfit_info",i, sep = "_") ,cvfit_info)
}
list.cvfit_info<- do.call("list",mget(ls(pattern = "^cvfit_info_.*")))

plot(list.cvfit_info[[5]])

#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
  coefinfomin <- coef(list.cvfit_info[[i]],s ="lambda.min")
  assign( paste("coefinfomin",i, sep = "_") ,coefinfomin)
  list.coefinfomin<- do.call("list",mget(ls(pattern = "^coefinfomin.*")))
  lstCoefinfomin <- list.coefinfomin[[i]]@Dimnames[[1]][which(list.coefinfomin[[i]] != 0 ) ] #feature names: intercept included
  lstCoefinfomin <- lstCoefinfomin[-1] # exclude constant
  assign( paste("lstCoefinfomin",i, sep = "_") ,lstCoefinfomin)
}
final.lstCoefinfomin<- do.call("list",mget(ls(pattern = "^lstCoefinfomin_.*")))



### 3) Perform model selection explaining fields by all the exogenous variables 

# Run cross validation
for (i in nfolds) {
  cvfit_fields <- cv.glmnet(data.matrix(dfExog), dat$fields_b,nfolds=i,family = "binomial",type.measure = "class")
  assign( paste("cvfit_fields",i, sep = "_") ,cvfit_fields)
}
list.cvfit_fields<- do.call("list",mget(ls(pattern = "^cvfit_fields_.*")))

plot(list.cvfit_fields[[3]])

#Get list of variables for "lambda.min"

for (i in 1:length(nfolds)){
  coeffieldsmin <- coef(list.cvfit_fields[[i]],s ="lambda.min")
  assign( paste("coeffieldsmin",i, sep = "_") ,coeffieldsmin)
  list.coeffieldsmin<- do.call("list",mget(ls(pattern = "^coeffieldsmin.*")))
  lstCoeffieldsmin <- list.coeffieldsmin[[i]]@Dimnames[[1]][which(list.coeffieldsmin[[i]] != 0) ]#feature names: intercept included
  lstCoeffieldsmin <- lstCoeffieldsmin[-1] # exclude constant
  assign( paste("lstCoeffieldsmin",i, sep = "_") ,lstCoeffieldsmin)
}
final.lstCoeffieldsmin<- do.call("list",mget(ls(pattern = "^lstCoeffieldsmin_.*")))




### Run the final model explaining Adoption by the union of variables selected in 1), 2) and 3). 

#save lists of vars selected in different steps
for (i in 1:length(nfolds)){
lstvarsselected <- lst(final.lstCoefAdoptmin[[i]],final.lstCoefinfomin[[i]],final.lstCoeffieldsmin[[i]])
assign( paste("lstvarsselected",i, sep = "_") ,lstvarsselected)
}

#save lists of vars selected in different steps
final.lstvarsselected<- do.call("list",mget(ls(pattern = "^lstvarsselected_.*")))


for (i in 1:length(nfolds)){
dfDouble1<- dat[,union(union(final.lstCoeffieldsmin[[i]],final.lstCoefAdoptmin[[i]]),c("q1_adopt","fields_b"))]
assign( paste("dfDouble1",i, sep = "_") ,dfDouble1)

dfDouble2 <- dat[,union(union(final.lstCoefinfomin[[i]],final.lstCoefAdoptmin[[i]]),c("q1_adopt", "info_b"))]
assign( paste("dfDouble2",i, sep = "_") ,dfDouble2)

dfDouble3 <- dat[,union(union(final.lstCoefinfomin[[i]],final.lstCoeffieldsmin[[i]]),c("fields_b", "info_b"))]
assign( paste("dfDouble3",i, sep = "_") ,dfDouble3)

}

final.dfDouble1<- do.call("list",mget(ls(pattern = "^dfDouble1_.*")))
final.dfDouble2<- do.call("list",mget(ls(pattern = "^dfDouble2_.*")))
final.dfDouble3<- do.call("list",mget(ls(pattern = "^dfDouble3_.*")))

#save double df for each nfolds selection

for (i in 1:length(nfolds)){
dfDouble_forLasso <- cbind(final.dfDouble1[[i]],final.dfDouble2[[i]],final.dfDouble3[[i]])
dfDouble_forLasso <-dfDouble_forLasso %>% 
 dplyr::select(unique(colnames(.)))
assign( paste("dfDouble_forLasso",i, sep = "_") ,dfDouble_forLasso)
}
final.dfDouble_forLasso<- do.call("list",mget(ls(pattern = "^dfDouble_forLasso_.*")))

#run final regression with vars choosen before
for (i in 1:length(nfolds)){
reg_double <- glm(q1_adopt ~ .,data=final.dfDouble_forLasso[[i]], family = "binomial")
assign( paste("reg_double",i, sep = "_") ,reg_double)
}
lst.reg_double<- do.call("list",mget(ls(pattern = "reg_double_.*")))
#summary(reg_double_1)

#save variables used in final lasso model
for (i in 1:length(nfolds)){
df.Lasso_selected <- lst.reg_double[[i]][["coefficients"]] 
assign( paste("df.Lasso_selected",i, sep = "_") ,df.Lasso_selected)
}
lst.df.Lasso_selected<- do.call("list",mget(ls(pattern = "df.Lasso_selected_.*")))


#marginal effects
for (i in 1:length(nfolds)){
m.Lasso_select_mfx <-probitmfx(lst.reg_double[[i]], data = final.dfDouble_forLasso[[i]])
marg.effects_lasso <-  m.Lasso_select_mfx[["mfxest"]]
assign( paste("m.Lasso_select_mfx",i, sep = "_") ,m.Lasso_select_mfx)
assign( paste("marg.effects_lasso",i, sep = "_") ,marg.effects_lasso)
}
lst.m.Lasso_select_mfx<- do.call("list",mget(ls(pattern = "m.Lasso_select_mfx_.*")))
lst.marg.effects_lasso<- do.call("list",mget(ls(pattern = "marg.effects_lasso_.*")))


#Compare that to the result that we would obtained for OLS on all variables
#ne loop needed as we only have one full model for all
reg_Full <- glm(q1_adopt ~ .,data=dat,family = "binomial")
summary(reg_Full)
m.reg_Full_mfx <-probitmfx(reg_Full, data = dat)
m.reg_Full_mfx

#now get lists of all vars choosen
#first vars choosen in step 1 (adoption)
for (i in 1:length(nfolds)){
vars_adoption<-as.data.frame(final.lstvarsselected[[i]][["final.lstCoefAdoptmin[[i]]"]])
vars_adoption$number <- 1
vars_adoption <- dplyr::rename(vars_adoption, "Variable"= 1)
assign( paste("vars_adoption",i, sep = "_") ,vars_adoption)
}
lst.vars_adoption<- do.call("list",mget(ls(pattern = "vars_adoption_.*")))

#create dataframe and calculate number of occurence of the certain variables
df.vars_adoption<-full_join(lst.vars_adoption[[1]], lst.vars_adoption[[2]], by = "Variable")
df.vars_adoption<-full_join(df.vars_adoption, lst.vars_adoption[[3]], by = "Variable")
df.vars_adoption<-full_join(df.vars_adoption, lst.vars_adoption[[4]], by = "Variable")
df.vars_adoption<-full_join(df.vars_adoption, lst.vars_adoption[[5]], by = "Variable")
df.vars_adoption<-full_join(df.vars_adoption, lst.vars_adoption[[6]], by = "Variable")
rownames(df.vars_adoption) <- df.vars_adoption$Variable #make first col row names
df.vars_adoption<-df.vars_adoption[,-c(1)] #remove first col
df.vars_adoption$sum <- rowSums(df.vars_adoption, na.rm = TRUE) #caulcuate sum of occurences
df.vars_adoption$Variables <- rownames(df.vars_adoption)
df.vars_adoption<-df.vars_adoption[,-c(1:6)]  #remove cols we don't need anymore
df.vars_adoption <- df.vars_adoption[ , c("Variables", "sum")]#swap col order
write_xlsx(df.vars_adoption,"Output/df.vars_adoption_seed200.xlsx")


#do the same for the vars selected in step 2(info)
for (i in 1:length(nfolds)){
  vars_info<-as.data.frame(final.lstvarsselected[[i]][["final.lstCoefinfomin[[i]]"]])
  vars_info$number <- 1
  vars_info <- dplyr::rename(vars_info, "Variable"= 1)
  assign( paste("vars_info",i, sep = "_") ,vars_info)
}
lst.vars_info<- do.call("list",mget(ls(pattern = "vars_info_.*")))

#create dataframe and calculate number of occurence of the certain variables
df.vars_info<-full_join(lst.vars_info[[1]], lst.vars_info[[2]], by = "Variable")
df.vars_info<-full_join(df.vars_info, lst.vars_info[[3]], by = "Variable")
df.vars_info<-full_join(df.vars_info, lst.vars_info[[4]], by = "Variable")
df.vars_info<-full_join(df.vars_info, lst.vars_info[[5]], by = "Variable")
df.vars_info<-full_join(df.vars_info, lst.vars_info[[6]], by = "Variable")
rownames(df.vars_info) <- df.vars_info$Variable #make first col row names
df.vars_info<-df.vars_info[,-c(1)] #remove first col
df.vars_info$sum <- rowSums(df.vars_info, na.rm = TRUE) #caulcuate sum of occurences
df.vars_info$Variables <- rownames(df.vars_info)
df.vars_info<-df.vars_info[,-c(1:6)]  #remove cols we don't need anymore
df.vars_info <- df.vars_info[ , c("Variables", "sum")]#swap col order
write_xlsx(df.vars_info,"Output/df.vars_info_seed200.xlsx")


#now we also want the mean marginal effect for info and field over all specifications
#save marg. effects as data frames
for (i in 1:length(nfolds)){
marg_effects<- as.data.frame(lst.marg.effects_lasso[[i]])
marg_effects$Variable <- rownames(marg_effects)
marg_effects <- marg_effects[,-c(2:4)]
marg_effects<- marg_effects[,c(2,1)]
assign( paste("marg_effects",i, sep = "_") ,marg_effects)
lst.marg_effects<- do.call("list",mget(ls(pattern = "marg_effects_.*")))
}

df.marg_effects<-full_join(lst.marg_effects[[1]], lst.marg_effects[[2]], by = "Variable")
df.marg_effects<-full_join(df.marg_effects, lst.marg_effects[[3]], by = "Variable")
df.marg_effects<-full_join(df.marg_effects, lst.marg_effects[[4]], by = "Variable")
df.marg_effects<-full_join(df.marg_effects, lst.marg_effects[[5]], by = "Variable")
df.marg_effects<-full_join(df.marg_effects, lst.marg_effects[[6]], by = "Variable")
df.marg_effects$mean_marg_effect <- rowMeans(df.marg_effects[,2:7], na.rm = TRUE)
df.marg_effects<-df.marg_effects[,-c(2:7)]
write_xlsx(df.marg_effects,"Output/df.marg_effects_seed200.xlsx")



plot_seed200 <- plot_summs(m.Lasso_select_mfx_1, m.Lasso_select_mfx_2,m.Lasso_select_mfx_3,m.Lasso_select_mfx_4,m.Lasso_select_mfx_5,m.Lasso_select_mfx_6,
           scale = TRUE, robust = TRUE, colors = c("YlOrRd"),
           coefs = c("knowing other farmers (info)"="info_b1",
                    # "Info_IV =knowing other farmers"="info_iv",
                     "observing fields (fields)"="fields_b1",
                     "Field_IV = observing fields"="fields_iv")) + theme(legend.position="bottom")



ggarrange(plot_seed10, plot_seed35, plot_seed50, plot_seed100,plot_seed200, ncol = 1, legend = FALSE,
          labels = c("seed 20", "seed 35", "seed 50", "seed 100", "seed 200"), font.label = list(size=10, face = "bold") )+ theme(legend.position="bottom")



