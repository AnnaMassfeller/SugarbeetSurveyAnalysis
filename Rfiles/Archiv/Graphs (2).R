library(ggpubr)
#file to create Graphs
#source("C:/Users/Massfeller/sciebo/PhD/Paper/Paper2_socioSpat/SurveyAnalysis/Rfiles/DataPreProcessing.R")

########################
#get some descriptives##
#######################

#summary(FullSample)
str(FullSample)
FullSample$plz <- as.factor(FullSample$plz)
FullSample$q1_adopt <- as.factor(FullSample$q1_adopt)
FullSample$FieldDist <- as.factor(FullSample$FieldDist)
#table(FullSample$q1_adopt)
st(FullSample, vars = c("q1_adopt","q3_info","plz","NrFields","FieldDist","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES"))
#, group = "'q1_adopt")

#stargazer(FullSample[c("q1_adopt","q3_info","plz","q5_len_other","FieldDist","q5_alt_fields","q6_col1","q6_col2","q6_col3","q7_age", "q7_size", "q7_farm","q7_speci_select", "q7_AES")], type = "text", 
#         title="Descriptive statistics/selected variables", digits=1, out="table.txt")

#let's show some graphs
colors <- c(rep("#08306B",1), rep("#9ECAE1",1))

g.adopt <- ggplot(FullSample, aes(q1_adopt))+
  geom_histogram(binwidth = 5, stat = "count", fill = colors)+
  labs(x = "Adoption", y = "Count")+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.adopt

g.info <- ggplot(FullSample, aes(q3_info))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Nr. of adopters known", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))
g.info

g.NrFields <- ggplot(FullSample, aes(NrFields))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Nr. of fields", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))
g.NrFields

g.FieldDist <- ggplot(FullSample, aes(FieldDist))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.FieldDist

g.Intent.Mech_trad <- ggplot(FullSample, aes(q6_col1))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use trad. mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_trad

g.Intent.Mech_modern <- ggplot(FullSample, aes(q6_col2))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use modern mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_modern

g.Intent.Mech_auton <- ggplot(FullSample, aes(q6_col3))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Intention", y = "Count",title ="Intention to use autonomous mechanical weeding in the future")+
  scale_x_discrete(labels = c("no Intention", "low","middle","high","already in use"))
g.Intent.Mech_auton

g.Age <- ggplot(FullSample, aes(q7_age))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Age in years", y = "Count",title ="Participants' age")+
  scale_x_discrete(labels = c("15-24", "25-34","35-44","45-54","55-64","65 and more","no Info"))
g.Age

g.Farmsize <- ggplot(FullSample, aes(q7_size))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Farmsize in hectar", y = "Count",title ="Farm Size")+
  scale_x_discrete(labels = c("less than 5", "5-9","10-19","20-49","50-99","100-199","200-499","500-999","1000 and more", "no Info"))
g.Farmsize

g.Specialization <- ggplot(FullSample, aes(q7_speci_select))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Main specialization", y = "Count",title ="Farm Specialization")+
  scale_x_discrete(labels = c("crop production", "livestock","special crops","mixed","no Info","others"))
g.Specialization

g.organic <- ggplot(FullSample, aes(q7_farm))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "farm type", y = "Count",title ="type")+
  scale_x_discrete(labels = c("conventional", "fully organic","crop production organic"))
g.organic

g.AES <- ggplot(FullSample, aes(q7_AES))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "participation", y = "Count",title ="AES participation")+
  scale_x_discrete(labels = c("yes", "no","no Info"))
g.AES

g.bundesland <- ggplot(FullSample, aes(bundesland))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "state", y = "Count",title ="Federal state")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.bundesland

g.tech <- ggplot(Adopters, aes(q2_technique))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "technique", y = "Count",title ="Techniques in use")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.tech

g.tech_time <- ggplot(Adopters, aes(q2_timeframe))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "year of adoption", y = "Count",title ="Techniques in use")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g.tech_time

Adopters[17,]$q2_machine <- "0"
Adopters[26,]$q2_machine <- "0"
g.ownership <- ggplot(Adopters, aes(q2_machine))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Type of ownership", y = "Count",title ="Ownership status of machines in use")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(labels = c("own","shared with neighbours","Machinery ring","Contractor","Other"))
g.ownership

#disentangle those who are double
Adopters[23,]<- Adopters[17,]

Adopters[23,]$q2_technique <- "Scharhacke"
Adopters[23,]$q2_timeframe <- "1980"

Adopters[17,]$q2_technique <- "Kombination Hacke-Bandspritze"
Adopters[17,]$q2_timeframe <- "1980"


Adopters[49,]<- Adopters[26,]

Adopters[49,]$q2_technique <- "Scharhacke"
Adopters[49,]$q2_timeframe <- "2021"

Adopters[26,]$q2_technique <- "Hackstriegel"
Adopters[26,]$q2_timeframe <- "2021"

Adopters_tech<- Adopters[Adopters$date != "2022-04-20 16:30:35",]

g.tech_over_time <- ggplot(Adopters_tech, aes(q2_timeframe, fill = q2_technique))+
  geom_bar(position = "stack")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Year of purchase", y = "Count") +#, title = "Usage of mechanical weeding machines over time")+
  scale_fill_brewer(palette = "Greys",name = "",labels = c("Farmdroid","Hoe", "Combination hoe-bandspray", "Rotorharrow", "Coulter hoe",
          "Fingerweeder", "Hoe harrow", "Rolling hoe", "Beet hoe")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.tech_over_time

#reasons for non-adoption
df.ReasonsNo[is.na(df.ReasonsNo)] <- "99"
df.ReasonsNo[] <- lapply(df.ReasonsNo, function(x) gsub(" ","", x)) 
df.ReasonsNo$R1 <- as.numeric(df.ReasonsNo$R1)
df.ReasonsNo$R2 <- as.numeric(df.ReasonsNo$R2)
df.ReasonsNo$R3 <- as.numeric(df.ReasonsNo$R3)
df.ReasonsNo$R4 <- as.numeric(df.ReasonsNo$R4)
df.ReasonsNo$R5 <- as.numeric(df.ReasonsNo$R5)
df.ReasonsNo$R6 <- as.numeric(df.ReasonsNo$R6)
df.ReasonsNo$R7 <- as.numeric(df.ReasonsNo$R7)
df.ReasonsNo$R8 <- as.numeric(df.ReasonsNo$R8)
df.ReasonsNo$R9 <- as.numeric(df.ReasonsNo$R9)
df.ReasonsNo$R10 <- as.numeric(df.ReasonsNo$R10)
#df.ReasonsNo <-  str_replace_all(df.ReasonsNo," ","")
#Freq.ReasonsNo<-as.data.frame(table(df.ReasonsNo$R1))
ldply(df.ReasonsNo, function(c) sum(c =="1"))
lvls <- unique(unlist(df.ReasonsNo))
# apply the summation per value 
freq <- sapply(df.ReasonsNo, 
               function(x) table(factor(x, levels = lvls, 
                                        ordered = TRUE)))
print(freq)
freq.ReasonsNo <- as.data.frame(freq)
freq.ReasonsNo <-  na_if(freq.ReasonsNo,"99")
#freq.ReasonsNo$sum <-apply(freq.ReasonsNo,1,sum,na.rm = TRUE)
freq.ReasonsNo$sum <- freq.ReasonsNo$R1+freq.ReasonsNo$R2+freq.ReasonsNo$R3+freq.ReasonsNo$R4+freq.ReasonsNo$R5+freq.ReasonsNo$R6+freq.ReasonsNo$R7+freq.ReasonsNo$R8
freq.ReasonsNo <- tibble::rownames_to_column(freq.ReasonsNo, "Nr.Reason")
freq.ReasonsNo<- freq.ReasonsNo %>% filter(freq.ReasonsNo$Nr.Reason != 99)
freq.ReasonsNo$Nr.Reason <- as.numeric(freq.ReasonsNo$Nr.Reason)

freq.ReasonsNo$Reason <- freq.ReasonsNo$Nr.Reason
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "0"] <- "running costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "1"] <- "investment costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "2"] <- "time constraints"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "3"] <- "low reliability"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "4"] <- "high risk of damaging the crop"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "5"] <- "not possible on my farm "
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "6"] <- "I don't know if the technology works for me"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "7"] <- "I don't trust the technology"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "8"] <- "colleagues'bad experiences"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "9"] <- "I don't know any colleagues"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "10"] <- "wait until the technology is more mature"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "11"] <- "no reason for me to change cultivation"
#freq.ReasonNo$Reason <-arrange(desc(sum))
g.ReasonsNo <- ggplot(freq.ReasonsNo,aes(x=reorder(factor(Reason),-sum),y=sum))+
  geom_col(fill='#08306B')+ #brewer.pal(9,"Blues") find one colour form the "blues" scheme#blue. #08306B
  labs(x = "", y = "Count")+#, title = "Reasons for Non-adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
g.ReasonsNo

#overview Intentions

df.intentions <- subset(FullSample, select = q6_col1:q6_col3)

ldply(df.intentions, function(c) sum(c =="1"))
lvls.i <- unique(unlist(df.intentions))
# apply the summation per value 
freq.i <- sapply(df.intentions, 
                 function(x) table(factor(x, levels = lvls.i, 
                                          ordered = TRUE)))

print(freq.i)
freq.Intentions <- as.data.frame(freq.i)
#freq.ReasonsNo$sum <- freq.ReasonsNo$R1+freq.ReasonsNo$R2+freq.ReasonsNo$R3+freq.ReasonsNo$R4+freq.ReasonsNo$R5+freq.ReasonsNo$R6+freq.ReasonsNo$R7+freq.ReasonsNo$R8
freq.Intentions <- tibble::rownames_to_column(freq.Intentions, "Intention")
freq.Intentions["Intention"][freq.Intentions["Intention"] == "0"] <- "no Intention"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "1"] <- "low"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "2"] <- "middle"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "3"] <- "high"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "4"] <- "technique in use"

freq.Intentions_t <- t(freq.Intentions)
library(reshape2)
freq.intentions_long <- melt(freq.Intentions)

g.Intentions<-ggplot(freq.intentions_long,aes(x= factor(Intention, level = c("no Intention", "low","middle","high","technique in use")),value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x = "Level of Intention", y = "Count")+#, title = "Intention to use different types of mechancial weeding in the future")+
  scale_fill_brewer(name = "Type of mechanical weeding", labels = c("traditional", "modern", "autonomous"),palette = "Blues")
g.Intentions

#mean distance to demonstration farm
#c + geom_density(
g.MinDemoDist <- ggplot(FullSample, aes(minDist_demo))+
  geom_density(kernel = "gaussian")+
 # labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 # scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.MinDemoDist  

g.MeanDemoDist <- ggplot(FullSample, aes(meanDist_demo))+
  geom_density(kernel = "gaussian")+
  #labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.MeanDemoDist  

g.MeanDistNei <- ggplot(FullSample_forDist, aes(meanDist_Nei))+
  geom_density(kernel = "gaussian")+
  #labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.MeanDistNei  

#plot IVS
g.ShareOrgFarms <- ggplot(SampleIV, aes(ShareOrgFarms))+
  geom_density(kernel = "gaussian")+
  #labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.ShareOrgFarms


g.ShareOrgArea <- ggplot(SampleIV, aes(ShareOrgArea))+
  geom_density(kernel = "gaussian")+
  #labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.ShareOrgArea


g.IV_fields <- ggplot(FullSample, aes(fields_b, ShareOrgArea))+ geom_boxplot()


g.IV_info <- ggplot(FullSample, aes(info_b, ShareOrgFarms))+ geom_boxplot()

#anaylse socio-spatial network by region/ sugar beet fabric

#g.NrPeers_SZ <- ggplot(FullSample[FullSample$advisory == "Südzucker",], aes(q3_info))+
 # geom_bar(aes(y = ..prop.., group = 1))+
  #labs(title= "Südzucker")+
 # labs(#x = "Nr. of adopters known",
  #  y = "Count")+
 # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
   #     axis.title.x=element_blank(),
    #    axis.text.x=element_blank(),
     #   axis.ticks.x=element_blank())+
 # labs(x = "", y = "")#+
  #scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))

#g.NrPeers_NZ <- ggplot(FullSample[FullSample$advisory == "Nordzucker",], aes(q3_info))+
 # geom_bar(aes(y = ..prop.., group = 1))+
  #labs(title= "Nordzucker")+
 # labs(#x = "Nr. of adopters known", 
  #  y = "Count")+
 # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
   #     axis.title.x=element_blank(),
    #    axis.text.x=element_blank(),
     #   axis.ticks.x=element_blank())+
 # labs(x = "", y = "")#+
  #scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))

g.NrPeers_SF<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),q3_info,fill = as.factor(advisory)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("blue","green","red"))+
  ylab("")+
  theme_bw()+
 # xlab("number of adopters known")
 # labs(title= "PfeifferLangen")+
 labs(x = "number of adopters known", legend.position = "none")+ 
 # legend("Sugar factories")+
   # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))


g.NrFields_SF<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),NrFields,fill = as.factor(advisory)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
ylab("")+
  theme_bw()+
  scale_fill_manual(values = c("blue","green","red"))+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))

g.DistFields_SF<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),FieldDist,fill = as.factor(advisory)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("blue","green","red"))+
 ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to fields observed [km]", fill = "sugar company")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))


ggarrange(g.NrPeers_SF, g.NrFields_SF, g.DistFields_SF,nrow = 2, ncol = 2)
 
#plot map
#save all participants centroid as sf
sf.Coordinates<- st_as_sf(df.Coordinates, coords = c("Long_Centroid", "Lat_Centroid"), crs = 4326)
plot(sf.Coordinates)
sf.coo<-mapview::mapview(sf.Coordinates, cex = 3,zcol= "q1_adopt", col.regions = c("#66A61E","#E6AB02"))
sp.techniques<- mapview::mapview(sf.Coordinates,zcol= "q2_timeframe", cex = 4)
#mapview::mapview(spdf, zcol="ShareSB",col.regions = brewer.pal(19, "Greens"))+sf.coo
#add sugar beet factories
sf.ZR_fabriken<- st_as_sf(df.SB_fabriken, coords = c("Long", "Lat"), crs = 4326)
zr_fabriken<- mapview::mapview(sf.ZR_fabriken,zcol= "Fabrikstandort", col.regions=c("darkred"), cex = 4)
mapview::mapview(spdf, zcol="ShareSB", col.regions = brewer.pal(20,"Greys"))#+sf.coo+zr_fabriken
#mapview::mapview(spdf, zcol="ShareAdopters", col.regions = brewer.pal(20,"Reds"))#+sf.coo+zr_fabriken


#to compare adopters and non-adopters create barplot
g.compareNon_Adopters<- ggplot(SampleIV)+
  geom_boxplot(aes(q1_adopt,(meanDist)))+
 # geom_boxplot(aes(q1_adopt,(minDist_demo)))
  scale_y_continuous(labels=scales::percent)+
  ylab("")+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to fields observed [km]", fill = "sugar factory")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+#,legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))


#bundesland per adoption
g.fabrics_A <- ggplot(SampleIV, aes(y = (..count..)/sum(..count..),advisory, fill = q1_adopt))+
  geom_bar(binwidth = 5, stat = "count", position = "fill")+#,fill =c("#66A61E","#E6AB02","#66A61E"))+
  labs(x = "", y = "Count", fill = "Adoption")+
  theme_bw()+
  scale_fill_manual(values = c("#66A61E","#E6AB02","#66A61E"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#, legend.position = "none")
g.fabrics_A

#check for Fabrikstandort
g.NrPeers_Fabrikstandort<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),q3_info,fill = as.factor(Fabrikstandort)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of adopters known", legend.position = "none")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))+
  scale_fill_manual(values = c("darkseagreen1", "dodgerblue1", "darkseagreen2","darkseagreen3","dodgerblue2",
                               "darkseagreen4","darkseagreen","dodgerblue3","gold",
                               "Salmon","pink","tomato","red3","blue","darkblue","orange","plum"))


g.NrFields_Fabrikstandort<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),NrFields,fill = as.factor(Fabrikstandort)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))+
  scale_fill_manual(values = c("darkseagreen1", "dodgerblue1", "darkseagreen2","darkseagreen3","dodgerblue2",
                               "darkseagreen4","darkseagreen","dodgerblue3","gold",
                               "Salmon","pink","tomato","red3","blue","darkblue","orange","plum"))

g.DistFields_Fabrikstandort<- ggplot(SampleIV)+
  geom_bar(aes(y = (..count..)/sum(..count..),FieldDist,fill = as.factor(Fabrikstandort)),position= "dodge")+
  scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to fields observed [km]")+#, fill = "sugar factory")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))+
  scale_fill_manual(values = c("darkseagreen1", "dodgerblue1", "darkseagreen2","darkseagreen3","dodgerblue2",
                               "darkseagreen4","darkseagreen","dodgerblue3","gold",
                               "Salmon","pink","tomato","red3","blue","darkblue","orange","plum"))


#colours
#P&L: green, SZ:rot, NZ: blau

ggarrange(g.NrPeers_Fabrikstandort, g.NrFields_Fabrikstandort, g.DistFields_Fabrikstandort,nrow = 3, ncol = 1)

g.FieldDist_mc <- ggplot(FullSample_forDist_mc, aes(FieldDist))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Distance in km", y = "Count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.FieldDist_mc


g.MeanDistNei_map <- ggplot(FullSample_forDist_map, aes(meanDist_Nei))+
  geom_density(kernel = "gaussian")+
 # scale_x_continuous(limits = c(0, 35))+
 scale_x_continuous(limits = c(0, 35))+
  theme_bw()+
  #labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")
  
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
g.MeanDistNei_map 



"brown1",
"brown3",
"red1",
"brown3",
"brown3",
"red1",
"red1",
"brown3",
"brown3",
"darkred",
"darkred",
"darkred",
"darkred",
"darkred",
"red1",
"red1",
"darkred",
"darkred"


g.NrPeers_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(q3_info,fill = as.factor(q1_adopt)),position= "dodge")+
 # scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#66A61E","#E6AB02"))+
  ylab("count in %")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of adopters known", legend.position = "none")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))


g.NrFields_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(NrFields,fill = as.factor(q1_adopt)),position= "dodge")+
 # scale_y_continuous(labels=scales::percent)+
  ylab("count %")+
  theme_bw()+
  scale_fill_manual(values = c("#66A61E","#E6AB02"))+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))

g.DistFields_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(FieldDist,fill = as.factor(q1_adopt)),position= "dodge")+
  #scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#66A61E","#E6AB02"))+
  ylab("%")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to fields observed [km]", fill = "Adoption")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+#,legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))


ggarrange(g.NrPeers_Adopt, g.NrFields_Adopt, g.DistFields_Adopt,nrow = 2, ncol = 2)
#check colours?!

##check ownership status dependant on type of machinery
#get sub-df for technique

df.technique <- SampleIV %>% dplyr::select(date:q2_summary)
df.technique <- df.technique[df.technique$q1_adopt == 1,]


#put it out as xlsx at give it back in to adjust those who slected several techniques
#library(xlsx)
#write.xlsx(df.technique,"df.technique.xlsx")
df.technique <-read_excel("df.technique.xlsx")

df.technique$type_tech <- 0
#0 if traditional
#1 if camera or GPS
#2 if autonomous
df.technique$type_tech <- ifelse(df.technique$question2_camera_choice == 1 |df.technique$question2_gps_choice == 1 ,1,0)
df.technique$type_tech <- ifelse(df.technique$question2_autonom_choice == 1 ,2,df.technique$type_tech)

#ownership
#0-Eigene Maschine
#1-Teile mit Nachbarn
#2-Maschinenring
#3-Lohnunternehmer
#4-Andere

g.ownersip_technology<- ggplot(df.technique)+
  geom_bar(aes(q2_machine,fill = as.factor(type_tech)),position= "fill")+
  # scale_y_continuous(labels=scales::percent)+
  ylab("count %")+
  theme_bw()+
 scale_fill_discrete(name = "type of technique", labels = c("traditional", "modern", "autonomous"))+
  xlab("type of ownership")+
 # labs(title= "PfeifferLangen")+
 # labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
       # panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#, legend.position = "none")+
 scale_x_discrete(labels = c("own", "share with neighbour","machinery ring","contractor","other"))

g.technology_ownership<- ggplot(df.technique)+
  geom_bar(aes(type_tech,fill = as.factor(q2_machine)),position= "fill")+
  # scale_y_continuous(labels=scales::percent)+
  ylab("count %")+
  theme_bw()+
  scale_fill_discrete(name = "type of ownership", labels = c("own", "share with neighbour","machinery ring","contractor","other"))+
  xlab("type of technique")+
  # labs(title= "PfeifferLangen")+
  # labs(x = "number of fields observed")+ 
  # y = "Count")+
  scale_x_discrete(labels = c("traditional", "modern", "autonomous"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        # panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))#, legend.position = "none")+
  

g.tech_over_time <- ggplot(df.technique, aes(q2_timeframe, fill = q2_technique))+
  geom_bar(position = "stack")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Year of purchase", y = "Count") +#, title = "Usage of mechanical weeding machines over time")+
  #scale_fill_brewer(palette = "Blues",name = "",labels = c("Farmdroid","Hoe", "Combination hoe-bandspray", "Rotorharrow", "Coulter hoe",
   #                                                        "Fingerweeder", "Hoe harrow", "Rolling hoe", "Beet hoe")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.tech_over_time




