library(ggpubr)
library(mapview)
library(sf)
library(leaflet)
library(tidyverse)
library(leafem)
library(webshot)
library(htmlwidgets)
library(reshape2) 
library(ggtext)
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
  labs(x = "Age in years", y = "Count")+#,title ="Participants' age")+
  scale_x_discrete(labels = c("15-24", "25-34","35-44","45-54","55-64","65 and more","no Info"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
g.Age

g.Farmsize <- ggplot(FullSample, aes(q7_size))+
  geom_histogram(binwidth = 5, stat = "count")+
  labs(x = "Farmsize in hectar", y = "Count")+#,title ="Farm Size")+
  scale_x_discrete(labels = c("less than 5", "5-9","10-19","20-49","50-99","100-199","200-499","500-999","1000 and more", "no Info"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
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
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "0"] <- "Zu hohe laufende Kosten"#"running costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "1"] <- "Zu hohe Investitionskosten"#"investment costs"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "2"] <- "Zu hoher Zeitaufwand"#"time constraints"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "3"] <- "Geringe Zuverlässigkeit in der Unkrautbekämpfung"#"low reliability"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "4"] <- "Hohes Risiko die Kulturpflanze zu schädigen"#"high risk of damaging the crop"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "5"] <- "Nicht möglich auf meinem Betrieb (z.B. durch Bodenbedingungen, Feldgrößen,..)"#"not possible on my farm "
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "6"] <- "Ich weiß nicht, ob die Technik bei mir funktioniert"#"I don't know if the technology works for me"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "7"] <- "Ich traue mir die Anwendung/Bedienung  nicht zu "#"I don't trust the technology"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "8"] <- "Meine Kollegen in der Region haben schlechte Erfahrungen gemacht und mir davon erzählt"#"colleagues'bad experiences"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "9"] <- "Ich kenne keine Kollegen in meiner Region die mir Tipps geben könnten"#"I don't know any colleagues"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "10"] <-"Ich möchte noch warten bis die Technik ausgereifter ist"#"wait until the technology is more mature"
freq.ReasonsNo["Reason"][freq.ReasonsNo["Reason"] == "11"] <-"Es gibt für mich keinen Grund für eine Umstellung des Anbaus"#"no reason for me to change cultivation"
#freq.ReasonNo$Reason <-arrange(desc(sum))
g.ReasonsNo <- ggplot(freq.ReasonsNo,aes(x=reorder(factor(Reason),-sum),y=sum))+
  geom_col(fill='#F46D43')+ #brewer.pal(9,"Spectral") find one colour form the "blues" scheme#blue. #08306B #08306B
  labs(x = "", y = "Anzahl")+#, title = "Reasons for Non-adoption")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom",
        text = element_text(size=20),
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
freq.Intentions["Intention"][freq.Intentions["Intention"] == "0"] <- "keine Intention"#"no Intention"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "1"] <- "gering"#"low"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "2"] <- "mittel"#"middle"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "3"] <- "hoch"#"high"
freq.Intentions["Intention"][freq.Intentions["Intention"] == "4"] <- "Technik schon im Einsatz"#"technique in use"

freq.Intentions_t <- t(freq.Intentions)
library(reshape2)
freq.intentions_long <- melt(freq.Intentions)

g.Intentions<-ggplot(freq.intentions_long,aes(x= factor(Intention, level = c("keine Intention", "gering","mittel","hoch","Technik schon im Einsatz")),value,fill=variable))+#c("no Intention", "low","middle","high","technique in use")
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))+
  labs(x= "",y = "Anzahl",caption = "Keine intention = Ich plane nichts in diese Richtung,
gering = Ich plane mich zu informieren und aktuelle Diskussionen und die Fachliteratur zu dem Thema zu verfolgen
mittel = Ich plane aktiv innerhalb der nächsten 5 Jahren Angebote einzuholen und Beratungsmöglichkeiten anzunehmen
hoch = Ich plane diese Technik innerhalb der nächsten 5 Jahren einzusetzen (Eigene Anschaffung, Lohnunternehmer, ...)")+#, title = "Intention to use different types of mechancial weeding in the future")+
  scale_fill_brewer(name = "Technik zur mechanischen Unkrautbekämpfung", labels = c("traditionell", "modern", "autonom"),palette = "Spectral")
g.Intentions



g.Intentions_eng<-ggplot(freq.intentions_long,aes(x= factor(Intention, level = c("keine Intention", "gering","mittel","hoch","Technik schon im Einsatz")),value,fill=variable))+#c("no Intention", "low","middle","high","technique in use")
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  scale_x_discrete(label = c("no intention", "low", "middle", "high", "technology already in use"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "right",
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))+
  labs(x= "",y = "Count",caption = "no intention = I don't plan anything in that direction,
low = I plan to get informed and to follow current discussions about the topic,
middle = I actively plan to get offers and to make use of advisory services within the next 5 years,
high = I pan to use this technology within the next 5 years (own investment, via contractor, ...)")+#, title = "Intention to use different types of mechancial weeding in the future")+
  scale_fill_brewer(name = "type of technology", labels = c("traditional", "modern", "autonomous"),palette = "Greys")
g.Intentions_eng


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

#g.MeanDistNei <- ggplot(FullSample_forDist, aes(meanDist_Nei))+
 # geom_density(kernel = "gaussian")+
  #labs(x = "Distance in km", y = "Count")+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
#g.MeanDistNei  

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

g.NrPeers_SF2<- ggplot(SampleIV)+
  geom_bar(aes(advisory,fill = as.factor(q3_info)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0", "1-5","6-10","mehr al 10"),palette = "Greens")+
  ylab("")+
  theme_bw()+
 # xlab("number of adopters known")
 labs(fill= "Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
# labs(x = "Zuckerhersteller")+ 
# legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
   # y = "Count")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
       axis.ticks.x=element_blank())#+#, legend.position = "none")+
 # scale_fill_discrete(labels = c("0", "1-5","6-10","more than 10"))
g.NrFields_SF2<- ggplot(SampleIV)+
  geom_bar(aes(advisory,fill = as.factor(NrFields)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0", "1-5","6-10","11-15","mehr als 15"),palette = "Greens")+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  labs(fill= "Wie viele Felder kennen Sie, auf denen Unkraut mechanisch oder chemisch-mechanisch kombiniert wird?")+
#  labs(x = "Zuckerhersteller")+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(axis.text.x = element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        axis.title.x=element_blank(),
       axis.ticks.x=element_blank())
#position= "fill",values = c("darkblue","darkgreen","darkred", "yellow"),
g.DistFields_SF2<- ggplot(SampleIV)+
  geom_bar(aes(advisory,fill = as.factor(FieldDist)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0-5", "6-10","11-15","16-20","21-30","mehr als 30","ich kenne keine Felder"),palette = "Greens")+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  labs(fill= "In welchem Umkreis liegen diese Felder? [in km] ")+
  labs(x = "Zuckerhersteller")+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(axis.text.x = element_text(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

ggarrange(g.NrPeers_SF2, g.NrFields_SF2, g.DistFields_SF2,nrow = 3, ncol = 1)


 

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
g.fabrics_A <- ggplot(SampleIV, aes(advisory, fill = q1_adopt))+
  geom_bar(binwidth = 5, stat = "count", position = "fill")+#,fill =c("#66A61E","#E6AB02","#66A61E"))+
  labs(x = "", y = "", fill = "Nutzen mechanische Unkrautbekämpfung")+
  theme_bw()+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#66A61E","#E6AB02","#66A61E"), labels=c("Nein", "Ja"))+
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
#write_xlsx(df.technique,"df.technique.xlsx")
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
   scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme_bw()+
 scale_fill_discrete(name = "Technik", labels = c("traditionel", "modern", "autonom"))+
  xlab("Wem gehört die Maschine?")+
 # labs(title= "PfeifferLangen")+
 # labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+#, legend.position = "none")+
 scale_x_discrete(labels = c("Eigene Maschine", "Teile mit Nachbarn","Maschinenring","Lohnunternehmer","Andere"))

g.technology_ownership<- ggplot(df.technique)+
  geom_bar(aes(type_tech,fill = as.factor(q2_machine)),position= "fill")+
  scale_x_discrete(labels = c("traditionel", "modern", "autonom"))+
   scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"))+
  
  theme_bw()+
  scale_fill_discrete(name = "Wem gehört die Maschine?", labels = c("Eigene Maschine", "Teile mit Nachbarn","Maschinenring","Lohnunternehmer","Andere"))+
  xlab("Technik")
  

g.tech_over_time <- ggplot(df.technique, aes(q2_timeframe, fill = q2_technique))+
  geom_bar(position = "stack")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Year of purchase", y = "Count") +#, title = "Usage of mechanical weeding machines over time")+
  #scale_fill_brewer(palette = "Rainbow")+#,name = "",labels = c("Farmdroid","Hoe", "Combination hoe-bandspray", "Rotorharrow", "Coulter hoe",
   #                                                        "Fingerweeder", "Hoe harrow", "Rolling hoe", "Beet hoe")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
g.tech_over_time


#compare mean distance  dependant on way of selection
#use only those who actually selected fields

#first check on numeric scale (e.g. those who selected via multiple chocie were given the mean of thei category)

f <- SampleIV %>% dplyr::filter(fields_b == 1) %>%
  ggplot(aes(selection_distance,meanDist))+ geom_boxplot()+
  scale_x_discrete(labels = c("via map","via multiple choice"))+
  scale_y_continuous(limits = c(0, 50))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
f

t.test(SampleIV[SampleIV$selection_distance == 0,]$meanDist,SampleIV[SampleIV$selection_distance == 1,]$meanDist )

#now test if those who selected on map were eingeordnet in mc-categories

g.FieldDist_diff <- ggplot(SampleIV, aes(FieldDist,fill = selection_distance))+
  geom_bar(binwidth = 5, stat = "count", position = "fill")+
  labs(x = "Distance in km", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))+
  scale_fill_discrete(name = "type of selection", labels = c("via map","via multiple choice","no fields selected"))
g.FieldDist_diff

#test for difference
#create subsample with all those who observed fields
Sample_dist<-SampleIV %>% dplyr::filter(fields_b == 1)
Sample_dist$FieldDist <-droplevels(Sample_dist$FieldDist)
Sample_dist$selection_distance <-droplevels(Sample_dist$selection_distance)
chisq.test(table(Sample_dist$selection_distance, Sample_dist$FieldDist))


#peer effecrs and adoption
g.NrPeers_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(q1_adopt,fill = as.factor(q3_info)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  coord_flip()+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0", "1-5","6-10","mehr als 10"),palette = "Spectral")+
  ylab("")+
 scale_x_discrete(name = "Nutzen Sie selbst mechanische Unkrautbekämpfung?",labels = c("Nein", "Ja"))+
  theme_bw()+
  ggtitle("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  #xlab(fill= "", title = "Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
 # labs()+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        plot.title = element_textbox_simple(),
        legend.title=element_blank())#,
       # axis.title.x=element_blank(),
      #  axis.text.x = element_blank(),
       # axis.ticks.x=element_blank())#+#, legend.position = "none")+


g.NrFields_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(q1_adopt,fill = as.factor(NrFields)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0", "1-5","6-10","11-15","mehr als 15"),palette = "Greens")+
  ylab("")+
  scale_x_discrete(name = "Nutzen Sie selbst mechanische Unkrautbekämpfung?",labels = c("Nein", "Ja"))+
  theme_bw()+
  ggtitle("Wie viele Felder kennen Sie, auf denen Unkraut mechanisch entfernt wird?")+
  #xlab(fill= "", title = "Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # labs()+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        legend.title=element_blank())#,
# axis.title.x=element_blank(),
#  axis.text.x = element_blank(),
# axis.ticks.x=element_blank())#+#, legend.position = "none")+


g.DistFields_Adopt<- ggplot(SampleIV)+
  geom_bar(aes(q1_adopt,fill = as.factor(FieldDist)),position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(labels = c("0-5", "6-10","11-15","16-20","21-30","mehr als 30","ich kenne keine Felder"),palette = "Greens")+
  ylab("")+
  scale_x_discrete(name = "Nutzen Sie selbst mechanische Unkrautbekämpfung?",labels = c("Nein", "Ja"))+
  theme_bw()+
  ggtitle("In welchem Umkreis liegen diese Felder [in km]?")+
  #xlab(fill= "", title = "Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # labs()+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")
        , legend.position = "bottom",
        legend.title=element_blank())#,
# axis.title.x=element_blank(),
#  axis.text.x = element_blank(),
# axis.ticks.x=element_blank())#+#, legend.position = "none")+


ggarrange(g.NrPeers_Adopt, g.NrFields_Adopt, g.DistFields_Adopt,nrow = 1, ncol = 3)

###################################################
#plot all vars included in LASSO
#add id column to dat
id.var <- sort(as.character(1:nrow(dat)))
dat.id <- cbind(dat, id.var)

#create subsets of variable groups
vars.lasso_interest <- dat.id %>% dplyr::select(id.var,q1_adopt, fields_b, info_b)
vars.lasso_farm <- dat.id %>% dplyr::select(id.var,mainly_crop,farmsize_b,AES_b,age_b)
vars.lasso_sugarbeet <- dat.id %>% dplyr::select(id.var,SB_region,Verband_agg,Fabrikstandort_agg)
vars.lasso_countyfarmstructure <- dat.id %>% dplyr::select(id.var,meanFarmSize2,ShareOrgFarms,ShareOrgArea,populationdensity,farmDens,areaDens,ShareSmallFarms,elevation_in_m_mean,sand_content_percent_mean,clay_content_percent_mean,sq.elevation_in_m_mean,sq.sand_content_percent_mean
                                                           ,sq.clay_content_percent_mean, ShareArableUAA, ShareArableInTotalArea)
vars.lasso_distance <- SampleIV %>% dplyr::select(date,minDist_demo,sq.demodist,sq.fields_dist ,fields_dist ,sq.meanDist, meanDist)
vars.lasso_soil <- dat.id %>% dplyr::select(id.var,elevation_in_m_mean,sand_content_percent_mean,clay_content_percent_mean,sq.elevation_in_m_mean,sq.sand_content_percent_mean,sq.clay_content_percent_mean)

#create data in long format
vars.lasso_countyfarmstructure <- na.omit(vars.lasso_countyfarmstructure)
vars.lasso_countyfarmstructure_long <- melt(data= vars.lasso_countyfarmstructure, id.vars = "id.var")
vars.lasso_soil_long <- melt(data= vars.lasso_soil, id.vars = "id.var")
vars.lasso_distance_long <- melt(data= vars.lasso_distance, id.vars = "date")
vars.lasso_interest_long <- melt(data= vars.lasso_interest, id.vars = "id.var")
vars.lasso_farm_long <- melt(data= vars.lasso_farm, id.vars = "id.var")
vars.lasso_sugarbeet_long <- melt(data= vars.lasso_sugarbeet, id.vars = "id.var")

#which ones are the outliers?

get_outliers = function(x){
  which(x > quantile(x)[4] + 1.5*IQR(x) | x < quantile(x)[2] - 1.5*IQR(x))
}

outliers <- get_outliers(vars.lasso_countyfarmstructure$meanFarmSize2)
df2 = vars.lasso_countyfarmstructure[outliers,]


#make boxplot for numeric vars
ggplot(vars.lasso_countyfarmstructure_long, aes(x = variable, y = value)) +            
  geom_boxplot()+
  scale_y_log10() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


ggplot(vars.lasso_soil_long, aes(x = variable, y = value)) +            
  geom_boxplot()+
  scale_y_log10() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

#distances
ggplot(vars.lasso_distance_long, aes(x = variable, y = value)) +            
  geom_boxplot()+
  scale_y_log10() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

ggplot(vars.lasso_distance, aes(meanDist)) +            
  geom_density()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

ggplot(vars.lasso_distance, aes(minDist_demo)) +            
  geom_density()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

ggplot(vars.lasso_distance, aes(fields_dist)) +            
  geom_density()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

#non-numeric ones
ggplot(vars.lasso_interest_long, aes(variable, fill = value)) +            
  geom_bar(position = "fill")

ggplot(vars.lasso_farm_long, aes(variable, fill = value)) +            
  geom_bar(position = "fill")

ggplot(vars.lasso_sugarbeet_long, aes(variable, fill = value)) +            
  geom_bar(position = "fill")

table(FullSample$q1_adopt)
table(FullSample$info_b)
table(FullSample$fields_b)
table(FullSample$info_b,FullSample$fields_b)
table(FullSample$q1_adopt,FullSample$info_b)
table(FullSample$q1_adopt,FullSample$fields_b)

#check for "baseline bias" between observers and non-observers
#we only need specific vars
FieldsIV_compare <- FieldsIV %>%  dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop","meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")
NonFieldsIV_compare <- NonFieldsIV %>% dplyr::select("q1_adopt","fields_b","info_b","minDist_demo","sq.demodist","farmsize_b","AES_b","age_b","farm_organic","mainly_crop","meanFarmSize2","ShareOrgFarms","ShareOrgArea","populationdensity","farmDens","areaDens","ShareSmallFarms","elevation_in_m_mean","sand_content_percent_mean","clay_content_percent_mean","sq.elevation_in_m_mean","sq.sand_content_percent_mean","sq.clay_content_percent_mean", "ShareArableUAA", "ShareArableInTotalArea","Fabrikstandort_agg")

vtable(FieldsIV_compare,summ = c('mean(x)'))# group = "fields_b")
vtable(NonFieldsIV_compare,summ = c('mean(x)'))


ggplot(dfExog2, aes(populationdensity, fill = fields_b)) +            
  geom_density(alpha = 0.5, position = 'identity')

ggplot(dfExog2, aes(minDist_demo, fill = fields_b)) +            
  geom_density(alpha = 0.5, position = 'identity')

ggplot(dfExog2, aes(elevation_in_m_mean, fill = fields_b)) +            
  geom_density(alpha = 0.5, position = 'identity')
  
ggplot(dfExog2, aes(age_b, fill = fields_b)) +            
  geom_bar(position = "fill")

ggplot(dfExog2, aes(farmsize_b, fill = fields_b)) +            
  geom_bar(position = "fill")

ggplot(dfExog2, aes(AES_b, fill = fields_b)) +            
  geom_bar(position = "fill")


###plot data on kreislevel
df.Kreise_Lasso_long <- df.Kreise_Lasso%>% dplyr::select(-KREISE)
df.Kreise_Lasso_long_prep <- na.omit(df.Kreise_Lasso_long)
df.Kreise_Lasso_long <- melt(data= df.Kreise_Lasso_long_prep, id.vars = "Kreis")

ggplot(df.Kreise_Lasso_long, aes(x = variable, y = value)) +            
  geom_boxplot()+
  scale_y_log10() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))


#check distance between own fields
#subsampel of those who selected own fields via map 
OwnFieldsSample<-SampleIV[!is.na(SampleIV$Mean_ownfield_dist),]
#OwnFieldsSample$Mean_ownfield_dist<- ifelse(OwnFieldsSample$Mean_ownfield_dist >= 5,5,OwnFieldsSample$Mean_ownfield_dist)

ggplot(OwnFieldsSample,aes(Mean_ownfield_dist))+#, fill = fields_b)) +            
  geom_density(alpha = 0.5, position = "identity")+
  scale_x_continuous(limits = c(0, 10))

mean(SampleIV$Mean_ownfield_dist, na.rm = TRUE)
mean(SampleIV$fields_dist, na.rm = TRUE)


table(is.na(SampleIV$Mean_ownfield_dist))
table(is.na(SampleIV$fields_dist))

ggplot(SampleIV, aes(q6_col3, fill = q1_adopt)) +            
  geom_bar()

fisher.test(table(SampleIV$q1_adopt, SampleIV$q6_col3))



####descriptive reuslts fro paper
g.NrPeers_SF<- ggplot(FullSample)+
  geom_bar(aes(q3_info))+#,position = position_fill(reverse = TRUE))+#y = (..count..)/sum(..count..)
  # scale_y_continuous(labels=scales::percent)+
  # scale_fill_brewer(labels = c("0", "1-5","6-10","mehr al 10"),palette = "Greens")+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  labs(x="number of adopters known", fill= "Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # labs(x = "Zuckerhersteller")+ 
  # legend("Wie viele LandwirtInnen kennen Sie, die mechanische Unkrautbekämpfung nutzen?")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","more than 10"))

g.NrFields_SF<- ggplot(FullSample)+
  geom_bar(aes(NrFields))+#,fill = as.factor(advisory)),position= "dodge")+
  # scale_y_continuous(labels=scales::percent)+
  ylab("")+
  theme_bw()+
  scale_fill_manual(values = c("darkblue","darkgreen","darkred"))+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "number of fields observed")+ 
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")+
  scale_x_discrete(labels = c("0", "1-5","6-10","11-15","more than 15"))

g.DistFields_SF<- ggplot(FullSample)+
  geom_bar(aes(FieldDist))+#,fill = as.factor(advisory)),position= "dodge")+
  #scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("darkblue","darkgreen","darkred"))+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to fields observed [km]", fill = "sugar company")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")+
  scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields obs."))

#make into ctegorcial
OwnFieldsSample$Mean_ownfield_dist_cat <- cut(OwnFieldsSample$Mean_ownfield_dist,
                                              breaks=c(0, 1, 2, 3, 4,5, 1000),
                                              labels=c('A', 'B', 'C', 'D','E','F'))

g.ownfields_cat<- ggplot(OwnFieldsSample)+
  geom_bar(aes(Mean_ownfield_dist_cat))+#,fill = as.factor(advisory)),position= "dodge")+
  #scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("darkblue","darkgreen","darkred"))+
  ylab("")+
  theme_bw()+
  # xlab("number of adopters known")
  # labs(title= "PfeifferLangen")+
  labs(x = "distance to own fields [km]", fill = "sugar company")+ 
  # legend("Sugar factories")+
  # y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")+
  scale_x_discrete(labels = c("0-1", ">1-2",">2-3",">3-4",">4-5","more than 5"))


p.own_fields2<-ggplot(OwnFieldsSample,aes(Mean_ownfield_dist)) +            
  geom_histogram(bins = 10)+
  theme_bw()+
  xlim(limits = c(0, 5.5))+
  labs(x = "distance to own fields [km]")+
  # scale_x_discrete(labels = c("0-5", "6-10","11-15","16-20","21-30","more than 30","no fields observed"))
  theme_bw()+
  theme(axis.text.x = element_text( hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")#+




ggarrange(g.NrPeers_SF, g.NrFields_SF, g.DistFields_SF,g.ownfields_cat,nrow = 2, ncol = 2)



#plot cross-table between adopters known and fields observed

table(FullSample$fields_b, FullSample$info_b)

d <- ggplot(FullSample, aes(info_b, fields_b))
d + geom_count(aes(size = after_stat(prop), group = 1)) +
  scale_size_area(max_size = 40)+
  labs(size = "Prop", x = "Knowing other farmers", y = "Observing fields")+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = c("No", "Yes"))+
  scale_y_discrete(labels = c("No", "Yes"))+
  theme(axis.text.x = element_text( hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "none")


d + geom_jitter(aes(colour= q1_adopt), position = position_jitter (0.3))+
  labs(x = "Knowing other farmers", y = "Observing fields")+
  theme_bw(base_size = 12)+
  scale_colour_manual(values = c("grey10", "grey60"),name = "Adoption", labels = c("No", "Yes"))+
  scale_x_discrete(labels = c("No", "Yes"))+
  scale_y_discrete(labels = c("No", "Yes"))+
  theme(axis.text.x = element_text( hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "bottom")
  


ggplot(FullSample, aes(q1_adopt))+
  geom_bar(position = "dodge", aes(fill = q1_adopt))+
  facet_grid(info_b~fields_b, switch = "both", labeller = as_labeller(c("0"='No',"1"='Yes')))+
  labs(x = "Knowing other farmers", y = "Observing fields")+
  theme_bw(base_size = 12)+
  scale_fill_manual(values = c("grey10", "grey60"),name = "Adoption", labels = c("No", "Yes"))+
  scale_x_discrete(labels = c("", ""))+
  #scale_y_discrete(labels = c("No", "Yes"))+
  theme(axis.text.x = element_text( hjust = 1),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")


#create df. to compare different adopter/observer groups
df.rq3 <- FullSample %>% dplyr::select(info_b, fields_b, q1_adopt,  minDist_demo, meanDist, Mean_ownfield_dist, q3_info, NrFields)#library(scales)
df.rq3$meanDist <- ifelse(df.rq3$meanDist == 50, NA,df.rq3$meanDist) #thise who didn't observe fields were st to 50 but now get "NA" again
library(scales)
df.rq3$minDist_demo_St <- rescale(df.rq3$minDist_demo,  to = c(0, 1))
df.rq3$meanDist_St <- rescale(df.rq3$meanDist,  to = c(0, 1))
df.rq3$Mean_ownfield_dist_St <- rescale(df.rq3$Mean_ownfield_dist,  to = c(0, 1))



#df.rq3$selection_distance <- as.numeric(as.character(df.rq3$selection_distance))
df.rq3$q1_adopt <- as.numeric(as.character(df.rq3$q1_adopt))
df.rq3$q3_info <- as.numeric(as.character(df.rq3$q3_info))
df.rq3$NrFields <- as.numeric(as.character(df.rq3$NrFields))
df.rq3$NrFields_St <- rescale(df.rq3$NrFields,  to = c(0, 1))
df.rq3$q3_info_St <- rescale(df.rq3$q3_info,  to = c(0, 1))

df.rq3 <- df.rq3 %>% 
  unite(group, c(info_b, fields_b), sep = "_", remove = FALSE)  



df.rq3 <- df.rq3 %>%
  group_by(group) %>%
  summarise_at(vars(q1_adopt, minDist_demo_St, meanDist_St, Mean_ownfield_dist_St,q3_info_St, NrFields_St ), list(mean = mean), na.rm = TRUE)

library(reshape2)
df.rq3.means.long<-melt(df.rq3,id.vars= "group")

df.rq3.means.long<-df.rq3.means.long %>% separate(group, c('info', 'fields'), sep= "_")

ggplot(df.rq3.means.long,aes(x=variable,y=value))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="group",
                      breaks=c(1, 2))+
  xlab("")+
  ylab("Mean")+
  facet_grid(info~fields, labeller = as_labeller(c("0"='No',"1"='Yes')))+
  theme_bw(base_size = 12)+
  #scale_fill_manual(values = c("grey10", "grey60"),name = "Adoption", labels = c("No", "Yes"))+
  scale_x_discrete(labels = c("Share of adoption", "Distance to demonstration farm", "Distance to adopters´ fields", "Distance between own fields", "Number of adopters known"
                              , "Number of fields observed"))+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")


#identify correlation between info and field
chisq.test(FullSample$info_b, FullSample$fields_b)
table(FullSample$info_b, FullSample$fields_b)

#n = 313
26/313
234/313
30/313
23/313


df.rq3.means.long %>% filter(variable == "q1_adopt_mean") %>% 
ggplot(aes(x=variable,y=value))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="group",
                      breaks=c(1, 2))+
  xlab("")+
  ylab("Mean share of adoption")+
  facet_grid(info~fields, labeller = as_labeller(c("0"='No',"1"='Yes')))+
  theme_bw(base_size = 12)+
  #scale_fill_manual(values = c("grey10", "grey60"),name = "Adoption", labels = c("No", "Yes"))+
  scale_x_discrete(labels = c(""))+
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = "right")




