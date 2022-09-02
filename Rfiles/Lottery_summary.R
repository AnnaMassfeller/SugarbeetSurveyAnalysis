#get number of participants who want to be in lottery and get summary of results

df.participants<- read_csv("Data_raw/emails.csv")#csv

#clean those out, who want neither = gave no email address
df.participants <- df.participants[!is.na(df.participants$email),]

#divide into lotery and summary people

df.lottery <- df.participants[df.participants$q10_lottery == 1,]

df.summary <- df.participants[df.participants$q9_results == 1,]


#all in all 10 vouchers, 3 for agri experts, 7 for our sample

#draw 7 particpants randomly

df.lottery_winners<-df.lottery %>% dplyr::slice_sample(n=1)

#out as excel
write_xlsx(df.lottery_winners,"df.lottery_winners18.07.xlsx")
