# data cleaning for IP dashboard:
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)

schedule<-read_excel('new_data.xlsx',3,skip=2)%>%select(1:6)
functionality<-read_excel('new_data.xlsx',1,skip=3)
budget<-read_excel('new_data.xlsx',2)%>%select(1:26)
all_proj<-read_excel('new_data.xlsx',4,skip=1)
proj_risk<-read_excel('new_data.xlsx',6,skip=1)
proj_issue<-read_excel('new_data.xlsx',5,skip=1)

colnames(budget)[7]<-'expenditure_to_date'
colnames(schedule)<-c('IP','Project','Directorate','Major.Milestone','Approved_finish_date','Actual_date')
budget[,4:26]<-lapply(budget[,4:26],as.numeric)

clean_dt<-function(cols){
  cols[cols=='TBD']<-NA
  cols<-as.Date(as.numeric(cols),origin="1899-12-30")
  cols
}
schedule[,5:6]<-lapply(schedule[,5:6],clean_dt)

#proj_issue$`Target Date for Resolution`<-clean_dt(proj_issue$`Target Date for Resolution`)
schedule$Major.Milestone<-paste0(schedule$IP,':',schedule$Major.Milestone)
#functionality$IP[functionality$IP=='346a']<-346

schedule<-schedule%>%
     mutate(`Schedule.Health`=case_when(between(difftime(Actual_date,Approved_finish_date,units='days'),60,180) ~ 'Forecasted completion date within 3-6 months of baseline date',
                                       difftime(Actual_date,Approved_finish_date,units='days')>180 ~ 'Forecasted completion date is over 6 months of baseline date',
                                        TRUE ~ 'Forecasted completion date is within 3 months of baseline date'))
# schedule$`Schedule.Health`<-'Black'
# 
# schedule_diff<-schedule%>%
#   filter(Approved_finish_date!=Actual_date)%>%
#   mutate(`Schedule.Health`=case_when(difftime(Actual_date,Approved_finish_date,units='days')>60 ~ 'Yellow',
#                                      difftime(Actual_date,Approved_finish_date,units='days')>180 ~ 'Red',
#                                      TRUE ~ 'Green'))%>%
#   mutate(Approved_finish_date=Actual_date)
# 
# schedule<-rbind(schedule,schedule_diff)%>%arrange(Approved_finish_date)

all_proj<-all_proj%>%
  mutate(status=case_when(`Overall Project Health`=='Red'~ 'Elevated Risk',
                          `Overall Project Health`=='Yellow'~ 'Caution',
                          `Overall Project Health`=='Green'~ 'On Track'))%>%
  mutate(stage=case_when(
                         `Current Stage`==2~ 'Stage 2 \n Initiation',
                         `Current Stage`==3~ 'Stage 3 \n Planning',
                         `Current Stage`==4~ 'Stage 4 \n Execution',
                         `Current Stage`==5~ 'Stage 5 \n Close Out',
                         TRUE ~ 'Stage 1 \n Idea Generation'))

#clean budget table:
budget_yr<-budget%>%select(c(1,13:26))%>%
  gather(v,value,2:15)%>%
  mutate(v=gsub('Forecasted|Forcasted|\r|\n','',v,ignore.case = T,perl=T))%>%
  separate(v,c('var','Year'),sep='\\s(?=[:digit:])')%>%
  mutate(var=trimws(var))%>%
  mutate(var=gsub('Projet','Project',var))%>%
  arrange(IP)

proj_risk$Probability[proj_risk$Probability=='red']<-'Red'
capital<-read.csv('IP_captalization.csv',stringsAsFactors = F)

#write to new xlsx file:

l<-list(functionality=functionality,budget=budget,schedule=schedule,all_proj=all_proj,proj_risk=proj_risk,
        proj_issue=proj_issue,budget_yr=budget_yr,capital=capital)
write.xlsx(l,file='data.xlsx')

#save a copy to Data folder for history keeping:
today<-Sys.Date()

#create folder './Data' if not exist:
if(!dir.exists('./Data')) dir.create('./Data')
file_name<-paste0('./Data/data_',today,'.xlsx')

#save a copy for tracking purposes:
write.xlsx(l,file_name)
