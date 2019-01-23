library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)
library(stringi)
library(plotly)
library(readxl)
library(scales)


source('functions.R')
schedule<-read_excel('data.xlsx',3,skip=2)%>%select(1:6)
functionality<-read_excel('data.xlsx',1,skip=3)
budget<-read_excel('data.xlsx',2,skip=4)%>%select(1:26)
all_proj<-read_excel('data.xlsx',4,skip=1)

colnames(budget)[7]<-'expenditure_to_date'
colnames(schedule)<-c('IP','Project','Directorate','Major.Milestone','Approved_finish_date','Actual_date')
budget[,4:26]<-lapply(budget[,4:26],as.numeric)

ip<-c(budget$IP[!is.na(budget$IP)])

functionality$IP[functionality$IP=='346a']<-'346'
#functionality$Scope.Health<-gsub('Test|-|\\s+','',functionality$Scope.Health)
schedule$Approved_finish_date<-as.Date(as.character(schedule$Approved_finish_date))
schedule$Actual_date<-as.Date(as.character(schedule$Actual_date))
schedule$Major.Milestone<-paste0(schedule$IP,':',schedule$Major.Milestone)
directorate<-c('All',all_proj$`Directorate Lead`)


#define Schedule.Health by criteria:
schedule$`Schedule.Health`<-'Black'

schedule_diff<-schedule%>%
               filter(Approved_finish_date!=Actual_date)%>%
               mutate(`Schedule.Health`=case_when(difftime(Actual_date,Approved_finish_date,units='days')>60 ~ 'Yellow',
                                             difftime(Actual_date,Approved_finish_date,units='days')>180 ~ 'Red',
                                             TRUE ~ 'Green'))%>%
               mutate(Approved_finish_date=Actual_date)

schedule<-rbind(schedule,schedule_diff)%>%arrange(Approved_finish_date)

all_proj<-all_proj%>%
  mutate(status=case_when(`Overall Project Health`=='Red'~ 'Elevated Risk',
                          `Overall Project Health`=='Yellow'~ 'Caution',
                          `Overall Project Health`=='Green'~ 'On Track'))%>%
  mutate(stage=case_when(`Current Stage`==1~ 'Stage 1 - Concept',
                         `Current Stage`==2~ 'Stage 2 - Initiation',
                         `Current Stage`==3~ 'Stage 3 - Planning',
                         `Current Stage`==4~ 'Stage 4 - Execution',
                         `Current Stage`==5~ 'Stage 5 - Close Out'))

#clean budget table:
budget_yr<-budget%>%select(c(1,13:26))%>%
           gather(v,value,2:15)%>%
           mutate(v=gsub('Forecasted|Forcasted|\r|\n','',v,ignore.case = T))%>%
           separate(v,c('var','Year'),sep='\\s(?=[:digit:])',perl=T)%>%
           mutate(var=trimws(var))%>%
           mutate(var=gsub('Projet','Project',var))%>%
           arrange(IP)

