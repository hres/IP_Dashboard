library(shiny)
library(shinyBS)
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
library(DT)


source('functions.R')
schedule<-read_excel('data.xlsx',3)
functionality<-read_excel('data.xlsx',1)
budget<-read_excel('data.xlsx',2)
all_proj<-read_excel('data.xlsx',4)
budget_yr<-read_excel('data.xlsx',7)
proj_risk<-read_excel('data.xlsx',5)
proj_issue<-read_excel('data.xlsx',6)

schedule$Actual_date<-as.Date(as.character(schedule$Actual_date))
schedule$Approved_finish_date<-as.Date(as.character(schedule$Approved_finish_date))
proj_issue$`Target Date for Resolution`<-as.Date(as.character(proj_issue$`Target Date for Resolution`))

ip<-c(budget$IP[!is.na(budget$IP)])
directorate<-c('All',all_proj$`Directorate Lead`)

all_proj$`Overall Project Health`[is.na(all_proj$`Overall Project Health`)]<-'Blue'
all_proj$status[is.na(all_proj$status)]<-'Not Available'


#date of which data is updated:
dat<-substring(file.info('data.xlsx')$mtime,1,11)