library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(shinyBS)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)
library(stringi)
library(plotly)
library(readxl)
library(scales)
library(magrittr)
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
all_proj$status[is.na(all_proj$status)]<-'Not yet started'
all_proj$`Internal or External`[all_proj$`Internal or External`=='Both']<-'External'

#date of which data is updated:
dat<-substring(file.info('data.xlsx')$mtime,1,11)

#add capitalization data to budget_yr:
capital<-read.csv('IP_captalization.csv',stringsAsFactors = F)
capital$var<-'Project Authority'
budget_yr%<>%left_join(capital)
budget_yr$capital<-ifelse(is.na(budget_yr$capital),0,budget_yr$capital)
budget_yr$non_capital<-budget_yr$value-budget_yr$capital
budget_yr$year<-as.numeric(substr(budget_yr$Year,1,4))
#budget_yr<-budget_yr%>%gather(key=capitalization,value=value2,capital,non_capital)