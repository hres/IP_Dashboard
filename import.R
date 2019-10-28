library(httr)
library(jsonlite)
library(openxlsx)


#
url = 'https://d8-dev.hres.ca/ipdashboard/api/v1'

username = 'apibot-1'
password = 'APIb0t!!!'


r<-GET(url,authenticate(username,password))
r<-content(r,'text',encoding = 'UTF-8')%>%fromJSON(simplifyDataFrame = TRUE)

#keep empty element after unlist:
unlist2<-function(ls){
  ls<-sapply(ls,function(x){if(length(x)==0){x<-NA
  }else{x<-x}})
  unlist(ls,use.names = F)
}

# overall project health
all_proj <- data.frame(`IP` = r$title%>%unlist2(),
                       `Project Name` = r$field_project_name %>% unlist2(),
                       `Internal or External` = r$field_internal_or_external %>% unlist2(),
                       `Current Stage` = r$field_current_stage %>% unlist2(),
                       `Directorate` = r$field_directorate %>% unlist2(),
                       `Project Objectives` = r$field_project_objectives %>% unlist2(),
                       #ask about how to format project status
                       `Current Project Status` = gsub("<.*?>", "", 
                                                       lapply(r$field_current_project_status, 
                                                              function(l) l[[1]]) %>% 
                                                         unlist2()),
                       `Overall Project Health` = r$field_overall_project_health %>% unlist2(),
                       `status` = r$status %>% unlist2(),
                       `stage` = r$field_current_stage %>% unlist2())

all_proj <- all_proj[order(all_proj[,1]),]
names(all_proj) <- c("IP", "Project Name", "Internal or External", "Current Stage", 
                     "Directorate Lead", "Project Objectives", "Current Project Status",
                     "Overall Project Health", "status", "stage")
#all_proj[is.na(all_proj)] <- ""


#budget table:
budget<-data.frame(IP=r$title%>%unlist2(),
                   `Project Name` = r$field_project_name %>% unlist2(),
                   `Directorate` = r$field_directorate %>% unlist2(),
                   `Approved Budget`=r$field_approved_budget%>%unlist2(),
                   `Forecasted Total Expenditures`=r$field_forecasted_total_expenditu%>%unlist2(),
                   `Variance / Remaining budget`=r$field_variance_remaining_budget%>%unlist2(),
                   `expenditure_to_date`=r$field_expenditures_to_date%>%unlist2(),
                   `Variance between remaining approved budget projected spending`=r$field_variance_between_remaining%>%unlist2(),
                   `Project Authority 2016-17`=r$field_project_authority_16_17%>%unlist2(),
                   `Project Expenditures 2016-17`=r$field_project_expenditures_16_17%>%unlist2(),
                   `Project Authority 2017-18`=r$field_project_authority_17_18%>%unlist2(),
                   `Project Expenditures 2017-18`=r$field_project_expenditures_17_18%>%unlist2(),
                   `Project Authority 2018-19`=r$field_project_authority_18_19%>%unlist2(),
                   `Project Expenditures 2018-19`=r$field_project_expenditures_18_19%>%unlist2(),
                   `Project Authority 2019-20`=r$field_project_authority_19_20%>%unlist2(),
                   `Project Expenditures 2019-20`=r$field_forecast_expenditure_19_20%>%unlist2(),
                   `Project Authority 2020-21`=r$field_project_authority_20_21%>%unlist2(),
                   `Project Expenditures 2020-21`=r$field_forecast_expenditure_20_21%>%unlist2(),
                   `Project Authority 2021-22`=r$field_project_authority_21_22%>%unlist2(),
                   `Project Expenditures 2021-22`=r$field_forecast_expenditure_21_22%>%unlist2(),
                   `Project Authority 2022-23`=r$field_project_authority_22_23%>%unlist2(),
                   `Project Expenditures 2022-23`=r$field_forecast_expenditure_22_23%>%unlist2())

names(budget) <- c("IP", "Project Name", "Directorate", "Approved Budget", 
                   "Forecasted\r\nTotal Expenditures", "Variance / Remaining budge",
                   "expenditure_to_date", "Variance between remaining approved budget projected spending",
                   "Project Authority 2016-17", "Project Expenditures 2016-17",
                   "Project Authority 2017-18", "Project \r\nExpenditures 2017-18",
                   "Project Authority 2018-19", "Project Expenditures 2018-19",
                   "Project Authority 2019-20", "Project \r\nForecasted\r\nExpenditures 2019-20",
                   "Project Authority 2020-21", "Project \r\nForecasted\r\nExpenditures 2020-21",
                   "Project Authority  2021-22", "Project \r\nForecasted\r\nExpenditures 2021-22",
                   "Project Authority  2022-23", "Project \r\nForecasted\r\nExpenditures 2022-23")

budget[is.na(budget)] <- ""


#project issues
proj_issue<-data.frame(`IP` = r$title%>%unlist2(),
                       `Project Name` = r$field_project_name %>% unlist2())

issues_collection <- r$field_project_issues_collection

proj_issue_rows <- nrow(proj_issue)

for(i in 1:proj_issue_rows){
  issue <- issues_collection[[i]]
  if(nrow(issue) > 0){
    proj_issue[i, "Issue"] <- issue$field_issue_type[1] %>% unlist2()
    proj_issue[i, "Issue Description"] <- issue$field_issue_description[1] %>% unlist2()
    proj_issue[i, "Impact"] <- issue$field_impact_issue[1] %>% unlist2()
    proj_issue[i, "Target Date for Resolution"] <- issue$field_target_date_for_resolution[1] %>% unlist2()
    proj_issue[i, "Resolution Strategy"] <- issue$field_resolution_strategy[1] %>% unlist2()
    proj_issue[i, "Resolution Lead"] <- issue$field_resolution_lead[1] %>% unlist2()
  }
  if(nrow(issue) > 1){
    for(j in 2:nrow(issue)){
      rowcount <- nrow(proj_issue)
      proj_issue[rowcount + 1, ] <- NA
      
      proj_issue$IP[rowcount + 1] <- proj_issue$IP[i] 
      proj_issue$Project.Name[rowcount + 1] <- proj_issue$Project.Name[i]
      proj_issue$Issue[rowcount + 1] <- issue$field_issue_type[j] %>% unlist2()
      proj_issue$`Issue Description`[rowcount + 1] <- issue$field_issue_description[j] %>% unlist2()
      proj_issue$Impact[rowcount + 1] <- issue$field_impact_issue[j] %>% unlist2()
      proj_issue$`Target Date for Resolution`[rowcount + 1] <- issue$field_target_date_for_resolution[j] %>% unlist2()
      proj_issue$`Resolution Strategy`[rowcount + 1] <- issue$field_resolution_strategy[j] %>% unlist2()
      proj_issue$`Resolution Lead`[rowcount + 1] <- issue$field_resolution_lead[j] %>% unlist2()
    }
  }
}

proj_issue <- proj_issue[order(proj_issue[,1]),]

names(proj_issue) <- c("IP", "Project Name", "Issue", "Issue Description", "Impact",
                       "Target Date for Resolution", "Resolution Strategy", 
                       "Resolution Lead")

proj_issue[is.na(proj_issue)] <- ""

#project risks
proj_risk <- data.frame(`IP` = r$title%>%unlist2(),
                        `Project Name` = r$field_project_name %>% unlist2())

risks <- r$field_project_risks

proj_risk_rows <- nrow(proj_risk)

for(i in 1:proj_risk_rows){
  risk <- risks[[i]]
  if(nrow(risk) > 0){
    proj_risk[i, "Risk"] <- risk$field_risk[[1]] %>% unlist2()
    proj_risk[i, "Risk description"] <- risk$field_risk_description[[1]] %>% unlist2()
    proj_risk[i, "Probability"] <- risk$field_probability[[1]] %>% unlist2()
    proj_risk[i, "Impact"] <- risk$field_impact[[1]] %>% unlist2()
    proj_risk[i, "Mitigation Strategy"] <- gsub("<.*?>", "",
                                                risk$field_mi[[1]][[1]] %>% unlist2())
    proj_risk[i, "Risk Management Lead"] <- risk$field_risk_manage[[1]] %>% unlist2()
  }
  if(nrow(risk) >1){
    for(j in 2:nrow(risk)){
      rowcount <- nrow(proj_risk)
      proj_risk[rowcount + 1, ] <- NA
      
      proj_risk$IP[rowcount + 1] <- proj_risk$IP[i]
      proj_risk$Project.Name[rowcount + 1] <- proj_risk$Project.Name[i]
      proj_risk$Risk[rowcount + 1] <- risk$field_risk[[j]] %>% unlist2()
      proj_risk$`Risk description`[rowcount + 1] <- risk$field_risk_description[[j]] %>% unlist2()
      proj_risk$Probability[rowcount + 1] <- risk$field_probability[[j]] %>% unlist2()
      proj_risk$Impact[rowcount + 1] <- risk$field_impact[[j]] %>% unlist2()
      proj_risk$`Mitigation Strategy`[rowcount + 1] <- gsub("<.*?>", "",
                                                            risk$field_mi[[j]][[1]] %>% unlist2())
      proj_risk$`Risk Management Lead`[rowcount + 1] <- risk$field_risk_manage[[j]] %>% unlist2()
    }
  }
}

proj_risk <- proj_risk[order(proj_risk[,1]),]

names(proj_risk) <- c("IP", "Project Name", "Risk", "Risk description", "Probability",
                      "Impact", "Mitigation Strategy", "Risk Management Lead")

proj_risk[is.na(proj_risk)] <- ""

#functionality

functionality <- data.frame(`IP` = r$title%>%unlist2(),
                            `Project Name` = r$field_project_name %>% unlist2(),
                            `Directorate` = r$field_directorate %>% unlist2())

requirement_func <- r$field_requirement_functionality

func_rows <- nrow(functionality)

for(i in 1:func_rows){
  func <- requirement_func[[i]]
  if(nrow(func) > 0){
    functionality[i, "Requirement"] <- func$field_requirement[[1]] %>% unlist2()
    functionality[i, "Functionality Met? (Y/N)"] <- func$field_functionality_met_[[1]] %>% unlist(2)
  }
  if(nrow(func) > 1){
    for(j in 2:nrow(func)){
      rowcount <- nrow(functionality)
      functionality[rowcount + 1, ] <- NA
      
      functionality$IP[rowcount + 1] <- functionality$IP[i]
      functionality$Project.Name[rowcount + 1] <- functionality$Project.Name[i]
      functionality$Directorate[rowcount + 1] <- functionality$Directorate[i]
      functionality$Requirement[rowcount + 1] <- func$field_requirement[[j]] %>% unlist2()
      functionality$`Functionality Met? (Y/N)`[rowcount + 1] <- func$field_functionality_met_[[j]] %>% unlist2()
    }
  }
}

names(functionality) <- c("IP", "Project Name", "Directorate", "Requirement",
                          "Functionality Met? (Y/N)")

functionality <- functionality[order(functionality[,1]),]

functionality[is.na(functionality)] <- ""

functionality <- functionality %>% mutate(`Functionality Met? (Y/N)`=case_when(
  `Functionality Met? (Y/N)`=="Yes"~ 'YES',
  `Functionality Met? (Y/N)`=="No"~ 'NO',
  `Functionality Met? (Y/N)`==""~ ''))

#schedule
schedule <- data.frame(`IP` = r$title%>%unlist2(),
                       `Project Name` = r$field_project_name %>% unlist2(),
                       `Directorate` = r$field_directorate %>% unlist2())

schedule_details <- r$field_schedule_details

sche_rows <- nrow(schedule)

for(i in 1:sche_rows){
  sche <- schedule_details[[i]]
  if(nrow(sche) > 0){
    schedule[i, "Major Milestone"] <- sche$field_major_milestone[[1]] %>% unlist2()
    schedule[i, "Approved_finish_date"] <- sche$field_approved_baseline_finish_d[[1]] %>% unlist2()
    schedule[i, "Actual_date"] <- sche$field_actual_forecasted_finish[[1]] %>% unlist2()
  }
  if(nrow(sche) > 1){
    for(j in 2:nrow(sche)){
      rowcount <- nrow(schedule)
      schedule[rowcount + 1, ] <- NA
      
      schedule$IP[rowcount + 1] <- schedule$IP[i]
      schedule$Project.Name[rowcount + 1] <- schedule$Project.Name[i]
      schedule$Directorate[rowcount + 1] <- schedule$Directorate[i]
      schedule$`Major Milestone`[rowcount + 1] <- sche$field_major_milestone[[j]] %>% unlist2()
      schedule$`Approved_finish_date`[rowcount + 1] <- paste0("", sche$field_approved_baseline_finish_d[[j]] %>% unlist2())
      schedule$`Actual_date`[rowcount + 1] <- paste0("", sche$field_actual_forecasted_finish[[j]] %>% unlist2())
    }
  }
}

schedule <- schedule[order(schedule[,1]),]

names(schedule) <- c("IP", "Project", "Directorate", "Major.Milestone", 
                     "Approved_finish_date", "Actual_date")

schedule[is.na(schedule)] <- ""




#copied from data cleaning file
colnames(budget)[7]<-'expenditure_to_date'
colnames(schedule)<-c('IP','Project','Directorate','Major.Milestone','Approved_finish_date','Actual_date')
#change dollar values to numerical
budget[,4:22]<-lapply(budget[,4:22], as.character)
budget[,4:22]<-lapply(budget[,4:22], as.numeric)
#enter 0 for missing values
budget[is.na(budget)] <- 0

clean_dt<-function(cols){
  cols[cols=='TBD']<-NA
  cols<-as.Date(as.numeric(cols),origin="1899-12-30")
  cols
}
#schedule[,5:6]<-lapply(schedule[,5:6],clean_dt)

#proj_issue$`Target Date for Resolution`<-clean_dt(proj_issue$`Target Date for Resolution`)
schedule$Major.Milestone<-paste0(schedule$IP,':',schedule$Major.Milestone)
#functionality$IP[functionality$IP=='346a']<-346


#add status column to schedule
schedule<-schedule%>%
  mutate(`Schedule.Health`=case_when(between(difftime(as.Date(Actual_date),as.Date(Approved_finish_date),units='days'),60,180) ~ 'Forecasted completion date within 3-6 months of baseline date',
                                     difftime(as.Date(Actual_date),as.Date(Approved_finish_date,units='days'))>180 ~ 'Forecasted completion date is over 6 months of baseline date',
                                     TRUE ~ 'Forecasted completion date is within 3 months of baseline date'))

#add status and Current Stage columns to all_proj
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
budget_yr<-budget%>%select(c(1,9:22))%>%
  gather(v,value,2:15)%>%
  mutate(v=gsub('Forecasted|Forcasted|\r|\n','',v,ignore.case = T))%>%
  separate(v,c('var','Year'),sep='\\s(?=[:digit:])',perl=T)%>%
  mutate(var=trimws(var))%>%
  mutate(var=gsub('Projet','Project',var))%>%
  arrange(IP)

#change IP in budget_yr to numeric
budget_yr[, 1] <- as.numeric(as.character(budget_yr[, 1] %>% unlist2()))

proj_risk$Probability[proj_risk$Probability=='red']<-'Red'

#read in IP_capitalization file
capital<-read.csv('IP_captalization.csv',stringsAsFactors = F)
capital[, 1] <- as.numeric(capital[, 1] %>% unlist2())


#write to new xlsx file:
l<-list(functionality=functionality,budget=budget,schedule=schedule,all_proj=all_proj,proj_risk=proj_risk,
        proj_issue=proj_issue,budget_yr=budget_yr,capital=capital)
write.xlsx(l,file='./data.xlsx')




#write.xlsx(functionality, file = "./data.xlsx", sheetName = "functionality", row.names = FALSE)
#write.xlsx(budget, file = "./data.xlsx", sheetName = "budget", append = TRUE,  row.names = FALSE)
#write.xlsx(schedule, file = "./data.xlsx", sheetName = "schedule", append = TRUE,  row.names = FALSE)
#write.xlsx(all_proj, file = "./data.xlsx", sheetName = "all_proj", append = TRUE,  row.names = FALSE)
#write.xlsx(proj_risk, file = "./data.xlsx", sheetName = "proj_risk", append = TRUE,  row.names = FALSE)
#write.xlsx(proj_issue, file = "./data.xlsx", sheetName = "proj_issue", append = TRUE,  row.names = FALSE)
#write.xlsx(budget_yr, file = "./data.xlsx", sheetName = "budget_yr", append = TRUE,  row.names = FALSE)
#write.xlsx(capital, file = "./data.xlsx", sheetName = "capital", append = TRUE,  row.names = FALSE)
