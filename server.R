

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(input, output,session) {
  

  output$project_name<-renderUI({
    project_name<-paste0('IP',input$selectip)
    h1(project_name, 
       style = "font-family: 'Arial';margin-left:50px;
        font-weight: 500; line-height: 1.1; 
        color: #2E4053;")
  })
  
  output$project_name2<-renderUI({
    
    project_name<-paste0('Projects:',input$selectdir)
    h1(project_name, 
       style = "font-family: 'Arial';margin-left:50px;
        font-weight: 500; line-height: 1.1; 
        color: #2E4053;")
  })
  
  
  ip_selected<-reactive({
    
      ip<-input$selectip
      
      if(input$selectdir=='All'){
        ips<-all_proj$IP
      }else{
        ips<-all_proj$IP[all_proj$`Directorate Lead`==input$selectdir]
      }
    
    return(list(ip=ip,ips=ips))
  })
  
  
  output$function_plt<-renderPlotly({
    
    summary<-functionality%>%
      filter(IP %in% ip_selected()$ips)%>%
      dplyr::count(`Functionality Met? (Y/N)`)
    
    colnames(summary)<-c('status','count')
    
    status_color<-data.frame(status=c("YES", "NO"),
                             color=c( "#00b050","#C00000"))
    
    summary<-left_join(summary,status_color)
    
    plot_ly(summary,x=~status,y=~count,type='bar',
            marker=list(color=as.character(summary$color)))%>%
      layout(showlegend = F,
             xaxis=list(title=''),
             yaxis=list(title=''))%>%
      add_annotations(x=summary$`Functionality Met? (Y/N)`,
                      y=summary$count+3,
                      text=summary$count,
                      showarrow=F)
      
  })
  
  output$function_tb<-DT::renderDataTable({

    df<-functionality%>%
      filter(IP==ip_selected()$ip)%>%
      mutate(Status=ifelse(`Functionality Met? (Y/N)`=='YES','\u2713',''))

    DT::datatable(df[,c('Status','Requirement')],
                  options=list(pageLength=5,
                               scrollX=TRUE,
                               autoWidth=T,
                               columnDefs=list(list(width='700px',targets=2))))
  })


  # output$projection_plt<-renderPlotly({
  #   
  #   test<-budget_rs()%>%filter(!is.na(burn_rate))
  #   
  #   shiny::validate(
  #     need(nrow(test)>0,'Burn rate estimation not available')
  #   )
  #   
  #   remain<-function(df){
  #     value<-seq(df$`Total Expenditure`[1],by=-1*df$burn_rate[1],length=(df$month_remaining[1]+1))
  #     date<-seq(as.Date('2018-12-01'),by='month',length=length(value))
  #     
  #     rs<-data.frame(date=date,value=value)
  #     return(rs)
  #   }
  #   
  #   ds<-list()
  #   for (i in 1:nrow(test)){
  #     ds[[i]]<-remain(test[i,])
  #   }
  #   
  #   ds<-ds%>%reduce(full_join,by='date')
  #   
  #   if(length(ds)>2){
  #   value<-rowSums(ds[,-1],na.rm=T)
  #   }else{
  #   value<-ds[[2]]
  #   }
  #   
  #   plot_ly(x=~ds$date,y=~value,type='bar')%>%
  #     layout(title='Budget projection',
  #            yaxis=list(title='Remaining budget'),
  #            xaxis=list(title=''))
  # })
  
  output$budget_all<-renderPlot({
    ds<-budget%>%filter(IP==input$selectip)%>%
                 summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                           `Expenditure to Date`=sum(expenditure_to_date,na.rm=T),
                           `Remaining Budget Projected`=sum(`Variance between remaining approved budget projected spending`,na.rm=T))%>%
                 gather(cat)
    
    budget_plot2(ds)
  })
  
  output$budget_all2<-renderPlot({
    ds<-budget%>%filter(IP %in% ip_selected()$ips)%>%
                 summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                `Expenditure to Date`=sum(expenditure_to_date,na.rm=T),
                `Remaining Budget Projected`=sum(`Variance between remaining approved budget projected spending`,na.rm=T))%>%
                 gather(cat)
    
    budget_plot2(ds)
  })
  
  
  output$budget_plt<-renderPlotly({
    ds<-budget_yr%>%filter(IP==input$selectip)
    budget_plot(ds)
  })
  
  output$budget_plt2<-renderPlotly({
    
    ds<-budget_yr%>%filter(IP %in% ip_selected()$ips)%>%
                    group_by(var,Year)%>%
                    summarise(value=sum(value,na.rm=T))
    budget_plot(ds)
  })
  
  
  output$budget_tbl<-DT::renderDataTable({
    ds<-budget_yr%>%filter(IP==input$selectip)%>%
                    mutate(value=dollar(value))%>%
                    spread(var,value)
    
    DT::datatable(ds)
  })
  
  
  output$budget_tbl2<-DT::renderDataTable({
    ds<-budget_yr%>%filter(IP %in% ip_selected()$ips)%>%
      group_by(var,Year)%>%
      summarise(value=sum(value,na.rm=T))%>%
      mutate(value=dollar(value))%>%
      spread(var,value)
    
    DT::datatable(ds)
  })
  
  
  schedule_overview<-reactive({
    schedule%>%filter(IP %in% ip_selected()$ips)%>%
               group_by(Major.Milestone)%>%
               filter(Approved_finish_date==Actual_date)%>%
               ungroup()
  })
  
  output$schedule_plt<-renderPlotly({
    df<-schedule%>%filter(IP==ip_selected()$ip)
    
    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on project schedule')
    ))
    
   timeplot(df)
    
  })
  
  output$schedule_plt2<-renderPlotly({
    df<-schedule_overview()
    
    if(input$selectdir=='All'){
      df<-df%>%
        filter(grepl('Go live',Major.Milestone,ignore.case=T))
    }
    
    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on project schedule')
    ))
    
    timeplot(df)
    
    
  })
  
  
  
  output$schedule_tb<-DT::renderDataTable({
    df<-schedule%>%filter(IP==ip_selected()$ip)%>%
        #filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))%>%
        select(Milestone=Major.Milestone,Date=Approved_finish_date)
    
    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })
  
  
  output$schedule_tb2<-DT::renderDataTable({
    df<-schedule_overview()%>%
      filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))%>%
      select(Milestone=Major.Milestone,`Actual/Forecasted Finish Date`=Approved_finish_date)
    
    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })
  
  
  
  output$overall2<-renderPlot({
    
    cols<-c('On Track'='#00B050','Caution'='#FFC000','Elevated Risk'='#C00000')
    
    df<-all_proj%>%
      filter(IP %in% ip_selected()$ips)%>%
      group_by(status)%>%
      summarise(IP=paste(paste0('IP',IP),collapse='\n'),count=n())
    
    p<-ggplot(df,aes(x=status,y=count,fill=status))+geom_col()+
      scale_fill_manual(values=cols)+
      scale_y_continuous(breaks=c(0,1,2,3,4))+
      geom_text(aes(y=count,label=IP),vjust=1.5)+
      geom_text(aes(label=as.character(count)),vjust=-0.5)+
      guides(fill=F)+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.text.x =element_text(size=12),
            axis.title.y =element_blank(),
            legend.title=element_blank()
      )
    
    p
    
  })
  
output$overall_stage2<-renderPlot({

    cols<-c('On Track'='#00B050','Caution'='#FFC000','Elevated Risk'='#C00000')
    
    df<-all_proj%>%
      filter(IP %in% ip_selected()$ips)%>%
      group_by(stage,status)%>%
      summarise(IP=paste(paste0('IP',IP),collapse='\n'),count=n())
    
    p<-ggplot(df,aes(x=stage,y=count,fill=status))+geom_col(position='dodge')+
      scale_fill_manual(values=cols)+
      scale_y_continuous(breaks=c(0,1,2,3,4))+
      geom_text(aes(y=count-0.5,label=IP),position=position_dodge(width=0.9))+
      geom_text(aes(label=as.character(count)),position=position_dodge(width=0.9),vjust=-0.5)+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.text.x =element_text(size=12),
            axis.title.y =element_blank(),
            legend.title=element_blank()
      )
    
    p
  })

 output$overall<-renderValueBox({
   status<-df<-all_proj%>%
     filter(IP == ip_selected()$ip)%>%
     select(status,`Overall Project Health`)
     
   
   valueBox(tags$p(status$status, style = "font-size: 80%;"),
            subtitle='Overall Project Health',color=tolower(status$`Overall Project Health`),
            width=3)
   
 })
 
 output$overall_stage<-renderValueBox({
   status<-all_proj%>%
     filter(IP ==input$selectip)%>%
     pull(stage)
   
   valueBox(tags$p(status, style = "font-size: 80%;"),
            subtitle='Project Stage',color='blue',width=3)
 })

  
 
 output$downloadData<-downloadHandler(
      
      filename<-function(){
        paste('ADM Project Portfolio Dashboard','xlsx',sep='.')
      },
      
      content<-function(file){
         file.copy('data.xlsx',file)
        
      }
    )
    
    
    
    output$downloadreport<-downloadHandler(
      filename='report.html',
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('report.Rmd', html_document())
        file.rename(out, file)
        
      }
     
        )
     
  
  # observeEvent(event_data("plotly_click", source = "select"),{
  #   df<-functions()
  #   event.data<-plotly::event_data("plotly_click", source = "select")
  #   
  #   if(is.null(event.data) == T) return(NULL)
  #   tab_title <- paste0('selected',as.character(event.data$x[1]))
  #   
  #   if(tab_title %in% tab_list == FALSE){
  #   
  #     functions_subset<-df%>%filter(Scope.Health ==event.data$x)
  #     
  #     appendTab(inputId = "tabs",
  #               tabPanel(
  #                 tab_title,
  #                 DT::renderDataTable(functions_subset,
  #                                     options=list(scrollX=T))
  #               ))
  #     
  #     tab_list <<- c(tab_list, tab_title)
  #   }
  #   
  #   updateTabsetPanel(session, "tabs", selected = tab_title)
  #   
  # })
  

})
