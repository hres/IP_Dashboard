

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(input, output,session) {
  
  plot_height<-reactive({
    
    n<-all_proj%>%filter(IP %in% ip_selected()$ips)%>%distinct(`Internal or External`)
    
    if(input$internal=='Yes' & nrow(n)==2){
      height<-600
    }else{
      height<-400
    }
    
    height
  })
  
  
  plot_height2<-reactive({
    
    n<-all_proj%>%filter(IP %in% ip_selected()$ips)%>%distinct(`Internal or External`)
    
    if(input$internal=='Yes' & nrow(n)==2){
      height<-800
    }else{
      height<-450
    }
    
    height
  })

  
  
  observeEvent(input$contact,{
    
    showModal(modalDialog(
      title='Contact Us',
      HTML(paste(
        "If you have any questions regarding data source or data quality, please contact:",br(),
        "Sarah-Emily Carle",br(),
        "MAnagement, Program Support",br(),
        "Business Informatics Division",br(),
        "RMOD, HFPB",br(),
        "sarah-emily.carle@canada.ca",br(),br(),
        "If you have technical questions regarding the application, please contact:",br(),
        "Nanqing (Nancy) Zhu",br(),
        "Data Scientist",br(),
        "Business Informatics Division",br(),
        "RMOD, HFPB",br(),
        "nanqing.zhu@canada.ca"
      )),
      easyClose=T
    ))
  })
  
  
  output$ip_tbl<-renderTable(
    all_proj[,1:2]%>%mutate(IP=as.character(IP))
  )

  output$project_name<-renderUI({
    name<-all_proj%>%filter(IP== input$selectip)%>%pull(`Project Name`)
    
    project_name<-paste0('IP',input$selectip,' ',name)
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
  
  
  output$function_plt<-renderPlot({
    
    if(input$internal=='No'){
    summary<-functionality%>%
      filter(IP %in% ip_selected()$ips)%>%
      dplyr::count(`Functionality Met? (Y/N)`)%>%
      rename(status=`Functionality Met? (Y/N)`)%>%
      mutate(color=ifelse(status=='YES',"#00b050","#C00000"))
    
    function_plot(summary)
    
   
    }else{
      
      summary<-functionality%>%
        filter(IP %in% ip_selected()$ips)%>%
        left_join(all_proj[,c('Project Name','Internal or External')])%>%
        count(`Functionality Met? (Y/N)`,`Internal or External`)%>%
        rename(status=`Functionality Met? (Y/N)`)%>%
        mutate(color=ifelse(status=='YES',"#00b050","#C00000"))
      
      function_plot(summary)+
        facet_grid(`Internal or External`~.)+
        theme(panel.spacing.y = unit(2,"lines"))
      
    }
  })
  
  output$function_tb<-DT::renderDataTable({

    df<-functionality%>%
      filter(IP==ip_selected()$ip)%>%
      mutate(Status=ifelse(`Functionality Met? (Y/N)`=='YES','\u2713',''))

    DT::datatable(df[,c('Status','Requirement')],
                  options=list(scroller = TRUE,
                               scrollX = TRUE,
                               scrollY = "275px",
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
    
    if(input$internal=='No'){
    ds<-budget%>%filter(IP %in% ip_selected()$ips)%>%
                 summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                `Expenditure to Date`=sum(expenditure_to_date,na.rm=T),
                `Remaining Budget Projected`=sum(`Variance between remaining approved budget projected spending`,na.rm=T))%>%
                 gather(cat)
    
    budget_plot2(ds)
    
    }else{
      
      ds<-budget%>%
        filter(IP %in% ip_selected()$ips)%>%
        left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
        group_by(internal_external)%>%
        summarise(`Approved Budget`=sum(`Approved Budget`,na.rm=T),
                  `Expenditure to Date`=sum(expenditure_to_date,na.rm=T),
                  `Remaining Budget Projected`=sum(`Variance between remaining approved budget projected spending`,na.rm=T))%>%
        gather(cat,value,-internal_external)
      
      budget_plot2(ds)+
        facet_grid(`internal_external`~.)+
        theme(panel.spacing.y = unit(2,"lines"))
      
      
    }
  })
  
  
  output$budget_plt<-renderPlotly({
    ds<-budget_yr%>%filter(IP==input$selectip)
    p<-budget_plot(ds)
    ggplotly(p,tooltip = "text")%>%layout(margin=list(b=50),xaxis=list(tickangle=-45))
  })
  
  
  output$budget_plt2<-renderPlotly({
    
    plot_hlt<-plot_height()
    
    if(input$internal=='No'){
    
    ds<-budget_yr%>%filter(IP %in% ip_selected()$ips)%>%
                    group_by(var,Year)%>%
                    summarise(value=sum(value,na.rm=T))
   
     p<-budget_plot(ds)
     
     ggplotly(p,tooltip = "text",height=plot_hlt)%>%layout(margin=list(b=50),xaxis=list(tickangle=-45))
    
    }else{
      
      ds<-budget_yr%>%
        filter(IP %in% ip_selected()$ips)%>%
        left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
        group_by(var,Year,internal_external)%>%
        summarise(value=sum(value,na.rm=T))
      
      
      p<-budget_plot(ds)+
         facet_grid(`internal_external`~.,shrink=FALSE,space='free_y')+
         theme(panel.spacing.y = unit(2,"lines"))
      
      p
      ggplotly(p,tooltip = "text")%>%layout(margin=list(b=50),xaxis=list(tickangle=-45),autosize=FALSE)
      
    }
  })
  
  
  output$budget_tbl<-DT::renderDataTable({
  
    ds<-budget_yr%>%filter(IP==input$selectip)%>%
                    mutate(value=dollar(value))%>%
                    spread(var,value)
    
    DT::datatable(ds)
  })
  
  
  output$budget_tbl2<-DT::renderDataTable({
    
    if(input$internal=='No'){
    ds<-budget_yr%>%filter(IP %in% ip_selected()$ips)%>%
      group_by(var,Year)%>%
      summarise(value=sum(value,na.rm=T))%>%
      mutate(value=dollar(value))%>%
      spread(var,value)
    }else{
      
      ds<-budget_yr%>%
        filter(IP %in% ip_selected()$ips)%>%
        left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
        group_by(var,Year,internal_external)%>%
        summarise(value=sum(value,na.rm=T))%>%
        mutate(value=dollar(value))%>%
        spread(var,value)
    }
    DT::datatable(ds)
  })
  
  
  schedule_overview<-reactive({
    schedule<-schedule%>%filter(IP %in% ip_selected()$ips)%>%
              left_join(all_proj%>%select(IP=IP,internal_external=`Internal or External`))%>%
              filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))
    
    if(input$selectdir=='All'){
      schedule<-schedule%>%
        filter(grepl('Go live',Major.Milestone,ignore.case=T))
    }
    
    return(schedule)
  })
  
  output$schedule_plt<-renderPlot({
    
    df<-schedule%>%filter(IP==ip_selected()$ip)
    
    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on project schedule')
    ))
    
    timeplot(df)
  })
  
  output$schedule_plt2<-renderPlot({
    
    withProgress(message='Plotting schedule',value=0, {
      
    df<-schedule_overview()%>%filter(!is.na(Approved_finish_date))

    shiny::validate((
      need(any(!is.na(df$Approved_finish_date)),'There is no information on project schedule')
    ))
    
    incProgress(0.5)
    
    if(input$internal=="No"){
    
    timeplot(df)
    # ggplotly(timeplot(df),height=450,tooltip=NULL)%>%
    #         layout(legend=list(orientation='h', y=-10,x=0.2))
      
    }else{
      
      timeplot(df)+
                   facet_grid(internal_external~.,shrink=FALSE)+ 
                   theme(panel.spacing.y = unit(2,"lines"))
      
      # ggplotly(timeline_plot,height=800, tooltip=NULL)%>%
      #   layout(legend=list(orientation='h',y=-10,x=0.2))
    }
    
    })
  })
  
  
  
  output$schedule_tb<-DT::renderDataTable({
    df<-schedule%>%filter(IP==ip_selected()$ip)%>%
        #filter(grepl('Start Date|End Date|Go live',Major.Milestone,ignore.case=T))%>%
        select(Milestone=Major.Milestone,
               `Baseline Finish Date`=Approved_finish_date,
               `Actual/Forecasted Finish Date`=Actual_date)
    
    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })
  
  
  output$schedule_tb2<-DT::renderDataTable({
    
    if(input$internal=='No'){
    df<-schedule_overview()%>%
      select(Milestone=Major.Milestone,
             `Baseline Finish Date`=Approved_finish_date,
             `Actual/Forecasted Finish Date`=Actual_date)
    }else{
      df<-schedule_overview()%>%
        select(Milestone=Major.Milestone,
               `Baseline Finish Date`=Approved_finish_date,
               `Actual/Forecasted Finish Date`=Actual_date,
               `Internal or External`=internal_external)
    
    }
    
    DT::datatable(df,options = list(dom = 'tip'), rownames = FALSE)
  })
  
  
  
  output$overall2<-renderPlotly({
    
    
    df<-all_proj%>%
      filter(`Overall Project Health`!='Blue')%>%
      filter(IP %in% ip_selected()$ips)%>%
      left_join(budget[,c('IP','Approved Budget')])
    
    df$status<-factor(df$status,levels=c('On Track','Caution','Elevated Risk'))
    
    
    if(input$internal=='No'){
    
    p<-status_plot(df)
    ggplotly(p,tooltip='text',height=400)
    
    }else{
      
      p<-status_plot(df)+
        facet_grid(`Internal or External`~.)+
        theme(panel.spacing.y = unit(2,"lines"))
      
      ggplotly(p,tooltip='text')
      
    }
    
  })
  
  
  output$ui_output1<-renderUI({
    fluidRow(
      box(title='Overall Project Health',plotlyOutput('overall2',height=plot_height())),
      box(title='Project Health and Current Stage',plotOutput('overall_stage2',height=plot_height()))
    )
  })
  
  output$ui_output2<-renderUI({
    fluidRow(
      box(title='Project Functionality',
          tabsetPanel(id='tabs',
                      tabPanel(title='Graph',
                               plotOutput("function_plt",height=450))
          )),
      box(title='Project Portfolio Budget',
          tabsetPanel(
            tabPanel(title='Breakdown by Year',
                     plotlyOutput('budget_plt2',height=450)),
            tabPanel(title='Table',
                     DT::dataTableOutput('budget_tbl2')),
            tabPanel(title='Projections',
                     plotOutput('budget_all2',height=450)))
          )
      )
    
  })
  
  
  output$ui_output3<-renderUI({
    fluidRow(
      box(title='Schedule',width=12,
          plotOutput('schedule_plt2',height=plot_height2()),
          br(),
          br(),
          DT::dataTableOutput('schedule_tb2'))
    )
    
  })
  
  
  
output$overall_stage2<-renderPlot({

    if(input$internal=='No'){
    df<-all_proj%>%
      filter(IP %in% ip_selected()$ips)%>%
      group_by(stage,status)%>%
      summarise(IP=paste(paste0('IP',IP),collapse='\n'),count=n())
    
    df$status<-factor(df$status,levels=c('On Track','Caution','Elevated Risk','Not yet started'))
    
    p<-stage_plot(df)
    
    }else{
    
      df<-all_proj%>%
        filter(IP %in% ip_selected()$ips)%>%
        group_by(stage,status,`Internal or External`)%>%
        summarise(IP=paste(paste0('IP',IP),collapse='\n'),count=n())
      
      df$status<-factor(df$status,levels=c('On Track','Caution','Elevated Risk','Not yet started'))
      
      p<-stage_plot(df)+ 
        facet_grid(`Internal or External`~.,shrink=FALSE,space='free_y')+
        theme(panel.spacing.y = unit(2,"lines"))
    }
    
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

 output$internal_external<-renderValueBox({
   internal<-all_proj%>%
     filter(IP ==input$selectip)%>%
     pull(`Internal or External`)
   
   valueBox(tags$p(internal, style = "font-size: 80%;"),
            subtitle='Internal or External ',color='blue',width=3)
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
      filename='report.pdf',
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('report.Rmd', pdf_document())
        file.rename(out, file)
        
      }
     
        )
     
    output$proj_risk_tb<-DT::renderDataTable({
      options<- list(pageLength=5,
                     scrollX=TRUE,
                     autoWidth=T,
                     columnDefs=list(list(width='500px',targets=2),
                                     list(width='50px',targets=3),
                                     list(width='50px',targets=4)))
      
      df<-proj_risk%>%filter(IP == input$selectip)%>%
                      select(3:7)
          datatable(df,options=options)%>%
          formatStyle('Probability',
                      backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                                 c( "#00B050", "#FFC000", "#C00000"))
                      )%>%
          formatStyle('Impact',
                        backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                                   c( "#00B050", "#FFC000", "#C00000"))) 
      
    })
    
    output$proj_issue_tb<-DT::renderDataTable({
      
      options<- list(pageLength=5,
                             scrollX=TRUE,
                             autoWidth=T,
                             columnDefs=list(list(width='500px',targets=2),
                                             list(width='40px',targets=3)))
      
      df<-proj_issue%>%filter(IP == input$selectip)%>%
        select(3:7)
      datatable(df,options=options)%>%
        formatStyle('Impact',
                    backgroundColor=styleEqual(c("Green","Yellow","Red"),
                                               c( "#00B050", "#FFC000", "#C00000"))
        )
      
    })
    
    output$projrisk<-renderPlot({
    
      shiny::validate({
        need(nrow(proj_risk%>%
                    filter(IP %in% ip_selected()$ips & !is.na(Risk)))>0,'Data Not Available')
      })
      
      
     proj_risk%>%
        filter(IP %in% ip_selected()$ips & !is.na(Risk))%>%
        count(Risk,sort=TRUE)%>%
        mutate(Risk=reorder(Risk,n))%>%
        ggplot(aes(x=Risk,y=n))+geom_col(fill='#1f77b4')+
        scale_y_continuous(breaks=c(0,2,4,6,8))+
        labs(x='',y='')+
        geom_text(aes(label=n,hjust=-1))+
        coord_flip()+
        theme_minimal()+
        theme(axis.title.x=element_blank(),
              axis.text.x =element_text(size=10),
              axis.text.y =element_text(size=11),
              axis.title.y =element_blank())   
        
       
    })
    
    
    
    output$projissue<-renderPlot({
      
      
      shiny::validate({
        need(nrow(proj_issue%>%
                    filter(IP %in% ip_selected()$ips & !is.na(Issue)))>0,'Data Not Available')
      })
    
    proj_issue%>%
        filter(IP %in% ip_selected()$ips & !is.na(Issue))%>%
        count(Issue,sort=TRUE)%>%
        mutate(Issue=reorder(Issue,n))%>%
        ggplot(aes(x=Issue,y=n))+geom_col(fill='#1f77b4')+
        scale_y_continuous(breaks=c(0,2,4,6,8))+
        labs(x='',y='')+
        geom_text(aes(label=n,hjust=-1))+
        coord_flip()+
        theme_minimal()+
        theme(axis.title.x=element_blank(),
              axis.text.x =element_text(size=10),
              axis.text.y =element_text(size=11),
              axis.title.y =element_blank())   
      
    })
    
    
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
