
ui<-dashboardPage(
  dashboardHeader(title='HPFB IP Dashboard',
                  titleWidth=400),
  
  dashboardSidebar(width=150,
                   sidebarMenu(id='sidebar',
                     menuItem('Overview',tabName='overview'),
                     menuItem('Individual',tabName='individual'),
                     
                     conditionalPanel(
                       condition="input.sidebar == 'individual' ",
                       selectInput('selectip',label="Select an IP project",choices=ip)
                     ),
                     conditionalPanel(
                       condition="input.sidebar == 'overview' ",
                       selectInput('selectdir',label="Select a Directorate",choices=directorate)
                     ),
                     
                     br(),br(),
                     tags$b(' Download:'),
                     br(),
                     br(),
                     downloadButton('downloadData','Data'),
                     br(),
                     br(),
                     downloadButton('downloadreport','Report')             
                     
                   )
                   
                     
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName='overview',
               fluidRow(
                        uiOutput('project_name2'),
               
               box(title='Overall Project Health',plotOutput('overall2')),
               box(title='Project Health and Current Stage',plotOutput('overall_stage2'))
               ),
              
              fluidRow(
               box(title='Project Functionality',
                           tabsetPanel(id='tabs',
                              tabPanel(title='Graph',
                                  plotlyOutput("function_plt"))
                           )),
               box(title='Project Portfolio Budget',
                       tabsetPanel(
                           tabPanel(title='Breakdown by Year',
                                    plotlyOutput('budget_plt2')),
                           tabPanel(title='Table',
                                    DT::dataTableOutput('budget_tbl2')),
                           tabPanel(title='Projections',
                                    plotOutput('budget_all2'))
                          )
                        )
              ),
              
              
              fluidRow(
                  box(title='Schedule',width=12,
                           plotlyOutput('schedule_plt2'),
                           br(),
                           br(),
                           DT::dataTableOutput('schedule_tb2'))
              )),
      
    
      tabItem(tabName='individual',
               
       fluidRow(width=12,
          uiOutput('project_name'),
          valueBoxOutput('overall'),
          valueBoxOutput('overall_stage')
     ),
     
     fluidRow(
  
      box(title='Project Functionality',
          tabsetPanel(id='tabs',
                      
              tabPanel(title='Table',
                       DT::dataTableOutput("function_tb"))
          )),
      
      box(title='Project Budget',
          tabsetPanel(
            tabPanel(title='Breakdown by Year',
                     plotlyOutput('budget_plt')),
            tabPanel(title='Table',
                     DT::dataTableOutput('budget_tbl')),
            tabPanel(title='Projections',
                     plotOutput('budget_all')))
            
       )),
     
    fluidRow(
      column(12,
      box(title='Schedule',width=NULL,
            plotlyOutput('schedule_plt'),
            br(),
            br(),
            DT::dataTableOutput('schedule_tb')))
     )
     
      )
    )
            
  )             
)