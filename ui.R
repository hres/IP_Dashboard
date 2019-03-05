
ui<-dashboardPage(
  dashboardHeader(title=textOutput('header'),
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
                       selectInput('selectdir',label="Select a Directorate",choices=directorate),
                       actionButton('info','View IP Name',icon=icon('eye'))
                     ),
                     
                     br(),br(),
                     tags$b('Download:',style="margin-left:10px;"),
                     br(),
                     br(),
                     tags$style(type="text/css", "#downloadData {color: black;margin-left:10px;}"),
                     downloadButton('downloadData','Data'),
                     br(),
                     br(),
                     tags$style(type="text/css", "#downloadreport {color: black;margin-left:10px;}"),
                     downloadButton('downloadreport','Report'),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     actionButton('contact','Contact us',icon=icon('phone')))
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName='overview',
               fluidRow(
                       div(style='display: inline-block;vertical-align:center;width:100px;', uiOutput('project_name2')),
                       div(style="display: inline-block;vertical-align:center; width: 300px;",HTML("<br>")),
                       bsModal('modal','IP Name','info',tableOutput('ip_tbl'))
                       
               ),
              
              fluidRow(
                box(title='Schedule',width=12,
                    plotlyOutput('schedule_plt2'),
                    br(),
                    br(),
                    br(),
                    br(),
                    DT::dataTableOutput('schedule_tb2'))
              ),
              
              fluidRow(
               box(title='Overall Project Health',plotlyOutput('overall2')),
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
              )
              
              # fluidRow(
              #   box(title='Project Risks',
              #       plotOutput('projrisk')),
              #   box(title='Project Issues',
              #       plotOutput('projissue'))
              # )
              ),
      
    
      tabItem(tabName='individual',
               
       fluidRow(width=12,
          uiOutput('project_name'),
          tags$style(".small-box.bg-red {background-color: #C00000 !important;}"),
          valueBoxOutput('overall'),
          valueBoxOutput('overall_stage')
          
     ),
     
     fluidRow(
       column(12,
              box(title='Schedule',width=NULL,
                  plotlyOutput('schedule_plt'),
                  br(),
                  br(),
                  DT::dataTableOutput('schedule_tb')))
     ),
     
     fluidRow(
  
      box(title='Project Functionality',height='500px',
          tabsetPanel(id='tabs',
                      
              tabPanel(title='Table',
                       DT::dataTableOutput("function_tb"))
          )),
      
      box(title='Project Budget',height='500px',
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
             box(title='Project Risks',width=NULL,
                 DT::dataTableOutput('proj_risk_tb')))
    ),
    
    fluidRow(
      column(12,
             box(title='Project Issues',width=NULL,
                 DT::dataTableOutput('proj_issue_tb')))
    )
    
     
      )
    )
            
  )             
)